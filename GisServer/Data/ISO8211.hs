module GisServer.Data.ISO8211 (ExchangeFile, LogicRecord, Leader, ddr, records) where

import Int
import Data.Binary
import Data.Binary.Get
import Data.Char
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B

data ExchangeFile =
  ExchangeFile {
    ddr :: LogicRecord,
    records :: [LogicRecord]
    } deriving (Eq, Show)

data LogicRecord = 
  LogicRecord {
    lr_leader :: Leader,
    lr_directory :: DirMap
    } deriving (Eq, Show)
  
type DirMap = M.Map String B.ByteString

class DDR r where 
  fieldcontrolField :: r -> ()
  
instance DDR LogicRecord where
  

data Leader = 
  Leader {
    leader_recordLength :: Int64,
    leader_interchangeLevel :: Char,
    leader_id :: Char,
    leader_extension :: Char,
    leader_version :: Char,
    leader_app :: Char,
    leader_fieldCtrlLen :: Int,
    leader_dataBase :: Int64,
    leader_charSet :: String,
    leader_entryMap :: EntryMap
    } deriving (Eq, Show)
               
data EntryMap = 
  EntryMap {
    em_sizeFieldLen :: Int,
    em_sizeFieldPos :: Int,
    em_sizeFieldTag :: Int
    } deriving (Eq, Show)
               
type Directory = [DirEntry]
type DirEntry = (String, Int64, Int64)


getDirectory :: EntryMap -> Get Directory
getDirectory m = do
  c <- lookAhead get
  if (c == fieldTerm) 
    then do skip 1
            return []
    else do e  <- getDirEntry m
            es <- getDirectory m
            return $ e : es 
    
getDirEntry :: EntryMap -> Get DirEntry
getDirEntry m = do
  tag <- getStringN $ em_sizeFieldTag m
  len <- getIntN $ em_sizeFieldLen m
  pos <- getIntN $ em_sizeFieldPos m  
  return (tag, fromIntegral pos, fromIntegral len)

getLogicRecords :: Get [LogicRecord] 
getLogicRecords = do
  e <- isEmpty
  if(e) 
    then do return []
    else do r <- get
            rs <- getLogicRecords
            return $ r : rs
    

instance Binary ExchangeFile where
  get = do
    ddr <- get
    drs <- getLogicRecords  
    return $ ExchangeFile ddr drs
  
instance Binary LogicRecord where
  get = do
    leader <- get
    dir <- getDirectory $ leader_entryMap leader
    let fieldAreaLen = (leader_recordLength leader) - (leader_dataBase leader)
    fields <- getLazyByteString fieldAreaLen
    let dir2lookup' = dir2lookup fields 
    return $ LogicRecord leader $ M.fromList (map dir2lookup' dir)

instance Binary Leader where
  get = do 
    recordLength <- getIntN 5
    interchangeLevel <- get
    id <- get
    extension <- get
    version <- get
    app <- get
    fieldCtrlLen' <- lookAhead get
    fieldCtrlLen <- if(isDigit fieldCtrlLen') 
                    then getIntN 2
                    else do skip 2
                            return 0
    dataBase <- getIntN 5
    charSet <- getStringN 3
    entryMap <- get
    return $ Leader (fromIntegral recordLength) interchangeLevel id extension version app fieldCtrlLen (fromIntegral dataBase) charSet entryMap
    

instance Binary EntryMap where
  get = do
    sizeFieldLen <- getIntN 1
    sizeFieldPos <- getIntN 1
    skip 1
    sizeFieldTag <- getIntN 1
    return $ EntryMap sizeFieldLen sizeFieldPos sizeFieldTag
  
fieldTerm = '\RS'
recordTerm = '\US'

ddrId = 'L'
drId = 'D'


dir2lookup :: B.ByteString -> DirEntry -> (String, B.ByteString) 
dir2lookup area (tag, pos, len) = (tag, 
                                   (B.take (fromIntegral len) 
                                    ((B.drop $ fromIntegral pos) area)))
  


isDDR (LogicRecord l _) = leader_id l == ddrId
isDR (LogicRecord l _) = leader_id l == drId                


getStringTill :: Char -> Get String
getStringTill c = do
  c' <- get
  if (c' == c)
    then do return [c']
    else do cs <- getStringTill c
            return $ c:cs

                 

getStringN :: Int -> Get String
getStringN n
  | n == 0    = do return []
  | n < 0     = fail "n must not be smaller 0"
  | otherwise = do
    c <- get
    s <- getStringN (n - 1)
    return $ c : s
  
getIntN :: Int -> Get Int  
getIntN n = do  
  bs <- fmap C.unpack $ getByteString n
  return $ read bs
  

module GisServer.Data.ISO8211 (ExchangeFile, LogicRecord, Leader, ddr, records) where

import Int
import Data.Binary
import Data.Binary.Get
import Data.Char
import Data.Bits
import qualified Data.Map as M
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
  
type DirMap = M.Map String (Maybe FieldControls, Field)
  

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
               
data Field = 
  Field {
    field_data :: [B.ByteString]
    } | 
  FieldCtrlField {
    fcf_fileTitle :: String,
    fcf_tagPairs :: [(String, String)]
    } | 
  DataDescriptiveField {
    ddf_name :: String,
    ddf_arrayDesc :: B.ByteString,
    ddf_formatCtrls :: B.ByteString
    }
  deriving (Eq, Show)
                          
           
data DataStructureCode = 
  FieldCtrlFieldStruct | LinearStruct | MultiDimensionalStructure
  deriving (Eq, Show, Ord, Enum)

instance Binary DataStructureCode where
  get = fmap toEnum $ getIntN 1  


data DataTypeCode =                               
  CharacterString | ImplicitPoint | BinaryForm | MixedDataTypes
  deriving (Eq, Show, Ord)                                                 
           
instance Enum DataTypeCode where
  fromEnum CharacterString = 0
  fromEnum ImplicitPoint = 1
  fromEnum BinaryForm = 5
  fromEnum MixedDataTypes = 6
  toEnum 0 = CharacterString
  toEnum 1 = ImplicitPoint
  toEnum 5 = BinaryForm
  toEnum 6 = MixedDataTypes
                                                 
instance Binary DataTypeCode where
  get = fmap toEnum $ getIntN 1  

data LexicalLevel = 
  LexLevel0 | LexLevel1 | LexLevel2                                
  deriving (Eq, Show, Ord, Enum)

instance Binary LexicalLevel where
  get = do
    escSeq <- getStringN 3
    return $ case escSeq of
      "   " -> LexLevel0
      "-A " -> LexLevel1
      "%/A" -> LexLevel2
 

data FieldControls = FieldControls {
  fc_dataStructCode :: DataStructureCode,
  fc_dataTypeCode :: DataTypeCode,
  fc_auxCtrl :: String,
  fc_printableGraphics :: String,
  fc_lexLevel :: LexicalLevel
  } deriving (Eq, Show)


instance Binary FieldControls where
  get = do
    structCode <- get
    typeCode <- get
    auxCtrl <- getStringN 2
    printableGraphics <- getStringN 2
    lexLevel <- get
    return $ FieldControls structCode typeCode auxCtrl printableGraphics lexLevel

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
    let dir2lookup' = dir2lookup fields (leader_fieldCtrlLen leader)
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
  

fieldTermChar = '\RS'
fieldTerm :: Word8
fieldTerm = fromIntegral $ ord fieldTermChar
recordTermChar = '\US'
recordTerm = fromIntegral $ ord recordTermChar

ddrId = 'L'
drId = 'D'


dir2lookup :: B.ByteString -> Int -> DirEntry -> (String, ((Maybe FieldControls), Field)) 
dir2lookup area fcl (tag, pos, len) = 
  let field = B.takeWhile ((/=) fieldTerm) $ B.take (fromIntegral len) 
              (B.drop (fromIntegral pos) area)
  in if (fcl > 0) 
     then let fctrls  = decode $ B.take (fromIntegral fcl) field
              lex = fc_lexLevel fctrls
              field' = B.drop (fromIntegral fcl) field
          in (tag, (Just fctrls, parseDDRField lex tag field'))
     else (tag, (Nothing, parseDRField field))



parseDRField :: B.ByteString -> Field
parseDRField field = Field $ B.split recordTerm field


parseDDRField :: LexicalLevel -> String -> B.ByteString -> Field
parseDDRField lex tag field = 
  let subfields = B.split recordTerm field
      splitTagPairs :: B.ByteString -> [(String,String)]
      splitTagPairs bs = 
        let (t1,t2s) = B.splitAt 4 bs
            (t2,ts) = B.splitAt 4 t2s
        in if(bs == B.empty) 
           then [] 
           else (byteS2String lex t1, byteS2String lex t2) : splitTagPairs ts
  in case tag of 
    "0000" -> FieldCtrlField (byteS2String lex (subfields !! 0)) (splitTagPairs $ subfields !! 1)
    otherwise -> DataDescriptiveField
                 (byteS2String lex (subfields !! 0))
                 (subfields !! 1)
                 (subfields !! 2)
    
                 
  

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
  bs <- fmap (byteS2String LexLevel0) $ getLazyByteString $ fromIntegral n
  return $ read bs
  

byteS2String enc bs = map (word8char enc) $ B.unpack bs

word8char :: LexicalLevel -> Word8 -> Char
word8char LexLevel0 w = chr.fromIntegral $ w .&. 0x7F
word8char LexLevel1 w = chr $ fromIntegral w
word8char LexLevel2 w = undefined


{-# OPTIONS -fglasgow-exts #-}

module GisServer.Data.Common ( LexicalLevel, lexLevel
                             , fieldTermChar, fieldTerm
                             , recordTermChar, recordTerm
                             , getStringTill, getStringTill'
                             , getStringN, getStringN', getInt
                             , getIntN, byteS2String, word8char) where


import Data.Binary
import Data.Binary.Get
import Data.Char
import Data.Bits       
import qualified Data.ByteString.Lazy as B

  
data LexicalLevel = 
  LexLevel0 | LexLevel1 | LexLevel2                                
  deriving (Eq, Show, Ord, Enum)
  
lexLevel :: Int -> LexicalLevel
lexLevel = toEnum           
  
fieldTermChar = '\RS'
fieldTerm :: Word8
fieldTerm = fromIntegral $ ord fieldTermChar
recordTermChar = '\US'
recordTerm :: Word8
recordTerm = fromIntegral $ ord recordTermChar

getStringTill :: Char -> Get String
getStringTill = getStringTill' LexLevel0
getStringTill' l c = do
  c' <- fmap (word8char l) get
  if (c' == c)
    then do return [c']
    else do cs <- getStringTill' l c
            return $ c:cs

                 

getStringN :: Int -> Get String
getStringN = getStringN' LexLevel0

getStringN' l n
  | n == 0    = do return []
  | n < 0     = fail "n must not be smaller 0"
  | otherwise = do
    c <- fmap (word8char l) get
    s <- getStringN' l (n - 1)
    return $ c : s
  
getIntN :: Int -> Get Int  
getIntN n = do  
  bs <- fmap (byteS2String LexLevel0) $ getLazyByteString $ fromIntegral n
  return $ read bs
  
getInt :: Bool -> Int -> Get Int       
getInt s 1 = 
  if (s) 
  then fmap fromIntegral getWord8
  else fmap (fromIntegral.(+ (-1)).complement) getWord8
getInt s 2 = 
  if (s) 
  then fmap fromIntegral getWord16le
  else fmap (fromIntegral.(+ (-1)).complement) getWord16le
getInt s 4 = 
  if (s) 
  then fmap fromIntegral getWord32le
  else fmap (fromIntegral.(+ (-1)).complement) getWord32le
       

byteS2String enc bs = map (word8char enc) $ B.unpack bs

word8char :: LexicalLevel -> Word8 -> Char
word8char LexLevel0 w = chr.fromIntegral $ w .&. 0x7F
word8char LexLevel1 w = chr $ fromIntegral w
word8char LexLevel2 w = undefined


readInt :: Bool -> Int -> Get Int       
readInt s 1 = 
  if (s) 
  then fmap fromIntegral getWord8
  else fmap (fromIntegral.(+ (-1)).complement) getWord8
readInt s 2 = 
  if (s) 
  then fmap fromIntegral getWord16le
  else fmap (fromIntegral.(+ (-1)).complement) getWord16le
readInt s 4 = 
  if (s) 
  then fmap fromIntegral getWord32le
  else fmap (fromIntegral.(+ (-1)).complement) getWord32le
       

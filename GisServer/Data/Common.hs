{-# OPTIONS -fglasgow-exts #-}

module GisServer.Data.Common ( LexicalLevel, lexLevel
                             , fieldTermChar, fieldTerm
                             , recordTermChar, recordTerm
                             , getStringN, getStringEncoded, getStringTill
                             , getInt, getIntN
                             ) where


import Data.Binary
import Data.Binary.Get
import Data.Char
import Int
import qualified Data.Bits as Bits      
import qualified Data.ByteString.Lazy as B

import Test.QuickCheck
import Data.Text as T


import  qualified  Data.Encoding as E
import Data.Encoding.ASCII
import Data.Encoding.UTF16
import Data.Encoding.ISO88591

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

getStringTill :: LexicalLevel -> Get String
getStringTill l = getStringTill' l fieldTerm

getStringTill' l c = undefined

--getStringTill = getStringTill' LexLevel0
--getStringTill' l c = do
--  c' <- fmap (word8char l) get
--  if (c' == c)
--    then do return [c']
--    else do cs <- getStringTill' l c
--            return $ c:cs

                 

getIntN :: Int -> Get Int  
getIntN n = fmap read $ getStringN LexLevel0 n

getStringN l n = fmap (getStringEncoded l) $ getLazyByteString $ fromIntegral n

getStringEncoded LexLevel0 = E.decodeLazyByteString ASCII
getStringEncoded LexLevel1 = E.decodeLazyByteString ISO88591
getStringEncoded LexLevel2 = E.decodeLazyByteString UTF16LE


getInt :: Bool -> Int -> Get Int       
getInt s 4 = 
  if (s) 
  then do  v <- fmap decomplement2 getWord32le
           return $ negate $ fromIntegral v
  else fmap fromIntegral getWord32le

prop_readInt8 :: Int8 -> Bool
prop_readInt8 i =
  let bs = encode i
      decode = runGet (getInt True 1) bs
  in decode == fromIntegral i
     
     
complement2 :: (Bits.Bits t) => t -> t
complement2 x = 1 + Bits.complement x

decomplement2 :: (Bits.Bits t) => t -> t
decomplement2 x = Bits.complement $ x - 1


instance Arbitrary Word64 where
  arbitrary = arbitrarySizedIntegral
instance Arbitrary Int8 where
  arbitrary = arbitrarySizedIntegral
  
  
prop_complement2 :: Word64 -> Bool
prop_complement2 x = x == decomplement2 (complement2 x)


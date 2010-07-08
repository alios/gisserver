{-# OPTIONS -fglasgow-exts #-}

module GisServer.Data.S57 () where

import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Tree
import qualified Data.Map as M
import Int
import Data.ByteString.Lazy
import GisServer.Data.Common
import GisServer.Data.ISO8211

data S57Data = UnsignedInt Int
             | SignedInt Int
             | ExplicitPoint Double
             | ImplicitPoint Int
             | CharData String
             | BitField ByteString

getExplicitPoint :: Int -> Get S57Data
getExplicitPoint n =
  do v <- getStringN (lexLevel 0) n
     return $ ExplicitPoint $ read v
     
getImplicitPoint :: Int -> Get S57Data
getImplicitPoint n =
  do v <- getIntN n
     return $ ImplicitPoint v

getSignedInt n = 
  do v <- getInt False n
     return $ SignedInt v

getUnsignedInt n = 
  do v <- getInt True n
     return $ SignedInt v

getCharData :: LexicalLevel -> Maybe Int -> Get S57Data
getCharData l (Just i) = 
  do s <- getStringN l i
     return $ CharData s
     
getCharData l Nothing = 
  do s <- getStringTill l recordTermChar
     return $ CharData s

getBitField bits =
  let needPad = (bits `mod` 8) /= 0
      bytes = bits `div` 8
      bytes' = if (needPad) then bytes + 1 else bytes
  in do bs <- getLazyByteString $ fromIntegral bytes'
        return $ BitField bs
     


fieldParser :: LogicRecord -> M.Map String (Get S57Data)
fieldParser r =
  let keys = M.keys $ dir
      dir = lr_directory r
      field k = snd $ fromJust $ M.lookup k dir
  in M.fromList []
     
       

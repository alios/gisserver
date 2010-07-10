
module Test where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Binary as B

import GisServer.Data.ISO8211
import GisServer.Data.S57

import Int 

main :: IO ()
main = do
  bs <- BS.readFile f1
  let r :: ExchangeFile
      r = B.decode bs
  print $ ddr r
  let rs = records r
  print rs
  --print $ take 5 $ filter (hasRecordField "DSPR") rs
  where f = "/home/alios/tmp/ENC_ROOT/US3AK21M/US3AK21M.000"
        f1 = "/home/alios/tmp/ENC_ROOT/CATALOG.031"

  
  
xxx :: Word8
xxx = -1

foo :: Word8 -> Int8
foo x = fromIntegral ((fromIntegral x) :: Word64)

{-# OPTIONS -fglasgow-exts #-}

module GisServer.Data.S57R312(fields, subfields) where

import Data.Maybe
import Data.Time
import Data.Binary
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import GisServer.Data.Common

{-|7.2.2.1 Data format
   Subfield data formats are specified by ISO/IEC 8211. The allowable data 
   formats are as follows:
-}
data DataFormat = 
  CharacterData Length
  | ImplicitPoint Length
  | ExplicitPoint Length
  | BitString Length
  
  -- | 'SubfieldLabel' is a row heading for a 2-D array or table of known length
  | SubfieldLabel 
  
  -- | 'UnsignedInteger' is a binary integer. Length must be 1, 2 or 4.
  | UnsignedInteger FixedLength
  
  -- | 'SignedInteger' is a two's complement binary integer. 
  --   Length must be 1,2 or 4
  | SignedInteger FixedLength
  deriving (Show, Read, Eq)

           
asciiFormatParser :: LexicalLevel -> AsciiFormat -> Get S57Data
asciiFormatParser l (format, domain) =
  let (get', con) = case (format) of
        (CharacterData (Just len)) -> (getStringN l len, S57String) 
        (CharacterData Nothing) -> (getStringTill l, S57String) 
      p = case (domain) of
        otherwise -> id
  in fmap (con.p) get'



{-|An extend of X(n) ('Just') indicates a fixed length subfield of 
   length n (in bytes).  
   An extent of X() ('Nothing') indicates a variable length subfield
   terminated by the appropriate delimter (see clause 2.5)
-}
type Length = Maybe FixedLength
type FixedLength = Int

{-|The width of a fixed length bit subfield must be specified in bits.
   If necessary, the last byte of a fixed length bit subfield must be
   filled on the right with binary zero's.
-}
type Width = Int



{-|7.2.2.2 Permitted S-57 (ASCII) Data domains
 
   The domain for ASCII data is specified by a domain code. The following 
   domain codes are used in the field tables:
-}
data AsciiDomain = 
  BasicTextDomain
  | GeneralTextDomain
  | DigitsDomain
  | DateDomain
  | IntegerDomain
  | RealDomain
  | AlphaNumericDomain
  | HexDomain
  deriving (Show, Read, Enum, Eq)
           
data S57Data =
  S57String String
  | S57Integer Integer
  | S57Real Double
  | S57Date UTCTime
  | S57Bytes B.ByteString
  deriving (Eq, Show)
           
type AsciiFormat = (DataFormat, AsciiDomain)           
          
                          
                   
                   
data Field =
  Field { 
    fieldLabel :: String, 
    fieldName ::  String, 
    fieldSubFields :: [Subfield]
    } deriving (Show, Eq)
           
newField lbl name sfs =
  let sfs' = [ fromJust $ M.lookup sf subfields | sf <- sfs]
  in Field lbl name sfs'



data Subfield = 
  Subfield {
    subfieldLabel :: String,
    subfieldAsciiFormat :: AsciiFormat,
    subfieldBinFormat :: Maybe DataFormat,
    subfieldName :: String    
    } deriving (Show, Eq)
    
basicString lbl name = 
  Subfield lbl (CharacterData Nothing, BasicTextDomain) Nothing name

date lbl name = 
  Subfield lbl (CharacterData $ Just 8, DateDomain) Nothing name

uIntegerN lbl name al bl
   | ((bl < 1) || (bl > 4)) = error $ "bl must be in [1..4] but is " ++ show bl 
   | (al < 0) = error $ "al must be greater 0 but is " ++ show al
   | otherwise = Subfield lbl (ImplicitPoint $ Just al, IntegerDomain) 
                 (Just $ UnsignedInteger bl) name

uInteger lbl name bl
 | ((bl < 1) || (bl > 4)) = error $ "bl must be in [1..4] but is " ++ show bl 
 | otherwise = Subfield lbl (ImplicitPoint Nothing, IntegerDomain) 
               (Just $ UnsignedInteger bl) name

real lbl name = 
  Subfield lbl (ExplicitPoint Nothing, RealDomain) Nothing name

realN lbl name al
  | (al < 0) = error $ "l must be greater 0 but is" ++ show al
  | otherwise = Subfield lbl (ExplicitPoint $ Just al, RealDomain) Nothing name

mixed lbl name al bl
  | ((bl < 1) || (bl > 4)) = error $ "bl must be in [1..4] but is " ++ show bl 
  | (al < 0) = error $ "al must be greater 0 but is " ++ show al
  | otherwise = Subfield lbl (alNumCharData al) (unsignedInt bl) name
    where alNumCharData l = (CharacterData $ Just l, AlphaNumericDomain)
          unsignedInt l = Just $ UnsignedInteger l
  
subfields = M.fromList [ (subfieldLabel sf, sf) | sf <- subfields' ]
subfields' = 
  [ mixed       "RCNM" "Record Name" 2 1
  , uIntegerN   "RCID" "Record identification number" 10 4
  , mixed       "EXPP" "Exchange Purpose" 1 1
  , uIntegerN   "INTU" "Intendes usage" 1 1
  , basicString "DSNM" "Data set name"
  , basicString "EDTN" "Edition number"
  , basicString "UPDN" "Update Number"
  , date        "UADT" "Update application date"
  , date        "ISDT" "Issue date"
  , realN       "STED" "Edition number of S-57" 4
  , mixed       "PRSP" "ProductSpecification" 3 1
  , basicString "PSDN" "Product specification description"
  , basicString "PRED" "Product specification edition number"
  , mixed       "PROF" "Application profile identification" 2 1
  , mixed       "AGEN" "Producing agency" 2 2
  , basicString "COMT" "Comment" 
  , mixed       "DSTR" "Data structure" 2 1
  , uIntegerN   "AALL" "ATTF lexical elevel" 1 1
  , uIntegerN   "NALL" "NATF lexical level" 1 1
  , uInteger    "NOMR" "Number of meta records" 4    
  , uInteger    "NOCR" "Number of cartographic records" 4
  , uInteger    "NOGR" "Number of geo records" 4
  , uInteger    "NOLR" "Number of collection records" 4  
  , uInteger    "NOIN" "Number of isolated node records" 4
  , uInteger    "NOCN" "Number of connected node records" 4
  , uInteger    "NOED" "Number of edge records" 4
  , uInteger    "NOFA" "Number of face records" 4
  ]
  
fields = M.fromList [(fieldLabel f , f) | f <- fields'] 
fields' =
  [ newField "DSID" "Data Set Identification" 
    ["AGEN","COMT","DSNM","EDTN","EXPP","INTU","ISDT","PRED","PROF","PRSP"
    ,"PSDN","RCID","RCNM","STED","UADT","UPDN"]
  , newField "DSSI" "Data Set Structure information" 
    ["DSTR","AALL","NALL","NOMR","NOCR","NOGR", "NOLR","NOIN","NOCN","NOED"
    , "NOFA"]
  ]
    

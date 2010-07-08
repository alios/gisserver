module GisServer.Data.S57R312 where

import Data.Maybe
import qualified Data.Map as M

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
  
  -- | 'UnsignedInteger' is a binary integer. Length must be 1,2 or 4.
  | UnsignedInteger FixedLength
  
  -- | 'SignedInteger' is a two's complement binary integer. 
  --   Length must be 1,2 or 4
  | SignedInteger FixedLength
  deriving (Show, Read, Eq)


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
 
   The domain for ASCII data is specified by a domain code. The following domain
   codes are used in the field tables:
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
    subfieldAsciiFormat :: DataFormat,
    subfieldBinFormat :: Maybe DataFormat,
    subfieldDomain :: AsciiDomain,
    subfieldName :: String
    } deriving (Show, Eq)
    
basicString lbl name = 
  Subfield lbl (CharacterData Nothing) Nothing BasicTextDomain name
date lbl name = 
  Subfield lbl (CharacterData $ Just 8) Nothing DateDomain name
integerN lbl name al bl= 
  Subfield lbl (ImplicitPoint $ Just al) (Just $ UnsignedInteger bl) IntegerDomain name
integer lbl name bl= 
  Subfield lbl (ImplicitPoint Nothing) (Just $ UnsignedInteger bl) IntegerDomain name
  
                
subfields = M.fromList [ (subfieldLabel sf, sf) | sf <- subfields']
                
subfields' = 
  [ Subfield "RCNM" (CharacterData $ Just  2) (Just $ UnsignedInteger 1) AlphaNumericDomain "Record Name"
  , integerN "RCID" "Record identification number" 10 4
  , Subfield "EXPP" (CharacterData $ Just  1) (Just $ UnsignedInteger 1) AlphaNumericDomain "Exchange Purpose"
  , integerN "INTU" "Intendes usage" 1 1
  , basicString "DSNM" "Data set name"
  , basicString "EDTN" "Edition number"
  , basicString "UPDN" "Update Number"
  , date "UADT" "Update application date"
  , date "ISDT" "Issue date"
  , Subfield "STED" (ExplicitPoint $ Just  4) Nothing RealDomain "Edition number of S-57"
  , Subfield "PRSP" (CharacterData $ Just  3) (Just $ UnsignedInteger 1) AlphaNumericDomain "ProductSpecification"
  , basicString "PSDN" "Product specification description"
  , basicString "PRED" "Product specification edition number"
  , Subfield "PROF" (CharacterData $ Just  2) (Just $ UnsignedInteger 1) AlphaNumericDomain "Application profile identification"
  , Subfield "AGEN" (CharacterData $ Just  2) (Just $ UnsignedInteger 2) AlphaNumericDomain "Producing agency"
  , basicString "COMT" "Comment" 
  , Subfield "DSTR" (CharacterData $ Just  2) (Just $ UnsignedInteger  1) AlphaNumericDomain "Data structure"
  , integerN "AALL" "ATTF lexical elevel" 1 1
  , integerN "NALL" "NATF lexical level" 1 1
  , integer "NOMR" "Number of meta records" 4    
  , integer "NOCR" "Number of cartographic records" 4
  , integer "NOGR" "Number of geo records" 4
  , integer "NOLR" "Number of collection records" 4  
  , integer "NOIN" "Number of isolated node records" 4
  , integer "NOCN" "Number of connected node records" 4
  , integer "NOED" "Number of edge records" 4
  , integer "NOFA" "Number of face records" 4
  ]
  
fields = M.fromList [(fieldLabel f , f) | f <- fields'] 

fields' =
  [ newField "DSID" "Data Set Identification" ["AGEN","COMT","DSNM","EDTN","EXPP","INTU","ISDT","PRED","PROF","PRSP","PSDN","RCID","RCNM","STED","UADT","UPDN"]
  , newField "DSSI" "Data Set Structure information" ["DSTR", "AALL", "NALL", "NOMR", "NOCR", "NOGR", "NOLR", "NOIN", "NOCN", "NOED", "NOFA"]
  ]
    

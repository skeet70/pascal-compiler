--Intermediate Code Helper Functions
--
--Authored by: Murph Murphy
--
--Edited: April 23, 2013

module IntermediateCode.IRHelpers where

-- Datatype to contain IR information. Contains a counter to be incremented to
-- keep track of label numbers, as well as a flag to be switched if the next
-- operation needs to use float/fixed numbers instead of integers.
data SemanticRecord = SemanticRecord { labelNumber :: Int
                                     , isFloat :: Bool
                                     , idType :: String
                                     , crement :: String
                                     , isBool :: Bool
                                    } deriving (Show)
--Intermediate Code Helper Functions
--
--Authored by: Murph Murphy
--
--Edited: April 23, 2013

module IntermediateCode.IRHelpers where

data SemanticRecord = SemanticRecord { labelNumber :: Int
                                     , isFloat :: Bool
                                    } deriving (Show)
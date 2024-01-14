module Parser.Parser
    ( parser
    ) where

import AST.Constants
import Lib

parser :: String -> Maybe [Chiasse]
parser s =
  case parseChiasse s of
    Just (result, "") -> Just result
    Just (result, rest) -> case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') rest of
        Just (_, "") -> Just result
        _ -> Nothing
    Nothing -> Nothing
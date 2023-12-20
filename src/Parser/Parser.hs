module Parser.Parser
    ( parser
    ) where

import AST.Constants
import Lib -- import the parser

parser :: String -> Maybe SExpr
parser s =
  case parseList parseSExpr s of
    Just (result, _) -> Just (SList result)
    Nothing -> Nothing
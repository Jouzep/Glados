module Parser.Parser
    ( parser
    ) where

import Lib -- import the parser

parser :: String -> Maybe SExpr
parser s =
  case parseList parseSExpr s of
    Just (result, _) -> Just (ListExpress result)
    Nothing -> Nothing
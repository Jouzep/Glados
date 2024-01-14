module Lib
    (
      parseMany,
      parseChar,
      parseOr,
      parseChiasse,
      parseFunctionCall,
      parseList,
    ) where

import AST.Constants

parseChar :: Char -> Parser Char
parseChar c (x : xs) | c == x = Just (c, xs)
parseChar _ _ = Nothing
_ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar (x : xs) (y : ys)
  | x == y = Just (x, ys)
  | otherwise = parseAnyChar xs (y : ys)
parseAnyChar _ _ = Nothing
_ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr parser1 parser2 input =
  case parser1 input of
    Just result -> Just result
    Nothing -> parser2 input
_ = Nothing

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd parser1 parser2 input =
  case parser1 input of
    Just (result1, rest1) ->
      case parser2 rest1 of
        Just (result2, rest2) -> Just ((result1, result2), rest2)
        Nothing -> Nothing
    Nothing -> Nothing
_ = Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f parser1 parser2 input =
  case parseAnd parser1 parser2 input of
    Just ((result1, result2), rest) -> Just (f result1 result2, rest)
    Nothing -> Nothing
_ = Nothing
parseMany :: Parser a -> Parser [a]
parseMany parser input =
  case parser input of
    Just (result, rest) ->
      case parseMany parser rest of
        Just (results, rest') -> Just (result : results, rest')
        Nothing -> Just ([result], rest)
    Nothing -> Just ([], input)
_ = Nothing

parseSome :: Parser a -> Parser [a]
parseSome parser input =
  case parseMany parser input of
    Just (results, rest) | null results -> Nothing
                         | otherwise -> Just (results, rest)
    Nothing -> Nothing
_ = Nothing

parseUInt :: Parser Int -- parse an unsigned Int
parseUInt input =
  case parseSome (parseAnyChar ['0'..'9']) input of
    Just (results, rest) -> Just (read results, rest)
    Nothing -> Nothing
_ = Nothing

parseInt :: Parser Int -- parse a signed Int
parseInt input =
  case parseChar '-' input of
    Just (_, rest) ->
      case parseUInt rest of
        Just (result, rest') -> Just (-result, rest')
        Nothing -> Nothing
    Nothing ->
      case parseUInt input of
        Just (result, rest) -> Just (result, rest)
        Nothing -> Nothing
_ = Nothing


parsePair :: Parser a -> Parser (a , a) -- parse a pair as a tuple
parsePair parser input =
  case parseChar '(' input of
    Just (_, rest) ->
      case parseAndWith (,) parser (parseChar ' ') rest of
        Just ((result1, _), rest') ->
          case parseAndWith (,) parser (parseChar ')') rest' of
            Just ((result2, _), rest'') -> Just ((result1, result2), rest'')
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing
_ = Nothing

parseList :: Parser a -> Parser [a] -- parse a list
parseList parser input =
  case parseChar '(' input of
    Just (_, rest) ->
      case parseMany (parseAndWith const parser (parseMany (parseChar ','))) rest of
        Just (results, rest') -> case parseMany (parseAndWith const (parseChar ')') (parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t'))) rest' of
            Just (_, rest'') -> Just (results, rest'')
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing
_ = Nothing

parseCond :: Parser a -> Parser [a]
parseCond parser input =
  case parseChar '(' input of
    Just (_, rest) -> case parseMany (parseAndWith const parser (parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t'))) rest of
        Just (results, rest') -> case parseMany (parseAndWith const (parseChar ')') (parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t'))) rest' of
            Just (_, rest'') -> Just (results, rest'')
            Nothing -> Nothing
        Nothing -> Nothing

parseLine :: Parser a -> Parser [a] -- parse a line
parseLine parser input =
  case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') input of
    Just (_, rest) -> case parseMany (parseAndWith const parser (parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t'))) rest of
        Just (results, rest') -> case parseChar ';' rest' of
            Just (_, rest'') -> Just (results, rest'')
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

parseBody :: Parser [Chiasse]
parseBody input =
  case parseChar '{' input of
    Just (_, rest) ->
      case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') rest of
        Just (_, rest') -> case parseChiasse rest' of
          Just (results, rest'') -> case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') rest'' of
            Just (_, rest''') -> case parseChar '}' rest''' of
              Just (_, rest'''') -> Just (results, rest'''')
              Nothing -> Nothing
            Nothing -> Nothing
          Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

parseChiasse :: Parser [Chiasse]
parseChiasse "" = Nothing
parseChiasse input =
  case parseFunction input of
    Just (result, rest) -> case parseChiasse rest of
      Just(result', rest') -> Just (CFunction result : result', rest')
      Nothing -> Just ([CFunction result], rest)
    Nothing -> case parseLine (parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+*-/%=<>&|#"))) input of
        Just (result, rest) -> case parseChiasse rest of
          Just(result', rest') -> Just (CLine result : result', rest')
          Nothing -> Just ([CLine result], rest)
        Nothing -> case parseFunctionCall input of
          Just (result, rest) -> case parseChiasse rest of
            Just(result', rest') -> Just (CCall result : result', rest')
            Nothing -> Just ([CCall result], rest)
          Nothing -> case parseIf input of
            Just (result, rest) -> case parseChiasse rest of
              Just(result', rest') -> Just (CIf result : result', rest')
              Nothing -> Just ([CIf result], rest)
            Nothing -> Just ([], input)

parseIf :: Parser CCond
parseIf input =
  case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') input of
    Just (_, prevRest) -> case parseMany (parseAnyChar "if") prevRest of
      Just ("if", rest) -> case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') rest of
        Just (_, rest') -> case parseCond (parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']++  "+*-/%=<>&|#"))) rest' of
          Just (results, rest'') -> case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') rest'' of
            Just (_, rest''') -> case parseBody rest''' of
              Just (results', rest'''') -> case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') rest'''' of
                Just (_, rest''''') -> case parseMany (parseAnyChar "else") rest''''' of
                  Just ("else", rest'''''') -> case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') rest'''''' of
                    Just (_, rest''''''') -> case parseBody rest''''''' of
                      Just (results'', rest'''''''') -> Just ((results, results', results''), rest'''''''')
                      Nothing -> Nothing
                    Nothing -> Nothing
                  _ -> Just ((results, results', []), rest''''')
                Nothing -> Just ((results, results', []), rest'''')
              Nothing -> Nothing
            Nothing -> Nothing
          Nothing -> Nothing
        Nothing -> Nothing
      _ -> Nothing
    Nothing -> Nothing
_ = Nothing


parseFunction :: Parser Func
parseFunction input =
  case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') input of
    Just (_, rest) -> case parseMany (parseAndWith const (parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z']))) (parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t'))) rest of
        Just (results, rest') -> case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') rest' of
          Just (_, rest'') -> case  parseList (parseSome (parseAnyChar (['a'..'z']))) rest'' of
            Just (results', rest''') -> case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') rest''' of
              Just (_, rest'''') -> case parseBody rest'''' of
                Just (results'', rest''''') -> Just ((results, results', results''), rest''''')
                Nothing -> Nothing
              Nothing -> Nothing
            Nothing -> Nothing
          Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing
_ = Nothing

parseFunctionCall :: Parser Call
parseFunctionCall input =
  case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') input of
    Just (_, rest) -> case parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'])) rest of
        Just (results, rest') -> case parseMany (parseChar ' ' `parseOr` parseChar '\n' `parseOr` parseChar '\t') rest' of
          Just (_, rest'') -> case parseList (parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+*-/%=<>&|#"))) rest'' of
            Just (results', rest''') -> case parseChar ';' rest''' of
              Just (_, rest'''') -> Just ((results, results'), rest'''')
              Nothing -> Nothing
            Nothing -> Nothing
          Nothing -> Nothing
        Nothing -> Nothing
import Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken str | str == "+" = Just Plus
            | str == "-" = Just Minus
            | str == ")" = Just RightBrace
            | str == "(" = Just LeftBrace
            | all (isDigit) str = Just (Number (read str::Int))
            | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize str = sequence tokenList where
  tokenList = map asToken (words str)

test = tokenize "1 + ( 7 - 2 )"

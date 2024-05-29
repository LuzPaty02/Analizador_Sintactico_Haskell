import System.Environment (getArgs)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)

-- Define token types
data TokenType = Variable | Assignment | Integer | Real | Operator String | Parenthesis String | Comment | Keyword String | Keys String
    deriving (Show, Eq)

-- Define a token data structure to hold the token value and its type
data Token = Token { tokenType :: TokenType, tokenValue :: String } deriving Show

-- Tokenize a string into individual tokens
tokenize :: String -> Maybe [Token]
tokenize [] = Just []  -- Return empty list for empty input
tokenize ('/':'/':rest) = Just [Token Comment rest]  -- Comment token
tokenize s@(x:xs)
    | isSpace x = tokenize xs
    | isAlpha x = let (token, rest) = span (\c -> isAlphaNum c || c == '_') s
                  in (getToken token >>= \t -> fmap (t:) (tokenize rest))
    | isDigit x || x == '.' || (x == '-' && (not (null xs) && (isDigit (head xs) || head xs == '.'))) =
        let (token, rest) = span (\c -> isDigit c || c == '.' || c == 'E' || c == '-' || c == '+') s
        in (getToken token >>= \t -> fmap (t:) (tokenize rest))
    | x `elem` "+-*/^=;" = fmap (\t -> Token (Operator [x]) [x] : t) (tokenize xs)
    | x `elem` "()=" = fmap (\t -> Token (Parenthesis [x]) [x] : t) (tokenize xs)
    | x `elem` "{}" = fmap (\t -> Token (Keys [x]) [x] : t) (tokenize xs)
    | otherwise = Nothing  -- Indicate failure for unexpected token

-- Get the token type for a token value
getToken :: String -> Maybe Token
getToken tokenValue
    | all isDigitOrDot tokenValue =
        if '.' `elem` tokenValue || 'E' `elem` tokenValue then Just $ Token Real tokenValue else Just $ Token Integer tokenValue
    | otherwise = case tokenValue of
                    "=" -> Just $ Token Assignment tokenValue
                    "Programa" -> Just $ Token (Keyword "Programa") tokenValue
                    "principal" -> Just $ Token (Keyword "principal") tokenValue
                    "Entero" -> Just $ Token (Keyword "Entero") tokenValue
                    "Real" -> Just $ Token (Keyword "Real") tokenValue
                    _ -> Just $ Token Variable tokenValue

-- Check if a character is a digit, dot, or hyphen
isDigitOrDot :: Char -> Bool
isDigitOrDot c = isDigit(c) || c `elem` ".-E+;"

-- Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            content <- readFile fileName
            putStrLn $ "Token" ++ replicate 11 ' ' ++ "Tipo"
            putStrLn $ replicate 30 '-'
            case mapM tokenize (lines content) of
                Just tokens -> do
                    mapM_ printTokens tokens
                    putStrLn "\nSyntax analysis completed successfully."
                Nothing -> putStrLn "Syntax error detected."
        _ -> putStrLn "Usage: programName fileName"

-- Print list of tokens with their types
printTokens :: [Token] -> IO ()
printTokens tokens = mapM_ printToken tokens

-- Print token with its TokenType
printToken :: Token -> IO ()
printToken (Token tokenType tokenValue) = do
    putStrLn $ padRight 15 tokenValue ++ "\t" ++ showTokenType tokenType

-- Convert TokenType to string representation
showTokenType :: TokenType -> String
showTokenType tokenType = case tokenType of
                            Operator op -> operatorType op
                            Integer -> "Entero"
                            Parenthesis pa -> parentesisType pa
                            Comment -> "Comentario"
                            Keyword kw -> kw
                            Keys kw -> keyType kw
                            _ -> show tokenType  -- For all other types, use the default show instance

-- Get the specific operation name for an operator token
operatorType :: String -> String
operatorType "+" = "Suma"
operatorType "-" = "Resta"
operatorType "*" = "Multiplicación"
operatorType "/" = "División"
operatorType "=" = "Asignación"
operatorType "^" = "Potencia"
operatorType ";" = "Punto y coma"
operatorType other = other  -- Default to the original operator symbol for any other operator

-- Get the specific operation name for an operator token
parentesisType :: String -> String
parentesisType "(" = "Paréntesis que abre"
parentesisType ")" = "Paréntesis que cierra"
parentesisType other = other  -- Default to the original operator symbol for any other operator

-- Get the specific operation name for an operator token: Keys
keyType :: String -> String
keyType "{" = "Llave que abre"
keyType "}" = "Llave que cierra"
keyType other = other  -- Default to the original operator symbol for any other operator

-- Helper function to pad a string to the right with spaces
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

import System.Environment (getArgs)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)

-- Define token types
data TokenType = Variable | Assignment | Integer | Real | Operator String | Parenthesis String | Comment | Keyword String | Keys String
    deriving (Show, Eq)

-- Define a token data structure to hold the token value and its type
  
data Token = Token { tokenType :: TokenType, tokenValue :: String } deriving Show
-- Tokenize a string into individual tokens
tokenize :: String -> [Token]
tokenize [] = []
tokenize ('/':'/':rest) = [Token Comment rest]  -- Comment token
tokenize s@(x:xs)
    | isSpace x = tokenize xs
    | isAlpha x = let (token, rest) = span (\c -> isAlphaNum c || c == '_') s
                  in getToken token : tokenize rest
    | isDigit x || x == '.' || (x == '-' && (not (null xs) && (isDigit (head xs) || head xs == '.'))) =
        let (token, rest) = span (\c -> isDigit c || c == '.' || c == 'E' || c == '-' || c == '+') s
        in getToken token : tokenize rest
    | x `elem` "+-*/^=;" = Token (Operator [x]) [x] : tokenize xs
    | x `elem` "()=" = Token (Parenthesis [x]) [x] : tokenize xs
    | x `elem` "{}" = Token (Keys [x]) [x] : tokenize xs
    | otherwise = error $ "Unexpected token at: " ++ s

-- Get the token type for a given token value  
getToken :: String -> Token
getToken tokenValue
    | all isDigitOrDot tokenValue =
        if '.' `elem` tokenValue || 'E' `elem` tokenValue then Token Real tokenValue else Token Integer tokenValue
    | otherwise = case tokenValue of
                    "=" -> Token Assignment tokenValue
                    "Programa" -> Token (Keyword "Programa") tokenValue
                    "principal" -> Token (Keyword "principal") tokenValue
                    "Entero" -> Token (Keyword "Entero") tokenValue
                    "Real" -> Token (Keyword "Real") tokenValue
                    _ -> Token Variable tokenValue
-- Check if a character is a digit, dot, or hyphen
isDigitOrDot :: Char -> Bool
isDigitOrDot c = isDigit c || c == '.' || c == '-' || c == 'E' || c == '+'

-- Main function
main :: IO ()
main = do  
    args <- getArgs
    case args of
        [fileName] -> do
            content <- readFile fileName
            putStrLn $ "Token" ++ replicate 11 ' ' ++ "Tipo"
            putStrLn $ replicate 30 '-'
            let tokens = concatMap tokenize (lines content)
            mapM_ printToken tokens
        _ -> putStrLn "Usage: programName fileName"

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
                            Comment -> "Commentario"
                            Keyword kw -> kw
                            Keys kw -> keyType kw
                            Variable -> "Variable"
                            _ -> show tokenType  -- For all other types, use the default show instance
  
operatorType :: String -> String
operatorType "+" = "Suma"
operatorType "-" = "Resta"
operatorType "*" = "Multiplicacion"
operatorType "/" = "Division"
operatorType "=" = "Asignacion"
operatorType ";" = "Punto y coma"
operatorType other = other  -- Default to the original operator symbol for any other operator
  
parentesisType :: String -> String
parentesisType "(" = "Parentesis que abre"
parentesisType ")" = "Parentesis que cierra"
parentesisType other = other  -- Default to the original operator symbol for any other operator
-- Get the specific operation name for an operator token: Keys
keyType :: String -> String
keyType "{" = "Llave que abre"
keyType "}" = "Llave que cierra"
keyType other = other  -- Default to the original operator symbol for any other operator

-- Helper function to pad a string to the right with spaces
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '
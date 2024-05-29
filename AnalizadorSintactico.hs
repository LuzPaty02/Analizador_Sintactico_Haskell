import System.Environment (getArgs)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import Debug.Trace (trace)

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
    | x `elem` " {}" = Token (Keys [x]) [x] : tokenize xs
    | otherwise = error $ "Unexpected token at: " ++ s

-- Get the token type for a token value
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
isDigitOrDot c = isDigit(c) || c == '.' || c == '-' || c == 'E' || c == '+' || c == ';'

parseProgram :: [Token] -> Maybe [Token]
parseProgram tokens = trace ("parseProgram: " ++ show tokens) $
    case tokens of
        (Token (Keyword "Programa") _ : Token (Keys "{") _ : rest) ->
            case parsePrincipal rest of
                Just rest' -> case rest' of
                    (Token (Keys "}") _ : rest'') -> Just rest''
                    _ -> trace ("Expected closing brace for Programa block, found: " ++ show rest') Nothing
                Nothing -> trace "Error in parsing principal block" Nothing
        _ -> trace ("Expected 'Programa' keyword and '{' to start, found: " ++ show tokens) Nothing

parsePrincipal :: [Token] -> Maybe [Token]
parsePrincipal tokens = trace ("parsePrincipal: " ++ show tokens) $
    case tokens of
        (Token (Keyword "principal") _ : Token (Keys "{") _ : rest) ->
            case parseStatements rest of
                Just rest' -> case rest' of
                    (Token (Keys "}") _ : rest'') -> Just rest''
                    _ -> trace ("Expected closing brace for principal block, found: " ++ show rest') Nothing
                Nothing -> trace "Error in parsing statements in principal block" Nothing
        _ -> trace ("Expected 'principal' keyword and '{' to start, found: " ++ show tokens) Nothing

parseStatements :: [Token] -> Maybe [Token]
parseStatements tokens = trace ("parseStatements: " ++ show tokens) $
    case parseAssignment tokens of
        Just (Token (Operator ";") _ : rest) -> parseStatements rest
        Just rest -> Just rest
        Nothing -> Just tokens

parseAssignment :: [Token] -> Maybe [Token]
parseAssignment tokens = trace ("parseAssignment: " ++ show tokens) $
    case tokens of
        (Token Variable _ : Token Assignment _ : rest) -> parseExpression rest
        (Token (Keyword tipo) _ : Token Variable _ : Token Assignment _ : rest)
            | tipo `elem` ["Entero", "Real"] -> parseExpression rest
        _ -> trace ("Expected variable or type declaration, found: " ++ show tokens) Nothing

parseExpression :: [Token] -> Maybe [Token]
parseExpression tokens = trace ("parseExpression: " ++ show tokens) $
    parseTerm tokens

parseTerm :: [Token] -> Maybe [Token]
parseTerm tokens = trace ("parseTerm: " ++ show tokens) $
    case tokens of
        (Token Variable _ : rest) -> Just rest
        (Token Integer _ : rest) -> Just rest
        (Token Real _ : rest) -> Just rest
        (Token (Parenthesis "(") _ : rest) ->
            case parseExpression rest of
                Just (Token (Parenthesis ")") _ : rest') -> Just rest'
                _ -> trace ("Expected closing parenthesis in term, found: " ++ show rest) Nothing
        _ -> trace ("Invalid term, found: " ++ show tokens) Nothing

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
            putStrLn "\n"
            case parseProgram tokens of
                Just _  -> putStrLn "Syntax analysis completed successfully."
                Nothing -> putStrLn "Syntax error detected."
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
operatorType "^"= "Potencia"
operatorType other = other -- Default to the original operator symbol for any other operator

-- Get the specific operation name for an operator token
parentesisType :: String -> String
parentesisType "(" = "Paréntesis que abre"
parentesisType ")" = "Paréntesis que cierra"
parentesisType other = other -- Default to the original operator symbol for any other operator

-- Get the specific operation name for an operator token: Keys
keyType :: String -> String
keyType "{" = "Llave que abre"
keyType "}" = "Llave que cierra"
keyType other = other -- Default to the original operator symbol for any other operator

-- Helper function to pad a string to the right with spaces
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

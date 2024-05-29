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
isDigitOrDot c = isDigit c || c == '.' || c == '-' || c == 'E' || c == '+'

-- Define the abstract syntax tree (AST) data structures
data AST
    = ASTProgram [AST]
    | ASTPrincipal [AST]
    | ASTBlock [AST]
    | ASTVariableDecl String AST
    | ASTVariable String
    | ASTInteger Int
    | ASTReal Double
    | ASTAssignment String AST
    | ASTBinaryOp String AST AST
    deriving Show

-- Define a parser that produces an AST from tokens
type Parser = [Token] -> (AST, [Token])

-- Parse the whole program
parseProgram :: Parser
parseProgram (Token (Keyword "Programa") _ : Token (Keys "{") _ : tokens) =
    let (principalTree, tokens') = parsePrincipal tokens
    in (ASTProgram [principalTree], tokens')
parseProgram tokens = error $ "Unexpected tokens: " ++ show tokens

-- Parse the principal block
parsePrincipal :: Parser
parsePrincipal (Token (Keyword "principal") _ : Token (Keys "{") _ : tokens) =
    let (blockTree, tokens') = parseBlock tokens
    in (ASTPrincipal [blockTree], tokens')
parsePrincipal tokens = error $ "Unexpected tokens: " ++ show tokens

-- Parse a block of statements
parseBlock :: Parser
parseBlock (Token (Keys "{") _ : tokens) =
    let (stmts, tokens') = parseStatements tokens
    in case tokens' of
         (Token (Keys "}") _ : tokens'') -> (ASTBlock stmts, tokens'')
         _ -> error $ "Expected closing }"
parseBlock tokens = error $ "Unexpected tokens: " ++ show tokens

-- Parse a sequence of statements
parseStatements :: [Token] -> ([AST], [Token])
parseStatements tokens@(Token (Keys "}") _ : _) = ([], tokens)  -- Handle end of block
parseStatements tokens =
    let (stmt, tokens') = parseStatement tokens
        (stmts, tokens'') = parseStatements tokens'
    in case tokens'' of
         (Token (Keys "}") _ : rest) -> (stmt : stmts, rest)  -- Check for end of block
         _ -> (stmt : stmts, tokens'')


-- Parse a single statement
parseStatement :: Parser
parseStatement tokens@(Token (Keyword "Entero") _ : _) = parseVarDecl tokens
parseStatement tokens@(Token (Keyword "Real") _ : _) = parseVarDecl tokens
parseStatement tokens = parseExprStatement tokens

-- Parse a variable declaration
parseVarDecl :: Parser
parseVarDecl (Token (Keyword kw) _ : Token Variable v : Token Assignment _ : tokens) =
    let (expr, tokens') = parseExpr tokens
    in case tokens' of
         (Token (Operator ";") _ : tokens'') -> (ASTVariableDecl v expr, tokens'')
         _ -> error $ "Expected ; after variable declaration"
parseVarDecl tokens = error $ "Unexpected tokens: " ++ show tokens

-- Parse an expression statement
parseExprStatement :: Parser
parseExprStatement tokens =
    let (expr, tokens') = parseExpr tokens
    in case tokens' of
         (Token (Operator ";") _ : tokens'') -> (expr, tokens'')
         _ -> error $ "Expected ; after expression"

-- Parse an expression
parseExpr :: Parser
parseExpr tokens = 
    let (termTree, tokens') = parseTerm tokens
    in parseExpr' termTree tokens'

parseExpr' :: AST -> Parser
parseExpr' left tokens@(Token (Operator op) _ : rest)
    | op `elem` ["+", "-"] =
        let (right, tokens'') = parseTerm rest
        in parseExpr' (ASTBinaryOp op left right) tokens''
parseExpr' left tokens = (left, tokens)

-- Parse a term
parseTerm :: Parser
parseTerm tokens = 
    let (facTree, tokens') = parseFac tokens
    in parseTerm' facTree tokens'

parseTerm' :: AST -> Parser
parseTerm' left tokens@(Token (Operator op) _ : rest)
    | op `elem` ["*", "/"] =
        let (right, tokens'') = parseFac rest
        in parseTerm' (ASTBinaryOp op left right) tokens''
parseTerm' left tokens = (left, tokens)

-- Parse a factor
parseFac :: Parser
parseFac (Token (Parenthesis "(") _ : tokens) = 
    let (exprTree, Token (Parenthesis ")") _ : tokens') = parseExpr tokens
    in (exprTree, tokens')
parseFac (Token Variable v : tokens) = (ASTVariable v, tokens)
parseFac (Token Integer n : tokens) = (ASTInteger (read n), tokens)
parseFac (Token Real r : tokens) = (ASTReal (read r), tokens)
parseFac tokens = error $ "Unexpected tokens: " ++ show tokens

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
            let (ast, _) = parseProgram tokens
            putStrLn "\nParsed AST:"
            print ast
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

-- Get the specific operation name for an operator token
operatorType :: String -> String
operatorType "+" = "Suma"
operatorType "-" = "Resta"
operatorType "*" = "Multiplicacion"
operatorType "/" = "Division"
operatorType "=" = "Asignacion"
operatorType ";" = "Punto y coma"
operatorType other = other  -- Default to the original operator symbol for any other operator

-- Get the specific operation name for an operator token
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

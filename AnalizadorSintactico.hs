import System.Environment (getArgs)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser(..))
import Text.Parsec.Language (emptyDef)

-- Define token types
data TokenType = Variable | Assign | Integer | Real | Operator String | Parenthesis String | Comment | Keyword String | Keys String
    deriving (Show, Eq)

-- Define a token data structure to hold the token value and its type
data Token = Token { tokenType :: TokenType, tokenValue :: String } deriving (Show, Eq)

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
    | x == '=' = Token Assign [x] : tokenize xs
    | x `elem` "+-*/^;" = Token (Operator [x]) [x] : tokenize xs
    | x `elem` "()" = Token (Parenthesis [x]) [x] : tokenize xs
    | x `elem` "{}" = Token (Keys [x]) [x] : tokenize xs
    | otherwise = error $ "Unexpected token at: " ++ s

getToken :: String -> Token
getToken tokenValue
    | all isDigitOrDot tokenValue =
        if '.' `elem` tokenValue || 'E' `elem` tokenValue then Token Real tokenValue else Token Integer tokenValue
    | otherwise = case tokenValue of
                    "=" -> Token Assign tokenValue
                    "Programa" -> Token (Keyword "Programa") tokenValue
                    "principal" -> Token (Keyword "principal") tokenValue
                    "Entero" -> Token (Keyword "Entero") tokenValue
                    "Real" -> Token (Keyword "Real") tokenValue
                    _ -> Token Variable tokenValue

isDigitOrDot :: Char -> Bool
isDigitOrDot c = isDigit c || c == '.' || c == 'E' || c == '+' || c == '-'

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
            case parse parseProgram "" tokens of
                Left err  -> do
                    putStrLn "Syntax error:"
                    print err
                Right ast -> do
                    putStrLn "Abstract Syntax Tree: "
                    putStrLn $ printAST ast
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
                            _ -> show tokenType  -- For all other types

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

keyType :: String -> String
keyType "{" = "Llave que abre"
keyType "}" = "Llave que cierra"
keyType other = other  -- Default to the original operator symbol for any other operator

-- Helper function to pad a string to the right with spaces
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

-- Parser part

-- Define the AST data structure
data AST = Program [AST]
         | Principal [AST]
         | Block [AST]
         | VarDecl String AST
         | AssignExpr AST AST
         | Var String
         | IntConst Integer
         | RealConst Double
         | Expr AST AST AST
         | Term AST AST
         | Factor AST
         | ParenExpr AST
         | OperatorNode String
         | Semicolon AST
         deriving (Show, Eq)

type TokenParser a = Parsec [Token] () a

parseProgram :: TokenParser AST
parseProgram = do
    _ <- parseToken (Token (Keyword "Programa") "Programa")
    parseToken (Token (Keys "{") "{")
    principal <- parsePrincipal
    parseToken (Token (Keys "}") "}")
    return $ Program [principal]

parsePrincipal :: TokenParser AST
parsePrincipal = do
    _ <- parseToken (Token (Keyword "principal") "principal")
    -- Optional parentheses
    _ <- optionMaybe $ do
        parseToken (Token (Parenthesis "(") "(")
        parseToken (Token (Parenthesis ")") ")")
    -- Mandatory block with braces
    parseToken (Token (Keys "{") "{")
    block <- parseBlock
    parseToken (Token (Keys "}") "}")
    return $ Principal [Block block]

parseBlock :: TokenParser [AST]
parseBlock = many parseStatement

parseStatement :: TokenParser AST
parseStatement = do
    stmt <- try parseVarDecl <|> parseExprStmt
    semicolonToken <- parseToken (Token (Operator ";") ";")
    return $ Semicolon stmt

parseVarDecl :: TokenParser AST
parseVarDecl = do
    t <- parseTokenType
    var <- parseVariableName
    _ <- parseToken (Token Assign "=")
    expr <- parseExpression
    return $ VarDecl t (AssignExpr (Var var) expr)

parseExprStmt :: TokenParser AST
parseExprStmt = do
    expr <- parseExpression
    _ <- parseToken (Token (Operator ";") ";")
    return expr

parseTokenType :: TokenParser String
parseTokenType = tokenPrim show updatePos testType
  where
    testType (Token (Keyword kw) _) | kw == "Entero" || kw == "Real" = Just kw
    testType _ = Nothing

parseExpression :: TokenParser AST
parseExpression = do
    t <- parseTerm
    rest <- many (parseOpTerm "+" <|> parseOpTerm "-")
    return $ foldl (\acc (op, term) -> Expr acc (OperatorNode op) term) t rest

parseOpTerm :: String -> TokenParser (String, AST)
parseOpTerm op = do
    parseToken (Token (Operator op) op)
    t <- parseTerm
    return (op, t)

parseTerm :: TokenParser AST
parseTerm = do
    f <- parseFactor
    rest <- many (parseOpFactor "*" <|> parseOpFactor "/")
    return $ foldl (\acc (op, factor) -> Expr acc (OperatorNode op) factor) f rest

parseOpFactor :: String -> TokenParser (String, AST)
parseOpFactor op = do
    parseToken (Token (Operator op) op)
    f <- parseFactor
    return (op, f)

parseFactor :: TokenParser AST
parseFactor = try parseParenExpr <|> try parseNumber <|> parseVariable

parseParenExpr :: TokenParser AST
parseParenExpr = do
    parseToken (Token (Parenthesis "(") "(")
    expr <- parseExpression
    parseToken (Token (Parenthesis ")") ")")
    return $ ParenExpr expr

parseNumber :: TokenParser AST
parseNumber = do
    token <- tokenPrim show updatePos testNum
    let ast = case tokenType token of
                Integer -> IntConst (read (tokenValue token))
                Real -> RealConst (read (tokenValue token))
    return ast
  where
    testNum t@(Token Integer _) = Just t
    testNum t@(Token Real _) = Just t
    testNum _ = Nothing

parseVariable :: TokenParser AST
parseVariable = do
    token <- tokenPrim show updatePos testVar
    return $ Var (tokenValue token)
  where
    testVar t@(Token Variable _) = Just t
    testVar _ = Nothing

-- Implement parseVariableName function
parseVariableName :: TokenParser String
parseVariableName = tokenPrim show updatePos testVarName
  where
    testVarName (Token Variable varName) = Just varName
    testVarName _ = Nothing

parseToken :: Token -> TokenParser Token
parseToken expectedToken = tokenPrim show updatePos testToken
  where
    testToken token
        | token == expectedToken = Just token
        | otherwise = Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ _ = pos

-- Print the AST in a hierarchical manner with tree characters
printAST :: AST -> String
printAST = unlines . draw
  where
    draw :: AST -> [String]
    draw (Program asts)       = "Program" : drawChildren asts
    draw (Principal asts)     = "Principal" : drawChildren asts
    draw (Block asts)         = "Block {}" : drawChildren asts
    draw (VarDecl t ast)      = ["VarDecl " ++ t] ++ shift "├─ " "│  " (draw ast)
    draw (AssignExpr var expr)= ["AssignExpr"] ++ shift "├─ " "│  " (draw var) ++ shift "└─ " "   " (draw expr)
    draw (Var name)           = ["Var " ++ name]
    draw (IntConst val)       = ["IntConst " ++ show val]
    draw (RealConst val)      = ["RealConst " ++ show val]
    draw (Expr left op right) = ["Expr"] ++ shift "├─ " "│  " (draw left) ++ shift "├─ " "│  " (draw op) ++ shift "└─ " "   " (draw right)
    draw (Term left right)    = ["Term"] ++ shift "├─ " "│  " (draw left) ++ shift "└─ " "   " (draw right)
    draw (Factor ast)         = ["Factor"] ++ shift "└─ " "   " (draw ast)
    draw (ParenExpr ast)      = ["ParenExpr ()"] ++ shift "└─ " "   " (draw ast)
    draw (OperatorNode op)    = ["Operator " ++ op]
    draw (Semicolon ast)      = draw ast ++ ["└─ Semicolon ;"]

    drawChildren :: [AST] -> [String]
    drawChildren []     = []
    drawChildren [t]    = shift "└─ " "   " (draw t)
    drawChildren (t:ts) = shift "├─ " "│  " (draw t) ++ drawChildren ts

    shift :: String -> String -> [String] -> [String]
    shift first other = zipWith (++) (first : repeat other)

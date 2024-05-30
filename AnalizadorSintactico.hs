import System.Environment (getArgs)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser(..))
import Text.Parsec.Language (emptyDef)
import Debug.Trace (trace)

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
                Left err  -> print err
                Right ast -> putStrLn $ printAST ast
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

keyType :: String -> String
keyType "{" = "Llave que abre"
keyType "}" = "Llave que cierra"
keyType other = other  -- Default to the original operator symbol for any other operator

-- Helper function to pad a string to the right with spaces
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

-- Parser part
data AST = Program [AST]
         | Principal [AST]
         | Block [AST]
         | VarDecl String AST AST  -- Added AST for Semicolon
         | AssignExpr AST AST
         | Var String
         | IntConst Integer
         | RealConst Double
         | Expr AST AST AST
         | Term AST AST
         | Factor AST
         | ParenExpr AST
         | Semicolon
         deriving (Show, Eq)



type TokenParser a = Parsec [Token] () a

parseProgram :: TokenParser AST
parseProgram = do
    debugPrint "Entering parseProgram"
    token <- parseToken (Token (Keyword "Programa") "Programa")
    debugPrintWithToken "Read token" token
    parseToken (Token (Keys "{") "{")
    principal <- parsePrincipal
    parseToken (Token (Keys "}") "}")
    debugPrint "Exiting parseProgram"
    return $ Program [principal]

parsePrincipal :: TokenParser AST
parsePrincipal = do
    debugPrint "Entering parsePrincipal"
    token <- parseToken (Token (Keyword "principal") "principal")
    debugPrintWithToken "Read token" token
    -- Optional parentheses
    _ <- optionMaybe $ do
        parseToken (Token (Parenthesis "(") "(")
        parseToken (Token (Parenthesis ")") ")")
    -- Mandatory block with braces
    parseToken (Token (Keys "{") "{")
    block <- parseBlock
    parseToken (Token (Keys "}") "}")
    debugPrint "Exiting parsePrincipal"
    return $ Principal [Block block]

parseBlock :: TokenParser [AST]
parseBlock = do
    debugPrint "Entering parseBlock"
    stmts <- many parseStatement
    debugPrint "Exiting parseBlock"
    return stmts

parseStatement :: TokenParser AST
parseStatement = do
    debugPrint "Entering parseStatement"
    stmt <- try parseVarDecl <|> parseExprStmt
    debugPrint "Exiting parseStatement"
    return stmt

parseVarDecl :: TokenParser AST
parseVarDecl = do
    debugPrint "Entering parseVarDecl"
    t <- parseTokenType
    debugPrintWithToken "Parsed token type" (Token (Keyword t) t)
    var <- parseVariableName
    debugPrintWithToken "Parsed variable name" (Token Variable var)
    assignToken <- parseToken (Token Assign "=")
    debugPrintWithToken "Parsed assignment operator" assignToken
    expr <- parseExpression
    debugPrint "Parsed expression"
    semicolonToken <- parseToken (Token (Operator ";") ";")
    debugPrintWithToken "Parsed semicolon" semicolonToken
    debugPrint "Exiting parseVarDecl"
    return $ VarDecl t (AssignExpr (Var var) expr) Semicolon

parseExprStmt :: TokenParser AST
parseExprStmt = do
    debugPrint "Entering parseExprStmt"
    expr <- parseExpression
    semicolonToken <- parseToken (Token (Operator ";") ";")
    debugPrintWithToken "Parsed semicolon" semicolonToken
    debugPrint "Exiting parseExprStmt"
    return $ Expr expr (Var ";") Semicolon


parseTokenType :: TokenParser String
parseTokenType = tokenPrim show updatePos testType
  where
    testType (Token (Keyword kw) _) | kw == "Entero" || kw == "Real" = Just kw
    testType _ = Nothing

parseExpression :: TokenParser AST
parseExpression = do
    debugPrint "Entering parseExpression"
    t <- parseTerm
    rest <- many (parseOpTerm "+" <|> parseOpTerm "-")
    debugPrint "Exiting parseExpression"
    return $ foldl (\acc (op, term) -> Expr acc (Var op) term) t rest

parseOpTerm :: String -> TokenParser (String, AST)
parseOpTerm op = do
    debugPrint $ "Entering parseOpTerm with operator: " ++ op
    token <- parseToken (Token (Operator op) op)
    debugPrintWithToken "Read token" token
    t <- parseTerm
    debugPrint "Exiting parseOpTerm"
    return (op, t)

parseTerm :: TokenParser AST
parseTerm = do
    debugPrint "Entering parseTerm"
    f <- parseFactor
    rest <- many (parseOpFactor "*" <|> parseOpFactor "/")
    debugPrint "Exiting parseTerm"
    return $ foldl (\acc (op, factor) -> Expr acc (Var op) factor) f rest

parseOpFactor :: String -> TokenParser (String, AST)
parseOpFactor op = do
    debugPrint $ "Entering parseOpFactor with operator: " ++ op
    token <- parseToken (Token (Operator op) op)
    debugPrintWithToken "Read token" token
    f <- parseFactor
    debugPrint "Exiting parseOpFactor"
    return (op, f)

parseFactor :: TokenParser AST
parseFactor = do
    debugPrint "Entering parseFactor"
    factor <- try parseParenExpr <|> try parseNumber <|> parseVariable
    debugPrint "Exiting parseFactor"
    return factor

parseParenExpr :: TokenParser AST
parseParenExpr = try $ do
    debugPrint "Entering parseParenExpr"
    _ <- parseToken (Token (Parenthesis "(") "(")
    expr <- parseExpression
    _ <- parseToken (Token (Parenthesis ")") ")")
    debugPrint "Exiting parseParenExpr"
    return $ ParenExpr expr

parseNumber :: TokenParser AST
parseNumber = try $ do
    debugPrint "Entering parseNumber"
    token <- tokenPrim show updatePos testNum
    debugPrintWithToken "Read number token" token
    let ast = case tokenType token of
                Integer -> IntConst (read (tokenValue token))
                Real -> RealConst (read (tokenValue token))
    debugPrint "Exiting parseNumber"
    return ast
  where
    testNum t@(Token Integer _) = Just t
    testNum t@(Token Real _) = Just t
    testNum _ = Nothing

parseVariable :: TokenParser AST
parseVariable = do
    debugPrint "Entering parseVariable"
    token <- tokenPrim show updatePos testVar
    debugPrintWithToken "Read variable token" token
    debugPrint "Exiting parseVariable"
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
        | otherwise = trace ("Expected: " ++ show expectedToken ++ ", but got: " ++ show token) Nothing

debugPrint :: String -> TokenParser ()
debugPrint msg = trace (msg ++ "\n") (return ())

debugPrintWithToken :: String -> Token -> TokenParser ()
debugPrintWithToken msg token = trace (msg ++ ": " ++ show token ++ "\n") (return ())

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
    draw (VarDecl t ast semicolon) = ["VarDecl " ++ t] ++ shift "├─ " "│  " (draw ast) ++ shift "└─ " "   " (draw semicolon)
    draw (AssignExpr var expr)= ["AssignExpr"] ++ shift "├─ " "│  " (draw var) ++ shift "└─ " "   " (draw expr)
    draw (Var name)           = ["Var " ++ name]
    draw (IntConst val)       = ["IntConst " ++ show val]
    draw (RealConst val)      = ["RealConst " ++ show val]
    draw (Expr left op right) = ["Expr"] ++ shift "├─ " "│  " (draw left) ++ shift "├─ " "│  " (draw op) ++ shift "└─ " "   " (draw right)
    draw (Term left right)    = ["Term"] ++ shift "├─ " "│  " (draw left) ++ shift "└─ " "   " (draw right)
    draw (Factor ast)         = ["Factor"] ++ shift "└─ " "   " (draw ast)
    draw (ParenExpr ast)      = ["ParenExpr"] ++ shift "└─ " "   " (draw ast)
    draw Semicolon            = ["Semicolon ;"]

    drawChildren :: [AST] -> [String]
    drawChildren []     = []
    drawChildren [t]    = shift "└─ " "   " (draw t)
    drawChildren (t:ts) = shift "├─ " "│  " (draw t) ++ drawChildren ts

    shift :: String -> String -> [String] -> [String]
    shift first other = zipWith (++) (first : repeat other)


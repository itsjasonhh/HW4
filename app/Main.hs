--Code taken from https://wiki.haskell.org/Parsing_a_simple_imperative_language
module Main where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (isSpace)
import Data.List
import Data.String
import qualified Data.Map.Strict as Map
data BExpr = BoolConst Bool
            | Not BExpr
            | And BExpr BExpr
            | Or BExpr BExpr
            | Greater AExpr AExpr
            | Less AExpr AExpr
            | Equal AExpr AExpr
             deriving (Show)
instance Pretty BExp where
  pretty (BoolConst True) = "true"
  pretty (BoolConst False) = "false"
  pretty (Equal a b) = "(" ++ pretty a ++ "=" ++ pretty b ++ ")"
  pretty (Less a b) = "(" ++ pretty a ++ "<" ++ pretty b ++ ")"
  pretty (Not a) = "¬" ++ pretty a
  pretty (Or a b) = "(" ++ pretty a ++ "∨" ++ pretty b ++ ")"
  pretty (And a b) = "(" ++ pretty a ++ "∧" ++ pretty b ++ ")"
type Var = String
data AExpr = Var String
            | IntConst Integer
            | Neg AExpr
            | Add AExpr AExpr
            | Subtract AExpr AExpr
            | Multiply AExpr AExpr
            | Mod AExpr AExpr
            deriving (Show)
instance Pretty AExpr where
  pretty (IntConst n) = show n
  pretty (Var s) = s
  pretty (Add a b) = "(" ++ pretty a ++ "+" ++ pretty b ++ ")"
  pretty (Subtract a b) = "(" ++ pretty a ++ "-" ++ pretty b ++ ")"
  pretty (Multiply a b) = "(" ++ pretty a ++ "*" ++ pretty b ++ ")"


data Stmt = Seq [Stmt]
           | Assign String AExpr
           | If BExpr Stmt Stmt
           | While BExpr Stmt
           | Skip
           deriving (Show)

instance Pretty Stmt where
  pretty Skip = "skip"
  pretty (Assign s a) = s ++ " := " ++ pretty a
  pretty (Seq a b) = pretty a ++ "; " ++ pretty b
  pretty (If c a b) = "if " ++ pretty c ++ " then { " ++ pretty a ++ " } else { " ++ pretty b ++ " }"
  pretty (While b c) = "while " ++ pretty b ++ " do { " ++ pretty c ++ " }"


languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.opLetter = oneOf "="
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "while"
                                      , "do"
                                      , "skip"
                                      , "true"
                                      , "false"
                                      , "not"
                                      , "and"
                                      , "or"
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", ":=", "="
                                      , "<", ">", "∧", "∨", "¬","%"

                                      ]
            }
lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                     --   parens p
                                     -- takes care of the parenthesis and
                                     -- uses p to parse what's inside them
braces     = Token.braces     lexer
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement
statement :: Parser Stmt
statement =   parens statement
           <|> braces statement
           <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
            <|> whileStmt
            <|> skipStmt
            <|> assignStmt

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm
bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (Multiply)) AssocLeft,
                Infix  (reservedOp "%" >> return (Mod )) AssocLeft
               ]
             , [Infix  (reservedOp "+"   >> return (Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "¬" >> return (Not             ))          ]
             , [Infix  (reservedOp "∧" >> return (And     )) AssocLeft,
                Infix  (reservedOp "∨"  >> return (Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ op a1 a2

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)
         <|> (reservedOp "=" >> return Equal)

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e -> error $ show e
    Right r -> r


type Val = Integer
evalA :: AExpr -> Map.Map String Integer -> Integer
evalA(IntConst n) s = n
evalA(Var x) s = case (Map.member x s) of
                True -> s Map.! x
                False -> do
                  let s' = Map.insert x 0 s
                  s' Map.! x
evalA(Neg x) s = -(evalA x s)
evalA(Add a1 a2) s = evalA a1 s + evalA a2 s
evalA(Subtract a1 a2) s = evalA a1 s - evalA a2 s
evalA(Multiply a1 a2) s = evalA a1 s * evalA a2 s
evalA(Mod a1 a2) s = rem (evalA a1 s) (evalA a2 s)

evalB :: BExpr -> Map.Map String Integer -> Bool
evalB(BoolConst b) s = b
evalB(Not b) s = not (evalB b s)
evalB(And b1 b2) s = (evalB b1 s) && (evalB b2 s)
evalB(Or b1 b2) s = (evalB b1 s) || (evalB b2 s)
evalB(Greater a1 a2) s = evalA a1 s > evalA a2 s
evalB(Less a1 a2) s = evalA a1 s < evalA a2 s
evalB(Equal a1 a2) s = evalA a1 s == evalA a2 s

evalStmt :: Stmt -> Map.Map String Integer -> Map.Map String Integer
evalStmt(Skip) s = s
evalStmt(Assign x a) s = Map.insert x (evalA a s) s
evalStmt(While b st) s | evalB b s /= False = evalStmt(Seq [st,While b st]) s
                       | otherwise  = s

evalStmt(Seq []) s = s
evalStmt(Seq(x:xs)) s = evalStmt(Seq xs) (evalStmt x s)

evalStmt(If b st1 st2) s | evalB b s /= False = evalStmt st1 s
                         | otherwise = evalStmt st2 s


reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
mapToString::Map.Map String Integer -> String
mapToString m =  let f = \(k,v) -> k++ " → "++ show v in unlines $ map f $ Map.toList m

toCommaSeparatedString :: [String] -> String
toCommaSeparatedString = intercalate ", "


main = do
  input <- getContents
  let input1 = lines input
  let input2 = unlines input1
  let s = Map.empty
  let input' = parseString input2
  let s' = evalStmt input' s
  let pair = mapToString s'
  let pair1 = lines pair
  let pair2 = toCommaSeparatedString pair1
  let printable = "{" ++ pair2 ++ "}"
  putStrLn $ filter (/= '\n')printable



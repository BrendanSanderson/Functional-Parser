-- The main parser

module Lab6 (
  Name,
  Number,
  MathExp(..),
  parse,
  BoolExp(..),
  CompExp(..)
) where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP
import Text.Regex.Posix

type Name   = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.

data MathExp
  = Number Number
  | Var    Name
  | Neg    MathExp
  | Plus   MathExp MathExp
  | Minus  MathExp MathExp
  | Mult   MathExp MathExp
  | Div    MathExp MathExp
  | Pow    MathExp MathExp
  | Let [Name] [MathExp] MathExp
  | IfExp BoolExp CompExp (MathExp, MathExp) MathExp MathExp
  deriving (Eq, Show)

data BoolExp
    = Yes
    | Not
    deriving (Eq, Show)

data CompExp
    = Greater
    | Lesser
    | Equal
    | GreaterEqual
    | LesserEqual
    | NotEqual
    deriving (Eq, Show)

parseMathExp :: ReadP MathExp
parseMathExp = parseLet <++ parseIf <++ parsePrecLevel1 where
  parsePrecLevel1 = chainl1 parsePrecLevel2 (parsePlus +++ parseMinus) where
    parsePrecLevel2 = chainl1 parsePrecLevel3 (parseMult +++ parseDiv) where
      parsePrecLevel3 = chainr1 parsePrecLevel4 (parsePow) where
        parsePrecLevel4 = parseParen <++ parseNegate <++ parseNumber <++ parseLet <++ parseIf <++ parseVar

parseVar :: ReadP MathExp
parseVar = do
  [str] <- parse1Variable
  return $ Var str

parseParen :: ReadP MathExp
parseParen = do
  skipSpaces
  char '('
  skipSpaces
  i <- parseMathExp
  skipSpaces
  char ')'
  return i

parseNegate :: ReadP MathExp
parseNegate = do
  skipSpaces
  char '-'
  i <- parseParen <++ parseNegate <++ parseNumber <++ parseVar
  return $ Neg i

parseNumber :: ReadP MathExp
parseNumber = Number . read <$> do
  skipSpaces
  i <- many1 $ satisfy isDigit
  return i

parsePlus :: ReadP (MathExp -> MathExp -> MathExp)
parsePlus = do
  skipSpaces
  char '+'
  return Plus

parseMinus :: ReadP (MathExp -> MathExp -> MathExp)
parseMinus = do
  skipSpaces
  char '-'
  return Minus

parseMult :: ReadP (MathExp -> MathExp -> MathExp)
parseMult = do
  skipSpaces
  char '*'
  return Mult

parseDiv :: ReadP (MathExp -> MathExp -> MathExp)
parseDiv = do
  skipSpaces
  char '/'
  return Div

parsePow :: ReadP (MathExp -> MathExp -> MathExp)
parsePow = do
  skipSpaces
  char '^'
  return Pow

parseLet :: ReadP MathExp
parseLet = do
  skipSpaces
  string "let"
  skipSpaces
  varList <- parse1Variable <++ parseVariables
  skipSpaces
  char '='
  skipSpaces
  numList <- parse1Number <++ parseNumbers
  skipSpaces
  string "in"
  skipSpaces
  mainMathExp <- parseMathExp
  return $ Let varList numList mainMathExp

parse1Variable :: ReadP [Name]
parse1Variable = do
  skipSpaces
  startChar <- satisfy isAlpha
  trailingStr <- many $ satisfy isAlphaNum
  return $ [[startChar] ++ trailingStr]

parseVariables :: ReadP [Name]
parseVariables = do
  skipSpaces
  char '('
  strings <- parseListOfVariables
  return $ strings where
    parseListOfVariables :: ReadP [Name]
    parseListOfVariables = do
      word <- parse1Variable
      c <- get
      case c of
        ',' -> do
          otherWords <- parseListOfVariables
          return $ word ++ otherWords
        ')' -> do
          return $ word
        otherwise -> pfail

parse1Number :: ReadP [MathExp]
parse1Number = do
  skipSpaces
  exp <- parseMathExp
  return $ [exp]

parseNumbers :: ReadP [MathExp]
parseNumbers = do
  skipSpaces
  char '('
  numbers <- parseListOfNumbers
  return $ numbers where
    parseListOfNumbers :: ReadP [MathExp]
    parseListOfNumbers = do
      number <- parse1Number
      c <- get
      case c of
        ',' -> do
          otherNumbers <- parseListOfNumbers
          return $ number ++ otherNumbers
        ')' -> do
          return $ number
        otherwise -> pfail

parseIf :: ReadP MathExp
parseIf = do
    skipSpaces
    string "if"
    maybeNot <- parseNotMathExp <++ parseYesMathExp
    skipSpaces
    a <- parseMathExp
    skipSpaces
    comp <- parseComparison
    skipSpaces
    b <- parseMathExp
    skipSpaces
    string "then"
    skipSpaces
    c <- parseMathExp
    skipSpaces
    string "else"
    skipSpaces
    d <- parseMathExp
    return $ IfExp maybeNot comp (a, b) c d


parseNotMathExp :: ReadP BoolExp
parseNotMathExp = do
    skipSpaces
    string "not"
    return Not

parseYesMathExp :: ReadP BoolExp
parseYesMathExp = do
    return Yes


parseComparison :: ReadP CompExp
parseComparison = parseEqual <++ parseEqLess <++ parseEqGreat <++ parseNotEq <++ parseGreater <++ parseLesser

parseGreater :: ReadP CompExp
parseGreater = do
    skipSpaces
    char '>'
    return Greater

parseLesser :: ReadP CompExp
parseLesser = do
    skipSpaces
    char '<'
    return Lesser

parseEqual :: ReadP CompExp
parseEqual = do
    skipSpaces
    string "=="
    return Equal

parseEqGreat :: ReadP CompExp
parseEqGreat = do
    skipSpaces
    string ">="
    return LesserEqual

parseEqLess :: ReadP CompExp
parseEqLess = do
    skipSpaces
    string "<="
    return GreaterEqual

parseNotEq :: ReadP CompExp
parseNotEq = do
    skipSpaces
    string "/="
    return NotEqual

parseTLE :: ReadP MathExp
parseTLE = do
  tle <- parseMathExp
  skipSpaces
  return tle

parse :: String -> Either String MathExp
parse str =
  case (completeParses, otherParses) of
    ([(result, "")], _  ) -> Right result  -- Only complete result.
    ([]            , [] ) -> Left $ "No parse."
    ([]            , _:_) -> Left $ "Incomplete parse. Unparsed: " ++ (show leastRemaning)
    (_:_           , _  ) -> Left $ "Ambiguous parse: " ++ (show completeParses)
  where
    parses = readP_to_S parseTLE str
    (completeParses, otherParses) =
        partition (\(_, remaining) -> remaining == "") parses
    leastRemaning = minimumBy (comparing length) . map snd $ otherParses

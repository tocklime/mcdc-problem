-- | A simple module to parse 'String's into 'BoolExp Char's.
-- largely cribbed [from here](https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html)
module Parser where

import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr
import           Data.Char                      (toLower)
import           Data.Maybe                     (fromMaybe)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Expressions

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

notExp :: Parser (BoolExp Char)
notExp = symbol "!" >> charBoolExp

charBoolExp :: Parser (BoolExp Char)
charBoolExp = makeExprParser term operators

operators :: [[Operator Parser (BoolExp Char)]]
operators =
  [ [Prefix (Not <$ symbol "!")]
  , [InfixL (And <$ symbol "&&")
    , InfixL (Or <$ symbol "||")]
  ]

term :: Parser (BoolExp Char)
term = parens charBoolExp
   <|> Var . toLower<$> lexeme letterChar

-- | Parse a 'String'. Returns 'Nothing' on error.
parseSafe :: String -> Maybe (BoolExp Char)
parseSafe = parseMaybe charBoolExp

-- | Parse a 'String' to a 'BoolExp Char'. Throws error on error.
parseUnsafe :: String -> BoolExp Char
parseUnsafe = fromMaybe (error "Failed to parse expression") . parseSafe

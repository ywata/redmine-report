{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
module Redmine.Reporter.Parser (readConfig) where
import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*))
import Text.Parsec      (Parsec, ParseError, many, many1, char,  noneOf, oneOf, parse, string, try, alphaNum, letter)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Combinator (eof)

import qualified Text.Parsec.Char as P (letter)
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as P
import qualified Data.Text as T (Text, pack, unpack)
import Data.Graph (Vertex(..),Graph(..), buildG, topSort)

import Redmine.Reporter.Config

import Debug.Trace (trace)


---------- Below are parser
readConfig :: String -> IO(Either ParseError (Config (String, String) String (Integer, String) Integer))
readConfig f = parseFromFile config f

langDef::P.LanguageDef ()
langDef = P.LanguageDef{
  P.commentStart = "{-"
  , P.commentEnd   = "-}"
  , P.commentLine = "#"
  , P.nestedComments = False
  , P.identStart = letter
  , P.identLetter = alphaNum
  , P.opStart = oneOf "=<"
  , P.opLetter = oneOf ""
  , P.reservedNames = []
  , P.reservedOpNames = ["=", "<", "<="]
  , P.caseSensitive = True
  }

lexer = P.makeTokenParser langDef
brackets = P.brackets lexer
ident  = P.identifier lexer
natural = P.natural lexer
parens = P.parens lexer
reservedOp = P.reservedOp lexer
strLiteral = P.stringLiteral lexer


keyVal = (,) <$> ident <* reservedOp "=" <*> strLiteral

order = (:) <$> natural <*> (many1 r)
  where
    r = reservedOp "<" *> natural

def = (flip (,)) <$> strLiteral <* reservedOp "<=" <*> natural

sectionName = brackets ident
sectionDef = SectionDef <$> sectionName <*> many def <*> many order

config = Config <$> many keyVal <*> many sectionDef



{-
c1 = parse config "" "kv=\"abc\"\n[abc]\"abc\"<=1\n1< 2"

s1 = parse sectionDef "" "[abc]"
s2 = parse sectionDef "" "[abc]1<2"
s3 = parse sectionDef "" "[abc]\"abc\"<=1\n1< 2"
s4 = parse sectionDef "" "[abc]"
d1 = parse def "" "\"abc\"<=2"

o1 = parse order "" "1< 2 "
o2 = parse order "" "1<2<3"
kv1 = parse (keyVal) "" "a ={- -} \"bc\""
kv2 = parse (keyVal) "" " a=\"bc\""

f1 = parse (parens P.letter) "" "()"
f2 = parse (parens P.letter) "" "(b)"
-}


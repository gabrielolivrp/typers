{-# LANGUAGE ImportQualifiedPost #-}

module Parser where

import Control.Applicative (Alternative (some))
import Data.Functor (($>))
import Debug.Trace (traceM)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token qualified as P
import Tree

type ParserM a = Parsec String () a

lexer :: P.TokenParser ()
lexer =
  P.makeTokenParser
    emptyDef
      { P.reservedNames = ["let", "in", "True", "False", "forall"],
        P.reservedOpNames = ["λ", "->", "=", ".", "∀"]
      }

pLexeme :: ParserM a -> ParserM a
pLexeme = P.lexeme lexer

pReserved :: String -> ParserM ()
pReserved = P.reserved lexer

pSymbol :: String -> ParserM String
pSymbol = P.symbol lexer

pParens :: ParserM a -> ParserM a
pParens = P.parens lexer

pIdentifier :: ParserM String
pIdentifier = P.identifier lexer

pDot :: ParserM String
pDot = P.dot lexer

pColon :: ParserM String
pColon = P.colon lexer

pLambda :: ParserM String
pLambda = pSymbol "λ"

pArrow :: ParserM String
pArrow = pSymbol "->"

pBrackets :: ParserM a -> ParserM a
pBrackets = P.brackets lexer

pVar :: ParserM Term
pVar = TmVar <$> pIdentifier

pLitInt :: ParserM Term
pLitInt = TmLit . LInt . fromIntegral <$> P.natural lexer

pLitBool :: ParserM Term
pLitBool = TmLit . LBool <$> go
  where
    go = (pReserved "True" $> True) <|> (pReserved "False" $> False)

pLiteral :: ParserM Term
pLiteral = pLitInt <|> pLitBool

pLet :: ParserM Term
pLet = do
  pReserved "let"
  x <- pIdentifier
  pSymbol "="
  e1 <- pTerm
  pReserved "in"
  TmLet x e1 <$> pTerm

pAbs :: ParserM Term
pAbs = do
  pLambda
  x <- pIdentifier
  pDot
  TmAbs x <$> pTerm

pTerm :: ParserM Term
pTerm = foldl1 TmApp <$> some go
  where
    go =
      pParens pTerm
        <|> pLet
        <|> pAbs
        <|> pLiteral
        <|> pVar

contents :: ParserM a -> ParserM a
contents p = P.whiteSpace lexer *> pLexeme p <* eof

runP :: String -> Either ParseError Term
runP = runParser (contents pTerm) () ""

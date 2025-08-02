{-# LANGUAGE ImportQualifiedPost #-}

module Parser where

import Control.Applicative (Alternative (some))
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token qualified as P
import Tree

type ParserM a = Parsec String () a

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

pLexeme :: ParserM a -> ParserM a
pLexeme = P.lexeme lexer

pReserved :: String -> ParserM ()
pReserved = P.reserved lexer

pSymbol :: String -> ParserM String
pSymbol = P.symbol lexer

pParens :: ParserM a -> ParserM a
pParens = P.parens lexer

pIdentifer :: ParserM String
pIdentifer = P.identifier lexer

pDot :: ParserM String
pDot = P.dot lexer

pColon :: ParserM String
pColon = P.colon lexer

pLambda :: ParserM String
pLambda = pSymbol "λ"

pArrow :: ParserM String
pArrow = pSymbol "->"

pVar :: ParserM Term
pVar = TmVar . Name <$> pIdentifer

pAbs :: ParserM Term
pAbs = do
  void pLambda
  param <- pIdentifer
  void pDot
  TmAbs (Name param) <$> pTerm

pTerm :: ParserM Term
pTerm = foldl1 TmApp <$> some go
 where
  go =
    try (pParens pTerm)
      <|> pAbs
      <|> pVar

contents :: ParserM a -> ParserM a
contents p = P.whiteSpace lexer *> pLexeme p <* eof

runP :: String -> Either ParseError Term
runP = runParser (contents pTerm) () ""

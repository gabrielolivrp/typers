{-# LANGUAGE ImportQualifiedPost #-}

module Parser where

import Control.Applicative (Alternative (some))
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

pBrackets :: ParserM a -> ParserM a
pBrackets = P.brackets lexer

pNat :: ParserM Term
pNat = intToNat . fromIntegral <$> P.natural lexer
  where
    intToNat 0 = TmZero
    intToNat n = TmSucc (intToNat (n - 1))

pZero :: ParserM Term
pZero = pReserved "Zero" >> pure TmZero

pSucc :: ParserM Term
pSucc = pReserved "Succ" >> TmSucc <$> pTerm

pUnit :: ParserM Term
pUnit = pSymbol "()" >> pure TmUnit

pVar :: ParserM Term
pVar = TmVar <$> pIdentifer

pAbs :: ParserM Term
pAbs = do
  pLambda
  x <- pIdentifer
  pColon
  t <- pType
  pDot
  TmAbs x t <$> pTerm

pTAbs :: ParserM Term
pTAbs = do
  pSymbol "Λ"
  x <- pIdentifer
  pDot
  TmTAbs x <$> pTerm

pBaseTerm :: ParserM Term
pBaseTerm =
  pParens pTerm
    <|> pAbs
    <|> pTAbs
    <|> pNat
    <|> pZero
    <|> pSucc
    <|> pUnit
    <|> pVar

pTApp :: ParserM Term
pTApp = do
  t <- pBaseTerm
  ts <- many (pBrackets pType)
  return $ foldl TmTApp t ts

pTerm :: ParserM Term
pTerm = foldl1 TmApp <$> some pTApp

pType :: ParserM Typ
pType = pSimpleType `chainr1` (pArrow >> pure TArrow)
  where
    pSimpleType =
      (pReserved "Unit" >> pure TUnit)
        <|> (pReserved "Nat" >> pure TNat)
        <|> (TForall <$> (pSymbol "∀" *> pIdentifer) <*> pType)
        <|> (TVar <$> pIdentifer)
        <|> pParens pType

contents :: ParserM a -> ParserM a
contents p = P.whiteSpace lexer *> pLexeme p <* eof

runP :: String -> Either ParseError Term
runP = runParser (contents pTerm) () ""

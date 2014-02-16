-- | Parser del Lenguaje extendido de LISA.
module HGUI.Parser where

import qualified Data.Map as M

-- Imports Parsec
import Text.Parsec 

-- Imports de Equ
import qualified Equ.Parser as PEqu
import qualified Equ.Expr as Equ

-- Imports de Hal
import Hal.Lang
import Hal.Parser

-- Imports de Hal-Gui
import HGUI.ExtendedLang

-- *** Comandos.
-- |Comando simples.
extSingle :: String -> (CommPos -> ExtComm) -> ParserH ExtComm
extSingle s ec = try $ do
              st <- getParserState
              let initp = statePos st
              _ <- sym s
              st' <- getParserState
              let endp = statePos st'
              return $ ec $ makeCommPos initp endp

-- | Skip
extSkip :: ParserH ExtComm
extSkip = extSingle "skip" ExtSkip

-- | Abort
extAbort :: ParserH ExtComm
extAbort = extSingle "abort" ExtAbort

-- |Asignación.
extAssignInt :: ParserH ExtComm
extAssignInt = try $ do
               st <- getParserState
               let initp = statePos st
               
               acc <- pintvar  
               oper ":="
               iexp <- intexp 
               
               st' <- getParserState
               let endp = statePos st'
               return $ ExtIAssig (makeCommPos initp endp) acc iexp

extAssignBool :: ParserH ExtComm
extAssignBool = try $ do
                st <- getParserState
                let initp = statePos st
                
                acc <- pboolvar  
                oper ":="
                bexp <- boolexp 
                
                st' <- getParserState
                let endp = statePos st'
                return $ ExtBAssig (makeCommPos initp endp) acc bexp

-- |Condicional.      
extIfthen :: ParserH ExtComm
extIfthen = try $ do
            st <- getParserState
            let initp = statePos st
            
            keyword "if" 
            b <- (boolexp <?> "Expresión booleana")
            keyword "then" 
            c <- extComm
            keyword "else"
            c' <- extComm
            keyword "fi"
            
            st' <- getParserState
            let endp = statePos st'
            return $ ExtIf (makeCommPos initp endp) b c c'

-- | Assert
extAssert :: ParserH ExtComm
extAssert = try $ do
            st <- getParserState
            let initp = statePos st
            
            f <- formfun
            
            st' <- getParserState
            let endp = statePos st'
            return $ ExtAssert (makeCommPos initp endp) f

-- | Do - While
extWhile :: ParserH ExtComm
extWhile = try $ do
           st <- getParserState
           let initp = statePos st
           
           whites
           keyword "while"
           b <- boolexp
           form <- formfun
           keyword "do"
           c <- extComm
           keyword "od"
           
           st' <- getParserState
           let endp = statePos st'
           return (ExtDo (makeCommPos initp endp) form b c)

-- | Precondición
extPrec :: ParserH ExtComm
extPrec = try $ do
          st <- getParserState
          let initp = statePos st
          
          _ <- sym "{"
          whites
          keyword "Pre"
          _ <- sym ":"
          whites 
          e <- PEqu.parsePreExpr
          whites
          _ <- sym "}"
          
          st' <- getParserState
          let endp = statePos st'
          return $ ExtPre (makeCommPos initp endp) (Equ.Expr e)

-- | Precondición
extPost :: ParserH (CommPos,FormFun)
extPost = try $ do
          st <- getParserState
          let initp = statePos st
          
          _ <- sym "{"
          whites
          keyword "Post"
          _ <- sym ":"
          whites 
          e <- PEqu.parsePreExpr
          whites
          _ <- sym "}"
          
          st' <- getParserState
          let endp = statePos st'
          return (makeCommPos initp endp, Equ.Expr e)

-- | Comandos del lenguaje LISA
extComms :: [ParserH ExtComm]
extComms = [ extSkip 
           , extAbort
           , extAssignInt
           , extAssignBool
           , extAssert
           , extIfthen
           , extWhile
           ]

extComm :: ParserH ExtComm
extComm = try $ whites >> sepEndBy1 (choice extComms) semip >>= return . foldl1 ExtSeq

-- | Un programa consta de declaraciones de variables, una precondición, un comando y una postcondición 
extProgram :: ParserH ExtProgram
extProgram = varinputs >>
             vardefs >>
             extPrec >>= \pre ->
             extComm >>= \c ->
             extPost >>= \postc ->
             getParserState >>= return . M.elems . pvars . stateUser >>= \vars ->
             return $ ExtProg vars pre c postc

-- | Función principal de parseo desde String
parseExtPrgFromString :: String -> Either ParseError ExtProgram
parseExtPrgFromString = runParser extProgram initSt "" 
    where initSt = PHalState { lvars = M.empty
                             , pvars = M.empty
                             , equPState = PEqu.initPExprState PEqu.UnusedParen
                             }

parseExtPrgFromFile :: FilePath -> IO ExtProgram
parseExtPrgFromFile f = 
    readFile f >>= return . parseExtPrgFromString >>=
    either (error . show) return

parseExtPrgFromFile' :: FilePath -> IO (Either ParseError ExtProgram)
parseExtPrgFromFile' f = readFile f >>= return . parseExtPrgFromString 

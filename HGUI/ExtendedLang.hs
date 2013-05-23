-- | Módulo de la sintáxis extendida del lenguaje imperativo simple con anotaciones (LISA).
-- En esta versión guardamos ademas la información sobre los números de lineas
-- de los comandos.
{-# Language GADTs #-}
module HGUI.ExtendedLang where

import Hal.Lang

import Text.Parsec.Pos

data CommPos = CommPos { begin :: SourcePos
                       , end   :: SourcePos
                       }
    deriving (Eq,Show)

makeCommPos :: SourcePos -> SourcePos -> CommPos
makeCommPos = CommPos

initPos :: CommPos
initPos = CommPos (initialPos "") (initialPos "")

takeCommLine :: ExtComm -> Int
takeCommLine (ExtSkip   pos)       = sourceLine $ begin pos
takeCommLine (ExtAbort  pos)       = sourceLine $ begin pos
takeCommLine (ExtPre    pos _)     = sourceLine $ begin pos
takeCommLine (ExtAssert pos _)     = sourceLine $ begin pos
takeCommLine (ExtIf     pos _ _ _) = sourceLine $ begin pos
takeCommLine (ExtIAssig pos _ _)   = sourceLine $ begin pos
takeCommLine (ExtBAssig pos _ _)   = sourceLine $ begin pos
takeCommLine (ExtDo     pos _ _ _) = sourceLine $ begin pos
takeCommLine (ExtSeq c c')         = takeCommLine c

getCommLines :: ExtComm -> [Int]
getCommLines (ExtSeq c c')        = getCommLines c ++ getCommLines c'
getCommLines cif@(ExtIf _ _ c c') = takeCommLine cif : 
                                    getCommLines c ++ getCommLines c'
getCommLines cdo@(ExtDo _ _ _ c)  = takeCommLine cdo : getCommLines c
getCommLines c                    = [takeCommLine c]

-- Los terminos que representan los comandos con la información extra
-- sobre en que linea se encuentran.
data ExtComm where
    ExtSkip   :: CommPos -> ExtComm
    ExtAbort  :: CommPos -> ExtComm
    
    ExtPre    :: CommPos -> FormFun -> ExtComm
    ExtAssert :: CommPos -> FormFun -> ExtComm
    
    ExtIf     :: CommPos -> BExp -> ExtComm -> ExtComm -> ExtComm
    
    ExtIAssig :: CommPos -> Identifier -> Exp -> ExtComm
    ExtBAssig :: CommPos -> Identifier -> BExp -> ExtComm
    
    ExtDo     :: CommPos -> FormFun -> BExp -> ExtComm -> ExtComm
    ExtSeq    :: ExtComm -> ExtComm -> ExtComm
    deriving Show

data ExtProgram where
    ExtProg :: LIdentifier -> ExtComm -> 
                              ExtComm -> 
                              (CommPos,FormFun) -> ExtProgram

extProgramGetExtComm :: ExtProgram -> ExtComm
extProgramGetExtComm (ExtProg _ _ c _) = c

convertExtCommToComm :: ExtComm -> Comm
convertExtCommToComm (ExtSkip _) = Skip
convertExtCommToComm (ExtAbort _) = Abort
convertExtCommToComm (ExtPre _ f) = Assert f
convertExtCommToComm (ExtAssert _ f) = Assert f
convertExtCommToComm (ExtIf _ b c c') = If b (convertExtCommToComm c)
                                             (convertExtCommToComm c')
convertExtCommToComm (ExtIAssig _ a e) = IAssig a e
convertExtCommToComm (ExtBAssig _ a e) = BAssig a e
convertExtCommToComm (ExtDo _ i b c) = Do i b (convertExtCommToComm c)
convertExtCommToComm (ExtSeq c c') = Seq (convertExtCommToComm c)
                                         (convertExtCommToComm c')

convertExtProgToProg :: ExtProgram -> Program
convertExtProgToProg (ExtProg vars (ExtPre _ pre) ecomms (_,pos)) = 
                         Prog vars pre (convertExtCommToComm ecomms) pos

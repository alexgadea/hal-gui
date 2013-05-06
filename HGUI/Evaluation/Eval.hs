{- | Evaluador del lenguaje Hal
    
    Para evaluar asumimos el programa typechekeo sin problemas.
-}
{-# LANGUAGE RecordWildCards #-}
module HGUI.Evaluation.Eval where

import Graphics.UI.Gtk hiding (get,Plus,eventKeyName)
import Graphics.UI.Gtk.Gdk.Events hiding ( eventButton, eventClick)
import Graphics.UI.Gtk.MenuComboToolbar.ToggleToolButton

import Lens.Family

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.State as ST (StateT,get,put,execStateT,evalStateT)
import Control.Monad.Fix (fix)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import System.IO

import qualified Data.List as L
import Data.Maybe
import Data.Either
import Data.Reference

-- Imports de Fun

-- Imports de Hal
import Hal.Lang

-- Imports de Hal-Gui
import HGUI.Evaluation.EvalState
import HGUI.GState
import HGUI.ExtendedLang
import Hal.Parser

getNewValue :: Window -> ExpectValue -> MVar EitherBI -> IO ()
getNewValue mainWin ev mvar = do
                       win      <- windowNew
                       vbox     <- vBoxNew False 0
                       entry    <- entryNew
                       errLabel <- labelNew Nothing
                       
                       set win [ windowWindowPosition := WinPosMouse
                               , windowModal          := True
                               , windowDecorated      := False
                               , windowHasFrame       := False
                               , windowTypeHint       := WindowTypeHintPopupMenu
                               , widgetCanFocus       := True
                               , windowTransientFor   := mainWin
                               ]
                       
                       onKeyPress entry (\event -> configEntry win ev entry errLabel event)
                       
                       containerAdd win vbox
                       boxPackStart vbox entry    PackNatural 1
                       boxPackStart vbox errLabel PackNatural 1
                       widgetSetNoShowAll errLabel True
                       widgetShowAll win
                        
                       return ()
    where
        configEntry ::  Window -> ExpectValue -> Entry -> 
                        Label -> Event -> IO Bool
        configEntry win ev entry label event = 
                    do
                    b <- case eventKeyName event of
                            "Return" -> getValue
                            "Escape" -> return True
                            _        -> return False
                    if b then widgetDestroy win >> return False
                         else return False
            where
                getValue :: IO Bool
                getValue = do
                    strValue <- entryGetText entry
                    case ev of
                        ExpectBool -> case parseBConFromString strValue of
                                           Left er -> setMsg "Valor no valido, intente de nuevo.\n" >> return False
                                           Right v -> do
                                                      v' <- ST.evalStateT (evalBExp v) (initState,mainWin)
                                                      putMVar mvar (Left v')
                                                      return True
                        ExpectInt -> case parseConFromString strValue of
                                           Left er -> setMsg "Valor no valido, intente de nuevo.\n" >> return False
                                           Right v -> do
                                                      v' <- ST.evalStateT (evalExp v) (initState,mainWin)
                                                      putMVar mvar (Right v')
                                                      return True
                    where
                        setMsg :: String -> IO ()
                        setMsg msg = widgetSetNoShowAll label False >>
                                     labelSetText label msg >> 
                                     widgetShowAll label

-- | Evaluador de los operadores binarios enteros.
evalIntBOp :: IntBOp -> ProgState Int -> ProgState Int -> ProgState Int
evalIntBOp Plus   = liftA2 (+)
evalIntBOp Times  = liftA2 (*)
evalIntBOp Substr = liftA2 (-)
evalIntBOp Div    = liftA2 div
evalIntBOp Mod    = liftA2 mod

-- | Evaluador de los operadores binarios boleanos.
evalBoolBOp :: BoolBOp -> ProgState Bool -> ProgState Bool -> ProgState Bool
evalBoolBOp And = liftA2 (&&)
evalBoolBOp Or  = liftA2 (||)

-- | Evaluador de los operadores unarios boleanos.
evalBoolUOp :: BoolUOp -> ProgState Bool -> ProgState Bool
evalBoolUOp Not = fmap not

-- | Evaluador de las relaciones binarias.
evalRelOp :: (Eq a, Ord a) => 
             RelOp -> ProgState a -> ProgState a -> ProgState Bool
evalRelOp Equal  = liftA2 (==)
evalRelOp Lt     = liftA2 (<)

-- | Evaluador de expresiones enteras.
evalExp :: Exp -> ProgState Int
evalExp (IBOp iop e e') = evalIntBOp iop (evalExp e) (evalExp e')
evalExp (ICon i)  = return i
evalExp ide@(IntId i) = do 
                    (st,win) <- ST.get
                    let idSts = vars st
                    
                    mvalue <- getValue i idSts
                    
                    case mvalue of
                        Just v -> return v
                        Nothing -> do
                            mvar  <- liftIO $ newEmptyMVar
                            liftIO $ postGUIAsync $ getNewValue win ExpectInt mvar
                            evalE <- liftIO $ takeMVar mvar
                                    
                            let idSts = vars st
                                idSts' = L.map (updateValue i evalE) idSts
                            
                            ST.put (st { vars =  idSts'},win)
                                    
                            return $ fromRight evalE
    where
        getValue :: Identifier -> [StateTuple] -> ProgState (Maybe Int)
        getValue i idSts = 
                case L.find (==(IntVar i Nothing)) idSts of
                    Nothing -> error "Imposible, siempre encontramos una variable."
                    Just (IntVar _ mv) -> return mv

-- | Evaluador de expresiones boleanas.
evalBExp :: BExp -> ProgState Bool
evalBExp (BRel rop e e') = evalRelOp rop (evalExp e) (evalExp e')
evalBExp (BUOp bop e)    = evalBoolUOp bop $ evalBExp e
evalBExp (BBOp bop e e') = evalBoolBOp bop (evalBExp e) (evalBExp e')
evalBExp (BCon b) = return b
evalBExp ide@(BoolId i) = do 
                    (st,win) <- ST.get
                    let idSts = vars st
                    
                    mvalue <- getValue i idSts
                    
                    case mvalue of
                        Just v -> return v
                        Nothing -> do
                            mvar  <- liftIO $ newEmptyMVar
                            liftIO $ postGUIAsync $ getNewValue win ExpectBool mvar
                            evalE <- liftIO $ takeMVar mvar
                                    
                            let idSts = vars st
                                idSts' = L.map (updateValue i evalE) idSts
                            
                            ST.put (st { vars =  idSts'},win)
                                    
                            return $ fromLeft evalE
    where
        getValue :: Identifier -> [StateTuple] -> ProgState (Maybe Bool)
        getValue i idSts = 
                case L.find (==(BoolVar i Nothing)) idSts of
                    Nothing -> error "Imposible, siempre encontramos una variable."
                    Just (BoolVar _ mv) -> return mv

-- | Actualiza el valor de un identificador en una tupla del estado.
updateValue :: Identifier -> Either Bool Int -> StateTuple -> StateTuple
updateValue i (Left v) stt@(BoolVar i' _) = if i == i' 
                                                then (BoolVar i $ Just v)
                                                else stt
updateValue i (Right v) stt@(IntVar i' _) = if i == i' 
                                                then (IntVar i $ Just v)
                                                else stt
updateValue _ _ stt = stt

-- | Agrega un identificador con su valor al estado.
addValue :: Identifier -> Either Bool Int -> State -> State
addValue i (Left v) st  = st {vars = (vars st)++[BoolVar i $ Just v]}
addValue i (Right v) st = st {vars = (vars st)++[IntVar i $ Just v]}

-- | Evaluador de los comandos.
evalExtComm :: ExtComm -> ProgState ()
evalExtComm (ExtSkip _) = return ()
evalExtComm (ExtAbort _) = return ()
evalExtComm (ExtAssert _ b) = return ()
evalExtComm (ExtIf _ b c c') = evalBExp b >>= \vb ->
                       if vb then evalExtComm c else evalExtComm c'
evalExtComm (ExtIAssig _ a e) = do 
                        evalE <- evalExp e
                        (st,win) <- ST.get 
                        let idSts = vars st
                        let idSts' = L.map (updateValue a (Right evalE)) idSts
                        ST.put (st { vars =  idSts'},win)
evalExtComm (ExtBAssig _ a e) = do 
                        evalE <- evalBExp e
                        (st,win) <- ST.get
                        let idSts = vars st
                        let idSts' = L.map (updateValue a (Left evalE)) idSts
                        ST.put (st { vars =  idSts'},win)
evalExtComm (ExtSeq c c') = evalExtComm c >> evalExtComm c'
evalExtComm (ExtDo _ inv b c) = fix evalDo
    where
        evalDo :: ProgState () -> ProgState ()
        evalDo f = do
                   vb <- evalBExp b
                   if vb then (evalExtComm (ExtSeq c (ExtAssert initPos inv))) >> f 
                         else return ()

evalStepExtComm :: ExtComm -> ProgState (Maybe ExtComm,Maybe ExtComm)
evalStepExtComm (ExtSeq c c') = evalStepExtComm c >>= \mcc' -> 
                          case (mcc') of
                              (Just c,Nothing)  -> return (Just c ,Just c')
                              (Just c,Just c'') -> return (Just c ,Just (ExtSeq c'' c'))
                              (Nothing,Just c)  -> return (Nothing,Just (ExtSeq c c'))
evalStepExtComm wc@(ExtDo _ _ b c) = do
                          vb <- evalBExp b
                          if vb 
                             then return (Nothing,Just $ ExtSeq c wc)
                             else return (Just wc,Nothing)
evalStepExtComm wc@(ExtIf _ b c c') = do
                          vb <- evalBExp b
                          if vb 
                             then return (Nothing,Just c)
                             else return (Nothing,Just c')
evalStepExtComm c = evalExtComm c >> return (Just c,Nothing)

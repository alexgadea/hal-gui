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
import Hal.Parser

-- Imports de Hal-Gui
import HGUI.Config
import HGUI.GState
import HGUI.ExtendedLang
import HGUI.Evaluation.EvalState

showErrMsg :: Window -> String -> IO ()
showErrMsg mainWin msg = postGUIAsync $ do
            win  <- windowNew
            vbox <- vBoxNew False 0
            
            label <- labelNew Nothing 
            set label [ labelLabel := formatErrorMsg msg
                      , labelUseMarkup := True
                      ] 
            
            readyB <- buttonNewWithLabel "Aceptar"
            
            set win [ windowWindowPosition := WinPosCenter
                    , windowModal          := True
                    , windowDecorated      := False
                    , windowHasFrame       := False
                    , windowTypeHint       := WindowTypeHintPopupMenu
                    , widgetCanFocus       := True
                    , windowTransientFor   := mainWin
                    ]
            
            containerAdd win vbox
            boxPackStart vbox label  PackNatural 2
            boxPackStart vbox readyB PackNatural 2
            
            onClicked readyB  $ widgetDestroy win
            
            widgetShowAll win
            
            return ()

-- | Evaluador de los operadores binarios enteros.
evalIntBOp :: IntBOp -> ProgState (Maybe Int) -> 
              ProgState (Maybe Int) -> ProgState (Maybe Int)
evalIntBOp Plus   = liftA2 $ liftA2 (+)
evalIntBOp Times  = liftA2 $ liftA2 (*)
evalIntBOp Substr = liftA2 $ liftA2 (-)
evalIntBOp Div    = liftA2 $ liftA2 div
evalIntBOp Mod    = liftA2 $ liftA2 mod

-- | Evaluador de los operadores binarios boleanos.
evalBoolBOp :: BoolBOp -> ProgState (Maybe Bool) -> 
               ProgState (Maybe Bool) -> ProgState (Maybe Bool)
evalBoolBOp And = liftA2 $ liftA2 (&&)
evalBoolBOp Or  = liftA2 $ liftA2 (||)

-- | Evaluador de los operadores unarios boleanos.
evalBoolUOp :: BoolUOp -> ProgState (Maybe Bool) -> ProgState (Maybe Bool)
evalBoolUOp Not = liftA $ fmap not

-- | Evaluador de las relaciones binarias.
evalRelOp :: (Eq a, Ord a) => 
             RelOp -> ProgState (Maybe a) -> ProgState (Maybe a) -> 
             ProgState (Maybe Bool)
evalRelOp Equal = liftA2 $ liftA2 (==)
evalRelOp Lt    = liftA2 $ liftA2 (<)

-- | Evaluador de expresiones enteras.
evalExp :: Exp -> ProgState (Maybe Int)
evalExp (IBOp iop e e') = evalIntBOp iop (evalExp e) (evalExp e')
evalExp (ICon i) = return $ Just i
evalExp ide@(IntId i) = do 
            (st,win) <- ST.get
            let idSts = vars st
            
            mvalue <- getValue i idSts
            
            maybe (liftIO (showErrMsg win defToInputMsg) >> return Nothing)
                    (return . Just) mvalue
    where
        getValue :: Identifier -> [StateTuple] -> ProgState (Maybe Int)
        getValue i idSts = 
                case L.find (==(IntVar i Nothing)) idSts of
                    Nothing -> error "Imposible, siempre encontramos una variable."
                    Just (IntVar _ mv) -> return mv

-- | Evaluador de expresiones boleanas.
evalBExp :: BExp -> ProgState (Maybe Bool)
evalBExp (BRel rop e e') = evalRelOp rop (evalExp e) (evalExp e')
evalBExp (BUOp bop e)    = evalBoolUOp bop $ evalBExp e
evalBExp (BBOp bop e e') = evalBoolBOp bop (evalBExp e) (evalBExp e')
evalBExp (BCon b) = return $ Just b
-- evalBExp ide@(BoolId i) = do 
--                     (st,win) <- ST.get
--                     let idSts = vars st
--                     
--                     mvalue <- getValue i idSts
--                     
--                     case mvalue of
--                         Just v -> return mvalue
--                         Nothing -> do
--                             mvar  <- liftIO $ newEmptyMVar
--                             liftIO $ postGUIAsync $ getNewValue win ExpectBool mvar
--                             mevalE <- liftIO $ takeMVar mvar
--                             
--                             case mevalE of
--                                 Nothing -> return Nothing
--                                 Just evalE -> do
--                                     let idSts = vars st
--                                         idSts' = L.map (updateValue i evalE) idSts
--                                     
--                                     ST.put (st { vars =  idSts'},win)
--                                             
--                                     return $ fromLeft evalE
--     where
--         getValue :: Identifier -> [StateTuple] -> ProgState (Maybe Bool)
--         getValue i idSts = 
--                 case L.find (==(BoolVar i Nothing)) idSts of
--                     Nothing -> error "Imposible, siempre encontramos una variable."
--                     Just (BoolVar _ mv) -> return mv

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
evalExtComm :: ExtComm -> ProgState (Maybe ())
evalExtComm (ExtSkip _) = return $ Just ()
evalExtComm (ExtAbort _) = return $ Just ()
evalExtComm (ExtAssert _ b) = return $ Just ()
evalExtComm (ExtIf _ b c c') = evalBExp b >>= \vb ->
                    case vb of
                        Nothing    -> return Nothing
                        Just True  -> evalExtComm c 
                        Just False -> evalExtComm c'
evalExtComm (ExtIAssig _ a e) = do 
            mevalE <- evalExp e
            case mevalE of
                Nothing -> return Nothing
                Just evalE -> do
                        (st,win) <- ST.get 
                        let idSts = vars st
                        let idSts' = L.map (updateValue a (Right evalE)) idSts
                        ST.put (st { vars =  idSts'},win)
                        return $ Just ()
evalExtComm (ExtBAssig _ a e) = do 
            mevalE <- evalBExp e
            
            case mevalE of
                Nothing -> return Nothing
                Just evalE -> do
                        (st,win) <- ST.get
                        let idSts = vars st
                        let idSts' = L.map (updateValue a (Left evalE)) idSts
                        ST.put (st { vars =  idSts'},win)
                        return $ Just ()
evalExtComm (ExtSeq c c') = evalExtComm c >> evalExtComm c'
evalExtComm (ExtDo _ inv b c) = fix evalDo
    where
        evalDo :: ProgState (Maybe ()) -> ProgState (Maybe ())
        evalDo f = do
            vb <- evalBExp b
            case vb of
                Nothing    -> return Nothing
                Just True  -> (evalExtComm (ExtSeq c (ExtAssert initPos inv))) >> f
                Just False -> return $ Just ()

evalStepExtComm :: ExtComm -> ProgState (Maybe (Maybe ExtComm,Maybe ExtComm))
evalStepExtComm (ExtSeq c c') = evalStepExtComm c >>= \mmcc' -> 
        case mmcc' of
            Nothing -> return Nothing
            Just (Just c,Nothing)  -> return $ Just (Just c ,Just c')
            Just (Just c,Just c'') -> return $ Just (Just c ,Just (ExtSeq c'' c'))
            Just (Nothing,Just c)  -> return $ Just (Nothing,Just (ExtSeq c c'))
evalStepExtComm wc@(ExtDo _ _ b c) = do
                vb <- evalBExp b
                case vb of
                    Nothing    -> return Nothing
                    Just True  -> return $ Just (Nothing,Just $ ExtSeq c wc)
                    Just False -> return $ Just (Just wc,Nothing)
evalStepExtComm wc@(ExtIf _ b c c') = do
                    vb <- evalBExp b
                    case vb of
                        Nothing    -> return Nothing
                        Just True  -> return $ Just (Nothing,Just c)
                        Just False -> return $ Just (Nothing,Just c')
evalStepExtComm c = evalExtComm c >>= \m ->
                    case m of
                        Nothing -> return Nothing
                        Just _  -> return (Just (Just c,Nothing))

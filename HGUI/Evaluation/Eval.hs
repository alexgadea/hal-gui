{- | Evaluador del lenguaje Hal
    
    Para evaluar asumimos el programa typechekeo sin problemas.
-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, DoAndIfThenElse #-}
module HGUI.Evaluation.Eval where

import Graphics.UI.Gtk hiding (get,Plus,eventKeyName)

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as ST (get,put)
import Control.Monad.Fix (fix)

import qualified Data.List as L
import qualified Data.Map as Map

-- Imports de equ.
import Equ.Expr
import qualified Equ.Types as EquType
import Equ.Syntax
import qualified Equ.PreExpr as PExpr
import qualified Equ.Theories.Arith as Arith
import qualified Equ.Theories.FOL as FOL

-- Imports de Fun
import qualified Fun.Eval.Eval as Fun

-- Imports de Hal
import Hal.Lang

-- Imports de Hal-Gui
import HGUI.Config
import HGUI.ExtendedLang
import HGUI.Evaluation.EvalState

showErrMsg :: Window -> String -> IO ()
showErrMsg mainWin msg = postGUIAsync $ do
            win  <- windowNew
            vbox <- vBoxNew False 0
            
            label <- labelNew (Nothing :: Maybe String)
            set label [ labelLabel := formatErrorMsg msg
                      , labelUseMarkup := True
                      ] 
            
            readyB <- buttonNewWithLabel ("Aceptar" :: String)
            
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
            
            _ <- onClicked readyB  $ widgetDestroy win
            
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
evalExp (IntId i) = do 
            (st,win) <- ST.get
            let idSts = vars st
            
            mvalue <- getValue i idSts
            
            maybe (liftIO (showErrMsg win defToInputMsg) >> return Nothing)
                  (return . Just) mvalue
    where
        getValue :: Identifier -> [StateTuple] -> ProgState (Maybe Int)
        getValue iden idSts = 
                case L.find (==(IntVar iden Nothing)) idSts of
                    Nothing -> error "Imposible, siempre encontramos una variable."
                    Just (BoolVar _ _) -> error "Imposible, siempre la variable es entera."
                    Just (IntVar _ mv) -> return mv

-- | Evaluador de expresiones boleanas.
evalBExp :: BExp -> ProgState (Maybe Bool)
evalBExp (BRel relop e e') = evalRelOp relop (evalExp e) (evalExp e')
evalBExp (BUOp bop e)    = evalBoolUOp bop $ evalBExp e
evalBExp (BBOp bop e e') = evalBoolBOp bop (evalBExp e) (evalBExp e')
evalBExp (BCon b) = return $ Just b
evalBExp (BoolId i) = do 
            (st,win) <- ST.get
            let idSts = vars st
            
            mvalue <- getValue i idSts
            
            maybe (liftIO (showErrMsg win defToInputMsg) >> return Nothing)
                  (return . Just) mvalue
    where
        getValue :: Identifier -> [StateTuple] -> ProgState (Maybe Bool)
        getValue iden idSts = 
            case L.find (==(BoolVar iden Nothing)) idSts of
                Nothing -> error "Imposible, siempre encontramos una variable."
                Just (IntVar _ _) -> error "Imposible, siempre la variable es booleana."
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

evalExprFun :: FormFun -> Bool -> ProgState (Maybe ())
evalExprFun (Expr f) isPre = 
                       if PExpr.preExprIsQuant f then return (Just ())
                       else do
                       (st,win) <- ST.get
                       let funExpr = Fun.eval f (makeEnv st) 
                       if Expr funExpr == FOL.true
                       then return $ Just ()
                       else if Expr funExpr == FOL.false
                       then liftIO (showErrMsg win guardFalseMsg) >>
                            if isPre then return (Just ())
                                     else return Nothing
                       else liftIO (showErrMsg win guardTypeErrorMsg) >> 
                            return Nothing
    where
        makeEnv :: State -> Fun.EvalEnv
        makeEnv = foldl makeFunVar Map.empty . vars
        makeFunVar :: Fun.EvalEnv -> StateTuple -> Fun.EvalEnv
        makeFunVar env (IntVar  i mv) = makeEFunVar env i mv
        makeFunVar env (BoolVar i mv) = makeBFunVar env i mv
        
        makeEFunVar :: Fun.EvalEnv -> Identifier -> Maybe Int -> Fun.EvalEnv
        makeEFunVar env i (Just v) = 
                            Map.insert (makevar i) ([],(makeIntVal v)) env
        makeEFunVar env _ _ = env
        makeBFunVar :: Fun.EvalEnv -> Identifier -> Maybe Bool -> Fun.EvalEnv
        makeBFunVar env i (Just v) = 
                            Map.insert (makevar i) ([],(makeBoolVal v)) env
        makeBFunVar env _ _ = env
        
        makevar :: Identifier -> Variable
        makevar i = PExpr.var (idName i) (makeType i)
        makeType :: Identifier -> EquType.Type
        makeType i = case idDataType i of
                        IntTy  -> EquType.tyInt
                        BoolTy -> EquType.tyBool
        
        makeBoolVal :: Bool -> PExpr.PreExpr
        makeBoolVal True  = let Expr t = FOL.true in t
        makeBoolVal False = let Expr t = FOL.false in t
        makeIntVal :: Int -> PExpr.PreExpr
        makeIntVal 0     = let Expr z  = Arith.zero in z
        makeIntVal n = let Expr n' = Arith.successor (Expr $ makeIntVal (n-1)) 
                           in n'

-- | Evaluador de los comandos.
evalExtComm :: ExtComm -> ProgState (Maybe ())
evalExtComm (ExtSkip _) = return $ Just ()
evalExtComm (ExtAbort _) = ST.get >>= \(_,win) -> 
                           liftIO (showErrMsg win abortMsg) >> return Nothing
evalExtComm (ExtAssert _ b) = evalExprFun b False
evalExtComm (ExtPre _ f) = evalExprFun f True
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
        Just (Just rc,Nothing) -> return $ Just (Just rc ,Just c')
        Just (Just rc,Just lc) -> return $ Just (Just rc ,Just (ExtSeq lc c'))
        Just (Nothing,Just lc) -> return $ Just (Nothing,Just (ExtSeq lc c'))
        Just (Nothing,Nothing) -> error "Impossible"
evalStepExtComm wc@(ExtDo _ inv b c) = do
        vb   <- evalBExp b
        vinv <- evalExprFun inv False
        case (vb,vinv) of
            (Nothing,_)    -> return Nothing
            (_,Nothing)    -> return Nothing
            (Just True,Just _)  -> return $ Just (Nothing,Just $ ExtSeq c wc)
            (Just False,Just _) -> return $ Just (Just wc,Nothing)
evalStepExtComm (ExtIf _ b c c') = do
        vb <- evalBExp b
        case vb of
            Nothing    -> return Nothing
            Just True  -> return $ Just (Nothing,Just c)
            Just False -> return $ Just (Nothing,Just c')
evalStepExtComm c = evalExtComm c >>= \m ->
        case m of
            Nothing -> return Nothing
            Just _  -> return (Just (Just c,Nothing))

{-# LANGUAGE RecordWildCards #-}
module HGUI.Evaluation.EvalState where

import Graphics.UI.Gtk (Window)

import Control.Monad.Trans.State (StateT)


-- Imports de Hal
import Hal.Lang

-- Imports de Hal-Gui
import HGUI.ExtendedLang

data ExecState = ExecState { executedTracePrg  :: Maybe ExtComm
                           , nexecutedTracePrg :: Maybe ExtComm
                           , prgState          :: State
                           , prgBreaks         :: [Int]
                           }

makeExecState :: ExtProgram -> ExecState
makeExecState (ExtProg vars pre comms post) = ExecState Nothing 
                                                        (Just comms)
                                                        (fillState initState vars)
                                                        []

restartExecSt :: ExecState -> ExtProgram -> ExecState
restartExecSt (ExecState _ _ st _) (ExtProg _ _ c _) = 
    ExecState Nothing (Just c) (fillState initState $ takeIdentifiers st) []

updateExecState :: ExecState -> (Maybe ExtComm,Maybe ExtComm) -> State -> ExecState
updateExecState execSt (mc,mc') st = 
        case (execSt,mc) of
            (ExecState Nothing _ _ bs,_) -> ExecState mc mc' st bs
            (ExecState (Just exec) _ _ bs,Just c) -> ExecState (Just $ ExtSeq exec c) mc' st bs
            (ExecState (Just exec) _ _ bs,Nothing) -> ExecState (Just exec) mc' st bs

addBreak :: ExecState -> Int -> Maybe ExecState
addBreak execSt b = if b  `elem`   (getValidLines execSt)
                       then Just $ execSt {prgBreaks = b : prgBreaks execSt}
                       else Nothing

getValidLines :: ExecState -> [Int]
getValidLines (ExecState mc mc' _ _) = case (mc,mc') of
                                           (Nothing,Nothing) -> []
                                           (Just c,Nothing)  -> getCommLines c
                                           (Nothing,Just c') -> getCommLines c'
                                           (Just c,Just c')  -> getCommLines c 
                                                                ++ 
                                                                getCommLines c'

headNExecComm :: ExecState -> Maybe ExtComm
headNExecComm (ExecState _ Nothing _ _)      = Nothing
headNExecComm (ExecState _ (Just comms) _ _) = Just $ takeHead comms
    where
        takeHead :: ExtComm -> ExtComm
        takeHead (ExtSeq c c') = takeHead c
        takeHead c = c

-- | Elemento de un estado. Representa el valor de una variable en un momento
-- de la evaluación.
data StateTuple = IntVar  Identifier (Maybe Int)
                | BoolVar Identifier (Maybe Bool)

instance Show StateTuple where
    show (IntVar  i mi) = show i ++ ":" ++ prettyMaybe mi
    show (BoolVar i mb) = show i ++ ":" ++ prettyMaybe mb

takeIdentifier :: StateTuple -> Identifier
takeIdentifier (IntVar i _) = i
takeIdentifier (BoolVar i _) = i
    
prettyMaybe :: Show a => Maybe a -> String
prettyMaybe Nothing  = "Sin valor."
prettyMaybe (Just v) = show v
    
fromRight :: Either a b -> b
fromRight (Right b) = b

fromLeft :: Either a b -> a
fromLeft (Left a) = a
    
instance Eq StateTuple where
    (IntVar i _) == (IntVar i' _) = i == i'
    (BoolVar i _) == (BoolVar i' _) = i == i'
    _ == _ = False

type EitherBI = Either Bool Int

data ExpectValue = ExpectBool | ExpectInt

-- | Estado de la evaluación.
data State = State { vars :: [StateTuple] }

-- | Mónada de la semántica denotacional.
type ProgState = StateT (State,Window) IO

instance Show State where
    show (State vars) = show vars

initState :: State
initState = makeState []

makeState :: [StateTuple] -> State
makeState = State 

makePrgState :: [(Identifier,EitherBI)] -> State
makePrgState = makeState . map (\(i,ev) -> either (BoolVar i . Just) (IntVar i . Just) ev)

takeIdentifiers :: State -> [Identifier]
takeIdentifiers = map takeIdentifier . vars

takeInputsIdentifiers :: State -> [Identifier]
takeInputsIdentifiers = filter ((==) IsInput . idType) . takeIdentifiers 

takeDefsIdentifiers :: State -> [Identifier]
takeDefsIdentifiers = filter ((==) IsVar . idType) . takeIdentifiers 

takeDefStateTuples :: State -> [StateTuple]
takeDefStateTuples = filter isVarDef . vars

isVarDef :: StateTuple -> Bool
isVarDef (IntVar i _) = IsVar == idType i
isVarDef (BoolVar i _) = IsVar == idType i

addInputsValue :: State -> [(Identifier,EitherBI)] -> State
addInputsValue st iev = let
                        prgDefSt   = makeState $ takeDefStateTuples st
                        prgInputSt = makePrgState iev
                        in
                        concatPrgState prgDefSt prgInputSt
               
concatPrgState :: State -> State -> State
concatPrgState st st' = State {vars = vars st ++ vars st'}

fillState :: State -> [Identifier] -> State
fillState st vars = st {vars = map makeVar vars}

makeVar :: Identifier -> StateTuple
makeVar i@(Identifier {..}) = case idDataType of
                                IntTy  -> IntVar  i Nothing
                                BoolTy -> BoolVar i Nothing

module HGUI.EvalConsole where

import Graphics.UI.Gtk hiding (get,eventKeyName)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Events hiding ( eventButton, eventClick)

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.State as ST (runStateT,evalStateT,get,put)
import Control.Monad.Trans.Cont
import Control.Arrow
import Control.Applicative((<$>))

import Control.Concurrent

import Lens.Family

import Data.Text (pack)
import Data.Maybe (fromJust,isJust)
import Data.IORef
import Data.Reference
import Data.Monoid (mempty)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Strict.Either as SEither

import Text.Parsec
import Text.Parsec.String

import HGUI.GState
import HGUI.Config
import HGUI.SymbolList
import HGUI.Utils
import HGUI.Console

import Control.Concurrent(forkIO)
import System.IO (hFlush,stdout)

import Hal.Lang
import Hal.Parser

import Hal.Evaluation.Eval
import Hal.Interpreter.Interpreter
import Hal.Interpreter.Parser

compile :: GuiMonad ()
compile = get >>= \ref -> ask >>= \content ->
        do
        code <- getCode
        case parseFromString code of
            Left er -> return ()
            Right prg -> io (processCmd' ref (Load prg)) >> return ()
        return ()

makeConsoleState :: HGReader -> HGStateRef -> IState
makeConsoleState gReader gStateRef = 
        makeIState Nothing 
                   Nothing 
                   (makeState [] getNewValue) -- El getNewValue este es el de
                                              -- Hal.Evaluation.Eval, por eso
                                              -- pide el valor de una variable
                                              -- por consola. Todavía no me sale
                                              -- hacerlo bien por la GUI.
--     where
--         getNewValue :: Identifier -> ExpectValue -> ProgState EitherBI
--         getNewValue i ev =
--             do
--             let win = gReader ^. gHalWindow
--             io $ getNewExp i ev win
--             
--             waitResult i
--         
--         waitResult :: Identifier -> ProgState EitherBI
--         waitResult i = do
--             st <- readRef gStateRef
--             let istate = fromJust $ st ^. gHalConsoleState
--                 prgSt  = prgState istate
--                 idSts  = vars prgSt
--             
--             case idDataType i of
--                 BoolTy -> do
--                           let val = fromJust $ L.find (==(BoolVar i Nothing)) idSts
--                           case val of
--                                (BoolVar i (Just v)) -> return $ Left v
--                                _ -> waitResult i
--                 IntTy  -> do
--                           let val = fromJust $ L.find (==(IntVar i Nothing)) idSts
--                           case val of
--                                (IntVar i (Just v)) -> return $ Right v
--                                _ -> waitResult i
--             
--         getNewExp :: Identifier -> ExpectValue -> Window -> IO ()
--         getNewExp i ev mainWin = postGUISync $
--                        do
--                        win      <- windowNew
--                        vbox     <- vBoxNew False 0
--                        entry    <- entryNew
--                        errLabel <- labelNew Nothing
--                        
--                        set win [ windowWindowPosition := WinPosMouse
--                                , windowModal          := True
--                                , windowDecorated      := False
--                                , windowHasFrame       := False
--                                , windowTypeHint       := WindowTypeHintPopupMenu
--                                , widgetCanFocus       := True
--                                , windowTransientFor   := mainWin
--                                ]
--                        
--                        containerAdd win vbox
--                        boxPackStart vbox entry    PackNatural 1
--                        boxPackStart vbox errLabel PackNatural 1
--                        widgetSetNoShowAll errLabel True
--                        widgetShowAll win
--                        
--                        onKeyPress entry $ configEntry win i ev entry errLabel
--                         
--                        return ()
--         
--         updateProgState i (Left v) = do
--                                    stRef <- ST.get
--                                    st    <- readRef stRef
--                                    let istate = fromJust $ st ^. gHalConsoleState
--                                        prgSt  = prgState istate
--                                    v' <- io $ ST.evalStateT (evalBExp v) prgSt
--                                    let idSts   = vars prgSt
--                                        idSts'  = L.map (updateValue i (Left v')) idSts
--                                        prgSt'  = prgSt {vars = idSts'}
--                                        istate' = istate {prgState = prgSt'}
--                                    writeRef stRef ((<~) gHalConsoleState (Just istate') st)
--                                    return ()
--         updateProgState i (Right v) = do
--                                    stRef <- ST.get
--                                    st    <- readRef stRef
--                                    let istate = fromJust $ st ^. gHalConsoleState
--                                        prgSt  = prgState istate
--                                    v' <- io $ ST.evalStateT (evalExp v) prgSt
--                                    let idSts   = vars prgSt
--                                        idSts'  = L.map (updateValue i (Right v')) idSts
--                                        prgSt'  = prgSt {vars = idSts'}
--                                        istate' = istate {prgState = prgSt'}
--                                    writeRef stRef ((<~) gHalConsoleState (Just istate') st)
--                                    return ()
--         configEntry ::  Window -> Identifier -> ExpectValue -> Entry -> Label -> Event -> IO Bool
--         configEntry win i ev entry label event = 
--                     do
--                     b <- case eventKeyName event of
--                             "Return" -> getValue
--                             "Escape" -> return True
--                             _        -> return False
--                     if b then widgetDestroy win >> return False
--                          else return False
--             where
--                 getValue :: IO Bool
--                 getValue = do
--                     strValue <- entryGetText entry
--                     case ev of
--                         ExpectBool -> case parseBConFromString strValue of
--                                            Left er -> setMsg "Valor no valido, intente de nuevo.\n" >> return False
--                                            Right v -> ST.evalStateT (updateProgState i $ Left v) gStateRef >> return True
--                         ExpectInt -> case parseConFromString strValue of
--                                            Left er -> setMsg "Valor no valido, intente de nuevo.\n" >> return False
--                                            Right v -> ST.evalStateT (updateProgState i $ Right v) gStateRef >> return True
--                     where
--                         setMsg :: String -> IO ()
--                         setMsg msg = widgetSetNoShowAll label False >>
--                                      labelSetText label msg >> 
--                                      widgetShowAll label

prependPrompt :: String -> String
prependPrompt = ("hal> "++)

resetEnv :: GuiMonad ()
resetEnv = ask >>= \content ->
           get >>= \ref ->
           io $ do
             let entry = content ^. (gHalCommConsole . commEntry)
             let buf = content ^. (gHalCommConsole . commTBuffer)
             let tv = content ^. (gHalCommConsole . commTView)
             printInfoMsg "Programa cargado" buf tv

configCommandConsole :: GuiMonad ()
configCommandConsole = ask >>= \content -> get >>= \ref ->
           io $ do
                let entry = content ^. (gHalCommConsole . commEntry)
                    buf = content ^. (gHalCommConsole . commTBuffer)
                    tv = content ^. (gHalCommConsole . commTView)
                configConsoleTV tv buf
                do _ <- entry `on` entryActivate $ io $
                        do
                        cmdLine <- entryGetText entry
                        forkIO (processCmd cmdLine ref >>= \res ->
                                res `seq`
                                (postGUIAsync $ putResult res buf tv >>
                                                      scrollTV buf tv))
                        entrySetText entry ""
                        printInfoMsg (prependPrompt cmdLine) buf tv
                        return ()
                   return ()
                let istate = makeConsoleState content ref
                st <- readRef ref
                writeRef ref ((<~) gHalConsoleState (Just istate) st)

processCmd :: String -> HGStateRef -> IO (Either String String)
processCmd cmdLine ref = 
        do
        case parseICFromString cmdLine of
            Right Restart -> processCmd' ref Restart
            Right Step    -> processCmd' ref Step
            Right View    -> processCmd' ref View
            Right _ -> return $ Left "Comando no reconocido."
            Left er -> return $ 
                         Left $ "Comando no reconocido.\n\n" 
                                ++ 
                                show er 

processCmd' :: HGStateRef -> ICommand -> IO (Either String String)
processCmd' ref cmd = do 
    st <- readRef ref
    (res,ist') <- ST.runStateT (evalInterpreter cmd) (fromJust $ st ^. gHalConsoleState)
    writeRef ref ((<~) gHalConsoleState (Just ist') st)
    return $ Right $ maybe "" id res

putResult :: Either String String -> TextBuffer -> TextView -> IO ()
putResult (Left er) = printErrorMsg er
putResult (Right e) = printInfoMsg e

scrollTV :: TextBuffer -> TextView -> IO ()
scrollTV buf tv = 
    do
        titer2 <- textBufferGetEndIter buf
        -- textViewScrollToIter no anda bien, por eso uso scrollToMark
        mark <- textBufferCreateMark buf Nothing titer2 False
        textViewScrollToMark tv mark 0 Nothing
        widgetShowAll tv

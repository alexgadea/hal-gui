module HGUI.EvalConsole where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView

import Control.Monad (when,unless,forM,forM_)
import qualified Control.Monad.Trans.Reader as R (ask)
import Control.Monad.Trans.RWS 
import qualified Control.Monad.Trans.State as ST (runStateT,evalStateT)

import Control.Concurrent

import Lens.Family

import Data.Maybe
import Data.Text (unpack)
import Data.Reference

import Hal.Parser
import Hal.Lang (Identifier (..), IdType (..), Type (..))

import HGUI.ExtendedLang
import HGUI.Config
import HGUI.GState
import HGUI.Parser
import HGUI.Utils
import HGUI.Console
import HGUI.Evaluation.Eval
import HGUI.Evaluation.EvalState

configEvalButton :: GuiMonad ()
configEvalButton = ask >>= \content -> do
            let ebutton = content ^. (gHalToolbar . evalButton)
            
            active <- io $ toggleToolButtonGetActive ebutton
            if active 
                then onActive
                else onDeactive
    where
        onDeactive :: GuiMonad ()
        onDeactive = ask >>= \content -> do
            let ebox      = content ^. (gHalCommConsole . cEvalBox)
                evalstbox = content ^. (gHalCommConsole . cEvalStateBox)
                tv        = content ^. gTextCode
            updateHGState ((<~) gHalConsoleState Nothing)
            
            cleanPaintLine $ castToTextView tv
            
            io $ textViewSetEditable tv True
            io $ widgetHideAll ebox
            io $ widgetHideAll evalstbox
        
        onActive :: GuiMonad ()
        onActive = ask >>= \content -> do
            let ebox      = content ^. (gHalCommConsole . cEvalBox)
                evalstbox = content ^. (gHalCommConsole . cEvalStateBox)
                tv        = content ^. gTextCode
                ebutton   = content ^. (gHalToolbar . evalButton)
            
            mprg <- compile'
            case mprg of
                Nothing -> io (toggleToolButtonSetActive ebutton False)
                            >> return ()
                Just prg -> do
                    let mExecState = Just $ makeExecState prg
                        stbox      = content ^. (gHalCommConsole . cStateBox)
                    
                    updateHGState ((<~) gHalConsoleState mExecState)
                    
                    startExecState mExecState
                    startStateView stbox $ prgState $ makeExecState prg
                    
                    io $ textViewSetEditable tv False
                    io $ widgetShowAll ebox
                    io $ widgetShowAll evalstbox

startExecState :: Maybe ExecState -> GuiMonad ()
startExecState Nothing   = return ()
startExecState (Just st) = do
                           let headC = headNExecComm st
                           maybe (return ()) paintLine headC

startStateView :: VBox -> State -> GuiMonad ()
startStateView stBox st = io $ do
            containerForall stBox (containerRemove stBox)
            
            mapM_ fillStBox $ takeIdentifiers st
            
            return ()
    where
        fillStBox :: Identifier -> IO ()
        fillStBox i = do
                hb <- hBoxNew False 2
                vl <- labelNew $ Just $ show i ++ " ="
                vl' <- labelNew $ Just "Sin valor"
                boxPackStart hb vl PackNatural 2
                boxPackStart hb vl' PackNatural 2
                set vl [ miscXalign := 0 
                       , miscXpad := 10
                       ]
                boxPackStart stBox hb PackNatural 2

updateStateView :: Bool -> State -> GuiMonad ()
updateStateView cleanAll prgSt = 
                ask >>= \content -> getHGState >>= \st -> do
                let stBox = content ^. (gHalCommConsole . cStateBox)
                childs <- io $ containerGetChildren stBox
                io $ postGUIAsync $ forM_ (zip childs (vars prgSt)) updateValue
    where
        updateValue :: (Widget,StateTuple) -> IO ()
        updateValue (w,BoolVar i mv) = updateValue' w i mv
        updateValue (w,IntVar  i mv) = updateValue' w i mv
        updateValue' :: Show a => Widget -> Identifier -> Maybe a -> IO ()
        updateValue' w i mv = do
                let hb = castToHBox w
                
                childs <- containerGetChildren hb
                
                let lv  = castToLabel $ childs!!0
                    lv' = castToLabel $ childs!!1
                
                labelSetText lv  $ show i ++ " ="
                labelSetText lv' $ maybe "Sin Valor" show mv

cleanPaintLine :: TextView -> GuiMonad ()
cleanPaintLine = io . cleanPaintLineIO

cleanPaintLineIO :: TextView -> IO ()
cleanPaintLineIO tv = do
            buf      <- textViewGetBuffer tv
            table    <- textBufferGetTagTable buf  
            mtag <- textTagTableLookup table "HighlightLine"
            maybe (return ()) (textTagTableRemove table) mtag

paintLine :: ExtComm -> GuiMonad ()
paintLine comm = ask >>= io . paintLineIO comm

paintLineIO :: ExtComm -> HGReader -> IO ()
paintLineIO comm content = do
            let textV = content ^. gTextCode
                line  = takeCommLine comm
            
            buf   <- textViewGetBuffer textV
            start <- textBufferGetStartIter buf
            end   <- textBufferGetEndIter buf
            
            tag   <- textTagNew (Just "HighlightLine")
            set tag [ textTagParagraphBackground := evalLineColour
                    , textTagParagraphBackgroundSet := True
                    ]
            
            table <- textBufferGetTagTable buf
            textTagTableAdd table tag
            
            textIterForwardLines start (line-1)
            start' <- textIterCopy start
            textIterForwardLines start' 1
            
            textBufferApplyTag buf tag start start'
            
            textBufferGetText buf start end False
            return ()

configEvalConsole :: GuiMonad ()
configEvalConsole = ask >>= \content -> get >>= \st -> io $ do
                let ebox     = content ^. (gHalCommConsole . cEvalBox      )
                    stbox    = content ^. (gHalCommConsole . cEvalStateBox )
                    stepB    = content ^. (gHalCommConsole . cStepButton   )
                    contB    = content ^. (gHalCommConsole . cContButton   )
                    breakB   = content ^. (gHalCommConsole . cBreakButton  )
                    restartB = content ^. (gHalCommConsole . cRestartButton)
                    cleanB   = content ^. (gHalCommConsole . cCleanButton  )
                    stopB    = content ^. (gHalCommConsole . cStopButton   )
                    forkFlag = content ^. gHalForkFlag
                
                onClicked stepB    (forkEvalStep forkFlag content st)
                onClicked contB    (forkEvalCont forkFlag content st)
                onClicked breakB   (eval evalBreak   content st)
                onClicked restartB (eval evalRestart content st)
                onClicked cleanB   (eval evalClean   content st)
                onClicked stopB    (eval evalStop    content st)
                
                widgetHideAll ebox
                widgetHideAll stbox

evalStop :: GuiMonad ()
evalStop = ask >>= \content -> io $ do
            let stopFlag = content ^. gHalStopFlag
                forkFlag = content ^. gHalForkFlag
            
            flagUpFlag <- io $ isEmptyMVar forkFlag
            if flagUpFlag 
               then return ()
               else takeMVar stopFlag >> return ()

forkEvalStep :: MVar () -> HGReader -> HGStateRef -> IO ()
forkEvalStep fflag content st = do
        flagUpFork <- tryPutMVar fflag ()
        if flagUpFork
            then forkIO (eval (evalStep >> return ()) content st >> 
                        takeMVar fflag >> return ()) >> return ()
            else return ()

evalStep :: GuiMonad Bool
evalStep = getHGState >>= \st -> do
    let Just execSt = st ^. gHalConsoleState
        prgSt       = prgState execSt
        mexecComm   = executedTracePrg execSt
        mnexecComm  = nexecutedTracePrg execSt
    
    flagSt <- io $ newEmptyMVar
    
    maybe (takeInputs prgSt flagSt)
          (const $ io $ putMVar flagSt (Just prgSt)) mexecComm
    
    mPrgSt <- io $ takeMVar flagSt
    
    case mPrgSt of
        Nothing -> return False
        Just prgSt -> do
            case mnexecComm of
                Nothing -> return True
                Just nexecComm -> 
                    ask >>= \content -> do
                    let win = content ^. gHalWindow
                    
                    (mmc,(prgSt',_)) <- io $ ST.runStateT (evalStepExtComm nexecComm) (prgSt,win)
                    
                    case mmc of
                        Nothing -> return False
                        Just mc -> do
                            let execSt' = updateExecState execSt mc prgSt'
                                headC   = headNExecComm execSt'
                                tv      = content ^. gTextCode
                            updateHGState ((<~) gHalConsoleState (Just execSt'))
                            io $ postGUIAsync $ cleanPaintLineIO $ castToTextView tv
                            updateStateView False prgSt'
                            maybe (return ()) 
                                  (io . postGUIAsync . flip paintLineIO content) headC
                            return True

takeInputs :: State -> MVar (Maybe State) -> GuiMonad ()
takeInputs prgSt flagSt = ask >>= \content -> 
                          getHGState >>= \st -> 
                          io $ postGUIAsync $ do
            let Just execSt = st ^. gHalConsoleState
                ids         = takeInputsIdentifiers $ prgState execSt
                mainWin     = content ^. gHalWindow
            
            if null ids
               then putMVar flagSt (Just $ prgState execSt)
               else do
                    win      <- windowNew
                    vbox     <- vBoxNew False 0
                    
                    buttonBox <- hBoxNew False 0
                    readyB    <- buttonNewWithLabel "Aceptar"
                    cancelB   <- buttonNewWithLabel "Cancelar"
                    containerAdd buttonBox readyB
                    containerAdd buttonBox cancelB
                    
                    set win [ windowWindowPosition := WinPosCenter
                            , windowModal          := True
                            , windowDecorated      := False
                            , windowHasFrame       := False
                            , windowTypeHint       := WindowTypeHintPopupMenu
                            , widgetCanFocus       := True
                            , windowTransientFor   := mainWin
                            ]
                    
                    containerAdd win vbox
                    
                    (idsBox,iels) <- fillEntryIds ids
                    
                    on win keyPressEvent $ configWinAccions win iels
                    
                    configButtons win readyB cancelB iels flagSt
                    
                    containerAdd vbox idsBox
                    containerAdd vbox buttonBox
                    
                    widgetShowAll win
                    
                    return ()
    where
        configWinAccions :: Window -> [(Identifier,Entry, Label)] -> EventM EKey Bool
        configWinAccions win iels = do
                           ev <- eventKeyName
                           case ev of
                               "Escape" -> io $ putMVar flagSt Nothing >> 
                                                widgetDestroy win >> 
                                                return True
                               "Return" -> io $ checkEntrys win iels >> 
                                                return True
                               _        -> return False
        configButtons :: Window -> Button -> Button -> 
                         [(Identifier,Entry, Label)] -> MVar (Maybe State) -> IO ()
        configButtons win readyB cancelB iels flagSt = do
            
            onClicked cancelB $ putMVar flagSt Nothing >> widgetDestroy win
            onClicked readyB  $ checkEntrys win iels
            
            return ()
            
        checkEntrys :: Window -> [(Identifier,Entry, Label)] -> IO ()
        checkEntrys win iels = forM iels checkEntry >>= \check ->
                           if and $ map isJust $ check
                              then putMVar flagSt (Just $ addInputsValue prgSt $ catMaybes check) >> widgetDestroy win
                              else return ()
            where
                checkEntry :: (Identifier,Entry, Label) -> IO (Maybe (Identifier,EitherBI))
                checkEntry (i,entry,label) = getValue win i entry label
        
        fillEntryIds :: [Identifier] -> IO (VBox,[(Identifier,Entry, Label)])
        fillEntryIds ids = do
                           idsBox <- vBoxNew False 0
                           fillEntryIds' ids [] idsBox
        
        fillEntryIds' :: [Identifier] -> [(Identifier,Entry, Label)] -> 
                         VBox -> IO (VBox,[(Identifier,Entry, Label)])
        fillEntryIds' [] iels idsBox = return (idsBox,iels)
        fillEntryIds' (i:ids) iels idsBox = do
                            idLabel  <- labelNew (Just $ unpack (idName i) 
                                                         ++ ":" ++ 
                                                         show (idDataType i))
                            entry    <- entryNew
                            errLabel <- labelNew Nothing
                            hbox     <- hBoxNew False 0
                            
                            boxPackStart hbox idLabel  PackNatural 1
                            boxPackStart hbox entry    PackNatural 1
                            boxPackStart hbox errLabel PackNatural 1
                            widgetSetNoShowAll errLabel True
                            
                            containerAdd idsBox hbox
                            
                            fillEntryIds' ids ((i,entry,errLabel):iels) idsBox
        
        getValue :: Window -> Identifier -> Entry -> 
                    Label -> IO (Maybe (Identifier,EitherBI))
        getValue mainWin i entry label = do
            strValue <- entryGetText entry
            case idDataType i of
                BoolTy -> case parseBConFromString strValue of
                            Left er -> setMsg "Valor no valido." >> 
                                       return Nothing
                            Right v -> do
                                    setMsg ""
                                    Just v' <- ST.evalStateT (evalBExp v) (initState,mainWin)
                                    return $ Just $ (i,Left $ v')
                IntTy -> case parseConFromString strValue of
                            Left er -> setMsg "Valor no valido." >> 
                                       return Nothing
                            Right v -> do
                                    setMsg ""
                                    Just v' <- ST.evalStateT (evalExp v) (initState,mainWin)
                                    return $ Just $ (i,Right $ v')
            where
                setMsg :: String -> IO ()
                setMsg msg = do
                             widgetSetNoShowAll label False
                             
                             let msg' = formatErrorMsg msg
                             
                             set label [ labelLabel := msg'
                                       , labelUseMarkup := True
                                       ] 
                             
                             widgetShowAll label
                formatErrorMsg :: String -> String
                formatErrorMsg msg = "<span foreground=\"red\">"++msg++"</span>"

forkEvalCont :: MVar () -> HGReader -> HGStateRef -> IO ()
forkEvalCont fflag content st = do
                flagUpFork <- tryPutMVar fflag ()
                if flagUpFork 
                    then forkIO evalC >> return ()
                    else return ()
    where
        evalC :: IO ()
        evalC = eval evalCont content st >> 
                takeMVar fflag >>
                return ()

evalCont :: GuiMonad ()
evalCont = ask >>= \content -> do
        let stopFlag = content ^. gHalStopFlag
        flagUpStop <- fmap not $ io $ isEmptyMVar stopFlag
        if flagUpStop
            then evalC
            else io $ putMVar stopFlag () >> return ()
    where
        evalC :: GuiMonad ()
        evalC = do
            io $ threadDelay evalContDelay
            makeStep <- evalStep
            
            st <- getHGState
            let Just execSt = st ^. gHalConsoleState
                mnexecComm = nexecutedTracePrg execSt
                
            case (mnexecComm,makeStep) of
                (Nothing,_)        -> return ()
                (_,False)          -> return ()
                (Just nexecComm,_) -> do 
                                let line   = takeCommLine nexecComm
                                    breaks = prgBreaks execSt 
                                if (line `elem` breaks) 
                                    then return ()
                                    else evalCont

evalBreak :: GuiMonad ()
evalBreak = ask >>= \content -> getHGState >>= \st -> do
            let textV  = content ^. gTextCode
                Just execSt = st ^. gHalConsoleState
            
            buf  <- io $ textViewGetBuffer textV
            mark <- io $ textBufferGetInsert buf
            iter <- io $ textBufferGetIterAtMark buf mark
            i <- io $ textIterGetLine iter
            
            gutter    <- io $ sourceViewGetGutter textV TextWindowLeft
            existMark <- delMarkBreak gutter buf i
            when existMark $
               case addBreak execSt (i+1) of
                   Nothing -> return ()
                   Just execSt' -> do
                        addMarkBreak buf iter
                        updateHGState ((<~) gHalConsoleState (Just $ execSt'))
    where
        delMarkBreak :: SourceGutter -> TextBuffer -> Int -> GuiMonad Bool
        delMarkBreak gutter buf i = io $ do
                let sbuf = (castToSourceBuffer buf)
                marks <- sourceBufferGetSourceMarksAtLine sbuf i breakMark
                mapM_ (delMark sbuf) marks
                sourceGutterQueueDraw gutter
                return $ null marks
        addMarkBreak :: TextBuffer -> TextIter -> GuiMonad ()
        addMarkBreak buf iter = io $ do
                sourceBufferCreateSourceMark (castToSourceBuffer buf) 
                                             Nothing
                                             breakMark 
                                             iter
                return ()

evalRestart :: GuiMonad ()
evalRestart = ask >>= \content -> getHGState >>= \st -> do
              let Just execSt = st ^. gHalConsoleState
                  Just prg    = st ^. gHalPrg
                  execSt'     = restartExecSt execSt prg
                  headC       = headNExecComm execSt'
                  tv          = content ^. gTextCode
              
              cleanPaintLine $ castToTextView tv
              updateHGState ((<~) gHalConsoleState (Just execSt'))
              updateStateView True $ prgState execSt'
              maybe (return ()) paintLine headC
              io $ mapM_ (removeAllMarks tv) (prgBreaks execSt)

evalClean :: GuiMonad ()
evalClean = ask >>= \content -> getHGState >>= \st -> do
            let Just execSt = st ^. gHalConsoleState
                sv          = content ^. gTextCode
            updateHGState ((<~) gHalConsoleState (Just execSt { prgBreaks = [] }))
            io $ mapM_ (removeAllMarks sv) (prgBreaks execSt)

removeAllMarks :: SourceView -> Int -> IO ()
removeAllMarks sv i = do
            buf    <- textViewGetBuffer sv
            gutter <- sourceViewGetGutter sv TextWindowLeft
            let sbuf = (castToSourceBuffer buf)
            marks <- sourceBufferGetSourceMarksAtLine sbuf (i-1) breakMark
            mapM_ (delMark sbuf) marks
            sourceGutterQueueDraw gutter

delMark :: SourceBuffer -> SourceMark -> IO ()
delMark sbuf mark = do
                    cat  <- sourceMarkGetCategory mark 
                    mmark <- textBufferGetMark sbuf breakMark
                    case (cat == breakMark,mmark) of
                        (_,Nothing) -> return ()
                        (False,_)   -> return ()
                        (True,Just mark) -> textBufferDeleteMark sbuf mark

compile' :: GuiMonad (Maybe ExtProgram)
compile' = get >>= \ref -> ask >>= \content ->
        do
        let tcode = content ^. gTextCode
        code <- getCode tcode
        case parseExtPrgFromString code of
            Left er -> printErrorMsg ("Error :\n" ++ show er) >> return Nothing
            Right prg -> updateHGState ((<~) gHalPrg (Just prg)) >>
                         printInfoMsg "Programa compilado con exito." >>
                         return (Just prg)

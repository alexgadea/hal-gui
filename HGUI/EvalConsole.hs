module HGUI.EvalConsole where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView

import Control.Lens hiding (set)
import Control.Monad.Trans.RWS 
import Control.Monad (forM,forM_,void)
import qualified Control.Monad.Trans.State as ST (runStateT,evalStateT)

import Control.Concurrent

import Data.Maybe
import Data.Text ( Text, unpack )
import Data.Reference

import Hal.Parser
import Hal.Lang (Identifier (..), Type (..))

import HGUI.ExtendedLang
import HGUI.Config
import HGUI.GState
import HGUI.Parser
import HGUI.Utils
import HGUI.Console
import HGUI.Evaluation.Eval
import HGUI.Evaluation.EvalState

import System.Glib.UTFString
import Control.Applicative

configEvalButton :: GuiMonad ()
configEvalButton = ask >>= \content -> do
            let ebutton = content ^. (gHalToolbar . evalButton)
            
            active <- io $ toggleToolButtonGetActive ebutton
            if active 
                then onActive
                else onDeactive
    where
        onDeactive :: GuiMonad ()
        onDeactive = ask >>= \content -> getHGState >>= \st -> do
            let ebox      = content ^. (gHalCommConsole . cEvalBox)
                evalstbox = content ^. (gHalCommConsole . cEvalStateBox)
                tv        = content ^. gTextCode
                mthreadId = st ^. gForkThread
            
            updateHGState (gHalConsoleState .~ Nothing)
            updateHGState (gHalPrg .~ Nothing)
            io $ maybe (return ()) killThread mthreadId
            
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
                    let mExecState = Just $ makeExecStateWithPre prg
                        stbox      = content ^. (gHalCommConsole . cStateBox)
                    
                    updateHGState (gHalConsoleState .~ mExecState)
                    
                    startExecState mExecState
                    startStateView stbox $ prgState $ makeExecStateWithPre prg
                    
                    io $ textViewSetEditable tv False
                    io $ widgetShowAll ebox
                    io $ widgetShowAll evalstbox

startExecState :: Maybe ExecState -> GuiMonad ()
startExecState Nothing   = return ()
startExecState (Just st) = let headC = headNExecComm st in
                           maybe (return ()) paintLine headC

startStateView :: VBox -> State -> GuiMonad ()
startStateView stBox st = io $ do
            containerForall stBox (containerRemove stBox)
            
            mapM_ fillStBox $ takeIdentifiers st
            
            return ()
    where
        fillStBox :: Identifier -> IO ()
        fillStBox i = do
                hb  <- hBoxNew False 2
                vl  <- labelNew $ Just $ show i ++ " ="
                vl' <- labelNew $ Just "Sin valor"
                boxPackStart hb vl PackNatural 2
                boxPackStart hb vl' PackNatural 2
                set vl [ miscXalign := 0 
                       , miscXpad := 10
                       ]
                boxPackStart stBox hb PackNatural 2

updateStateView :: State -> GuiMonad ()
updateStateView prgSt = 
                ask >>= \content -> do
                let stBox = content ^. (gHalCommConsole . cStateBox)
                childs <- io $ containerGetChildren stBox
                io $ postGUIAsync $ forM_ (zip childs (vars prgSt)) updValue
    where
        updValue :: (Widget,StateTuple) -> IO ()
        updValue (w,BoolVar i mv) = updateValue' w i mv
        updValue (w,IntVar  i mv) = updateValue' w i mv
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
            buf   <- textViewGetBuffer tv
            table <- textBufferGetTagTable buf  
            mtag  <- textTagTableLookup table "HighlightLine"
            maybe (return ()) (textTagTableRemove table) mtag

paintLine :: ExtComm -> GuiMonad ()
paintLine eComm = ask >>= io . paintLineIO eComm

paintLineIO :: ExtComm -> HGReader -> IO ()
paintLineIO eComm content = do
            let textV = content ^. gTextCode
                line  = takeCommLine eComm
            
            buf     <- textViewGetBuffer textV
            tbStart <- textBufferGetStartIter buf
            tbEnd   <- textBufferGetEndIter buf
            
            tag   <- textTagNew (Just $ stringToGlib "HighlightLine")
            set tag [ textTagParagraphBackground := evalLineColour
                    , textTagParagraphBackgroundSet := True
                    ]
            
            table <- textBufferGetTagTable buf
            textTagTableAdd table tag
            
            void $ textIterForwardLines tbStart (line-1)
            tbStart' <- textIterCopy tbStart
            void $ textIterForwardLines tbStart' 1
            
            void $ textBufferApplyTag buf tag tbStart tbStart'
            
            void $ stringToGlib <$> textBufferGetText buf tbStart tbEnd False
            return ()

configEvalConsole :: GuiMonad ()
configEvalConsole = ask >>= \content -> get >>= \st -> io $ do
                let ebox     = content ^. (gHalCommConsole . cEvalBox      )
                    stbox    = content ^. (gHalCommConsole . cEvalStateBox )
                    stepUB   = content ^. (gHalCommConsole . cStepUButton  )
                    stepDB   = content ^. (gHalCommConsole . cStepDButton  )
                    contB    = content ^. (gHalCommConsole . cContButton   )
                    execB    = content ^. (gHalCommConsole . cExecButton   )
                    breakB   = content ^. (gHalCommConsole . cBreakButton  )
                    restartB = content ^. (gHalCommConsole . cRestartButton)
                    cleanB   = content ^. (gHalCommConsole . cCleanButton  )
                    stopB    = content ^. (gHalCommConsole . cStopButton   )
                    forkFlag = content ^. gHalForkFlag
                
                void $ onClicked stepUB   (eval evalStepUp  content st)
                void $ onClicked stepDB   (forkEvalStepDown forkFlag content st)
                void $ onClicked contB    (forkEvalCont     forkFlag content st)
                void $ onClicked execB    (forkEvalExec     forkFlag content st)
                void $ onClicked breakB   (eval evalBreak   content st)
                void $ onClicked restartB (eval evalRestart content st)
                void $ onClicked cleanB   (eval evalClean   content st)
                void $ onClicked stopB    (eval evalStop    content st)
                
                widgetHideAll ebox
                widgetHideAll stbox

forkEvalExec :: MVar () -> HGReader -> HGStateRef -> IO ()
forkEvalExec fflag content stref = do
        flagUpFork <- tryPutMVar fflag ()
        if flagUpFork
            then forkIO (eval (evalExecute >> 
                               updateHGState ((.~) gForkThread Nothing)
                              ) content stref >> 
                        takeMVar fflag >> return ()) >>= \tid ->
                 readRef stref >>= \st ->
                 writeRef stref ((.~) gForkThread (Just tid) st)
            else return ()

evalExecute :: GuiMonad ()
evalExecute = ask >>= \content -> getHGState >>= \st -> do
    let Just execSt = st ^. gHalConsoleState
        prgSt       = prgState execSt
        mexecComm  = executedTracePrg execSt
        mnexecComm  = nexecutedTracePrg execSt
        win         = content ^. gHalWindow
             
    flagSt <- io $ newEmptyMVar
    
    maybe (takeInputs prgSt flagSt)
          (const $ io $ putMVar flagSt (Just prgSt)) mexecComm
    
    mPrgSt <- io $ takeMVar flagSt
    
    case (mPrgSt,mnexecComm) of
        (_,Nothing)-> return ()
        (Nothing,_)-> return ()
        (Just prgSt',Just nexecComm) -> do
            (m,(prgSt'',_)) <- io $ ST.runStateT 
                                       (evalExtComm nexecComm) (prgSt',win)
            case m of
                Nothing -> return ()
                Just _ -> do
                    let execSt' = updateExecState execSt 
                                        (Nothing,Nothing) prgSt' prgSt''
                    updateHGState ((.~) gHalConsoleState (Just execSt'))
                    updateStateView prgSt''

evalStop :: GuiMonad ()
evalStop = ask >>= \content -> io $ do
            let stopFlag = content ^. gHalStopFlag
                forkFlag = content ^. gHalForkFlag
            
            flagUpFlag <- io $ isEmptyMVar forkFlag
            if flagUpFlag 
               then return ()
               else takeMVar stopFlag >> return ()

evalStepUp :: GuiMonad ()
evalStepUp = ask >>= \content -> getHGState >>= \st ->
        let Just execSt = st ^. gHalConsoleState
            hPrgStlist = hPrgState execSt
        in
        case hPrgStlist of
            [] -> return ()
            (c,prgst):hprgst -> do
                let execSt' = undoUpdateExecState execSt c hprgst prgst
                    tv      = content ^. gTextCode
                cleanPaintLine $ castToTextView tv
                paintLine c
                updateStateView prgst
                updateHGState (gHalConsoleState .~ (Just execSt'))

forkEvalStepDown:: MVar () -> HGReader -> HGStateRef -> IO ()
forkEvalStepDown fflag content stref = do
        flagUpFork <- tryPutMVar fflag ()
        if flagUpFork
            then forkIO (eval (evalStepDown >> 
                               updateHGState ((.~) gForkThread Nothing)
                              ) content stref >> 
                        takeMVar fflag >> return ()) >>= \tid ->
                 readRef stref >>= \st ->
                 writeRef stref ((.~) gForkThread (Just tid) st)
            else return ()

evalStepDown :: GuiMonad Bool
evalStepDown = getHGState >>= \st -> do
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
        Just prgSt' -> do
            case mnexecComm of
                Nothing -> return True
                Just nexecComm -> 
                    ask >>= \content -> do
                    let win = content ^. gHalWindow
                    
                    (mmc,(prgSt'',_)) <- io $ ST.runStateT 
                                         (evalStepExtComm nexecComm) (prgSt',win)
                    
                    case mmc of
                        Nothing -> return False
                        Just mc -> do
                            let execSt' = updateExecState 
                                                    execSt mc prgSt' prgSt''
                                headC   = headNExecComm execSt'
                                tv      = content ^. gTextCode
                            updateHGState ((.~) gHalConsoleState (Just execSt'))
                            io $ postGUIAsync $ cleanPaintLineIO $ 
                                                castToTextView tv
                            updateStateView prgSt''
                            maybe (return ()) 
                                  (io . postGUIAsync . flip paintLineIO content)
                                  headC
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
                    vbox     <- vBoxNew False 2
                    
                    infolLabel <- labelNew $ Just "Ingresar valores"
                    
                    buttonBox <- hBoxNew False 2
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
                    
                    void $ on win keyPressEvent $ configWinAccions win iels
                    
                    configButtons win readyB cancelB iels flagSt
                    
                    containerAdd vbox infolLabel
                    containerAdd vbox idsBox
                    containerAdd vbox buttonBox
                    
                    widgetShowAll win
                    
                    return ()
    where
        configWinAccions :: Window -> [(Identifier,Entry, Label)] -> 
                            EventM EKey Bool
        configWinAccions win iels = do
                           ev <- eventKeyName
                           case glibToString ev of
                               "Escape" -> io $ putMVar flagSt Nothing >> 
                                                widgetDestroy win >> 
                                                return True
                               "Return" -> io $ checkEntrys win iels >> 
                                                return True
                               _        -> return False
        configButtons :: Window -> Button -> Button -> 
                         [(Identifier,Entry, Label)] -> MVar (Maybe State) -> 
                         IO ()
        configButtons win readyB cancelB iels flagst = do
            
            void $ onClicked cancelB $ putMVar flagst Nothing >>
                                       widgetDestroy win
            void $ onClicked readyB  $ checkEntrys win iels
            
            return ()
            
        checkEntrys :: Window -> [(Identifier,Entry, Label)] -> IO ()
        checkEntrys win iels = forM iels checkEntry >>= \check ->
                           if and $ map isJust $ check
                              then putMVar flagSt (Just $ addInputsValue prgSt $
                                                          catMaybes check) >>
                                   widgetDestroy win
                              else return ()
            where
                checkEntry :: (Identifier,Entry, Label) -> 
                              IO (Maybe (Identifier,EitherBI))
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
                            errLabel <- labelNew (Nothing :: Maybe Text)
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
                            Left _ -> setMsg "Valor no valido." >> 
                                       return Nothing
                            Right v -> do
                                    setMsg ""
                                    Just v' <- ST.evalStateT (evalBExp v)
                                                             (initState,mainWin)
                                    return $ Just $ (i,Left $ v')
                IntTy -> case parseConFromString strValue of
                            Left _ -> setMsg "Valor no valido." >> 
                                       return Nothing
                            Right v -> do
                                    setMsg ""
                                    Just v' <- ST.evalStateT (evalExp v) 
                                                             (initState,mainWin)
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

forkEvalCont :: MVar () -> HGReader -> HGStateRef -> IO ()
forkEvalCont fflag content stref = do
                flagUpFork <- tryPutMVar fflag ()
                if flagUpFork 
                    then forkIO evalC >>= \tid ->
                         readRef stref >>= \st ->
                         writeRef stref ((.~) gForkThread (Just tid) st)
                    else return ()
    where
        evalC :: IO ()
        evalC = eval (evalCont >> 
                      updateHGState ((.~) gForkThread Nothing)
                     ) content stref >> 
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
            makeStep <- evalStepDown
            
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
            let textV       = content ^. gTextCode
                Just execSt = st ^. gHalConsoleState
            
            buf  <- io $ textViewGetBuffer textV
            mark <- io $ textBufferGetInsert buf
            iter <- io $ textBufferGetIterAtMark buf mark
            i    <- io $ textIterGetLine iter
            
            marks  <- getMark buf i
            
            if null marks
               then case addBreak execSt (i+1) of
                        Nothing -> return ()
                        Just execSt' -> do
                            addMarkBreak buf iter
                            updateHGState (gHalConsoleState .~ (Just $ execSt'))
               else case delBreak execSt (i+1) of
                        Nothing -> return ()
                        Just execSt' -> do
                            delMarkBreak buf marks
                            io $ widgetQueueDraw textV
                            updateHGState (gHalConsoleState .~ (Just $ execSt'))
    where
        getMark :: TextBuffer -> Int -> GuiMonad [SourceMark]
        getMark buf i = io $ do
                let sbuf = castToSourceBuffer buf
                sourceBufferGetSourceMarksAtLine sbuf i breakMark
        delMarkBreak :: TextBuffer -> [SourceMark] -> GuiMonad ()
        delMarkBreak buf marks = io $ do
                let sbuf = castToSourceBuffer buf
                mapM_ (delMark sbuf) marks
        addMarkBreak :: TextBuffer -> TextIter -> GuiMonad ()
        addMarkBreak buf iter = io $ void $ 
                sourceBufferCreateSourceMark (castToSourceBuffer buf) 
                                             Nothing
                                             breakMark 
                                             iter

evalRestart :: GuiMonad ()
evalRestart = ask >>= \content -> getHGState >>= \st -> do
              let Just execSt = st ^. gHalConsoleState
                  Just prg    = st ^. gHalPrg
                  execSt'     = restartExecSt execSt prg
                  headC       = headNExecComm execSt'
                  tv          = content ^. gTextCode
              
              cleanPaintLine $ castToTextView tv
              updateHGState ((.~) gHalConsoleState (Just execSt'))
              updateStateView $ prgState execSt'
              maybe (return ()) paintLine headC
              io $ mapM_ (removeAllMarks tv) (prgBreaks execSt)

evalClean :: GuiMonad ()
evalClean = ask >>= \content -> getHGState >>= \st -> do
            let Just execSt = st ^. gHalConsoleState
                sv          = content ^. gTextCode
            updateHGState (gHalConsoleState .~ (Just execSt { prgBreaks = [] }))
            io $ mapM_ (removeAllMarks sv) (prgBreaks execSt)

removeAllMarks :: SourceView -> Int -> IO ()
removeAllMarks sv i = do
            buf    <- textViewGetBuffer sv
            let sbuf = (castToSourceBuffer buf)
            marks <- sourceBufferGetSourceMarksAtLine sbuf (i-1) breakMark
            mapM_ (delMark sbuf) marks
            io $ widgetQueueDraw sv

delMark :: SourceBuffer -> SourceMark -> IO ()
delMark sbuf mark = do
                    cat  <- sourceMarkGetCategory mark 
                    mmark <- textBufferGetMark sbuf (stringToGlib breakMark)
                    case (cat == breakMark,mmark) of
                        (_,Nothing) -> return ()
                        (False,_)   -> return ()
                        (True,Just mark') -> textBufferDeleteMark sbuf mark'

compile' :: GuiMonad (Maybe ExtProgram)
compile' = ask >>= \content ->
        do
        let tcode = content ^. gTextCode
        code <- getCode tcode
        case parseExtPrgFromString code of
            Left er -> printErrorMsg ("Error :\n" ++ show er) >> return Nothing
            Right prg -> updateHGState ((.~) gHalPrg (Just prg)) >>
                         printInfoMsg "Programa compilado con exito." >>
                         return (Just prg)

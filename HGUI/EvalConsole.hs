module HGUI.EvalConsole where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView

import Control.Monad (when)
import Control.Monad.Trans.RWS 
import Control.Monad.Trans.State (runStateT)

import Lens.Family

import HGUI.ExtendedLang
import HGUI.Config
import HGUI.GState
import HGUI.Parser
import HGUI.Utils
import HGUI.Console
import HGUI.Evaluation.Eval

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
                     let ebox = content ^. (gHalCommConsole . cEvalBox)
                         tv   = content ^. gTextCode
                     updateHGState ((<~) gHalConsoleState Nothing)
                     
                     cleanPaintLine $ castToTextView tv
                     
                     io $ textViewSetEditable tv True
                     io $ widgetHideAll ebox
        
        onActive :: GuiMonad ()
        onActive = ask >>= \content -> do
                   let ebox    = content ^. (gHalCommConsole . cEvalBox)
                       tv      = content ^. gTextCode
                       ebutton = content ^. (gHalToolbar . evalButton)
                   
                   mprg <- compile'
                   case mprg of
                       Nothing -> io (toggleToolButtonSetActive ebutton False)
                                  >> return ()
                       Just prg -> do
                                   let mExecState = Just $ makeExecState prg
                                   
                                   updateHGState ((<~) gHalConsoleState mExecState)
                                   
                                   startExecState mExecState
                                   
                                   io $ textViewSetEditable tv False
                                   io $ widgetShowAll ebox

startExecState :: Maybe ExecState -> GuiMonad ()
startExecState Nothing   = return ()
startExecState (Just st) = do
                           let headC = headNExecComm st
                           maybe (return ()) paintLine headC

cleanPaintLine :: TextView -> GuiMonad ()
cleanPaintLine tv = io $ do
                    buf      <- textViewGetBuffer tv
                    table    <- textBufferGetTagTable buf  
                    mtag <- textTagTableLookup table "HighlightLine"
                    maybe (return ()) (textTagTableRemove table) mtag

paintLine :: ExtComm -> GuiMonad ()
paintLine comm = ask >>= \content -> io $ do
            let textV = content ^. gTextCode
                line  = takeCommLine comm
            
            buf   <- textViewGetBuffer textV
            start <- textBufferGetStartIter buf
            end   <- textBufferGetEndIter buf
            
            tag   <- textTagNew (Just "HighlightLine")
            set tag [ textTagBackground := "orange"]
            
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
                let ebox     = content ^. (gHalCommConsole . cEvalBox)
                    stepB    = content ^. (gHalCommConsole . cStepButton)
                    contB    = content ^. (gHalCommConsole . cContButton)
                    breakB   = content ^. (gHalCommConsole . cBreakButton)
                    restartB = content ^. (gHalCommConsole . cRestartButton)
                    cleanB   = content ^. (gHalCommConsole . cCleanButton)
                
                onClicked stepB    (eval evalStep    content st)
                onClicked contB    (eval evalCont    content st)
                onClicked breakB   (eval evalBreak   content st)
                onClicked restartB (eval evalRestart content st)
                onClicked cleanB   (eval evalClean   content st)
                
                widgetHideAll ebox

evalStep :: GuiMonad ()
evalStep = getHGState >>= \st -> do
           let Just execSt = st ^. gHalConsoleState
               mnexecComm = nexecutedTracePrg execSt
           
           case mnexecComm of
               Nothing -> return ()
               Just nexecComm -> ask >>= \content -> do
                                 let prgSt = prgState execSt
                                 (mc,prgSt') <- io $ runStateT (evalStepExtComm nexecComm) prgSt
                                 let execSt' = updateExecState execSt mc prgSt'
                                     headC   = headNExecComm execSt'
                                     tv      = content ^. gTextCode
                                 updateHGState ((<~) gHalConsoleState (Just execSt'))
                                 cleanPaintLine $ castToTextView tv
                                 updateStateView prgSt'
                                 maybe (return ()) paintLine headC
           return ()

updateStateView :: State -> GuiMonad ()
updateStateView prgSt = ask >>= \content -> do
                        let evalL = content ^. (gHalCommConsole . cEvalLabel)
                        io $ labelSetText evalL (show prgSt)

evalCont :: GuiMonad ()
evalCont = do
           evalStep
           
           st <- getHGState
           let Just execSt = st ^. gHalConsoleState
               mnexecComm = nexecutedTracePrg execSt
               
           case mnexecComm of
               Nothing -> return ()
               Just nexecComm -> do 
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
                  execSt' = restartExecSt execSt
                  headC   = headNExecComm execSt'
                  tv      = content ^. gTextCode
              
              cleanPaintLine $ castToTextView tv
              updateStateView $ prgState execSt'
              updateHGState ((<~) gHalConsoleState (Just execSt'))
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
        code <- getCode
        case parseExtPrgFromString code of
            Left er -> printErrorMsg ("Error :\n" ++ show er) >> return Nothing
            Right prg -> updateHGState ((<~) gHalPrg (Just prg)) >>
                         printInfoMsg "Programa compilado con exito." >>
                         return (Just prg)
                         

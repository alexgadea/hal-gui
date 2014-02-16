module HGUI.Gui where

import Graphics.UI.Gtk hiding (get)

import Control.Concurrent

import Control.Lens hiding (set)
import Control.Monad.Trans.RWS
import Data.Reference

import HGUI.TextPage
import HGUI.File
import HGUI.Console
import HGUI.EvalConsole
import HGUI.GState
import HGUI.SymbolList
import HGUI.AxiomList
import HGUI.Config

mainHalGui :: Builder -> IO ()
mainHalGui xml = do
                (gReader,gState) <- makeGState xml

                _ <- runRWST (do configWindow
                                 configMenuBarButtons xml
                                 configToolBarButtons xml
                                 configInfoConsole
                                 configSymbolList
                                 configAxiomList
                                 eventsSourceView
                                 configEvalConsole
                             ) gReader gState

                return ()

-- | Genera el estado inicial de la mónada.
makeGState :: Builder -> IO (HGReader,HGStateRef) 
makeGState xml = do
        
        symFraB <- builderGetObject xml castToToggleToolButton "symHFrameButton"
        
        symFrame   <- builderGetObject xml castToFrame "symFrame"
        goLeftBox  <- builderGetObject xml castToHBox "symGoLeftBox"
        scrollW    <- builderGetObject xml castToScrolledWindow "swSymbolList"
        symIV      <- builderGetObject xml castToIconView "symbolList"
        goRightBox <- builderGetObject xml castToHBox "symGoRightBox"
        
        axFrame  <- builderGetObject xml castToFrame "axiomFrame"
        axTV     <- builderGetObject xml castToTreeView "axiomList"
        axRel    <- builderGetObject xml castToComboBox "comboAxioms"
        axFraB   <- builderGetObject xml castToToggleToolButton "AxiomFrameButton"
        axLabExpr <- builderGetObject xml castToLabel "axiomExpr"

        edPaned <- builderGetObject xml castToVPaned "edPaned"
        
        window <- builderGetObject xml castToWindow "mainWindow"

        infoTV <- builderGetObject xml castToTextView "infoConsoleTView"
        
        evalBox      <- builderGetObject xml castToVBox "evalBox"
        evalStateBox <- builderGetObject xml castToVBox "evalStateBox"
        stateBox     <- builderGetObject xml castToVBox "stateBox"
        evalB        <- builderGetObject xml castToToggleToolButton "evalButton"
        
        stepUB   <- builderGetObject xml castToButton "stepUpButton"
        stepDB   <- builderGetObject xml castToButton "stepDownButton"
        contB    <- builderGetObject xml castToButton "contButton"
        execB    <- builderGetObject xml castToButton "execButton"
        breakB   <- builderGetObject xml castToButton "breakButton"
        restartB <- builderGetObject xml castToButton "restartButton"
        cleanB   <- builderGetObject xml castToButton "cleanButton"
        stopB    <- builderGetObject xml castToButton "stopButton"
        
        boxLisa <- builderGetObject xml castToVBox "boxLisaCode"
        boxFun  <- builderGetObject xml castToVBox "boxFunCode"
        
        textcode <- createSourceView halLangInfo
        textverif <- createSourceView funLangInfo
        
        configText boxLisa textcode
        configText boxFun textverif
        
        forkFlag <- newEmptyMVar
        stopFlag <- newMVar ()
        
        let halToolbarST   = HalToolbar symFraB axFraB evalB
            halSymListST   = HalSymList symFrame goLeftBox scrollW symIV goRightBox
            halAxListST    = HalAxList axFrame axTV axRel axLabExpr
            halEditorPaned = HalEditorPaned edPaned
            halCommConsole = HalCommConsole evalBox evalStateBox stateBox
                                            stepUB stepDB contB execB breakB 
                                            restartB cleanB stopB
        
        gState <- newRef $ 
                      HGState Nothing
                              Nothing
                              textcode
                              Nothing
                              Nothing
                              
        let gReader = HGReader halToolbarST
                               halSymListST
                               halAxListST
                               halEditorPaned
                               window
                               (HalInfoConsole infoTV)
                               textcode
                               textverif
                               infoTV
                               halCommConsole
                               forkFlag
                               stopFlag
        
        return (gReader,gState)

-- | Configura los botones de la barra, tales como abrir, cerrar, etc...
configToolBarButtons :: Builder -> GuiMonad ()
configToolBarButtons xml = ask >>= \content -> get >>= \st ->
        io $ do
        
        newFButton      <- builderGetObject xml castToToolButton "newHFileButton"
        openFButton     <- builderGetObject xml castToToolButton "openHFileButton"
        saveFButton     <- builderGetObject xml castToToolButton "saveHFileButton"
        saveAtFButton   <- builderGetObject xml castToToolButton "saveHFileAtButton"
        proofOFButton   <- builderGetObject xml castToToolButton "proofOHFileButton"
        compileMButton  <- builderGetObject xml castToToolButton "compileHModuleButton"
        evaButton       <- builderGetObject xml castToToggleToolButton "evalButton"
        symFButton      <- builderGetObject xml castToToggleToolButton "symHFrameButton"
        axiomFButton    <- builderGetObject xml castToToggleToolButton "AxiomFrameButton"
        
        _ <- onToolButtonClicked newFButton      (eval createNewFile content st)
        _ <- onToolButtonClicked openFButton     (eval openFile content st)
        _ <- onToolButtonClicked saveFButton     (eval saveFile content st)
        _ <- onToolButtonClicked saveAtFButton   (eval saveAtFile content st)
        _ <- onToolButtonClicked proofOFButton   (eval genProofObligations content st)
        _ <- onToolButtonClicked compileMButton  (eval compile content st >> return ())
        _ <- onToolButtonClicked evaButton       (eval configEvalButton content st)
        _ <- onToolButtonClicked symFButton      (eval configSymFrameButton content st)
        _ <- onToolButtonClicked axiomFButton    (eval configAxFrameButton content st)
        
        return ()

configMenuBarButtons :: Builder -> GuiMonad ()
configMenuBarButtons xml = ask >>= \content -> get >>= \st ->
            io $ do
            let window = content ^. gHalWindow
            
            newB    <- builderGetObject xml castToMenuItem "newButton"
            openB   <- builderGetObject xml castToMenuItem "openButton"
            saveB   <- builderGetObject xml castToMenuItem "saveButton"
            saveAsB <- builderGetObject xml castToMenuItem "saveAsButton"
            quitB   <- builderGetObject xml castToMenuItem "quitButton"
            
            _ <- onActivateLeaf newB    $ eval createNewFile    content st
            _ <- onActivateLeaf openB   $ eval openFile         content st
            _ <- onActivateLeaf saveB   $ eval saveFile         content st
            _ <- onActivateLeaf saveAsB $ eval saveAtFile       content st
            _ <- onActivateLeaf quitB   $ widgetDestroy window
            
            return ()

-- | Configura la ventana principal.
configWindow :: GuiMonad ()
configWindow = ask >>= \content -> get >>= \stref ->
            io $ do
            let window   = content ^. gHalWindow
            
            windowMaximize window
            widgetShowAll window
            _ <- onDestroy window $ mainQuit >> forkQuit stref
            return ()
    where
        forkQuit :: HGStateRef -> IO ()
        forkQuit stref = readRef stref >>= \st ->
            let mthreadId = st ^. gForkThread in
            maybe (return ()) killThread mthreadId

eventsSourceView :: GuiMonad ()
eventsSourceView = ask >>= \content ->
                   get >>= \st ->
    do
        let textcode = content ^. gTextCode
        let textverif = content ^. gTextVerif
    
        _ <-  io (textcode `on` buttonPressEvent $ io $
                eval (updateHGState ((.~) gCurrentText textcode)) content st >>
                return False)
                
        _ <- io (textverif `on` buttonPressEvent $ io $
                eval (updateHGState ((.~) gCurrentText textverif)) content st >>
                return False)
        
        return ()
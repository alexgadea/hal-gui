module HGUI.Gui where

import Graphics.UI.Gtk hiding (get)

import Control.Monad.Trans.RWS
import Data.Reference

import Lens.Family
import Lens.Family.TH

import HGUI.TextPage
import HGUI.File
import HGUI.Console
import HGUI.EvalConsole
import HGUI.GState
import HGUI.SymbolList
import HGUI.AxiomList
import HGUI.Config

main :: IO ()
main = do 
    initGUI
    
    xml <- builderNew
    builderAddFromFile xml "hal.ui"
    
    mainHalGui xml

    mainGUI

mainHalGui :: Builder -> IO ()
mainHalGui xml = do
                (gReader,gState) <- makeGState xml

                runRWST (do configWindow
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
        
        symFrameB <- builderGetObject xml castToToggleToolButton "symHFrameButton"
        
        symFrame   <- builderGetObject xml castToFrame "symFrame"
        goLeftBox  <- builderGetObject xml castToHBox "symGoLeftBox"
        scrollW    <- builderGetObject xml castToScrolledWindow "swSymbolList"
        symIV      <- builderGetObject xml castToIconView "symbolList"
        goRightBox <- builderGetObject xml castToHBox "symGoRightBox"
        
        axFrame  <- builderGetObject xml castToFrame "axiomFrame"
        axTV     <- builderGetObject xml castToTreeView "axiomList"
        axRel    <- builderGetObject xml castToComboBox "comboAxioms"
        axFrameB <- builderGetObject xml castToToggleToolButton "AxiomFrameButton"
        axLabExpr <- builderGetObject xml castToLabel "axiomExpr"

        edPaned <- builderGetObject xml castToVPaned "edPaned"
        
        window <- builderGetObject xml castToWindow "mainWindow"

        infoTV <- builderGetObject xml castToTextView "infoConsoleTView"
        
        evalBox  <- builderGetObject xml castToVBox "evalBox"
        evalL    <- builderGetObject xml castToLabel "evalLabel"
        evalB    <- builderGetObject xml castToToggleToolButton "evalButton"
        
        stepB    <- builderGetObject xml castToButton "stepButton"
        contB    <- builderGetObject xml castToButton "contButton"
        breakB   <- builderGetObject xml castToButton "breakButton"
        restartB <- builderGetObject xml castToButton "restartButton"
        cleanB   <- builderGetObject xml castToButton "cleanButton"
        
        evalBox  <- builderGetObject xml castToVBox "evalBox"
        evalL    <- builderGetObject xml castToLabel "evalLabel"
        evalB    <- builderGetObject xml castToToggleToolButton "evalButton"
        
        boxLisa <- builderGetObject xml castToVBox "boxLisaCode"
        boxFun  <- builderGetObject xml castToVBox "boxFunCode"
        
        textcode <- createSourceView halLangInfo
        textverif <- createSourceView funLangInfo
        
        configText boxLisa textcode
        configText boxFun textverif
        
        let halToolbarST   = HalToolbar symFrameB axFrameB evalB
            halSymListST   = HalSymList symFrame goLeftBox scrollW symIV goRightBox
            halAxListST    = HalAxList axFrame axTV axRel axLabExpr
            halEditorPaned = HalEditorPaned edPaned
            halTextPage    = HalTextPage Nothing False
            halCommConsole = HalCommConsole evalBox evalL stepB contB breakB restartB cleanB
        
        gState <- newRef $ 
                      HGState halTextPage
                              Nothing
                              Nothing
                              textcode
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
        evalButton      <- builderGetObject xml castToToggleToolButton "evalButton"
        symFButton      <- builderGetObject xml castToToggleToolButton "symHFrameButton"
        axiomFButton    <- builderGetObject xml castToToggleToolButton "AxiomFrameButton"
        
        onToolButtonClicked newFButton      (eval createNewFile content st)
        onToolButtonClicked openFButton     (eval openFile content st)
        onToolButtonClicked saveFButton     (eval saveFile content st)
        onToolButtonClicked saveAtFButton   (eval saveAtFile content st)
        onToolButtonClicked proofOFButton   (eval genProofObligations content st)
        onToolButtonClicked compileMButton  (eval compile content st)
        onToolButtonClicked evalButton      (eval configEvalButton content st)
        onToolButtonClicked symFButton      (eval configSymFrameButton content st)
        onToolButtonClicked axiomFButton    (eval configAxFrameButton content st)
        
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
            
            onActivateLeaf newB    $ eval createNewFile    content st
            onActivateLeaf openB   $ eval openFile    content st
            onActivateLeaf saveB   $ eval saveFile         content st
            onActivateLeaf saveAsB $ eval saveAtFile         content st
            onActivateLeaf quitB   $ widgetDestroy window
            
            return ()

-- | Configura la ventana principal.
configWindow :: GuiMonad ()
configWindow = ask >>= \content -> 
            io $ do
            let window = content ^. gHalWindow
            windowMaximize window
            widgetShowAll window
            onDestroy window mainQuit
            return ()

            
eventsSourceView :: GuiMonad ()
eventsSourceView = ask >>= \content ->
                   get >>= \st ->
    do
        let textcode = content ^. gTextCode
        let textverif = content ^. gTextVerif
    
        io (textcode `on` buttonPressEvent $ io $
                eval (updateHGState ((<~) gCurrentText textcode)) content st >>
                return False)
                
        io (textverif `on` buttonPressEvent $ io $
                eval (updateHGState ((<~) gCurrentText textverif)) content st >>
                return False)
        
        return ()
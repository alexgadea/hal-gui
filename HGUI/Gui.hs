module HGUI.Gui where

import Graphics.UI.Gtk hiding (get)

import Control.Monad.Trans.RWS
import Data.Reference

import Lens.Family
import Lens.Family.TH

-- Import de la interfaz de fun como libreria.
-- import qualified GUI.Gui as GuiFun (mainFunGui)
-- import qualified GUI.GState as GStateFun (GReader,GStateRef)

import HGUI.TextPage
import HGUI.File
import HGUI.Console
import HGUI.GState
import HGUI.SymbolList
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
                            configTextCode
                            configTextVerif
                            configMenuBarButtons  xml
                            configSymbolList
                            configNotebook
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

        edPaned <- builderGetObject xml castToVPaned "edPaned"
        
        window <- builderGetObject xml castToWindow "mainWindow"
        
        notebook <- builderGetObject xml castToNotebook "notebook1"

        infoTV <- builderGetObject xml castToTextView "infoConsoleTView"
        
        infoTBuf <- textViewGetBuffer infoTV
        
        configConsoleTV infoTV infoTBuf
        
        textcode <- createSourceView halLangInfo
        
        textverif <- createSourceView funLangInfo
        
        let halToolbarST   = HalToolbar symFrameB
            halSymListST   = HalSymList symFrame goLeftBox scrollW symIV goRightBox
            halEditorPaned = HalEditorPaned edPaned
            halTextPage    = HalTextPage Nothing False
        
        gState <- newRef $ 
                      HGState halTextPage
                              Nothing
                              Nothing
                              
        let gReader = HGReader halToolbarST
                               halSymListST
                               halEditorPaned
                               window
                               notebook
                               textcode
                               textverif
                               textcode
        
        return (gReader,gState)

-- | Configura los botones de la barra, tales como abrir, cerrar, etc...
configMenuBarButtons :: Builder -> GuiMonad ()
configMenuBarButtons xml = ask >>= \content -> get >>= \st ->
        io $ do
        
        newFButton      <- builderGetObject xml castToToolButton "newHFileButton"
        openFButton     <- builderGetObject xml castToToolButton "openHFileButton"
        saveFButton     <- builderGetObject xml castToToolButton "saveHFileButton"
        saveAtFButton   <- builderGetObject xml castToToolButton "saveHFileAtButton"
        proofOFButton   <- builderGetObject xml castToToolButton "proofOHFileButton"
        compileMButton  <- builderGetObject xml castToToolButton "compileHModuleButton"
        symFButton      <- builderGetObject xml castToToggleToolButton "symHFrameButton"
        axiomFButton      <- builderGetObject xml castToToggleToolButton "AxiomFrameButton"
        
        onToolButtonClicked newFButton      (eval createNewFile content st)
        onToolButtonClicked openFButton     (eval openFile content st)
        onToolButtonClicked saveFButton     (eval saveFile content st)
        onToolButtonClicked saveAtFButton   (eval saveAtFile content st)
        onToolButtonClicked proofOFButton   (eval genProofObligations content st)
        onToolButtonClicked compileMButton  (eval compile content st)
        onToolButtonClicked symFButton      (eval configSymFrameButton content st)
        
        return ()
-- 
-- -- | Configura los botones del menude archivo.
-- configToolBarButtons :: Builder -> GuiMonad ()
-- configToolBarButtons xml = ask >>= \content -> get >>= \st ->
--             io $ do
--             let window = content ^. gFunWindow
--             newB  <- builderGetObject xml castToMenuItem "newButton"
--             openB <- builderGetObject xml castToMenuItem "openButton"
--             saveB  <- builderGetObject xml castToMenuItem "saveButton"
--             saveAsB <- builderGetObject xml castToMenuItem "saveAsButton"
--             closeB  <- builderGetObject xml castToMenuItem "closeButton"
--             quitB  <- builderGetObject xml castToMenuItem "quitButton"
--             
--             checkB <- builderGetObject xml castToMenuItem "checkButton"
--             
--             onActivateLeaf newB   $ eval createNewFile    content st
--             onActivateLeaf openB   $ eval openFile    content st
--             onActivateLeaf saveB  $ eval saveFile         content st
--             onActivateLeaf saveAsB  $ eval saveAtFile         content st
--             onActivateLeaf closeB $ eval closeCurrentFile content st
--             onActivateLeaf quitB  $ widgetDestroy window
--             
--             onActivateLeaf checkB $ eval checkSelectFile content st
--             return ()
            
eval :: GuiMonad () -> HGReader -> HGStateRef -> IO ()
eval action content str = evalRWST action content str >> return ()


-- | Configura la ventana principal.
configWindow :: GuiMonad ()
configWindow = ask >>= \content -> 
            io $ do
            let window = content ^. gHalWindow
            windowMaximize window
            widgetShowAll window
            onDestroy window mainQuit
            return ()

configNotebook :: GuiMonad ()
configNotebook = return ()
    
    
    
    
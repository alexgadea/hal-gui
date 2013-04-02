module HGUI.Gui where

import Graphics.UI.Gtk hiding (get)

import Control.Monad.Trans.RWS
import Data.Reference

import Lens.Family
import Lens.Family.TH

-- Import de la interfaz de fun como libreria.
import qualified GUI.Gui as GuiFun (mainFunGui)
import qualified GUI.GState as GStateFun (GReader,GStateRef)

import HGUI.TextPage
import HGUI.File
import HGUI.EvalConsole
import HGUI.Console
import HGUI.GState
import HGUI.SymbolList

main :: IO ()
main = do 
    initGUI
    
    xml <- builderNew
    builderAddFromFile xml "hal.ui"
    
    (gReaderFun,gStateRefFun) <- GuiFun.mainFunGui xml
    mainHalGui gReaderFun gStateRefFun xml

    mainGUI

mainHalGui :: GStateFun.GReader -> GStateFun.GStateRef -> Builder -> IO ()
mainHalGui gReaderFun gStateRefFun xml = do
                (gReader,gState) <- makeGState gReaderFun gStateRefFun xml

                runRWST (do configTextPage
                            configMenuBarButtons  xml
                            configSymbolList
                            configCommandConsole
                         ) gReader gState

                return ()

-- | Genera el estado inicial de la mónada.
makeGState :: GStateFun.GReader -> GStateFun.GStateRef -> 
              Builder -> IO (HGReader,HGStateRef) 
makeGState gReaderFun gStateRefFun xml = do
        
        symFrameB <- builderGetObject xml castToToggleToolButton "symHFrameButton"
        
        loadedMod  <- builderGetObject xml castToLabel "halLabelLoadedModule"
        
        symFrame   <- builderGetObject xml castToFrame "halSymFrame"
        goLeftBox  <- builderGetObject xml castToHBox "halSymGoLeftBox"
        scrollW    <- builderGetObject xml castToScrolledWindow "halSwSymbolList"
        symIV      <- builderGetObject xml castToIconView "halSymbolList"
        goRightBox <- builderGetObject xml castToHBox "halSymGoRightBox"
        
        window <- builderGetObject xml castToWindow "mainWindow"
        
        edPaned <- builderGetObject xml castToVPaned "halEditorPaned"
        commTV <- builderGetObject xml castToTextView "halCommandTView"
        commEntry <- builderGetObject xml castToEntry "halCommandEntry"
        halStatusbar <- builderGetObject xml castToStatusbar "halStatusBar"
        infoTV <- builderGetObject xml castToTextView "halInfoConsoleTView"
        
        panedSetPosition edPaned 400
        
        commTBuf <- textViewGetBuffer commTV
        
        infoTBuf <- textViewGetBuffer infoTV
        
        configConsoleTV infoTV infoTBuf
        
        let halToolbarST   = HalToolbar symFrameB
            halSymListST   = HalSymList symFrame goLeftBox scrollW symIV goRightBox
            halCommConsole = HalCommConsole commEntry commTBuf commTV
            halEditorPaned = HalEditorPaned edPaned
            halTextPage    = HalTextPage Nothing False
        
        gState <- newRef $ 
                      HGState gStateRefFun 
                              halTextPage
                              Nothing
                              
        let gReader = HGReader gReaderFun
                               halToolbarST
                               halSymListST
                               halCommConsole
                               halEditorPaned
        
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

-- | Mensaje de error en caso de no encontrar el archivo glade correspondiente.
msgErrGladeNotFound :: String
msgErrGladeNotFound = "Archivo fun.glade no encontrado"

-- | Modulo respectivo a la parte derecha de la interfaz, es decir, el 
-- campo de texto.
module HGUI.TextPage where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.SourceView

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import Control.Arrow

import Lens.Family

import Data.Text (Text,pack,unpack)
import Data.Maybe (fromJust,fromMaybe)
import Data.List (delete)

import HGUI.GState
import HGUI.Config
import HGUI.Utils

-- Configura el lenguaje para el sourceView.
configLanguage :: SourceBuffer -> GuiMonad ()
configLanguage buf = io $ do
    -- Language Spec
    slm <- sourceLanguageManagerNew
    path <- sourceLanguageManagerGetSearchPath slm
    sourceLanguageManagerSetSearchPath slm (Just $ languageSpecFolder:path)
    
    mlang <- sourceLanguageManagerGuessLanguage 
                --slm (Just languageSpecFunFile) (Just funMimeType)
                slm (Just languageSpecFunFile) (Just funMimeType)
    case mlang of
        Nothing -> putStrLn "WARNING: No se puede cargar el highlighting para el lenguaje"
        Just lang -> do
            langId <- sourceLanguageGetId lang
            putStrLn ("Lenguaje = "++show langId)
            sourceBufferSetLanguage buf (Just lang)

            sourceBufferSetHighlightSyntax buf True
            sourceBufferSetHighlightMatchingBrackets buf True        
            -- Style Scheme
            stm <- sourceStyleSchemeManagerNew
            sourceStyleSchemeManagerSetSearchPath stm (Just [textStylesFolder])
            styleSch <- sourceStyleSchemeManagerGetScheme stm "fun"        
            sourceBufferSetStyleScheme buf (Just styleSch)

-- | Configuración del sourceView.
configSourceView :: SourceView -> GuiMonad ()
configSourceView sv = io $ do
        sourceViewSetIndentWidth sv funIdentWidth
        sourceViewSetAutoIndent sv autoIdent
        sourceViewSetIndentOnTab sv setIndentOnTab
        sourceViewSetInsertSpacesInsteadOfTabs sv spacesInsteadTab
        sourceViewSetShowLineNumbers sv True
        

-- | Configuración de la ventana de scroll, que contiene el campo de texto.
configScrolledWindow :: ScrolledWindow -> GuiMonad ()
configScrolledWindow sw = io $
            set sw [ scrolledWindowHscrollbarPolicy := PolicyAutomatic 
                   , scrolledWindowVscrollbarPolicy := PolicyAlways
                   ]

-- | Configuración del aspecto del notebook que contiene los archivos abiertos.
configNotebook :: Notebook -> GuiMonad ()
configNotebook nb = io $
            set nb [ notebookTabBorder  := 0
                   , notebookTabHborder := 0
                   , notebookTabVborder := 0
                   ]

-- | Crea un campo de texto y lo llena, de ser posible, con el string.
createTextEntry :: Maybe String -> GuiMonad SourceView
createTextEntry mcode = do
            hbox <- io $ hBoxNew False 0
            buf <- io $ sourceBufferNew Nothing
            configLanguage buf
            
            maybe (return ()) (io . loadCode buf) mcode
            
            sourceview <- io $ sourceViewNewWithBuffer buf

            configSourceView sourceview
            
            return sourceview
    where
        loadCode :: TextBufferClass tbuffer => tbuffer -> String -> IO ()
        loadCode buf code = do
                start <- textBufferGetStartIter buf
                textBufferInsert buf start code

-- | Crea un campo de texto con su respectivo scrollWindow.
createTextEdit :: Maybe String -> GuiMonad ScrolledWindow
createTextEdit mcode = do
            swindow <- io $ scrolledWindowNew Nothing Nothing
            configScrolledWindow swindow
                        
            texte <- createTextEntry mcode
            
            io $  containerAdd swindow texte
            
            io $ widgetShowAll texte
            
            io $ widgetShowAll swindow
            
            return swindow

-- | Crea un editBook, el cual tiene un primer campo de texto con nombre 
-- y contenido de ser posible.
createTextPage :: Maybe String -> GuiMonad ()
createTextPage mcode = ask >>= \content -> do
            
            texte <- createTextEdit mcode
            
            let editorPaned = content ^. (gHalEditorPaned . epaned)
            
            Just da <- io $ panedGetChild1 editorPaned
            
            io $ containerRemove editorPaned da
            
            io $ panedAdd1 editorPaned texte
            io $ widgetShowAll editorPaned
            
            return ()

configTextPage :: GuiMonad ()
configTextPage = createTextPage Nothing

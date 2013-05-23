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
configLanguage :: SourceBuffer -> LangInfo -> IO ()
configLanguage buf langinfo = do
    -- Language Spec
    slm <- sourceLanguageManagerNew
    path <- sourceLanguageManagerGetSearchPath slm
    sourceLanguageManagerSetSearchPath slm (Just $ languageSpecFolder:path)
    
    mlang <- sourceLanguageManagerGuessLanguage 
                --slm (Just languageSpecFunFile) (Just funMimeType)
                slm (Just (specFile langinfo)) (Just (mimeType langinfo))
    case mlang of
        Nothing -> putStrLn $ "WARNING: No se puede cargar el highlighting para el lenguaje" ++ (specFile langinfo)
        Just lang -> do
            langId <- sourceLanguageGetId lang
            putStrLn ("Lenguaje = "++show langId)
            sourceBufferSetLanguage buf (Just lang)

            sourceBufferSetHighlightSyntax buf True
            sourceBufferSetHighlightMatchingBrackets buf True        
            -- Style Scheme
            stm <- sourceStyleSchemeManagerNew
            sourceStyleSchemeManagerSetSearchPath stm (Just [textStylesFolder])
            styleSch <- sourceStyleSchemeManagerGetScheme stm (langName langinfo)        
            sourceBufferSetStyleScheme buf (Just styleSch)

-- | Configuración del sourceView.
configSourceView :: SourceView -> IO ()
configSourceView sv = do
        sourceViewSetIndentWidth sv funIdentWidth
        sourceViewSetAutoIndent sv autoIdent
        sourceViewSetIndentOnTab sv setIndentOnTab
        sourceViewSetInsertSpacesInsteadOfTabs sv spacesInsteadTab
        sourceViewSetShowLineNumbers sv True
        sourceViewSetShowLineMarks sv True
        sourceViewSetHighlightCurrentLine sv True
        sourceViewSetMarkCategoryIconFromStock sv breakMark (Just stockMediaRecord)

-- | Configuración de la ventana de scroll, que contiene el campo de texto.
configScrolledWindow :: ScrolledWindow -> IO ()
configScrolledWindow sw = 
            set sw [ scrolledWindowHscrollbarPolicy := PolicyAutomatic 
                   , scrolledWindowVscrollbarPolicy := PolicyAlways
                   ]

-- | Crea un campo de texto y lo llena, de ser posible, con el string.
createSourceView :: LangInfo -> IO SourceView
createSourceView langi = do
            hbox <- hBoxNew False 0
            buf <- sourceBufferNew Nothing
            configLanguage buf langi
            
            sourceview <- sourceViewNewWithBuffer buf

            configSourceView sourceview
            
            return sourceview

configText :: VBox -> SourceView -> IO ()
configText box sv = 
        do
            swindow <- scrolledWindowNew Nothing Nothing
            configScrolledWindow swindow
            
            containerAdd swindow sv
            
            containerAdd box swindow
            
            widgetShowAll box

createTextPage :: Maybe String -> GuiMonad ()
createTextPage mcode = ask >>= \content -> do
            
            let textcode = content ^. gTextCode
            
            -- borramos el código viejo:
            tbuf <- io $ textViewGetBuffer textcode
            
            start <- io $ textBufferGetStartIter tbuf
            end   <- io $ textBufferGetEndIter tbuf
            io $ textBufferDelete tbuf start end
            
            maybe (return ())
                  (io . textBufferInsert tbuf start)
                  mcode
                  
createTextFunPage :: Maybe String -> GuiMonad ()
createTextFunPage mcode = ask >>= \content -> do
            
            let textverif = content ^. gTextVerif
            
            -- borramos el código viejo:
            tbuf <- io $ textViewGetBuffer textverif
            
            start <- io $ textBufferGetStartIter tbuf
            end   <- io $ textBufferGetEndIter tbuf
            io $ textBufferDelete tbuf start end
            
            maybe (return ())
                  (io . textBufferInsert tbuf start)
                  mcode

configTextPage :: GuiMonad ()
configTextPage = createTextPage Nothing

{-# LANGUAGE NoMonomorphismRestriction #-}
-- Módulo para definir funciones útiles, generales a la interfaz
module HGUI.Utils where

import Graphics.UI.Gtk
import System.Glib.GType
import System.Glib.GObject

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Trans.State
import Control.Monad.Trans.RWS 

import Control.Applicative

import Lens.Family

import HGUI.GState

textBufferInsertLn buf str = textBufferGetEndIter buf >>= \titer ->
                             textBufferInsert buf titer ('\n':str)


    
-- | Inserta un string al final de un text buffer y scrollea el text view.
--   Retorna el iter inicial y final del texto ingresado
putStrAtEnd :: TextBuffer -> TextView -> String -> IO ()
putStrAtEnd buf tv msg = do
        textBufferInsertLn buf msg
        -- textViewScrollToIter no anda bien, por eso uso scrollToMark
        textBufferInsertLn buf ""
        titer2 <- textBufferGetEndIter buf
        
        mark <- textBufferCreateMark buf Nothing titer2 False
        textViewScrollToMark tv mark 0 Nothing

-- | Pone un mensaje en una área de estado.
putMsgSB :: Statusbar -> ContextId -> String -> IO ()
putMsgSB st cid m = statusbarPush st cid m >> return ()
                 
-- | 
setLoadedModuleInfo :: Label -> Maybe String -> IO ()
setLoadedModuleInfo label Nothing = labelSetText label "Error al cargar el módulo" >>
                                    styleInfoError >>= widgetModifyFont label
setLoadedModuleInfo label (Just mod) = styleInfoModule >>= widgetModifyFont label >>
                                       labelSetText label mod


-- -- | Estilo para títulos en info-boxes
styleInfoModule ::  IO (Maybe FontDescription)
styleInfoModule = Just <$> fontBold

styleInfoError :: IO (Maybe FontDescription)
styleInfoError = Just <$> fontItalic

fontItalic :: IO FontDescription
fontItalic = fontDescriptionNew >>= \fd -> 
             fontDescriptionSetStyle fd StyleItalic >>
             return fd

fontBold :: IO FontDescription
fontBold = fontDescriptionNew >>= \fd -> 
           fontDescriptionSetWeight fd WeightBold >>
           return fd


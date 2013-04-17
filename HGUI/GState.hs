{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}

module HGUI.GState where

import qualified GUI.GState as GStateFun (GReader,GStateRef,gFunWindow)

import Lens.Family
import Lens.Family.TH

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State hiding (State,get,put)
import Control.Monad.Trans.RWS
import Data.IORef
import Data.Reference
import Data.Text (Text)
import qualified Data.Strict.Either as SEither

import qualified Hal.Evaluation.Eval as HEval
import Hal.Interpreter.Interpreter
import Hal.Lang(Program)

type TextFilePath = Text

-- | Información sobre los items del toolBar.
data HalToolbar = HalToolbar { _symFrameB :: ToggleToolButton }
$(mkLenses ''HalToolbar)

-- | Información sobre la lista de símbolos.
data HalSymList = HalSymList { _gSymFrame    :: Frame
                             , _gGoLeftBox   :: HBox
                             , _gScrollW     :: ScrolledWindow
                             , _gSymIconView :: IconView
                             , _gGoRightBox  :: HBox
                             }
$(mkLenses ''HalSymList)

data HalEditorPaned = HalEditorPaned { _epaned :: VPaned }
$(mkLenses ''HalEditorPaned)

data HGReader = HGReader { _gHalToolbar         :: HalToolbar
                         , _gHalSymbolList      :: HalSymList
                         , _gHalEditorPaned     :: HalEditorPaned
                         , _gHalWindow          :: Window
                         , _gHalNotebook        :: Notebook
                         , _gTextCode           :: SourceView
                         }
$(mkLenses ''HGReader)

-- | Información sobre el panel derecho de la interfaz.
data HalTextPage = HalTextPage { _fileName :: Maybe TextFilePath
                               , _isSave   :: Bool
                               }
$(mkLenses ''HalTextPage)

-- | Tipo de mónada de estado, llevamos el environment de un modulo bien 
-- chequeado y la info sobre la parte derecha de la interfaz, es decir, 
-- la que contiene los campos de texto para escribir programas.
data HGState = HGState { _gHalTextPage       :: HalTextPage
                       , _gHalConsoleState   :: Maybe IState
                       , _gHalPrg :: Maybe Program
                       }
$(mkLenses ''HGState)

-- | Referencia del estado.
type HGStateRef = IORef HGState

-- | Mónada de la interfaz.
type GuiMonad' = RWST HGReader () HGStateRef 
type GuiMonad = GuiMonad' IO


instance Reference IORef (StateT HEval.State IO) where
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

instance Reference IORef (StateT HGStateRef IO) where
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

instance Reference IORef GuiMonad where
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

-- | Retorna el estado de la mónada de la interfaz.
getHGState :: GuiMonad HGState
getHGState = get >>= readRef

-- | Actualiza el estado de la mónada de la interfaz.
updateHGState :: (HGState -> HGState) -> GuiMonad ()
updateHGState f = do
                r <- get
                gst <- readRef r
                writeRef r $ f gst
                put r

io = liftIO

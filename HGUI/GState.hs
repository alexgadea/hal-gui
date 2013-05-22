{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}

module HGUI.GState where

import qualified GUI.GState as GStateFun (GReader,GStateRef,gFunWindow)

import Lens.Family
import Lens.Family.TH

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.MenuComboToolbar.ToggleToolButton
import Graphics.UI.Gtk.SourceView

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State hiding (State,get,put)
import Control.Monad.Trans.RWS
import Data.IORef
import Data.Reference
import Data.Text (Text)
import qualified Data.Strict.Either as SEither

import qualified Hal.Evaluation.EvalLang as HEval

import HGUI.Evaluation.EvalState
import HGUI.ExtendedLang (ExtProgram)

type TextFilePath = Text

-- | Información sobre los items del toolBar.
data HalToolbar = HalToolbar { _symFrameB  :: ToggleToolButton
                             , _axFrameB   :: ToggleToolButton
                             , _evalButton :: ToggleToolButton
                             }
$(mkLenses ''HalToolbar)

-- | Información sobre la lista de símbolos.
data HalSymList = HalSymList { _gSymFrame    :: Frame
                             , _gGoLeftBox   :: HBox
                             , _gScrollW     :: ScrolledWindow
                             , _gSymIconView :: IconView
                             , _gGoRightBox  :: HBox
                             }
$(mkLenses ''HalSymList)

-- | Información sobre la lista de axiomas.
data HalAxList = HalAxList { _gAxFrame     :: Frame 
                           , _gAxTreeView  :: TreeView
                           , _gAxRel       :: ComboBox
                           , _gAxLabelExpr :: Label
                           }
$(mkLenses ''HalAxList)

data HalInfoConsole = HalInfoConsole { _infoConTView :: TextView }
$(mkLenses ''HalInfoConsole)

data HalCommConsole = HalCommConsole { _cEvalBox       :: VBox
                                     , _cEvalStateBox  :: VBox
                                     , _cStateBox      :: VBox
                                     , _cStepButton    :: Button
                                     , _cContButton    :: Button
                                     , _cBreakButton   :: Button
                                     , _cRestartButton :: Button
                                     , _cCleanButton   :: Button
                                     , _cStopButton    :: Button
                                     }
$(mkLenses ''HalCommConsole)

data HalEditorPaned = HalEditorPaned { _epaned :: VPaned }
$(mkLenses ''HalEditorPaned)

-- Con ForkFlag restringimos la creación de un thread para evaluar. La idea
-- es que si damos dos steps, el primero crea un thread pero el segundo no hace
-- nada, recién cuando el primero termina al dar de nuevo step, se crea
-- otro thread.
-- Con StopFlag restringimos el avance de la evaluación, la idea principal
-- es parar cualquier ejecución que no tenga final.
data HGReader = HGReader { _gHalToolbar         :: HalToolbar
                         , _gHalSymbolList      :: HalSymList
                         , _gHalAxList          :: HalAxList
                         , _gHalEditorPaned     :: HalEditorPaned
                         , _gHalWindow          :: Window
                         , _gHalInfoConsole     :: HalInfoConsole
                         , _gTextCode           :: SourceView
                         , _gTextVerif          :: SourceView
                         , _gInfoConsole        :: TextView
                         , _gHalCommConsole     :: HalCommConsole
                         , _gHalForkFlag        :: MVar ()
                         , _gHalStopFlag        :: MVar ()
                         }
$(mkLenses ''HGReader)

-- | Tipo de mónada de estado, llevamos el environment de un modulo bien 
-- chequeado y la info sobre la parte derecha de la interfaz, es decir, 
-- la que contiene los campos de texto para escribir programas.
data HGState = HGState { -- _gHalTextPage       :: HalTextPage,
                         _gHalConsoleState   :: Maybe ExecState
                       , _gHalPrg            :: Maybe ExtProgram
                       , _gCurrentText       :: SourceView
                       -- El siguiente campo es el nombre del archivo sin la extensión.
                       -- Un archivo de Hal consistira de uno .lisa y uno .fun
                       , _gFileName      :: Maybe FilePath
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

io :: MonadIO m => IO a -> m a
io = liftIO

eval :: GuiMonad a -> HGReader -> HGStateRef -> IO a
eval action content str = evalRWST action content str >>= return . fst

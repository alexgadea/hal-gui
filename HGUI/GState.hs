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

import qualified Hal.Evaluation.EvalLang as HEval

import HGUI.Evaluation.Eval
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
                                     , _cEvalLabel     :: Label
                                     , _cStepButton    :: Button
                                     , _cContButton    :: Button
                                     , _cBreakButton   :: Button
                                     , _cRestartButton :: Button
                                     , _cCleanButton   :: Button
                                     }
$(mkLenses ''HalCommConsole)

data HalEditorPaned = HalEditorPaned { _epaned :: VPaned }
$(mkLenses ''HalEditorPaned)

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
                       , _gHalConsoleState   :: Maybe ExecState
                       , _gHalPrg            :: Maybe ExtProgram
                       , _gCurrentText       :: SourceView
                       , _gFileName          :: Maybe FilePath
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

eval :: GuiMonad () -> HGReader -> HGStateRef -> IO ()
eval action content str = evalRWST action content str >> return ()

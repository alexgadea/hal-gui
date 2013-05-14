module HGUI.File where

import Graphics.UI.Gtk hiding (get)

import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as C

import qualified Data.Foldable as F
import System.FilePath.Posix
import Data.Maybe (fromMaybe,isJust,fromJust)
import Data.Text hiding (take,init,drop)

import HGUI.Console
import HGUI.GState
import HGUI.TextPage
import HGUI.Utils
import HGUI.ExtendedLang
import HGUI.Parser(parseExtPrgFromString)

import Hal.Verification.WeakPre(generateFunFileString)

import Lens.Family

-- | En general, salvo aclaración, un archivo en este contexto es directamente
-- un campo de texto con su respectivo nombre en la interfaz.

-- | Crea un campo de texto al realizar una carga por archivo.
createNewFileFromLoad :: Maybe TextFilePath -> Maybe (String,String) -> 
                         GuiMonad ()
createNewFileFromLoad mfp mcode = getHGState >>= \st -> ask >>= \content ->
        do
--         unless (st ^. (gHalTextPage . isSave)) saveFile
--         updateHGState ((<~) gHalTextPage (HalTextPage mfp True))
        maybe (return ())
              (\name -> updateHGState ((<~) gFileName (Just $ unpack name)) >>
                        updateHGState ((<~) gHalPrg Nothing))
              mfp
              
        createTextPage (mcode >>= return . fst)
        createTextFunPage (mcode >>= return . snd)

-- | Crea un nuevo archivo en blanco.
createNewFile :: GuiMonad ()
createNewFile = createNewFileFromLoad Nothing Nothing

genProofObligations :: GuiMonad ()
genProofObligations = ask >>= \content -> getHGState >>= \st ->
    do
        let mprg = st ^. gHalPrg
        let mfile = st ^. gFileName
        
        maybe (compile >>= flip when genProofObligations)
              (\prg ->
                maybe (printErrorMsg "El archivo no está guardado")
                      (\fname -> io (generateFunFileString (takeFileName fname) $ convertExtProgToProg prg) >>=
                       \strfun -> createTextFunPage $ Just strfun)
                      mfile)
              mprg

-- | Función para cargar un archivo.
openFile :: GuiMonad ()
openFile = ask >>= \ct -> get >>= \st ->
           let infoTV = ct ^. gInfoConsole in
           io $ dialogLoad "Cargar programa" halFileFilter (openFile' ct st) infoTV >>
            return ()
    where
        openFile' :: HGReader -> HGStateRef -> Maybe TextFilePath ->
                     Maybe (String,String) -> IO ()
        openFile' content st mfp mcode = 
            evalRWST (createNewFileFromLoad mfp mcode) content st >> 
            return ()

-- | Dialogo general para la carga de archivos.
dialogLoad :: String -> (FileChooserDialog -> IO ()) -> 
              (Maybe TextFilePath -> Maybe (String,String) -> IO ()) -> 
              TextView ->
              IO Bool
dialogLoad label fileFilter action consoleView = do
    dialog <- fileChooserDialogNew (Just label) 
                                    Nothing 
                                    FileChooserActionOpen
                                    [ ("Cargar",ResponseAccept)
                                    , ("Cancelar",ResponseCancel)]

    fileFilter dialog 
    response <- dialogRun dialog
    
    case response of
        ResponseAccept -> do
            selected <- fileChooserGetFilename dialog
            F.mapM_ (\filepath -> 
                    let filename = dropExtension filepath in
                        readFile (filename++".lisa") >>= \code ->
                        catch (readFile (filename++".fun") >>= return . Just)
                        (\e -> printErrorMsgIO ("Error cargando archivo. No existe el archivo de verificación "++
                                   filename++".fun") consoleView >> return Nothing)
                        >>= \mcodefun ->
                        maybe (return ()) 
                              (\codefun -> action (Just $ pack filename) (Just (code,codefun)) >>
                                printInfoMsgIO "Archivo cargado con éxito." consoleView)
                              mcodefun >>
                        widgetDestroy dialog) 
                    selected 
            return True
        _ -> widgetDestroy dialog >> return False

-- | Generador de filtros para la carga y guardado de archivos.
setFileFilter :: FileChooserClass f => f -> [String] -> String -> IO ()
setFileFilter fChooser patterns title = do
                                hsfilt <- fileFilterNew
                                mapM_ (fileFilterAddPattern hsfilt) patterns
                                fileFilterSetName hsfilt title
                                fileChooserAddFilter fChooser hsfilt    

-- | Guardado directo de un archivo.
saveFile :: GuiMonad ()
saveFile = getHGState >>= \st -> ask >>= \content ->
        let (tc,tv) = (content ^. gTextCode,content ^. gTextVerif) in
            case st ^. gFileName of
                Nothing -> return ()
                Just fn -> do
                        codelisa <- getCode tc
                        codefun <- getCode tv
                        save fn codelisa codefun
    where
        save:: FilePath -> String -> String ->GuiMonad ()
        save filename codelisa codefun = 
            let (filelisa,filefun) = (filename++".lisa",filename++".fun") in
                io (writeFile filelisa codelisa) >>
                io (writeFile filefun codefun)

-- | Guardado en, de un archivo. (esto sería guardar como no?, es decir saveAs)
saveAtFile :: GuiMonad ()
saveAtFile = getHGState >>= \st -> ask >>= \content ->
             do
             let nFile = maybe "" id (st ^. gFileName)
             
             mfp <- saveDialog "Guardar programa" (nFile++".lisa") halFileFilter
             when (isJust mfp) (updateFL $ fromJust mfp)
             return ()
    where
        updateFL :: FilePath -> GuiMonad ()
        updateFL fp = let fname = dropExtension fp in
                          updateHGState ((<~) gFileName $ Just fname)

-- | Dialogo general para guardar un archivo.
saveDialog :: String -> String -> (FileChooserDialog -> IO ()) -> 
              GuiMonad (Maybe FilePath)
saveDialog label filename fileFilter = ask >>= \content ->
        do
        let tcode = content ^. gTextCode
            tverif = content ^. gTextVerif
        dialog <- io $ fileChooserDialogNew (Just label) 
                                            Nothing 
                                            FileChooserActionSave 
                                            [ ("Guardar",ResponseAccept)
                                            , ("Cancelar",ResponseCancel)
                                            ]
        
        io $ fileChooserSetCurrentName dialog filename
        io $ fileFilter dialog
        response <- io $ dialogRun dialog

        codelisa <- getCode tcode
        codefun <- getCode tverif
        
        case response of
            ResponseAccept -> io (fileChooserGetFilename dialog) >>= 
                              \fp -> 
                              maybe (return ())
                                    (\f -> 
                                    let filename = dropExtension f in
                                        save (filename++".lisa") codelisa >> 
                                        save (filename++".fun") codefun)
                                    fp >>
                                io (widgetDestroy dialog) >> 
                                return fp
            _ -> io (widgetDestroy dialog) >> return Nothing
    where
        save:: FilePath -> String -> GuiMonad ()
        save filepath code = io $ writeFile filepath code

-- | Filtro de programas de fun.
halFileFilter :: (FileChooserClass f, MonadIO m) => f -> m ()
halFileFilter dialog = io $ setFileFilter dialog ["*.lisa"] "Programa de hal"


compile :: GuiMonad Bool
compile = get >>= \st -> ask >>= \content ->
          getHGState >>= \gst ->
        do
        let consoleTV = content ^. (gHalInfoConsole . infoConTView)
            mfile = gst ^. gFileName
        maybe (saveAtFile >> return False)
              (\fp -> io $ C.catch (let file = fp++".lisa" in
                                    readFile file >>= \code ->
                                    eval (parseCode consoleTV code) content st)
                      (\e -> let err = show (e :: C.IOException)  in
                             printErrorMsgIO ("Error leyendo archivo:\n" ++err) consoleTV >>
                             return False)
                             )
              mfile
        
    where parseCode consoleTV code =
            case parseExtPrgFromString code of
                Left er -> io (printErrorMsgIO ("Error compilando código: " ++ show er) consoleTV)
                           >> return False
                Right prg -> updateHGState ((<~) gHalPrg (Just prg)) >>
                    io (printInfoMsgIO "Código compilado" consoleTV) >>
                    return True


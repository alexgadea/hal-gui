module HGUI.Console (configConsoleTV,printInfoMsg, printErrorMsg) where

import Control.Monad.Trans.RWS (ask)

import Lens.Family

import Graphics.UI.Gtk hiding (get)

import HGUI.Config
import HGUI.GState
import HGUI.Utils

configConsoleTV :: TextView ->  IO ()
configConsoleTV tv  = do
        buf <- textViewGetBuffer tv
        -- Tags para el text buffer, para formatear texto:
        tagTable <- textBufferGetTagTable buf
        tag <- textTagNew (Just "ErrorScheme")
        set tag [ textTagForegroundGdk := textErrColorCommTV
                , textTagForegroundSet := True]
        textTagTableAdd tagTable tag
        
        tag <- textTagNew (Just "InfoScheme")
        set tag [ textTagForegroundGdk := textColorCommTV
                , textTagForegroundSet := True]
        textTagTableAdd tagTable tag
        
        widgetModifyBase tv StateNormal backColorCommTV
        widgetModifyText tv StateNormal textColorCommTV
        widgetShowAll tv        

printInfoMsg :: String -> TextView -> IO ()
printInfoMsg msg = printMsg msg "InfoScheme"
                
printErrorMsg :: String -> TextView -> IO ()
printErrorMsg msg = printMsg msg "ErrorScheme"

printMsg :: String -> TagName -> TextView -> IO ()
printMsg msg tagname infoTV  =
    io $ do infoBuf <- textViewGetBuffer infoTV
            titer <- textBufferGetEndIter infoBuf
            lineStart <- textIterGetLine titer
                
            -- Ingresamos el texto en el buffer
            putStrAtEnd infoBuf infoTV msg
                
            titer <- textBufferGetEndIter infoBuf
            lineEnd <- textIterGetLine titer
                
            start <- textBufferGetIterAtLine infoBuf lineStart
            end <- textBufferGetIterAtLine infoBuf lineEnd
                
            textBufferApplyTagByName infoBuf tagname start end
            widgetShowAll infoTV

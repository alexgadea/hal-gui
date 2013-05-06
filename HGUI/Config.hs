module HGUI.Config where

import Graphics.UI.Gtk


-- Para definir el lenguaje del resaltado

languageSpecFolder = "HGUI/sourceview/language-specs"

languageSpecFunFile = languageSpecFolder ++ "/fun.lang"

anotherLanguage = languageSpecFolder ++ "/haskell.lang"

textStylesFolder = "HGUI/sourceview/styles"

textStyleFileFun = textStylesFolder ++ "/fun.xml"

funMimeType = "text/fun"

languageSpecHalFile = languageSpecFolder ++ "/hal.lang"

textStyleFileHal = textStylesFolder ++ "/hal.xml"

halMimeType = "text/hal"

data LangInfo = LangInfo {
                    specFile :: String
                  , styleFile :: String
                  , mimeType :: String
                  , langName :: String
}

funLangInfo = LangInfo languageSpecFunFile textStyleFileFun funMimeType "fun"

halLangInfo = LangInfo languageSpecHalFile textStyleFileHal halMimeType "hal"

breakMark :: String
breakMark = "break"

evalLineColour :: String
evalLineColour = "#ee351de96116"

-- Para el identado:

funIdentWidth :: Int
funIdentWidth = 4
spacesInsteadTab = True
setIndentOnTab = True
autoIdent = True


-- Colores

backColorCommTV :: Color
backColorCommTV = Color 8000 8000 8000

textColorCommTV :: Color
textColorCommTV = Color 60000 60000 60000

textErrColorCommTV :: Color
textErrColorCommTV = Color 60000 20000 20000

scrollInc :: Double
scrollInc = 10.0

scrollDec :: Double
scrollDec = - scrollInc

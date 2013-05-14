module HGUI.Config where

import Graphics.UI.Gtk


-- Para definir el lenguaje del resaltado

languageSpecFolder = "HGUI/sourceview/language-specs"

languageSpecFunFile = languageSpecFolder ++ "/fun.lang"

anotherLanguage = languageSpecFolder ++ "/haskell.lang"

textStylesFolder = "HGUI/sourceview/styles"

textStyleFileFun = textStylesFolder ++ "/fun.xml"

funMimeType = "text/fun"

languageSpecHalFile = languageSpecFolder ++ "/lisa.lang"

textStyleFileHal = textStylesFolder ++ "/lisa.xml"

halMimeType = "text/lisa"

data LangInfo = LangInfo {
                    specFile :: String
                  , styleFile :: String
                  , mimeType :: String
                  , langName :: String
}

funLangInfo = LangInfo languageSpecFunFile textStyleFileFun funMimeType "fun"

halLangInfo = LangInfo languageSpecHalFile textStyleFileHal halMimeType "lisa"

breakMark :: String
breakMark = "break"

defToInputMsg :: String
defToInputMsg = "Esta vardef tal vez podría ser varinput."

formatErrorMsg :: String -> String
formatErrorMsg msg = "<span foreground=\"red\">"++msg++"</span>"

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

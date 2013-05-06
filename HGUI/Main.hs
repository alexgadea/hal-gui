module Main where

import Graphics.UI.Gtk hiding (get)

import HGUI.Gui

main :: IO ()
main = do 
    initGUI
    
    xml <- builderNew
    builderAddFromFile xml "hal.ui"
    
    mainHalGui xml
    
    mainGUI

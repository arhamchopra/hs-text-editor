module Lib.Graphics.Views where

import Graphics.UI.Gtk
import Lib.Graphics.FileMenuOptions
import Lib.Graphics.Notebook

uiDef =
    "<ui>\
    \  <menubar>\
    \    <menu name=\"File\" action=\"FileAction\">\
    \      <menuitem name=\"New\" action=\"NewAction\" />\
    \      <menuitem name=\"Open\" action=\"OpenAction\" />\
    \      <menuitem name=\"Save\" action=\"SaveAction\" />\
    \      <menuitem name=\"SaveAs\" action=\"SaveAsAction\" />\
    \      <separator/>\
    \      <menuitem name=\"Exit\" action=\"ExitAction\" />\
    \      <menuitem name=\"ExitAll\" action=\"ExitAllAction\" />\
    \      <placeholder name=\"FileMenuAdditions\" />\
    \    </menu>\
    \    <menu name=\"Edit\" action=\"EditAction\">\
    \      <menuitem name=\"Search\" action=\"SearchAction\"/>\
    \    </menu>\
    \  </menubar>\
    \</ui>"

data BufferState = BufferState {path :: String, buffer :: TextBuffer}

loadWindow = do
    initGUI

    rootWindow <- windowNew
    rootWindow `onDestroy` mainQuit
    {- rootWindow `onSizeRequest` return (Requisition 200 100) -}

    windowSetDefaultSize rootWindow 800 600
    windowSetPosition rootWindow WinPosCenter

    editorPane <- createNotebook
    addEventHandlers rootWindow editorPane

    -- Create the menus
    fileAct <- actionNew "FileAction" "File" Nothing Nothing
    editAct <- actionNew "EditAction" "Edit" Nothing Nothing

    -- Create menu items
    newAct <- actionNew "NewAction" "New"
            (Just "Clear the spreadsheet area.")
            (Just stockNew)
    newAct `onActionActivate` putStrLn "New activated."
    newAct `onActionActivate` (insertPageHandler editorPane)

    openAct <- actionNew "OpenAction" "Open"
            (Just "Open an existing spreadsheet.")
            (Just stockOpen)
    openAct `onActionActivate` (openPageHandler editorPane)

    saveAct <- actionNew "SaveAction" "Save"
            (Just "Save the current spreadsheet.")
            (Just stockSave)
    saveAct `onActionActivate` (savePageHandler editorPane)

    saveAsAct <- actionNew "SaveAsAction" "SaveAs"
            (Just "Save spreadsheet under new name.")
            (Just stockSaveAs)
    saveAsAct `onActionActivate` (saveAsPageHandler editorPane)

    exitAct <- actionNew "ExitAction" "Exit Tab"
            (Just "Exit this application.")
            (Just stockQuit)
    exitAct `onActionActivate` putStrLn "Exit activated"
    exitAct `onActionActivate` (closePageHandler editorPane)
    exitAllAct <- actionNew "ExitAllAction" "Exit All"
            (Just "Exit all the application.")
            (Just stockQuit)
    exitAllAct `onActionActivate` putStrLn "Exit activated"
    let action = do widgetDestroy editorPane
                    widgetDestroy rootWindow
                    mainQuit
    exitAllAct `onActionActivate` action
    searchAct <- actionNew "SearchAction" "Search"
            (Just "Search a word in the current tab.")
            (Just stockFind)
    searchAct `onActionActivate` (searchHandler editorPane)

    standardGroup <- actionGroupNew "standard"
    mapM_ (actionGroupAddAction standardGroup) [fileAct, editAct]
    mapM_ (actionGroupAddAction standardGroup)
        [newAct, openAct, saveAct, saveAsAct, exitAct, exitAllAct, searchAct]

    ui <- uiManagerNew
    mid <- uiManagerAddUiFromString ui uiDef
    uiManagerInsertActionGroup ui standardGroup 0

    (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"

    vBox <- vBoxNew False 0
    set vBox [boxHomogeneous := False]
    boxPackStart vBox menuBar PackNatural 0
    boxPackStart vBox editorPane PackGrow 1

    containerAdd rootWindow vBox
    widgetShowAll rootWindow

    mainGUI

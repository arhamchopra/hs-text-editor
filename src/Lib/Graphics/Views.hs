{-# LANGUAGE TemplateHaskell #-}
module Lib.Graphics.Views where

import Graphics.UI.Gtk
import Lib.Graphics.FileMenuOptions

uiDef =
    "<ui>\
    \  <menubar>\
    \    <menu name=\"File\" action=\"FileAction\">\
    \      <menuitem name=\"New\" action=\"NewAction\" />\
    \      <menuitem name=\"Open\" action=\"OpenAction\" />\
    \      <menuitem name=\"Save\" action=\"SaveAction\" />\
    \      <menuitem name=\"SaveAs\" action=\"SaveAsAction\" />\
    \      <separator/>\
    \      <menuitem name=\"Exit\" action=\"ExitAction\"/>\
    \      <placeholder name=\"FileMenuAdditions\" />\
    \    </menu>\
    \    <menu name=\"Edit\" action=\"EditAction\">\
    \      <menuitem name=\"Cut\" action=\"CutAction\"/>\
    \      <menuitem name=\"Copy\" action=\"CopyAction\"/>\
    \      <menuitem name=\"Paste\" action=\"PasteAction\"/>\
    \    </menu>\
    \  </menubar>\
    \  <toolbar>\
    \    <toolitem action=\"TabAction\" />\
    \  </toolbar>\
    \</ui>"

data BufferState = BufferState {path :: String, buffer :: TextBuffer}

loadWindow = do
    initGUI

    win <- windowNew
    win `onDestroy` mainQuit
    win `onSizeRequest` return (Requisition 200 100)

    adjust1 <- adjustmentNew 0 0 100 10 50 300
    adjust2 <- adjustmentNew 0 0 100 10 50 300

    edit <- textViewNew
    scroll_window <- scrolledWindowNew (Just adjust1) (Just adjust2)
    containerAdd scroll_window edit

    -- Create the menus
    fileAct <- actionNew "FileAction" "File" Nothing Nothing
    editAct <- actionNew "EditAction" "Edit" Nothing Nothing

    -- Create menu items
    newAct <- actionNew "NewAction" "New"
            (Just "Clear the spreadsheet area.")
            (Just stockNew)
    newAct `onActionActivate` putStrLn "New activated."

    openAct <- actionNew "OpenAction" "Open"
            (Just "Open an existing spreadsheet.")
            (Just stockOpen)
    openAct `onActionActivate` (fileOpen edit)

    saveAct <- actionNew "SaveAction" "Save"
            (Just "Save the current spreadsheet.")
            (Just stockSave)
    saveAct `onActionActivate` (fileSave edit path)

    saveAsAct <- actionNew "SaveAsAction" "SaveAs"
            (Just "Save spreadsheet under new name.")
            (Just stockSaveAs)
    saveAsAct `onActionActivate` (fileSave edit Nothing)

    exitAct <- actionNew "ExitAction" "Exit"
            (Just "Exit this application.")
            (Just stockSaveAs)
    exitAct `onActionActivate` mainQuit
    cutAct <- actionNew "CutAction" "Cut"
            (Just "Cut out the current selection.")
            (Just stockCut)
    cutAct `onActionActivate` putStrLn "Cut activated."
    copyAct <- actionNew "CopyAction" "Copy"
            (Just "Copy the current selection.")
            (Just stockCopy)
    copyAct `onActionActivate` putStrLn "Copy activated."
    pasteAct <- actionNew "PasteAction" "Paste"
            (Just "Paste the current selection.")
            (Just stockPaste)
    pasteAct `onActionActivate` putStrLn "Paste activated."

    standardGroup <- actionGroupNew "standard"
    mapM_ (actionGroupAddAction standardGroup) [fileAct, editAct]
    mapM_ (actionGroupAddAction standardGroup)
        [newAct, openAct, saveAct, saveAsAct, exitAct, cutAct, copyAct, pasteAct]
    {- mapM_ (\act -> actionGroupAddActionWithAccel standardGroup act Nothing) -}
    {-     [newAct, openAct, saveAct, saveAsAct, exitAct, cutAct, copyAct, pasteAct] -}
    ui <- uiManagerNew
    mid <- uiManagerAddUiFromString ui uiDef
    uiManagerInsertActionGroup ui standardGroup 0

    (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"
    (Just toolBar) <- uiManagerGetWidget ui "/ui/toolbar"

    vBox <- vBoxNew False 0
    set vBox [boxHomogeneous := False]
    boxPackStart vBox menuBar PackNatural 0
    boxPackStart vBox toolBar PackNatural 0
    boxPackStart vBox scroll_window PackGrow 0

    containerAdd win vBox
    widgetShowAll win

    mainGUI

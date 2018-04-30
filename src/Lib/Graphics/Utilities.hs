{-# LANGUAGE OverloadedStrings #-}
module Lib.Graphics.Utilities (
  fileSave,
  fileRead,
  extractAllDataTextView,
  runSimpleDialog,
  getTextViewFromNotebook,
  setDataTextView) where

import Graphics.UI.Gtk
import Control.Monad
import Control.Monad.IO.Class


------------------------- Set/Get data to/from widgets ----------------------
extractDataOffsetsTextView textview offset_start offset_end = do
  textbuffer <- textViewGetBuffer textview
  startIter <- textBufferGetIterAtOffset textbuffer offset_start
  endIter <- textBufferGetIterAtOffset textbuffer offset_end
  textData <- textBufferGetText textbuffer startIter endIter True
  return textData

extractAllDataTextView textview = extractDataOffsetsTextView textview 0 (-1)

setDataTextView textview text = do
  textbuffer <- textViewGetBuffer textview
  textBufferSetText textbuffer text

getTextViewFromNotebook notebook pageIndex = do
  (Just widget) <- notebookGetNthPage notebook pageIndex
  childWidgets <- containerGetChildren (castToContainer widget)
  return (castToTextView (head childWidgets))

--------------------------- Dialog Box ----------------------------------------
runSimpleDialog acceptStr cancelStr = do
  dialog <- dialogNew
  acceptButton <- dialogAddButton dialog acceptStr ResponseAccept
  rejectButton <- dialogAddButton dialog cancelStr ResponseCancel

  dialog `on` keyPressEvent $ tryEvent $ do
    "Return" <- eventKeyName
    liftIO $ buttonClicked acceptButton

  dialog `on` keyPressEvent $ tryEvent $ do
    "Esc" <- eventKeyName
    liftIO $ buttonClicked rejectButton

  getText <- textViewNew

  vbox <- dialogGetUpper dialog
  boxPackStart vbox getText PackNatural 10

  widgetShowAll dialog
  response <- dialogRun dialog
  text <- extractAllDataTextView getText
  widgetDestroy dialog

  return (response, text)

runFileChooseDialog title acceptStr cancelStr overwriteConf = do
  fchdal <- fileChooserDialogNew (Just title) Nothing
                                  FileChooserActionSave
                                  [(cancelStr, ResponseCancel),
                                  (acceptStr, ResponseAccept),
                                  ("Backup", ResponseUser 100)]
  fileChooserSetDoOverwriteConfirmation fchdal overwriteConf
  widgetShow fchdal
  response <- dialogRun fchdal
  filename <- fileChooserGetFilename fchdal
  widgetDestroy fchdal
  return (response, filename)
--------------------------------------------------------------------------------------

--------------------------- Internal Utilities ----------------------------------------
fileWriteData text = do
  (response, filename)<- runFileChooseDialog ("Save"::String) ("Save"::String) ("Cancel"::String) True
  case response of
    ResponseCancel -> return Nothing
    ResponseAccept -> do case filename of
                          Nothing -> return Nothing
                          Just path -> do
                            writeFile path text
                            return (Just path)
    ResponseUser 100 -> return Nothing
    ResponseDeleteEvent -> return Nothing

fileReadData :: IO (Maybe String, Maybe String)
fileReadData = do
  (response, filename)<- runFileChooseDialog ("Open"::String) ("Open"::String) ("Cancel"::String) False
  case response of
    ResponseCancel -> return (Nothing, Nothing)
    ResponseAccept -> do case filename of
                          Nothing -> return (Nothing, Nothing)
                          Just path -> do
                            text <- readFile path
                            return (Just path, Just text)
    ResponseUser 100 -> return (Nothing, Nothing)
    ResponseDeleteEvent -> return (Nothing, Nothing)

--------------------------- File Utilities ----------------------------------------
fileRead = fileReadData

fileSave :: String -> Maybe String -> IO (Maybe String)
fileSave textData path = do
  case path of
    (Just "") -> do
      newPath <- fileWriteData textData
      return newPath
    (Just oldPath) -> do
      writeFile oldPath textData
      return Nothing
-----------------------------------------------------------------------------------

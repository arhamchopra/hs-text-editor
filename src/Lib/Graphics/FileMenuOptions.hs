module Lib.Graphics.FileMenuOptions (
    fileSave,
    fileOpen
                                    ) where

import Graphics.UI.Gtk

{- Extract/Submit data from/to widgets -}
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

fileWriteData = \text -> do
    (response, filename)<- runFileChooseDialog "Save" "Save" "Cancel" True
    case response of
      ResponseCancel -> putStrLn "You cancelled..."
      ResponseAccept -> do case filename of
                              Nothing -> return ()
                              Just path -> writeFile path text
      ResponseUser 100 -> putStrLn "You pressed the backup button"
      ResponseDeleteEvent -> putStrLn "You closed the dialog window..."

fileReadData :: IO (Maybe String)
fileReadData = do
    (response, filename)<- runFileChooseDialog "Open" "Open" "Cancel" False
    case response of
      ResponseCancel -> do putStrLn "You cancelled..."
                           return (Nothing, Nothing)
      ResponseAccept -> do case filename of
                             Nothing -> return (Nothing, Nothing)
                             Just path -> do
                                 text <- readFile path
                                 return (Just path, Just text)
      ResponseUser 100 -> do putStrLn "You pressed the backup button"
                             return (Nothing, Nothing)
      ResponseDeleteEvent -> do putStrLn "You closed the dialog window..."
                                return (Nothing, Nothing)

{- Main functions -}
fileOpen textview = do (path, text) <- fileReadData
                       case text of
                            Nothing -> setDataTextView textview ""
                            Just str -> setDataTextView textview str
                       return path

fileSave textview path = do textData <- extractAllDataTextView textview
                            fileWriteData textData path


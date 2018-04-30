{-# LANGUAGE OverloadedStrings #-}
module Lib.Graphics.Commands (
  createCommandView
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.IORef
import Graphics.UI.Gtk
import Lib.Graphics.Utilities
import Lib.Graphics.Notebook
import Language.Haskell.Interpreter
import Data.List.Split

editorCommandsList=["new", "open", "search", "close", "closeAll", "save", "saveAs", "tab"]

createCommandView rootWindow editorPane = do
  commandView <- textViewNew

  commandView `on` keyPressEvent $ tryEvent $ do
    "Return" <- eventKeyName
    liftIO $ commandExecuteHandler commandView editorPane

  commandView `on` keyPressEvent $ tryEvent $ do
    "Tab" <- eventKeyName
    liftIO $ return ()

  rootWindow `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "e" <- eventKeyName
    liftIO $ widgetGrabFocus commandView

  return commandView

commandExecuteHandler commandView editorPane = do
  commandText <- extractAllDataTextView commandView
  case isSystemCommand commandText of
    True -> do
      runSystemCommand commandText editorPane
      setDataTextView commandView (""::String)
    False -> do 
      result <- runHaskellCommand commandText
      case result of
        (Right r) -> setDataTextView commandView (show r)
        (Left r) -> setDataTextView commandView (show "Error in command")
  widgetGrabFocus editorPane

isSystemCommand command = elem (head (splitOn " " command)) editorCommandsList

runHaskellCommand command = runInterpreter $ setImports ["Prelude"] >> eval command

runSystemCommand command editorPane = case commandType of
                                        "new" -> insertPageHandler editorPane
                                        "open" -> openPageHandler editorPane
                                        "search" -> searchHandler editorPane
                                        "close" -> closePageHandler editorPane
                                        "closeAll" -> closePageHandler editorPane
                                        "save" -> savePageHandler editorPane
                                        "saveAs" -> saveAsPageHandler editorPane
                                        "tab" -> tabPageHandler editorPane ((read commandArg)::Int)
                                      where commandType = head (splitOn " " command)
                                            commandArg = head ( tail (splitOn " " command))

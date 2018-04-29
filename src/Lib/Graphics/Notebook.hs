{-# LANGUAGE OverloadedStrings #-}
module Lib.Graphics.Notebook (
  createNotebook,
  addEventHandlers,
  insertPageHandler,
  closePageHandler,
  switchPageHandler,
  switchPrevPageHandler
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.IORef
import Graphics.UI.Gtk

data NotebookTab =
    NotebookTab {ntBox          :: HBox
                ,ntSpinner      :: Spinner      {- Not needed -}
                ,ntLabel        :: Label
                ,ntCloseButton  :: ToolButton
                ,ntSize         :: Int
                ,filePath       :: Maybe (IORef [Char])}

-- | Main
createNotebook :: IO Notebook
createNotebook = do
  notebook <- notebookNew
  {- Allow scrolling over the tabs -}
  set notebook [ notebookScrollable := True ]
  return notebook

addEventHandlers window notebook = do
  window `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "n" <- eventKeyName
    liftIO $ insertPageHandler notebook  -- Show window.

  window `on` keyPressEvent $ tryEvent $ do
    -- Close a tab when user press Ctrl+w
    [Control] <- eventModifier
    "w" <- eventKeyName
    liftIO $ closePageHandler notebook  -- Show window.

  window `on` keyPressEvent $ tryEvent $ do
    -- Close a tab when user press Ctrl+w
    [Control] <- eventModifier
    "Tab" <- eventKeyName
    liftIO $ switchPageHandler notebook  -- Show window.

  window `on` keyPressEvent $ tryEvent $ do
    -- Close a tab when user press Ctrl+w
    [Alt] <- eventModifier
    "Tab" <- eventKeyName
    liftIO $ switchPrevPageHandler notebook  -- Show window.

  {- window `on` keyPressEvent $ tryEvent $ do -}
    -- Close a tab when user press Ctrl+w
    {- [Control] <- eventModifier -}
    {- "s" <- eventKeyName -}
    {- liftIO $ savePageHandler notebook  -- Show window. -}

{- savePageHandler :: Notebook -> IO () -}
{- savePageHandler notebook =  -}

switchPrevPageHandler :: Notebook -> IO ()
switchPrevPageHandler notebook = notebookPrevPage notebook

switchPageHandler :: Notebook -> IO ()
switchPageHandler notebook = notebookNextPage notebook

closePageHandler :: Notebook -> IO ()
closePageHandler notebook = do
  pageIndex <- get notebook notebookCurrentPage
  (Just pageIndex) ?>= \i -> notebookRemovePage notebook i


insertPageHandler :: Notebook -> IO ()
insertPageHandler notebook = do
  -- Create Scrolling View
  adjust1 <- adjustmentNew 0 0 100 10 30 300
  adjust2 <- adjustmentNew 0 0 100 10 30 300
  scroll_window <- scrolledWindowNew (Just adjust1) (Just adjust2)

  -- Create text view.
  textView <- textViewNew
  widgetShowAll textView -- must show before add notebook,
                        -- otherwise notebook won't display child widget 
                        -- even have add in notebook.

  -- Add textview to the scrolling window to allow scroll
  containerAdd scroll_window textView
  widgetShowAll scroll_window

  -- Create notebook tab.
  tab <- notebookTabNew (Just "Untitled.txt") Nothing
  menuLabel <- labelNew (Nothing :: Maybe String)

  -- Add widgets in notebook.
  pageIndex <- notebookAppendPageMenu notebook scroll_window (ntBox tab) menuLabel

  -- Move to the new page
  notebookSetCurrentPage notebook pageIndex

  -- Start spinner animation when create tab.
  {- notebookTabStart tab -}

  -- Stop spinner animation after finish load.
  {- timeoutAdd (notebookTabStop tab >> return False) 5000 -}

  -- Close tab when click button.
  ntCloseButton tab `onToolButtonClicked` do
    index <- notebookPageNum notebook scroll_window
    index ?>= \i -> notebookRemovePage notebook i

  return ()


-- | Create notebook tab.
notebookTabNew :: Maybe String -> Maybe Int -> IO NotebookTab
notebookTabNew name size = do
  -- Init.
  let iconSize = fromMaybe 12 size
  box <- hBoxNew False 0
  spinner <- spinnerNew
  label <- labelNew name
  image <- imageNewFromIcon "text-x-component" iconSize
  closeButton <- toolButtonNew (Just image) (Nothing :: Maybe String)

  -- Show.
  boxPackStart box label PackNatural 0
  boxPackStart box closeButton PackNatural 0
  widgetShowAll box

  return $ NotebookTab box spinner label closeButton iconSize Nothing

-- | Set tab name.
notebookTabSetName :: NotebookTab -> String -> IO ()
notebookTabSetName tab = 
  labelSetText (ntLabel tab)

-- | Start spinner animation.
notebookTabStart :: NotebookTab -> IO ()
notebookTabStart NotebookTab {ntBox     = box
                             ,ntSpinner = spinner
                             ,ntSize    = size} = do
  boxTryPack box spinner PackNatural (Just 0) (size `div` 2)
  spinnerStart spinner
  widgetShow spinner

-- | Stop spinner animation.
notebookTabStop :: NotebookTab -> IO ()
notebookTabStop NotebookTab {ntBox     = box
                            ,ntSpinner = spinner} = do
  containerTryRemove box spinner
  spinnerStop spinner

-- | Create image widget with given icon name and size. 
imageNewFromIcon :: String -> Int -> IO Image
imageNewFromIcon iconName size = do
  iconTheme <- iconThemeGetDefault
  pixbuf <- do 
    -- Function 'iconThemeLoadIcon' can scale icon with specified size.
    pixbuf <- iconThemeLoadIcon iconTheme iconName size IconLookupUseBuiltin
    case pixbuf of
      Just p  -> return p
      Nothing -> error $ "imageNewFromIcon : search icon " ++ iconName ++ " failed."
  imageNewFromPixbuf pixbuf

-- | Try to packing widget in box.
-- If @child@ have exist parent, do nothing,
-- otherwise, add @child@ to @parent@.
boxTryPack :: (BoxClass parent, WidgetClass child) => parent -> child -> Packing -> Maybe Int -> Int -> IO ()
boxTryPack box widget packing order space = do
  parent <- widgetGetParent widget
  when (isNothing parent) $ do
    boxPackStart box widget packing space
    order ?>= boxReorderChild box widget

-- | Try to remove child from parent.    
containerTryRemove :: (ContainerClass parent, WidgetClass child) => parent -> child -> IO ()     
containerTryRemove parent widget = do
  hasParent <- widgetGetParent widget
  unless (isNothing hasParent) $ containerRemove parent widget

-- | Maybe.
(?>=) :: Monad m => Maybe a -> (a -> m ()) -> m () 
m ?>= f = maybe (return ()) f m
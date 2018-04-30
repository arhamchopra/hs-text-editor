{-# LANGUAGE OverloadedStrings #-}
module Lib.Graphics.Notebook (
  createNotebook,
  addEventHandlers,
  insertPageHandler,
  closePageHandler,
  switchPageHandler,
  switchPrevPageHandler,
  savePageHandler,
  saveAsPageHandler,
  openPageHandler,
  searchHandler,
  tabPageHandler
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.IORef
import Graphics.UI.Gtk
import Lib.Graphics.Utilities

data NotebookTab =
    NotebookTab {ntBox          :: HBox
                ,ntSpinner      :: Spinner      {- Not needed -}
                ,ntLabel        :: Label
                ,ntCloseButton  :: ToolButton
                ,ntSize         :: Int}

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

  window `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "f" <- eventKeyName
    liftIO $ searchHandler notebook

  window `on` keyPressEvent $ tryEvent $ do
    {- Close a tab when user press Ctrl+w -}
    [Control] <- eventModifier
    "s" <- eventKeyName
    liftIO $ savePageHandler notebook -- Show window.

  window `on` keyPressEvent $ tryEvent $ do
    {- Close a tab when user press Ctrl+w -}
    [Control] <- eventModifier
    "o" <- eventKeyName
    liftIO $ openPageHandler notebook -- Show window.

tabPageHandler :: Notebook -> Int -> IO ()
tabPageHandler notebook pageIndex = do
  notebookSetCurrentPage notebook pageIndex

openPageHandler :: Notebook -> IO ()
openPageHandler notebook = do
  (path, text) <- fileRead
  case (text,path) of
    (Just textData, Just textPath) -> do
      maybeCurMenuLabel <- readMenuLabel notebook
      case maybeCurMenuLabel of 
        (Just "") -> do updateCurrentPage notebook textPath textPath textData
                        return ()
        s -> do pageIndex <- createPageNew notebook textPath textPath textData
                return ()
    (_, _) -> return ()

saveAsPageHandler :: Notebook -> IO ()
saveAsPageHandler notebook = do
  maybePath <- readMenuLabel notebook
  case maybePath of
    Nothing -> return ()
    p@(Just justPath) -> do
      pageIndex <- get notebook notebookCurrentPage
      textView <- getTextViewFromNotebook notebook pageIndex
      textData <- extractAllDataTextView textView
      path <- fileSave textData (Just "")
      case path of 
        Nothing -> return ()
        (Just newPath) -> do
          writeMenuLabel notebook newPath
          writeTabLabel notebook newPath


savePageHandler :: Notebook -> IO ()
savePageHandler notebook = do
  maybePath <- readMenuLabel notebook
  case maybePath of
    Nothing -> return ()
    p@(Just justPath) -> do
      pageIndex <- get notebook notebookCurrentPage
      textView <- getTextViewFromNotebook notebook pageIndex
      textData <- extractAllDataTextView textView
      path <- fileSave textData p
      case path of 
        Nothing -> return ()
        (Just newPath) -> do
          writeMenuLabel notebook newPath
          writeTabLabel notebook newPath

searchHandler :: Notebook -> IO ()
searchHandler notebook = do
  pageIndex <- get notebook notebookCurrentPage
  textView <- getTextViewFromNotebook notebook pageIndex
  (response, query) <- runSimpleDialog ("Search"::String) ("Cancel"::String)
  case response of
      ResponseCancel -> putStrLn "You Cancelled"
      ResponseAccept -> do
          modifySearchText textView query

switchPrevPageHandler :: Notebook -> IO ()
switchPrevPageHandler notebook = notebookPrevPage notebook

switchPageHandler :: Notebook -> IO ()
switchPageHandler notebook = notebookNextPage notebook

closePageHandler :: Notebook -> IO ()
closePageHandler notebook = do
  pageIndex <- get notebook notebookCurrentPage
  (Just pageIndex) ?>= \i -> notebookRemovePage notebook i

addFindTag textView = do
            textBuf <- textViewGetBuffer textView
            tagTable <- textBufferGetTagTable textBuf --(Just "hello")
            tag <- textTagNew (Just "find")
            textTagTableAdd tagTable tag
            --set textBuf [textBufferTagTable := tagTable]
            set tag [textTagBackground := ("Yellow" :: String)]

insertPageHandler :: Notebook -> IO ()
insertPageHandler notebook = do
  pageIndex <- createPageNew notebook "Untitled.txt" "" ""  
  return ()

updateCurrentPage :: Notebook -> String -> String -> String -> IO ()
updateCurrentPage notebook tabLabel menuLabel textData = do
  writeTabLabel notebook tabLabel
  writeMenuLabel notebook menuLabel

  pageIndex <- get notebook notebookCurrentPage
  textView <- getTextViewFromNotebook notebook pageIndex
  setDataTextView textView textData

createPageNew :: Notebook -> String -> String -> String -> IO Int
createPageNew notebook tabLabel menuLabel textData = do
  -- Create Scrolling View
  adjust1 <- adjustmentNew 0 0 100 10 30 300
  adjust2 <- adjustmentNew 0 0 100 10 30 300
  scroll_window <- scrolledWindowNew (Just adjust1) (Just adjust2)

  -- Create text view.
  textView <- textViewNew
  addFindTag textView

  setDataTextView textView textData

  -- Add textview to the scrolling window to allow scroll
  containerAdd scroll_window textView
  widgetShowAll scroll_window

  -- Create notebook tab.
  tab <- notebookTabNew (Just tabLabel) Nothing
  menuLabel <- labelNew (Just menuLabel)

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

  return pageIndex

-- | Create notebook tab.
notebookTabNew :: Maybe String -> Maybe Int -> IO NotebookTab
notebookTabNew name size = do
  -- Init.
  let iconSize = fromMaybe 12 size
  box <- hBoxNew False 0
  spinner <- spinnerNew
  label <- labelNew name
  image <- imageNewFromIcon "go-first" iconSize
  closeButton <- toolButtonNew (Just image) (Nothing :: Maybe String)

  -- Show.
  boxPackStart box label PackNatural 0
  boxPackStart box closeButton PackNatural 0
  widgetShowAll box

  return $ NotebookTab box spinner label closeButton iconSize

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

---------Search Utility Functions -------------
modifySearchText textView pat = do
    textBuf <- textViewGetBuffer textView
    startIter <- textBufferGetIterAtOffset textBuf 0
    endIter <- textBufferGetIterAtOffset textBuf (-1)
    textBufferRemoveTagByName textBuf "find" startIter endIter
    text <- extractAllDataTextView textView
    changeSearchText textView (searchPattern text pat 0) (length pat)

match :: String -> String -> Bool
match text [] = True
match [] pat = False
match (c:text) (p:pat) | c==p = match text pat
                       | otherwise = False

searchPattern :: String -> String -> Int -> [Int]
searchPattern "" _ _ = []
searchPattern (c:text) pat pos = case match (c:text) pat of
                                    True -> (pos:rest)
                                    False -> rest
                                where rest = searchPattern text pat (pos+1)

changeSearchText textView [] l = return ()
changeSearchText textView (pos:poses) l = do 
        changeText textView pos (pos+l)
        changeSearchText textView poses l

changeText textView pos1 pos2 = do
    textBuf <- textViewGetBuffer textView
    startIter <- textBufferGetIterAtOffset textBuf pos1
    endIter <- textBufferGetIterAtOffset textBuf pos2
    textBufferApplyTagByName textBuf "find" startIter endIter
----------------------------------------------------------

---------------------Menu Label Utility Functions----------------------------

readMenuLabel:: Notebook -> IO (Maybe String)
readMenuLabel notebook = do
  pageIndex <- get notebook notebookCurrentPage
  pagewidget <- notebookGetNthPage notebook pageIndex
  case pagewidget of
    Nothing -> return Nothing
    Just pagewidget -> do
      text <- notebookGetMenuLabelText notebook pagewidget
      return text

writeMenuLabel :: Notebook -> String -> IO ()
writeMenuLabel notebook newLabel = do
  pageIndex <- get notebook notebookCurrentPage
  (Just pagewidget) <- notebookGetNthPage notebook pageIndex
  notebookSetMenuLabelText notebook pagewidget newLabel
--------------------------------------------------------------------------------

---------------------Tab Label Utility Functions----------------------------

writeTabLabel :: Notebook -> String -> IO ()
writeTabLabel notebook newLabel = do
  pageIndex <- get notebook notebookCurrentPage
  (Just pagewidget) <- notebookGetNthPage notebook pageIndex
  newTab <- notebookTabNew (Just newLabel) Nothing
  notebookSetTabLabel notebook pagewidget (ntBox newTab)

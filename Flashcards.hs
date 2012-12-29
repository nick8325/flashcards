module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.IORef
import Data.List
import Data.Tuple
import Graphics.UI.Gtk
import System.Random
import Test.QuickCheck.Gen

attr c = c minBound maxBound

newLabel bold size = do
  label <- labelNew Nothing
  set label
    [labelJustify := JustifyCenter,
     labelAttributes :=
       [attr AttrFamily "Warnock Pro",
        attr AttrWeight (if bold then WeightSemibold else WeightMedium),
        attr AttrSize size]]
  return label

glue :: BoxClass box => box -> IO ()
glue box = drawingAreaNew >>= boxPackStartDefaults box

data Model = Model {
  getState :: IO State,
  putState :: State -> IO ()
  }

data State = State {
  questions :: [(String, String)],
  cards :: [Card]
  }

type Card = (Maybe String, Maybe String)

model :: Label -> Label -> [(String, String)] -> IO Model
model question answer questions = do
  cards <- shuffleIO questions
  ref <- newIORef (State questions cards)
  update ref (State questions cards)
  return (Model (readIORef ref) (update ref))
  where
    update ref state = do
      writeIORef ref state
      draw (head (cards state))

    draw (mq, ma) = do
      drawOne question mq
      drawOne answer ma

    drawOne label Nothing = set label [labelText := ""]
    drawOne label (Just xs) = set label [labelText := xs]

repeatM :: Monad m => m a -> m [a]
repeatM x = liftM2 (:) x (repeatM x)

shuffle :: [(String, String)] -> Gen [Card]
shuffle qs = fmap (concat . noRepeating) (repeatM oneCard)
  where
    oneCard = do
      (x, y) <- elements qs
      return [(Just x, Nothing), (Just x, Just y)]
    noRepeating = map head . group

shuffleIO :: [(String, String)] -> IO [Card]
shuffleIO qs = do
  g <- newStdGen
  return (unGen (shuffle qs) g 0)

nextCard :: Model -> IO ()
nextCard model = do
  state <- getState model
  putState model state { cards = tail (cards state) }

reshuffle :: ((String, String) -> (String, String)) -> Model -> IO ()
reshuffle permute model = do
  state <- getState model
  cards <- shuffleIO (map permute (questions state))
  putState model state { cards = cards }

parse :: String -> [(String, String)]
parse = map split . lines
  where
    split xs =
      case span (/= ':') xs of
        (ys, ':':zs) -> (trim ys, trim zs)
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

event :: Model -> Window -> EventM EKey ()
event model window = do
  Just char <- fmap (fmap toLower . keyToChar) eventKeyVal
  liftIO $ case char of
    'q' -> widgetDestroy window
    ' ' -> nextCard model
    '1' -> reshuffle id model
    '2' -> reshuffle swap model

main = do
  initGUI
  window <- windowNew
  window `onDestroy` mainQuit
  
  box <- vBoxNew False 0
  question <- newLabel True 72
  answer <- newLabel False 48

  glue box
  boxPackStart box question PackNatural 0
  glue box
  boxPackStart box answer PackNatural 0
  glue box
  
  model <- model question answer =<< fmap parse (readFile "cards")
  window `on` keyPressEvent $ tryEvent $ event model window
  set window
    [windowTitle := "Russian flashcards",
     containerChild := box]
  widgetShowAll window
  
  mainGUI
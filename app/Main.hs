{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Monad
import           System.IO
import           UI.NCurses

import           Hedit


data Screen         = Screen VerticalOffset Height Width
type Height         = Int
type Width          = Int
type VerticalOffset = Int

main ∷ IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  mainloop w (Screen 0 0 0) (State (Cursor 0 0) [])

mainloop ∷ Window → Screen -> State → Curses ()
mainloop w (Screen verticalOffset height width) (State (Cursor cursorY cursorX) buffer) = do
  updateWindow w $ do
    drawBuffer buffer (Screen verticalOffset height width)
    drawCursor (Cursor cursorY cursorX)
  render
  ev <- getEvent w Nothing
  case ev of
    Just ev' | ev' == EventCharacter 'q' -> return ()
    Just ev'                             -> mainloop w (Screen verticalOffset height width) (State (Cursor cursorY cursorX) buffer)

drawCursor ∷ Cursor → Update()
drawCursor (Cursor cursorY cursorX) =
  moveCursor (fromIntegral cursorY) (fromIntegral cursorX)

drawBuffer ∷ Buffer → Screen → Update()
drawBuffer buffer (Screen verticalOffset height width) = do
  clear
  forM_ lines drawString
  where lines = fmap snd
                $ filter (\ (i, s) -> i >= verticalOffset && i < verticalOffset + height)
                $ zip [0..] buffer

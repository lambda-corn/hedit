{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Monad
import           Data.Char
import           System.IO
import           UI.NCurses

import           Hedit


newtype Screen      = Screen Int

main ∷ IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  mainloop w (Screen 0) (State (Cursor 0 0) [[]])

mainloop ∷ Window → Screen -> State → Curses ()
mainloop w (Screen verticalOffset) (State (Cursor cursorY cursorX) buffer) = do
  updateWindow w $ do
    drawBuffer buffer (Screen verticalOffset)
    drawCursor (Cursor cursorY cursorX)
  render
  ev <- getEvent w Nothing
  case ev of
    Just ev' | ev' == EventCharacter 'q' -> return ()
    Just (EventCharacter c) | isPrint c  -> mainloop w (Screen verticalOffset) (write c verticalOffset (State (Cursor cursorY cursorX) buffer))
    Just ev'                             -> mainloop w (Screen verticalOffset) (State (Cursor cursorY cursorX) buffer)

drawCursor ∷ Cursor → Update()
drawCursor (Cursor cursorY cursorX) =
  moveCursor (fromIntegral cursorY) (fromIntegral cursorX)

drawBuffer ∷ Buffer → Screen → Update()
drawBuffer buffer (Screen verticalOffset) = do
  clear
  (h, _) <- windowSize
  forM_ (lines $ fromIntegral h) drawString
    where lines h = fmap snd
                $ filter (\ (i, s) -> i >= verticalOffset && i < verticalOffset + h)
                $ zip [0..] buffer

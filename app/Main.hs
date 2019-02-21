{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Monad
import           Data.Char
import           Data.List
import           System.IO
import           UI.NCurses

import           Hedit


main ∷ IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  mainloop w (State (VirtualScreen 0 0) (Cursor 0 0) [[]])

mainloop ∷ Window → State → Curses ()
mainloop w s@(State (VirtualScreen vsy vsx) (Cursor cursorY cursorX) buffer) = do
  updateWindow w $ do
    drawBuffer buffer (VirtualScreen vsy vsx)
    drawCursor (Cursor cursorY cursorX)
  render
  ev <- getEvent w Nothing
  case ev of
    Just ev' | ev' == EventCharacter 'q'  -> return ()
    Just (EventSpecialKey KeyRightArrow)  -> mainloop w (moveRight s)
    Just (EventSpecialKey KeyLeftArrow)   -> mainloop w (moveLeft s)
    Just (EventSpecialKey KeyDownArrow)   -> mainloop w (moveDown s)
    Just (EventSpecialKey KeyUpArrow)     -> mainloop w (moveUp s)
    Just (EventCharacter c) | isPrint c   -> mainloop w (write c s)
    Just (EventCharacter b) | b == '\DEL' -> mainloop w (backspace s)
    Just (EventCharacter e) | e == '\n'   -> mainloop w (newLine s)
    Just ev'                              -> mainloop w s

drawCursor ∷ Cursor → Update()
drawCursor (Cursor cursorY cursorX) =
  moveCursor (fromIntegral cursorY) (fromIntegral cursorX)

drawBuffer ∷ Buffer → VirtualScreen → Update()
drawBuffer buffer (VirtualScreen vsy vsx) = do
    clear
    (h, _) <- windowSize
    forM_ (reverse $ lines $ fromIntegral h) (\ (number, text) -> do
      moveCursor (fromIntegral number) 0
      drawString text)
  where
    lines h = filter (\ (i, s) -> i >= vsy && i < vsy + h)
            $ zip [0..] buffer

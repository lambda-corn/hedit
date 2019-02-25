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
  s' <- updateWindow w $ updateScreen s
  render
  ev <- getEvent w Nothing
  case ev of
    Just ev' | ev' == EventCharacter 'q'  -> return ()
    Just (EventSpecialKey KeyRightArrow)  -> mainloop w (moveRight s')
    Just (EventSpecialKey KeyLeftArrow)   -> mainloop w (moveLeft s')
    Just (EventSpecialKey KeyDownArrow)   -> mainloop w (moveDown s')
    Just (EventSpecialKey KeyUpArrow)     -> mainloop w (moveUp s')
    Just (EventCharacter c) | isPrint c   -> mainloop w (write c s')
    Just (EventCharacter b) | b == '\DEL' -> mainloop w (backspace s')
    Just (EventCharacter e) | e == '\n'   -> mainloop w (newLine s')
    Just ev'                              -> mainloop w s'

updateScreen ∷ State → Update State
updateScreen (State vs@(VirtualScreen vsy vsx) c@(Cursor cy cx) buffer) = do
    clear
    (h, w) <- windowSize
    let (newVirtualScreen, Cursor ncy ncx) = updateScreenCoordinates vs c (fromIntegral h) (fromIntegral w)
    forM_ (reverse $ lines $ fromIntegral h) (\ (number, text) -> do
      moveCursor (fromIntegral (number - vsy)) 0
      drawString text)
    moveCursor (fromIntegral ncy) (fromIntegral ncx)
    return (State newVirtualScreen (Cursor ncy ncx) buffer)
  where
    lines h = filter (\ (i, s) -> i >= vsy && i < vsy + h)
            $ zip [0..] buffer

    updateScreenCoordinates ∷ VirtualScreen → Cursor → Int → Int → (VirtualScreen, Cursor)
    updateScreenCoordinates (VirtualScreen vsy vsx) (Cursor cy cx) height width =
      let
        nvsy
          | cy >= height = vsy + 1
          | cy < 0       = vsy - 1
          | otherwise    = vsy
        nvsx
          | cx >= width  = vsx + 1
          | cx < 0       = vsx - 1
          | otherwise    = vsx
        ncx
          | cx < 0       = 0
          | cx >= width  = width - 1
          | otherwise    = cx
        ncy
          | cy < 0       = 0
          | cy >= height = height - 1
          | otherwise    = cy
      in (VirtualScreen nvsy nvsx, Cursor ncy ncx)

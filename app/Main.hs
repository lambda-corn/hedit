{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.List
import           System.Environment
import           System.IO
import           UI.NCurses

import           Hedit


main ∷ IO ()
main = do
  args <- getArgs
  let filepath = if not (null args) then head args
                 else "a.out"
  buffer <- if not (null args) then lines <$> readFile (head args)
            else  return [[]]
  runCurses $ do
    setEcho False
    w <- defaultWindow
    setKeypad w True
    setRaw True
    mainloop w (State (VirtualScreen 0 0) (Cursor 0 0) buffer) filepath

mainloop ∷ Window → State → FilePath → Curses ()
mainloop w s@(State (VirtualScreen vsy vsx) (Cursor cursorY cursorX) buffer) filepath = do
  s'@(State vs cursor buffer') <- updateWindow w $ updateScreen s
  render
  ev <- getEvent w Nothing
  case ev of
    Just (EventCharacter c) | c == '\ETX' -> return ()
    Just (EventCharacter c) | c == '\DC3' -> do
                                             liftIO (save filepath (intercalate "\n" buffer))
                                             mainloop w s' filepath
    Just (EventSpecialKey KeyRightArrow)  -> mainloop w (moveRight s') filepath
    Just (EventSpecialKey KeyLeftArrow)   -> mainloop w (moveLeft s') filepath
    Just (EventSpecialKey KeyDownArrow)   -> mainloop w (moveDown s') filepath
    Just (EventSpecialKey KeyUpArrow)     -> mainloop w (moveUp s') filepath
    Just (EventCharacter c) | isPrint c   -> mainloop w (write c s') filepath
    Just (EventCharacter c) | c == '\DEL' -> mainloop w (backspace s') filepath
    Just (EventCharacter c) | c == '\n'   -> mainloop w (newLine s') filepath
    Just ev'                              -> mainloop w s' filepath
    Nothing                               -> mainloop w s' filepath

save ∷ FilePath → String → IO ()
save = writeFile

updateScreen ∷ State → Update State
updateScreen (State vs@(VirtualScreen vsy vsx) c@(Cursor cy cx) buffer) = do
    clear
    (h, w) <- windowSize
    let (newVirtualScreen, Cursor ncy ncx) = updateScreenCoordinates vs c (fromIntegral h) (fromIntegral w)
    forM_ (lines $ fromIntegral h) (\ (number, text) -> do
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
          | cy >= height = min (vsy + 1) (length buffer)
          | cy < 0       = max (vsy - 1) 0
          | otherwise    = vsy
        nvsx
          | cx >= width  = min (vsx + 1) (length (buffer!!cy))
          | cx < 0       = max (vsx - 1) 0
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

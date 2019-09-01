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
    -- setKeypad w True
    setRaw True
    mainloop w (State (VirtualScreen 0 0) (Cursor 0 0) buffer Insert) filepath

mainloop ∷ Window → State → FilePath → Curses ()
mainloop w s@(State (VirtualScreen vsy vsx) (Cursor cursorY cursorX) buffer mode) filepath = do
  s' <- updateWindow w $ updateScreen s
  render
  ev <- getEvent w Nothing
  case mode of
    Insert  ->  handleKey ev w s' filepath
    Command -> handleCommand ev w s' filepath

handleCommand ∷ Maybe Event → Window → State → FilePath → Curses ()
handleCommand ev w state@(State vs cursor buffer mode) filepath =
  case ev of
    Just (EventCharacter c) | c == 's' -> do
                                          liftIO (save filepath (intercalate "\n" buffer))
                                          mainloop w (State vs cursor buffer Insert) filepath
    Just (EventCharacter c) | c == 'q' -> return ()
    Nothing                            -> mainloop w state filepath

handleKey ∷ Maybe Event → Window → State → FilePath → Curses()
handleKey ev w state@(State vs cursor buffer mode) filepath =
  case ev of
    Just (EventSpecialKey KeyCommand)     -> mainloop w (State vs cursor ("suka" : buffer) Command) filepath
    Just (EventSpecialKey KeyRightArrow)  -> mainloop w (moveRight state) filepath
    Just (EventSpecialKey KeyLeftArrow)   -> mainloop w (moveLeft state) filepath
    Just (EventSpecialKey KeyDownArrow)   -> mainloop w (moveDown state) filepath
    Just (EventSpecialKey KeyUpArrow)     -> mainloop w (moveUp state) filepath
    Just (EventCharacter c) | isPrint c   -> mainloop w (write c state) filepath
    Just (EventCharacter b) | b == '\DEL' -> mainloop w (backspace state) filepath
    Just (EventCharacter e) | e == '\n'   -> mainloop w (newLine state) filepath
    Just ev'                              -> mainloop w state filepath
    Nothing                               -> mainloop w state filepath

save ∷ FilePath → String → IO ()
save = writeFile

updateScreen ∷ State → Update State
updateScreen (State vs@(VirtualScreen vsy vsx) c@(Cursor cy cx) buffer mode) = do
    clear
    (h, w) <- windowSize
    let (newVirtualScreen, Cursor ncy ncx) = updateScreenCoordinates vs c (fromIntegral h) (fromIntegral w)
    forM_ (lines $ fromIntegral h) (\ (number, text) -> do
      moveCursor (fromIntegral (number - vsy)) 0
      drawString text)
    moveCursor (fromIntegral ncy) (fromIntegral ncx)
    return (State newVirtualScreen (Cursor ncy ncx) buffer mode)
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

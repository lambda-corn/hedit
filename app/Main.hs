module Main where

import Control.Monad
import System.IO
import UI.NCurses

import Hedit

-- | Height and width of the physical screen
data Screen = Screen Int Int

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  mainloop w (State (VirtualScreen 0) (Cursor 0 0) [])

mainloop :: Window -> State -> Curses ()
mainloop w (State (VirtualScreen vs) (Cursor cursorY cursorX) buffer) = do
  updateWindow w $ do
    drawBuffer buffer (VirtualScreen vs)
    drawCursor (Cursor cursorY cursorX) (VirtualScreen vs) (Screen 0 0)
  render
  ev <- getEvent w Nothing
  case ev of
    Just ev' | ev' == EventCharacter 'q' -> return ()
    Just ev'                             -> mainloop w (State (VirtualScreen vs) (Cursor cursorY cursorX) buffer)

drawCursor :: Cursor -> VirtualScreen -> Screen -> Update()
drawCursor (Cursor cursorY cursorX) (VirtualScreen vsY) (Screen width height) =
  moveCursor 0 0
  -- | To implement

drawBuffer :: Buffer -> VirtualScreen -> Update()
drawBuffer buffer (VirtualScreen vs) =
  drawString "Buffer content"
  -- | To implement

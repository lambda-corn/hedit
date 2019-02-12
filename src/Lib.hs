module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Buffer = [String]

data State = State { virtualScreenY :: Int
                   , cursorX        :: Int
                   , cursorY        :: Int
                   , buffer         :: Buffer
                   }

write :: Char -> State -> State
write c s = s

backspace :: State -> State
backspace s = s

moveRight :: State -> State
moveRight s = s

moveLeft :: State -> State
moveLeft s = s

moveDown :: State -> State
moveDown s = s

moveUp :: State -> State
moveUp s = s

tab :: State -> State
tab s = s

save :: String -> Buffer -> IO ()
save filename buffer = print "Culo"

load :: String -> IO (Buffer)
load filename = return (["Hello"])

pageUp :: State -> State
pageUp s = s

pageDown :: State -> State
pageDown s = s

main :: IO ()
main = do
  print "Hello World !"
  

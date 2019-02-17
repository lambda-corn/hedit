{-# LANGUAGE UnicodeSyntax #-}

module Hedit
    ( Buffer
    , Cursor(..)
    , State(..)
    , write
    , backspace
    , moveRight
    , moveLeft
    , moveDown
    , moveUp
    , tab
    , save
    , load
    , pageUp
    , pageDown
    ) where

type Buffer = [String]

data Cursor = Cursor Int Int

data State = State Cursor Buffer

write ∷ Char → State → State
write c s = s

backspace ∷ State → State
backspace s = s

moveRight ∷ State → State
moveRight s = s

moveLeft ∷ State → State
moveLeft s = s

moveDown ∷ State → State
moveDown s = s

moveUp ∷ State → State
moveUp s = s

tab ∷ State → State
tab s = s

save ∷ String → Buffer → IO ()
save filename buffer = print "Hello"

load ∷ String → IO Buffer
load filename = return ["Hello"]

pageUp ∷ State → State
pageUp s = s

pageDown ∷ State → State
pageDown s = s


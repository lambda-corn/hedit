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
    , newLine
    ) where

type Buffer = [String]

data Cursor = Cursor Int Int

data State = State Cursor Buffer

write ∷ Char → Int → State → State
write c offset (State (Cursor y x) buffer) =
    State (Cursor y (x + 1)) updatedBuffer
  where
    updatedBuffer = updateAt (y + offset) x writeAt buffer

    writeAt ∷ Int → String → String
    writeAt 0 as     = c:as
    writeAt i (a:as) = a : writeAt (i - 1) as

backspace ∷ Int → State → State
backspace offset (State (Cursor y x) buffer) =
    State (Cursor y (x - 1)) updatedBuffer
  where
    updatedBuffer = updateAt (y + offset) (x - 1) deleteAt buffer

    deleteAt ∷ Int → String → String
    deleteAt 0 (a:as) = as
    deleteAt i (a:as) = a : deleteAt (i - 1) as

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

newLine ∷ Int → State → State
newLine offset (State (Cursor y _) buffer) =
    State (Cursor (y + 1) 0) updatedBuffer
  where
    updatedBuffer = addNewLineAt (y + offset) buffer

    addNewLineAt ∷ Int → Buffer → Buffer
    addNewLineAt 0 (a:as) = a : [] : as
    addNewLineAt i (a:as) = a : addNewLineAt (i - 1) as

updateAt ∷ Int → Int → (Int -> String -> String) -> Buffer → Buffer
updateAt 0 j f (a:as) = f j a : as
updateAt i j f (a:as) = a : updateAt (i - 1) j f as

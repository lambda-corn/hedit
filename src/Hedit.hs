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

write ∷ Char → Int → State → State
write c offset (State (Cursor y x) buffer) =
    State (Cursor y (x + 1)) updatedBuffer
      where
        updatedBuffer = updateAt (y + offset) x buffer

        writeAt ∷ Int → String → String
        writeAt 0 as     = c:as
        writeAt i (a:as) = a : writeAt (i - 1) as

        updateAt ∷ Int → Int → Buffer → Buffer
        updateAt 0 j (a:as) = writeAt j a : as
        updateAt i j (a:as) = a : updateAt (i - 1) j as

backspace ∷ Int → State → State
backspace offset (State (Cursor y x) buffer) =
    State (Cursor y (x - 1)) updatedBuffer
      where
        updatedBuffer = updateAt (y + offset) (x - 1) buffer

        deleteAt ∷ Int → String → String
        deleteAt 0 (a:as) = as
        deleteAt i (a:as) = a : deleteAt (i - 1) as

        updateAt ∷ Int → Int → Buffer → Buffer
        updateAt 0 j (a:as) = deleteAt j a : as
        updateAt i j (a:as) = a : updateAt (i - 1) j as

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

insertAt ∷ Char → Int → String → String
insertAt newElement 0 as     = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

updateAt ∷ Char → Int → Int → Buffer → Buffer
updateAt newElement 0 j (a:as) = insertAt newElement j a : as
updateAt newElement i j (a:as) = a : updateAt newElement (i - 1) j as


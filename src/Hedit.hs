{-# LANGUAGE UnicodeSyntax #-}

module Hedit
    ( Buffer
    , Cursor(..)
    , State(..)
    , VirtualScreen(..)
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

data VirtualScreen = VirtualScreen Int Int

data State = State VirtualScreen Cursor Buffer

write ∷ Char → State → State
write c (State (VirtualScreen vsy vsx) (Cursor cy cx) buffer) =
    State (VirtualScreen vsy vsx) (Cursor cy (cx + 1)) updatedBuffer
  where
    updatedBuffer = updateAt (cy + vsy) cx writeAt buffer

    writeAt ∷ Int → String → String
    writeAt 0 as     = c:as
    writeAt i (a:as) = a : writeAt (i - 1) as

backspace ∷ State → State
backspace (State (VirtualScreen vsy vsx) (Cursor y x) buffer) =
    State (VirtualScreen vsy vsx) (Cursor y (x - 1)) updatedBuffer
  where
    updatedBuffer = updateAt (y + vsy) (x - 1) deleteAt buffer

    deleteAt ∷ Int → String → String
    deleteAt 0 (a:as) = as
    deleteAt i (a:as) = a : deleteAt (i - 1) as

moveRight ∷ State → State
moveRight state@(State (VirtualScreen vsy vsx) (Cursor cy cx) buffer)
    | cx < length (buffer!!(vsy + cy)) = State (VirtualScreen vsy vsx) (Cursor cy (cx + 1)) buffer
    | otherwise                        = state

moveLeft ∷ State → State
moveLeft state@(State (VirtualScreen vsy vsx) (Cursor cy cx) buffer)
    | cx > 0    = State (VirtualScreen vsy vsx) (Cursor cy (cx - 1)) buffer
    | otherwise = state

moveDown ∷ State → State
moveDown state@(State (VirtualScreen vsy vsx) (Cursor cy cx) buffer)
    | cy < length buffer - 1 = let nextLine = buffer!!(vsy + cy + 1)
                               in State (VirtualScreen vsy vsx) (Cursor (cy + 1) (min (length nextLine) cx)) buffer
    | otherwise              = state

moveUp ∷ State → State
moveUp state@(State (VirtualScreen vsy vsx) (Cursor cy cx) buffer)
    | cy > 0    = let previousLine = buffer!!(vsy + cy - 1)
                  in State (VirtualScreen vsy vsx) (Cursor (cy - 1) (min (length previousLine) cx)) buffer
    | otherwise = state

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

newLine ∷ State → State
newLine (State (VirtualScreen vsy vsx) (Cursor cy cx) buffer) =
    State (VirtualScreen vsy vsx) (Cursor (cy + 1) 0) updatedBuffer
  where
    updatedBuffer = addLineAtRow cy cx buffer

    cutLineAtColumn ∷ Int → String → (String, String)
    cutLineAtColumn _ []      = ([], [])
    cutLineAtColumn 0 xs      = ([], xs)
    cutLineAtColumn cx (x:xs) = let (left, right) = cutLineAtColumn (cx - 1) xs
                                in (x : left, right)

    addLineAtRow ∷ Int → Int → Buffer → Buffer
    addLineAtRow _ _ []     = [[]]
    addLineAtRow 0 x (l:ls) = let (left, right) = cutLineAtColumn x l
                              in left : right : ls
    addLineAtRow y x (l:ls) = l : addLineAtRow (y - 1) x ls

updateAt ∷ Int → Int → (Int → String → String) → Buffer → Buffer
updateAt 0 j f (a:as) = f j a : as
updateAt i j f (a:as) = a : updateAt (i - 1) j f as

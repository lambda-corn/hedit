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
    , pageUp
    , pageDown
    , newLine
    ) where

type Buffer = [String]

data Cursor = Cursor Int Int deriving (Eq, Show)

data VirtualScreen = VirtualScreen Int Int deriving (Eq, Show)

data State = State VirtualScreen Cursor Buffer deriving (Eq, Show)

write ∷ Char → State → State
write c (State (VirtualScreen vsy vsx) (Cursor cy cx) buffer) =
    State (VirtualScreen vsy vsx) (Cursor cy (cx + 1)) updatedBuffer
  where
    updatedBuffer = updateAt (cy + vsy) cx writeAt buffer

    writeAt ∷ Int → String → String
    writeAt 0 as     = c:as
    writeAt i (a:as) = a : writeAt (i - 1) as

backspace ∷ State → State
backspace (State (VirtualScreen vsy vsx) cursor@(Cursor y x) buffer) =
    State (VirtualScreen vsy vsx) updatedCursor updatedBuffer
  where
    updatedCursor = updateCursor cursor buffer
    updatedBuffer = backspaceAt y x buffer

    deleteAt ∷ Int → String → String
    deleteAt 0 (a:as) = as
    deleteAt i (a:as) = a : deleteAt (i - 1) as

    backspaceAt ∷ Int → Int → Buffer → Buffer
    backspaceAt y x buffer
      | x > 0  = updateAt (y + vsy) (x - 1) deleteAt buffer
      | x == 0 = deleteLine (y + vsy) buffer

    deleteLine ∷ Int → Buffer → Buffer
    deleteLine row (p:l:ls)
      | row == 0 = p : l : ls
      | row > 1  = p : deleteLine (row - 1) (l : ls)
      | row == 1 = (p ++ l) : ls

    updateCursor ∷ Cursor → Buffer → Cursor
    updateCursor (Cursor y x) buffer
      | y == 0 && x == 0 = Cursor y x
      | x > 0            = Cursor y (x - 1)
      | x == 0           = let y' = max (y - 1) 0
                           in Cursor y' (length (buffer!!y'))

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
    | vsy + cy < length buffer - 1 = let nextLine = buffer!!(vsy + cy + 1)
                                      in State (VirtualScreen vsy vsx) (Cursor (cy + 1) (min (length nextLine) cx)) buffer
    | otherwise                    = state

moveUp ∷ State → State
moveUp state@(State (VirtualScreen vsy vsx) (Cursor cy cx) buffer)
    | cy == 0 && vsy == 0 = state
    | cy >= 0             = let previousLine = buffer!!(vsy + cy - 1)
                            in State (VirtualScreen vsy vsx) (Cursor (cy - 1) (min (length previousLine) cx)) buffer
    | otherwise           = state

tab ∷ State → State
tab s = s

pageUp ∷ State → State
pageUp s = s

pageDown ∷ State → State
pageDown s = s

newLine ∷ State → State
newLine (State (VirtualScreen vsy vsx) (Cursor cy cx) buffer) =
    State (VirtualScreen vsy vsx) (Cursor (cy + 1) 0) updatedBuffer
  where
    updatedBuffer = addLineAtRow (vsy + cy) (vsx + cx) buffer

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


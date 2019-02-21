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
    | cy < length buffer = let nextLine = buffer!!(vsy + cy + 1) 
                           in State (VirtualScreen vsy vsx) (Cursor (cy + 1) (min (length nextLine) cx)) buffer
    | otherwise          = state

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
    updatedBuffer = addNewLineAt cx (cy + vsy) buffer

    -- addNewLineAt ∷ Int → String -> Buffer → Buffer
    -- addNewLineAt 0 s (a:as) = a : s : as
    -- addNewLineAt i s (a:as) = a : addNewLineAt (i - 1) as

    addNewLineAt :: Int -> Int -> Buffer -> Buffer
    addNewLineAt 0 cx (l:ls) = let (left, right) = cutLineAt cx l
                              in (left : right : ls)
    addNewLineAt y cx (l:ls) = l : addNewLineAt (y - 1) cx ls

    cutLineAt :: Int -> String -> (String, String)
    cutLineAt 0 (x:xs) = ([x], xs)
    cutLineAt cx (x:xs) = let (left, right) = cutLineAt (cx - 1) xs
                          in (x:left, right)

updateAt ∷ Int → Int → (Int -> String -> String) -> Buffer → Buffer
updateAt 0 j f (a:as) = f j a : as
updateAt i j f (a:as) = a : updateAt (i - 1) j f as

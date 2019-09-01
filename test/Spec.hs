{-# LANGUAGE UnicodeSyntax #-}
import           Test.Hspec

import           Hedit

main ∷ IO ()
main = hspec $ do
  describe "Hedit.write" $ do
    it "writes first character to buffer" $
      write 'a' (State (VirtualScreen 0 0) (Cursor 0 0) [[]] Insert) `shouldBe` State (VirtualScreen 0 0) (Cursor 0 1) [['a']] Insert

    it "writes any character to buffer" $
      write 'b' (State (VirtualScreen 0 0) (Cursor 0 1) [['a']] Insert) `shouldBe` State (VirtualScreen 0 0) (Cursor 0 2) ["ab"] Insert

    it "writes first character on the second line of buffer" $
       write 'c' (State (VirtualScreen 0 0) (Cursor 1 0) ["ab", ""] Insert) `shouldBe` State (VirtualScreen 0 0) (Cursor 1 1) ["ab", "c"] Insert

  describe "Hedit.backspace" $ do
    it "deletes last character in a line with a single character" $
      backspace (State (VirtualScreen 0 0) (Cursor 0 1) ["a"] Insert) `shouldBe` State (VirtualScreen 0 0) (Cursor 0 0) [[]] Insert

    it "deletes last character in a line with more than one character" $
      backspace (State (VirtualScreen 0 0) (Cursor 0 2) ["ab"] Insert) `shouldBe` State (VirtualScreen 0 0) (Cursor 0 1) ["a"] Insert

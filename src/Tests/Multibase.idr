
module Tests.Multibase

import Data.Multibase
import Specdris.Spec
import Data.Multibase.Char
import Data.Vect

tests : IO ()
tests = spec $ do 
  describe "Successful parsing tests" $ do
    it "should decode base 1" $ do
      decodeMulti "1000000" `shouldBe` 
      Right (1 ** MkMultibaseDigest SBase1 [0, 0, 0, 0, 0, 0])

  describe "Multibase encoding" $ do
    it "should encode in base 2" $ do
      encode "yes mani !" SBase2 `shouldBe` "001111001011001010111001100100000011011010110000101101110011010010010000000100001"
    it "should encode in base 8" $ do
      encode "yes mani !" SBase8 `shouldBe` "7171312714403326055632220041"
    it "should encode in base 10" $ do
      encode "yes mani !" SBase10 `shouldBe` "9573277761329450583662625"
    it "should encode in base 16" $ do
      encode "yes mani !" SBase16 `shouldBe` "f796573206d616e692021"
    it "should encode in base 32" $ do
      encode "yes mani !" SBase32Hex `shouldBe` "vf5in683dc5n6i811"
    it "should encode in base 58btc" $ do
      encode "yes mani !" SBase58btc `shouldBe` "z7paNL19xttacUY"
    it "should encode in base 64" $ do
      encode "yes mani !" SBase58btc `shouldBe` "meWVzIG1hbmkgIQ"

  describe "Multibase decoding" $ do
    it "should decode in base 2" $ do
      decodeStr "001111001011001010111001100100000011011010110000101101110011010010010000000100001" `shouldBe` Right "yes mani !"
    it "should decode in base 8" $ do
      decodeStr "7171312714403326055632220041" `shouldBe` Right "yes mani !"
    it "should decode in base 10" $ do
      decodeStr "9573277761329450583662625" `shouldBe` Right "yes mani !"
    it "should decode in base 16" $ do
      decodeStr "f796573206d616e692021" `shouldBe` Right "yes mani !"
    it "should decode in base 32" $ do
      decodeStr "vf5in683dc5n6i811" `shouldBe` Right "yes mani !"
    it "should decode in base 58" $ do
      decodeStr "z7paNL19xttacUY" `shouldBe` Right "yes mani !"
    it "should decode in base 64" $ do
      decodeStr "meWVzIG1hbmkgIQ" `shouldBe` Right "yes mani !"

  describe "Two way coding" $ do
    it "should encode and decode base 2" $ do
      decodeStr (encode "abcdefg" SBase2) `shouldBe` Right "abcdefg"
    it "should encode and decode base 8" $ do
      decodeStr (encode "abcdefg" SBase8) `shouldBe` Right "abcdefg"
    it "should encode and decode base 10" $ do
      decodeStr (encode "abcdefg" SBase10) `shouldBe` Right "abcdefg"
    it "should encode and decode base 16" $ do
      decodeStr (encode "abcdefg" SBase16) `shouldBe` Right "abcdefg"
    it "should encode and decode base 32" $ do
      decodeStr (encode "abcdefg" SBase32Hex) `shouldBe` Right "abcdefg"
    it "should encode and decode base 58" $ do
      decodeStr (encode "abcdefg" SBase58btc) `shouldBe` Right "abcdefg"
    it "should encode and decode base 64" $ do
      decodeStr (encode "abcdefg" SBase64) `shouldBe` Right "abcdefg"

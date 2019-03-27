
module Tests.Multibase

import Data.Multibase
import Specdris.Spec
import Data.Vect

tests : IO ()
tests = spec $ do 

  describe "Multibase encoding" $ do
    it "should encode in base 2" $ do
      encode SBase2 "yes mani !" `shouldBe` "001111001011001010111001100100000011011010110000101101110011010010010000000100001"
    it "should encode in base 8" $ do
      encode SBase8 "yes mani !" `shouldBe` "7171312714403326055632220041"
    it "should encode in base 10" $ do
      encode SBase10 "yes mani !" `shouldBe` "9573277761329450583662625"
    it "should encode in base 16" $ do
      encode SBase16 "yes mani !" `shouldBe` "f796573206d616e692021"
    it "should encode in base 32" $ do
      encode SBase32Hex "yes mani !" `shouldBe` "vf5in683dc5n6i811"
    it "should encode in base 58btc" $ do
      encode SBase58btc "yes mani !" `shouldBe` "z7paNL19xttacUY"
    it "should encode in base 64" $ do
      encode SBase58btc "yes mani !" `shouldBe` "meWVzIG1hbmkgIQ"

  describe "Multibase decoding" $ do
    it "should decode in base 2" $ do
      decode "001111001011001010111001100100000011011010110000101101110011010010010000000100001" `shouldBe` Right "yes mani !"
    it "should decode in base 8" $ do
      decode "7171312714403326055632220041" `shouldBe` Right "yes mani !"
    it "should decode in base 10" $ do
      decode "9573277761329450583662625" `shouldBe` Right "yes mani !"
    it "should decode in base 16" $ do
      decode "f796573206d616e692021" `shouldBe` Right "yes mani !"
    it "should decode in base 32" $ do
      decode "vf5in683dc5n6i811" `shouldBe` Right "yes mani !"
    it "should decode in base 58" $ do
      decode "z7paNL19xttacUY" `shouldBe` Right "yes mani !"
    it "should decode in base 64" $ do
      decode "meWVzIG1hbmkgIQ" `shouldBe` Right "yes mani !"

  describe "Two way coding" $ do
    it "should encode and decode base 2" $ do
      decode (encode SBase2 "abcdefg") `shouldBe` Right "abcdefg"
    it "should encode and decode base 8" $ do
      decode (encode SBase8 "abcdefg") `shouldBe` Right "abcdefg"
    it "should encode and decode base 10" $ do
      decode (encode SBase10 "abcdefg") `shouldBe` Right "abcdefg"
    it "should encode and decode base 16" $ do
      decode (encode SBase16 "abcdefg") `shouldBe` Right "abcdefg"
    it "should encode and decode base 32" $ do
      decode (encode SBase32Hex "abcdefg") `shouldBe` Right "abcdefg"
    it "should encode and decode base 58" $ do
      decode (encode SBase58btc "abcdefg") `shouldBe` Right "abcdefg"
    it "should encode and decode base 64" $ do
      decode (encode SBase64 "abcdefg") `shouldBe` Right "abcdefg"

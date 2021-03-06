
module Data.Multibase.Convert

import Data.Vect
import Data.Nat.DivMod

%access export
%default total 

mkFin : {n, m : Nat} -> Fin (S (n + m))
mkFin {n} {m=Z}   = rewrite plusZeroRightNeutral n in last {n}
mkFin {n} {m=S k} = rewrite sym $ plusSuccRightSucc n k in weaken mkFin

finSuccPlusRight : Fin (S (n + m)) -> Fin (n + (S m))
finSuccPlusRight x {n} {m} = rewrite sym $ plusSuccRightSucc n m in x

lteEqPlus : {a, b : Nat} -> a `LTE` b -> b = a + (b - a)
lteEqPlus LTEZero {b} = rewrite minusZeroRight b in Refl
lteEqPlus (LTESucc x) = cong $ lteEqPlus x

natLteFin : (n : Nat) -> LT n (S m) -> Fin (S m)
natLteFin      Z    lte           = FZ
natLteFin {m} (S k) (LTESucc lte) = rewrite lteEqPlus lte in mkFin {n=S k} {m=minus m (S k)}

||| Convert a natural number into the selected base (actually the predecessor of the base). The output
||| is a list of natural numbers smaller than the base which acts as an upper bound. LSB
convertBaseNat : (predBase : Nat) -> Nat -> List (Fin (S predBase))
convertBaseNat predBase n = convertBaseList predBase n []
  where 
    convertBaseList : (predBase : Nat) -> Nat -> List (Fin (S predBase)) -> List (Fin (S predBase))
    convertBaseList predBase num acc with (num `divMod` predBase)
      convertBaseList predBase (remainder + (    Z * (S predBase))) acc | (MkDivMod     Z remainder remainderSmall) = 
        (natLteFin remainder remainderSmall) :: acc
      convertBaseList predBase (remainder + ((S q) * (S predBase))) acc | (MkDivMod (S q) remainder remainderSmall) = 
        -- This assert_total is a bit frustrating, we can see that  q is smaller than r + q * b but idris can't figure it out
        assert_total $ convertBaseList predBase (S q) ((natLteFin remainder remainderSmall) :: acc)

padWithZero : Nat -> List (Fin (S n)) -> List (Fin (S n))
padWithZero k xs with (k `minus` length xs)
  | Z = xs
  | S n = replicate (S n) FZ ++ xs

||| Convert a Nat without base into a list of digits in the selected base
||| The predecessor of the base has to be passed in argument since `0` is not a valid base
unaryToBase : (predBase : Nat) -> List Nat -> List (Fin (S predBase))
unaryToBase predBase xs = integerToBase (charsToInteger xs) []
  where
  charsToInteger : List Nat -> Integer
  charsToInteger = foldl (\acc, char => acc*256 + toIntegerNat char) 0
  base : Integer
  base = toIntegerNat predBase + 1
  integerToBase : Integer -> List (Fin (S predBase)) -> List (Fin (S predBase))
  integerToBase 0 acc = acc
  integerToBase n acc = assert_total $
                        integerToBase (n `div` base) (convertBaseNat predBase (fromIntegerNat $ n `mod` base) ++ acc)

||| Convert a string into a list of Nat each representing a number between 0 and 255
stringToBase256 : String -> List Nat
stringToBase256 = map (toNat . ord) . unpack

base256ToString : List Nat -> String
base256ToString = pack . map (chr . toIntNat)

||| This in effect, transforms a list of numbers in a base into a unary base (Nat)
listBaseToNat : List (Fin base) -> Nat
listBaseToNat ls = listBaseToNatHelper ls 0 0
  where
    listBaseToNatHelper : List (Fin base) -> Nat -> Nat -> Nat
    listBaseToNatHelper [] index acc = acc
    listBaseToNatHelper (y :: xs) index acc {base} = 
      listBaseToNatHelper xs (S index) (acc + ((finToNat y) * (base `power` index)))

decodeFromBase : List (Fin b) -> String
decodeFromBase xs {b} = base256ToString $ integerToChars [] $ baseToInteger xs
  where
    baseInt : Integer
    baseInt = toIntegerNat b
    baseToInteger : List (Fin b) -> Integer
    baseToInteger = foldl (\acc, v => acc * baseInt + toIntegerNat (finToNat v)) 0
    integerToChars : List Nat -> Integer -> List Nat
    integerToChars acc 0 = acc
    integerToChars acc i = assert_total $
                           integerToChars (fromInteger (i `mod` 256) :: acc) (i `div` 256)

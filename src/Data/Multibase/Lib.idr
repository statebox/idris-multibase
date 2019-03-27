module Data.Multibase.Lib

import Data.Fin
import Data.Vect
import Data.Multibase.Types

%default total

dictionary : BaseSymbol b -> Vect b Char
dictionary SBase1     = fromList $ unpack "0"
dictionary SBase2     = fromList $ unpack "01"
dictionary SBase8     = fromList $ unpack "01234567"
dictionary SBase10    = fromList $ unpack "0123456789"
dictionary SBase16    = fromList $ unpack "0123456789abcdef"
dictionary SBase32Hex = fromList $ unpack "0123456789abcdefghijklmnopqrstuv"
dictionary SBase58btc = fromList $ unpack "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
dictionary SBase64    = fromList $ unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

parseBaseChar : Char -> Either (MultibaseError Char) (n ** (BaseSymbol n))
parseBaseChar '1' = Right (1 ** SBase1)
parseBaseChar '0' = Right (2 ** SBase2)
parseBaseChar '7' = Right (8 ** SBase8)
parseBaseChar '9' = Right (10 ** SBase10)
parseBaseChar 'f' = Right (16 ** SBase16)
parseBaseChar 'v' = Right (32 ** SBase32Hex)
parseBaseChar 'z' = Right (58 ** SBase58btc)
parseBaseChar 'm' = Right (64 ** SBase64)
parseBaseChar c   = Left (UnknownBase c)

baseChar : BaseSymbol b -> Char
baseChar SBase1     = '1'
baseChar SBase2     = '0'
baseChar SBase8     = '7'
baseChar SBase10    = '9'
baseChar SBase16    = 'f'
baseChar SBase32Hex = 'v'
baseChar SBase58btc = 'z'
baseChar SBase64    = 'm'

export
toString : BaseSymbol b -> List (Fin b) -> String
toString base vals = pack $ (baseChar base) :: map (decodeChar base) vals
  where decodeChar : BaseSymbol b -> Fin b -> Char
        decodeChar base val = index val $ dictionary base

||| Record that holds the digest and the meta-data about it
||| It is indexed by the length of the digest and the size of the base.
||| This representation is agnostic from the original encoding, that is
||| there could be multiple encodings that represent 64bits bases but
||| they will have the same `MultibaseDigest`
public export record MultibaseDigest (n : Nat) where
  constructor MkMultibaseDigest
  base   : BaseSymbol n
  digest : List (Fin n)

||| This probably should be in STD-lib
export implementation (DecEq a, {y : a} -> Eq (p y)) => Eq (DPair a p) where
   (x ** pf) == (y ** pf') with (decEq x y)
     (x ** pf) == (x ** pf') | Yes Refl = pf == pf'
     (x ** pf) == (y ** pf') | No contra = False

export
Eq (MultibaseDigest b) where
  (MkMultibaseDigest b1 d1) == (MkMultibaseDigest b2 d2) = b1 == b2 && d1 == d2

export
Show (Fin b) where
  show = show . finToInteger

export
Show (MultibaseDigest b) where
  show (MkMultibaseDigest base digest) = show base ++ ":" ++ show digest

||| Interface for values which can be converted to Nat
||| The Nat representation is used to parse the symbol as a digit in 
||| any arbitrary base
interface ParsableSymbol a where
   symbolToNat : BaseSymbol b -> a -> Either (MultibaseError a) Nat
   parseBase : a -> Either (MultibaseError a) (n ** BaseSymbol n)

||| Given a base and a vector of nat check if any nat go out of bound of the specified base
parseDigest : (b : BaseSymbol n) -> Vect l Nat -> Either (MultibaseError a) (MultibaseDigest n)
parseDigest b [] {n = n} = Right (MkMultibaseDigest b [])
parseDigest b (x :: xs) {n = n} = do fin <- maybe (Left (OutOfRangeSymbol x)) Right $ natToFin x n
                                     MkMultibaseDigest b fs <- parseDigest b xs
                                     pure (MkMultibaseDigest b (fin :: fs))

||| Given a base and a vector of symbols check if all symbols are correctly encoded in the base
parse : ParsableSymbol sym => (b : BaseSymbol n) -> Vect l sym -> Either (MultibaseError sym) (MultibaseDigest n)
parse b digest = do symbols <- traverse (symbolToNat b) digest
                    parseDigest b symbols

||| Given a list of parsable symbols return the digest indexed by its length and base number
parseSymbols : ParsableSymbol s => List s -> Either (MultibaseError s) (b ** (MultibaseDigest b))
parseSymbols [] = Left DigestEmpty
parseSymbols (x :: xs) with (parseBase x) 
  | (Left y) = Left y
  | (Right (n ** base)) = let vect = fromList xs in
                              case parse base vect of
                                   Left err => Left err
                                   Right parsed => Right (n ** parsed)

ParsableSymbol Char where
  symbolToNat base char = let dict = dictionary base 
                              index = char `elemIndex` dict in 
                              maybe (Left (IllegalSymbolFound char)) (Right . finToNat) index
  parseBase = parseBaseChar

export
decodeMulti : String -> Either (MultibaseError Char) (b ** (MultibaseDigest b))
decodeMulti = parseSymbols . unpack

reencode : MultibaseDigest b -> String
reencode (MkMultibaseDigest base digest) = pack $ map (`index` dictionary base) digest

groupBy : Nat -> List a -> List (List a)
groupBy len [] = []
groupBy len xs = if length xs < len then [xs] else let (head, tail) = splitAt len xs in assert_total $ head :: groupBy len tail

fromBaseToNat : List (Fin b) -> Nat
fromBaseToNat xs {b} = snd $ foldl (\(index, sum), val => (S index, val * (b `power` index) + sum)) (Z, Z) $ map finToNat $ reverse xs

export
baseLength : Nat -> Nat
baseLength b = toNat $ the Int $ cast (ceiling (log 256 / log (cast b)))

export
decodeFromBase : List (Fin b) -> String
decodeFromBase xs {b} = let digitLength = baseLength b 
                            digitList = groupBy digitLength xs 
                            natList = map fromBaseToNat digitList in
                            pack $ map (chr . toIntNat) natList

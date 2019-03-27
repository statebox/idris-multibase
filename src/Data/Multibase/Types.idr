module Data.Multibase.Types

export
data MultibaseError char = UnknownBase char 
                         | IllegalSymbolFound char 
                         | DigestEmpty 
                         | OutOfRangeSymbol Nat

export
Show c => Show (MultibaseError c) where
  show (UnknownBase c) = "UnknownBase: " ++ show c
  show (IllegalSymbolFound s) = "IllegalSymbolFound: " ++ show s
  show DigestEmpty = "DigestEmpty"
  show (OutOfRangeSymbol n) = "OutOfRangeSymbol: " ++ show n

export
Eq c => Eq (MultibaseError c) where
  (UnknownBase c)        == (UnknownBase c')        = c == c'
  (IllegalSymbolFound s) == (IllegalSymbolFound s') = s == s'
  DigestEmpty            == DigestEmpty             = True 
  (OutOfRangeSymbol n)   == (OutOfRangeSymbol n')   = n == n'
  _ == _ = False

public export
data BaseSymbol : Nat -> Type where
  SBase1     : BaseSymbol 1
  SBase2     : BaseSymbol 2
  SBase8     : BaseSymbol 8
  SBase10    : BaseSymbol 10
  SBase16    : BaseSymbol 16
  SBase32Hex : BaseSymbol 32
  SBase58btc : BaseSymbol 58
  SBase64    : BaseSymbol 64

export
Uninhabited (BaseSymbol Z) where
  uninhabited SBase1 impossible
  uninhabited SBase2 impossible
  uninhabited SBase8 impossible
  uninhabited SBase10 impossible
  uninhabited SBase16 impossible
  uninhabited SBase32Hex impossible
  uninhabited SBase58btc impossible
  uninhabited SBase64 impossible

export
Show (BaseSymbol n) where
  show SBase1     = "1"
  show SBase2     = "binary"
  show SBase8     = "octal"
  show SBase10    = "decimal"
  show SBase16    = "hexadecimal"
  show SBase32Hex = "32bitshex"
  show SBase58btc = "58bits"
  show SBase64    = "64bits"

export
Eq (BaseSymbol n) where
  SBase1     == SBase1     = True 
  SBase2     == SBase2     = True
  SBase8     == SBase8     = True
  SBase10    == SBase10    = True
  SBase16    == SBase16    = True
  SBase32Hex == SBase32Hex = True
  SBase58btc == SBase58btc = True
  SBase64    == SBase64    = True
  _ == _ = False


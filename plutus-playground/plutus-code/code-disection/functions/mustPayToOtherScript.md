
[](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/src/Ledger.Constraints.TxConstraints.html#MustPayToOtherScript)
```haskell
{-# INLINABLE mustPayToOtherScript #-}
-- | @mustPayToOtherScript vh d v@ locks the value @v@ with the given script
-- hash @vh@ alonside a datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a script
-- output with @vh@, @d@ and @v@ and adds @d@ in the transaction's datum
-- witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the script transaction output with
-- @vh@, @d@ and @v@ is part of the transaction's outputs.
mustPayToOtherScript :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o
mustPayToOtherScript vh dv vl =
    singleton (MustPayToOtherScript vh dv vl)
    <> singleton (MustIncludeDatum dv)
```


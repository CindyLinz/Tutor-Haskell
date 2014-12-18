{-# LANGUAGE GADTs, ScopedTypeVariables, UnboxedTuples, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric, FlexibleContexts, FlexibleInstances, ConstrainedClassMethods, MultiParamTypeClasses, FunctionalDependencies, MagicHash, ExistentialQuantification, UnicodeSyntax, PostfixOperators, PatternGuards, LiberalTypeSynonyms, RankNTypes, TypeOperators, ExplicitNamespaces, RecursiveDo, ParallelListComp, EmptyDataDecls, KindSignatures, GeneralizedNewtypeDeriving #-}
-- -f glasgow-exts

data MyBool :: * where
  MyFalse :: MyBool
  MyTrue :: MyBool

main :: IO ()
main = putStrLn "Hi"

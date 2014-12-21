class: center, middle, inverse

# Tutor of Haskell - syntax
## CindyLinz

2014.12.24

---

layout: false
class: center, middle

# 一個 Haskell 程式檔的長相

---

layout: true

.header[一個 Haskell 程式檔的完整長相]

---

layout: false

class: center, middle

# (我覺得) Z > B 的 extensions

---

layout: true

.header[(我覺得) Z > B 的 extensions]

---

```haskell
{- LANGUAGE
  ScopedTypeVariables, UnboxedTuples, TypeSynonymInstances
, StandaloneDeriving, DeriveDataTypeable, DeriveFunctor
, DeriveFoldable, DeriveTraversable, DeriveGeneric
, FlexibleContexts, FlexibleInstances, ConstrainedClassMethods
, MultiParamTypeClasses, FunctionalDependencies, MagicHash
, ExistentialQuantification, UnicodeSyntax, PostfixOperators
, PatternGuards, LiberalTypeSynonyms, RankNTypes, TypeOperators
, ExplicitNamespaces, RecursiveDo, ParallelListComp
, EmptyDataDecls, KindSignatures, GeneralizedNewtypeDeriving
, LambdaCase, EmptyCase, MultiWayIf, NamedFieldPuns
, RecordWildCards, OverloadedStrings, TupleSections
, OverloadedLists, ViewPatterns, TransformListComp
, MonadComprehensions -}
```

--

+ `{- ... -}` 是 Haskell 的 block comment, 裡面可以放 pragma,
  其中 `LANGUAGE` 用來開 extension

--

+ extension 的啟動以檔案為單位, 一般習慣寫在檔案開頭.

--

+ 通常不會一次用足這麼多 extension,
  不過為了教學順暢一點, 就假設這些都有開了..

--

+ 可以找時間一個一個去讀上面列出的 GHC extension 說明, 當成學習目錄來用<br>
  (我自己學 Haskell 有蠻大比例的內容是這樣學的..)

--

+ 有的大 extension 會釣出不少篇論文.....||

---

layout: false

class: center, middle

# 定義資料型態

---

layout: true

.header[定義資料型態]

---

```haskell
data Bool :: * where
  False :: Bool
  True :: Bool
```

--

```haskell
data Maybe :: * -> * where
  Nothing :: Maybe a
  Just :: a -> Maybe a
```

--

```haskell
data Either :: * -> * -> * where
  Left :: a -> Either a b
  Right :: b -> Either a b
  -- Right :: a -> Either b a 也可以
```

--

```haskell
data List :: * -> * where
  Nil :: List a
  Cons :: a -> List a -> List a
```

--

```haskell
data BinaryTree :: * -> * where
  Leave :: a -> BinaryTree a
  Branch :: BinaryTree a -> a -> BinaryTree a -> BinaryTree a
```

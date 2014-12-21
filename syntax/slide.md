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

# Type Signature 寫法

---

layout: true

.header[Type Signature 寫法]

---

## 單型 (有這樣講的嗎?)

+ 某 `a` 的 type 是 `Int`
  ```haskell
  a :: Int
  ```

--

+ 某 `f` 的 type 是吃一個 `Int` 吐一個 `Int` 的函數
  ```haskell
  f :: Int -> Int
  ```

--

+ 某 `g` 的 type 是吃一個 `Int`, 再吃一個 `Int`, 吐一個 `Int` 的函數
  ```haskell
  g :: Int -> Int -> Int
  ```

--

+ 某 `h` 的 type 是吃一個 `Int`, 吐一個.... 要吃一個 `Int`, 吐一個 `Int` 的函數
  ```haskell
  h :: Int -> Int -> Int
  ```
  其實跟 `g` 是一樣的, 不同的是我們看待它們的角度

---

## 多型

+ 某 `b` 的 type 是.. 任意的 `a`
  ```haskell
  b :: a
  ```

--

+ 某 `k` 的 type 是.. 吃一個任意的 `a` 吐一個任意的 `b`
  ```haskell
  k :: a -> b
  ```

--

+ 某 `m` 的 type 是.. 吃一個任意的 `a`, 再吃一個也是 `a` 的東西, 吐一個 `a`
  ```haskell
  m :: a -> a -> a
  ```
  m 的兩個參數和吐出來的東西都是任意 type, 但得是同一種 type.

--

+ 某 `cmp` 的 type 是.. 吃一個符合 `Eq` class (有實作 `a == a` 和 `a /= a`) 的任意 type `a`, 再吃一個 `a`, 吐一個 `Int`
  ```haskell
  cmp :: Eq a => a -> a -> Int
  cmp a b = if a == b then 1 else 0
  ```

--

  + 這邊寫 a 的位置可以寫任意小寫開頭的字; 它和 expression 裡面用到的東西是在不同的 name scope, 所以可以混用一樣的名字 (如果不會因此閱讀困難的話)

--

  + 這裡的多型比較類似 C++ 的泛型 template, 或 Java 的 generics; 而不是物件導向裡的多型

---

layout: false

# 物件導向多型與泛型多型的差異

---

layout: true

.header[物件導向多型與泛型多型的差異]

---

  + (以 Java 為例) 物件多型的 method return type 如果是 Object, 我們呼叫它以後會拿到其中一種 Object;
    呼叫端拿到 return 以後要想辦法 runtime 處理這有可能是任何一種 type 的物件.
    我們作一個呼叫的動作時, compiler 不需要去解析出
  + Haskell 函數如果會吐出任意 type a, 它能吐出任何一種呼叫端要求的 type;

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

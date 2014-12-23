class: center, middle, inverse

# Tutor of Haskell - syntax
## CindyLinz

2014.12.24

---

layout: false
class: center, middle

# 一個 Haskell 程式檔的完整長相

---

layout: true

.header[一個 Haskell 程式檔的完整長相]

---

```haskell
{-# LANGUAGE OverloadedStrings, LambdaCases #-}
-- 這邊開啟一些這一個檔案想用的 extension

module Test
  ( func1
  , func2
  , Data1(Con1, Con2)
  )
  -- 這邊整串括號裡面的是要 export 的 public member

  -- 然後是一串要 import 進來用的東西
import Control.Monad
import qualified Data.Map as M

  -- 最後是一堆 top-level member declaration
main = undefined
```

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
, LambdaCases, EmptyCase, MultiWayIf, NamedFieldPuns
, RecordWildCards, OverloadedStrings, TupleSections
, OverloadedLists, ViewPatterns, TransformListComp
, MonadComprehensions -}
```

--

 0. `{- ... -}` 是 Haskell 的 block comment, 裡面可以放 pragma,
    其中 `LANGUAGE` 用來開 extension

--
 0. extension 的啟動以檔案為單位, 一般習慣寫在檔案開頭.

--
 0. 通常不會一次用足這麼多 extension,
    不過為了教學順暢一點, 就假設這些都有開了..

--
 0. 可以找時間一個一個去讀上面列出的 GHC extension 說明, 當成學習目錄來用<br>
    (我自己學 Haskell 有蠻大比例的內容是這樣學的..)

--
 0. 有的大 extension 會釣出不少篇論文.....||

---

layout: false

class: center, middle

# Type Signature 寫法

---

layout: true

.header[Type Signature 寫法]

---

## 單型 (有這樣講的嗎?)

 0. 某 `a` 的 type 是 `Int`
    ```haskell
    a :: Int
    ```

--
 0. 某 `f` 的 type 是吃一個 `Int` 吐一個 `Int` 的函數
    ```haskell
    f :: Int -> Int
    ```

--
 0. 某 `g` 的 type 是吃一個 `Int`, 再吃一個 `Int`, 吐一個 `Int` 的函數
    ```haskell
    g :: Int -> Int -> Int
    ```

--
 0. 某 `h` 的 type 是吃一個 `Int`, 吐一個.... 要吃一個 `Int`, 吐一個 `Int` 的函數
    ```haskell
    h :: Int -> Int -> Int
    ```
    其實跟 `g` 是一樣的, 不同的是我們看待它們的角度

---

## 多型

 0. 某 `b` 的 type 是.. 任意的 `a`
    ```haskell
    b :: a
    ```
     0. 這邊寫 a 的位置可以寫任意小寫開頭的字; 它和 expression 裡面用到的東西是在不同的 name scope,
        所以可以混用一樣的名字 (也可以寫成 `b :: b`, 如果不會因此閱讀困難的話)
     0. identifier 和 `::` 之間可以不用留空白, 不過習慣會留.

--
 0. 某 `k` 的 type 是.. 吃一個任意的 `a` 吐一個任意的 `b`
    ```haskell
    k :: a -> b
    ```

--
 0. 某 `m` 的 type 是.. 吃一個任意的 `a`, 再吃一個也是 `a` 的東西, 吐一個 `a`
    ```haskell
    m :: a -> a -> a
    ```
    m 的兩個參數和吐出來的東西都是任意 type, 但得是同一種 type.

---
 0. 某 `cmp` 的 type 是.. 吃一個符合 `Eq` class (有實作 `a == a` 和 `a /= a`) 的任意 type `a`, 再吃一個 `a`, 吐一個 `Int`
    ```haskell
    cmp :: Eq a => a -> a -> Int
    cmp a b = if a == b then 1 else 0
    ```

--
 0. 如果需要的話, 標 `Eq` 的地方也可以放好幾個 class, 描述好幾個, 用括號裝起來;
    另外, 有的 class 是一次描述好幾個參與的 type (這個以後再講實例好了).
    ```haskell
    cmp2 :: (Eq a, Show a) => a -> a -> (Int, String)
    cmp2 a b =
      if a == b
      then (1, show a ++ " is equal to " ++ show b)
      else (0, show a ++ " is not equal to " ++ show b)
    ```

--
 0. 這裡的多型比較類似 C++ 的泛型 template, 或 Java 的 generics;
    而不是物件導向裡的多型

---

layout: false

class: center, middle

.header[[插播]]

# 物件導向多型
與
# 泛型多型
之
# 異同

---

layout: true

.header[[插播] 物件導向多型與泛型多型之異同]

---

 0. 參數部分是差不多的:
     0. Java 的一個 `Object` type 參數表示呼叫時可以給它任意一個 type, 這個 method 會想辦法處理這個來路不明的 type

     0. Haskell 的一個 `a` type 參數也表示呼叫時可以給它任意一個 type, 這個函數都可以處理

--
     0. 如果 Java 的 method 要吃一個 `Cup` type 的物件當參數, 那我們可以餵給它任何一個 `Cup` 物件或是 `Cup` 的 sub class 的物件

     0. 如果 Haskell 的 function 要吃一個 `Eq a => a` 當參數, 我們可以餵給它任何一個有實作 `Eq` 的 type 的 value

--
     0. 不過 Java 會在 run time dynamic 處理, Runtime 取得它真正的類型資訊

     0. Haskell 是在 compile time static 處理, compile 的時候 GHC 要決定好每一次呼叫所傳進去的是什麼 type<br>
        (dynamic 在 Haskell 是用別的作法, 會用到一個叫作 `forall` 的關鍵字)

--
     0. Haskell 可以描述兩個都是任意 type 的參數必須任意為同一種 type 或是可同可不同; Java 沒有這樣的描述方式

---
 0. return type 的部分方向是相反的:
     0. Java 的 method return type 如果是 `Object`, 我們呼叫它以後會拿到其中一種 `Object`:
        method 的實作可以隨便選一種 type return, 甚至可以亂數決定每次都不一樣.
        呼叫端拿到 return 值以後要想辦法處理這有可能是任何一種 type 的物件.

     0. Haskell 函數如果是吐出任意 type `a`, 它要能吐出任何一種呼叫端要求的 type:
        compile 的時候 GHC 會決定好每一次呼叫時想要拿到什麼 type, 並且使這函數吐出指定的 type.

--
     0. Java method 的 return type 如果越自由, method 的實作就越自由, 呼叫端要嘛越侷限,
        (侷限指的是: 越 super 的 class, 可用的通用 method 通常越少)
        要嘛就是一排髒髒的 `switch` `case` + `instanceof` test

     0. Haskell 函數吐出的 type 如果越自由, 函數的實作就越侷限, 呼叫端就越自由.
         0. 以全無限制的 `a` 為例, 大概只能剛好參數裡面有 `a` 就直接把它吐出去,
            或是以無窮遞迴之類的方式直接把程式 halt 住或 crash 掉.<br>
            (FFI 等級的黑魔法不列入討論)
         0. 如果是 `Num a => a` 的話, 那有可能可以拿參數的 `a` 作一些處理, 例如說兩個參數 `a` 加起來再吐出去
         0. 呼叫端自由, 因為可以運用的 type 比較多

---

layout: false

class: center, middle

.header[回到]
# Type Signature 寫法

---

layout: true

.header[Type Signature 寫法]

---

## 可以寫 Type Signature 的地方

  + 在任何用 `let` 或 `where` 定義變數 / 函數的地方 (top-level 算作 `where` 好了, 雖然省略寫法看不到這個字 :p),
    type signatue 與 value 寫在同一組裡面
    ```haskell
    let
      abc :: Int
      abc = 345

      def = "ooo"
      def :: String
      -- 順序不重要, 不過寫在後面看起來很奇怪

      ghi, jkl :: Char
      ghi = 'x'
      jkl = 'y'
      -- 也可以一次寫好幾個, type signatue 和 value 部分不用連續寫

      a1 :: Int
      [a1, a2, a3] = [1, 2, 3]
      -- 只要指定 a1, 那麼 a2, a3 就會被自動算出來也一定都是 Int

      b1 :: Int
      b2 :: String
      b3 :: Bool
      (b1, b2, b3) = (3, "str", True)
    ```

---
## 可以寫 Type Signature 的地方

  + 任何一個 expression 都可以標 type
     0. ```haskell
        3 * (4 :: Int)
        ```

--
     0. ```haskell
        filter (\a -> a < 10 :: Bool) [1..100]
          -- 這邊是標到 a < 10 的部分, (::) 的優先權比 lambda (\ ... -> ...) 高
        ```

--
     0. ```haskell
        filter ((\a -> a < 10) :: Int -> Bool) [1..100]
        ```

--
     0. ```haskell
        (filter :: (Int -> Bool) -> [Int] -> [Int]) (\a -> a < 10) [1..100]
          -- 這是在標 filter
        ```

--
     0. ```haskell
        let
          a = 25 / 7 :: Double
          -- 這邊是標到右半 25 / 7 的部分
          -- (右半才是 expression, 加上 = 和左半就不是了)
        ```

---
## 可以寫 Type Signature 的地方
  + 在有標 type 的泛型函數內, 可以引用泛型函數 type signatue 裡的 type variable 來標內部的 type
    ```haskell
    f :: Num a => a -> b -> (a, b)
    f a b =
      let
        aa :: a
        aa = a * a
      in
        (aa, b)
    ```

---
## 可以寫 Type Signature 的地方
  + 在定義 Data Type
    的地方

  + 定義 class member function 的地方

--
  + 讓我們接著看下去....

---

layout: false

class: center, middle

# 定義資料型態

---

layout: true

.header[定義資料型態]

---
## 完整寫法 (學名叫作 GADT)

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
  -- 對 Nothing 這一列的 a 叫作 phantom type, 有興趣可當關鍵字查資料
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

---
## 其他學名
  + `Bool`, `Maybe`, `Either`, `List`, `BinaryTree` 叫作 type constructor
  + `False`, `True`, `Nothing`, `Just`, `Left`, `Right`, ... 叫作 data constructor
  + 這邊出現的 `a`, `b` 叫作 type variable

--

## 上一頁對應的省略寫法

```haskell
data Bool = False | True
```

```haskell
data Maybe a = Nothing | Just a
```

```haskell
data Either a b = Left a | Right b
```

```haskell
data List a = Nil | Cons a
```

```haskell
data BinaryTree a = Leave a | Branch (BinaryTree a) a (BinaryTree a)
```

打的字少很多, 但是用法比較限制

---

layout: false
class: center, middle

# 使用資料型態

---

layout: true

.header[使用資料型態]

---
## 建立屬於這些資料型態的資料

--
```haskell
let
  a = True
  b = Just 3 :: Maybe Int
  c1 = Right (Left 'c') :: Either a (Either Char b)
  c2 = Right (Left 'c') :: Either a (Either Char a)
  c3 = Right (Left 'c') :: Either a (Either Char Int)
  c4 = Just (Right "Now") :: Maybe (Either a String)
  -- 不同 expression 用到的 type a 不代表同一個 type
  -- 但如果用到的是外層的 a, 那就會是同一個外層的 a type
  -- 沒有登場的 phantom type 不會被 value expression 限制住
  list = Cons 1 (Cons 2 (Cons 3 Nil)) :: List Int
```

---
## 從屬於這些資料型態的資料中截取東西

--
  + 用在 expression 或 statement 環境
    ```haskell
    case maybeInt of
      Just n -> putStrLn (show n)
      Nothing -> putStrLn "No number"
    ```
    這邊 of 之後是縮排式或大括號分號式

---
## 從屬於這些資料型態的資料中截取東西
  + 用在函數接參數的地方
    ```haskell
    f Nothing = "nothing"
    f (Just 2) = "got two"
    f (Just n) = "got " ++ show n
    ```

---
## 從屬於這些資料型態的資料中截取東西
  + 混合 lambda 與 case
    ```haskell
    map
      ( \case
        Nothing -> 0
        Just n -> n
      )
      [Nothing, Just 2, Just 3, Nothing]
    ```
    這邊 case 之後是縮排式或大括號分號式

---
## 從屬於這些資料型態的資料中截取東西

 0. 這個動作學名叫作
     pattern matching

--
 0. literal (1, 2, 3, "abc", 'x', 'y' 等等) 可以拿來放在 pattern 裡面 match

--
 0. 變數是拿來 capture 東西的, 如果是外面用過的變數,
    會 shadow

--
 0. .del[我以前以為會拿變數的內容當 pattern]

---

layout: false

# 

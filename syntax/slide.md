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
{-# LANGUAGE
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
, OverloadedLists, ViewPatterns, TransformListComp, PatternSynonyms
, MonadComprehensions #-}
```

--

 0. `{- ... -}` 是 Haskell 的 block comment, 用 `{-# ... #-}` 的方式放 pragma,
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

  + 參數部分是差不多的:
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
  + return type 的部分方向是相反的:
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
         0. 如果是 `Num a => a` 的話, 那可用的操作多一點點, 可以拿參數的 `a` 作一些處理, 例如說兩個參數 `a` 加起來再吐出去
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
  + 在有標 type 的泛型函數內, 可以引用泛型函數 type signatue 裡的 type variable 來標內部的 type<br>
    不過在外層要額外把 signature 裡所有的 type variable 加進 `forall` 宣告, 像這樣:
    ```haskell
    f :: forall a b. Num a => a -> b -> (a, b)
    f a b =
      let
        aa :: a
        aa = a * a
      in
        (aa, b)
    ```
    因為這是個後來才加進的用法 (extension: ScopedTypeVariables),
    為了讓老程式可以相容, 所以加上額外的啟動方式.

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

## 上一頁對應的省略寫法 (學名叫作 ADT)

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
## GADT 可以寫, 但 ADT 不能寫的例子

```haskell
data Expr :: * -> * where
  I   :: Int  -> Expr Int
  B   :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq  :: Expr Int -> Expr Int -> Expr Bool
```
 0. 當 `Expr` 後面跟的 type 不同時, 需要對應不同的 data constructor 的時候
 0. 這一個作法可以讓 GHC compile 時挑出 `Add (I 3) (B True)` 這種有錯誤的式子<br>
    (建立 value 的 `I 3`, `B True` 式子後面會講)

---
## 這邊的 \* 也可以放別的 type 來作一些效果

```haskell
data Nat = Ze | Su Nat
  -- 這是自然數的 Church encoding 表示法
  -- Ze 是 0
  -- Su Ze 是 1
  -- Su (Su Ze) 是 2
  -- ...

data Vec :: * -> Nat -> * where
  Nil  :: Vec a Ze
  Cons :: a -> Vec a n -> Vec a (Su n)
  -- Vec a n 這個 type 裡面 n 放入的 type 的意義是 Vec 的長度
```
 0. type 裡面放入有 value 意味的東西, 可以拿來作某種程度的行為證明.
    例如說證明對某個 `Vec` 的 access 一定不會出界
 0. 能放在 type 裡的 data type 只有比較簡單的 type, 前面看到的都是簡單的 type.
    這邊的 `Vec` 不可以. 別的不簡單的 type 以後再看.
 0. 好奇想查資料的話, 列一下關鍵字:
      * 這邊第一列放的 `*` 和 `Nat` 叫作 Kind
      * 把 value 當 type 用的叫作 DataKind

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
class: center, middle

# class 與 instance

---

layout: true

.header[class 與 instance]

---
## 描述一個 type 的 class

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  a == b = not (a /= b) -- 預設實作
  a /= b = not (a == b) -- 預設實作
```
(後面會講 Haskell 的 operator)

--
```haskell
instance Eq Bool where
  True == True = True
  True == False = False
  False == True = False
  False == False = True
  -- 沒定義 (/=) 的部分, 那它就會去拿預設實作來用
```

--
```haskell
instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Just a == Just b = a == b -- 這邊的 a == b 是利用 Eq a 這個前提的 ==
  _ == _ = False
```
這邊 `Eq a` 是充分條件, 可以這樣讀: 若 `Eq a` 則 `Eq (Maybe a)`

---
## 描述一個 type 的 class

```haskell
class Eq a => Ord a where
  (<) :: a -> a -> Bool
  ...
```
這邊 `Eq a` 是必要條件, 可以這樣讀: 若有 `Eq a` 才能 `Ord a`

--
```haskell
instance Ord Bool where
  False < True = True
  _ < _ = False
```

--
```haskell
instance Ord a => Ord (Maybe a) where
  Nothing < Nothing = False
  Nothing < _ = True
  Just _ < Nothing = False
  Just a < Just b = a < b
```

---
## 描述兩個以上 type 的 class

```haskell
class Mult a b c | a b -> c where
  (*) :: a -> b -> c
```
先不看第一列裡面的 `| a b -> c`, 這邊說, a 和 b 可相乘, 乘出來會得到 type c

--
```haskell
data Vector = Vector Int Int
data Matrix = Matrix Vector Vector

instance Num Vector where
  Vector a1 b1 + Vector a2 b2 = Vector (a1+a2) (b1+b2)
  ...
instance Num Matrix where
  Matrix a1 b1 + Matrix a2 b2 = Matrix (a1+a2) (b1+b2)
  ...
```

--
```haskell
instance Mult Int Int Int where
  a * b = a Prelude.* b
instance Mult Int Vector Vector where n * Vector a b = Vector (n * a) (n * b)
instance Mult Vector Int Vector where Vector a b * n = Vector (a * n) (b * n)
instance Mult Int Matrix Matrix where n * Matrix a b = Matrix (n * a) (n * b)
instance Mult Matrix Int Matrix where Matrix a b * n = Matrix (a * n) (b * n)
instance Mult Matrix Matrix Matrix where ...
instance Mult Matrix Vector Matrix where ...
```

---
## 描述兩個以上 type 的 class
  + `| a b -> c` 的部分用來引導 Haskell 推論 type 的方向

      + 告訴 Haskell 說如果 `a` 和 `b` 的 type 已知, `c` 就會只剩唯一解 (找出那個 `a` 和 `b` 符合的 instance, 它指出來的 `c` 就是答案)
      + 否則原本也需要獨立找出 `c` 的 type, 不拿 `a` 和 `b` 當作限制條件
      + 這個關鍵字叫作 Functional Dependency

--
  + 有加這個引導, 會讓這個式子用起來比較方便
    ```haskell
    a, b, c :: Matrix
    (a, b, c) = ... -- 假設有寫好 :p
    d = (a * b) * c -- 沒加引導的話, Haskell 也不知道要怎麼找出 (a * b) 的 type..
    e = (a * b :: Matrix) * c -- 沒加引導的話, 要寫成這樣才 compile 得過
    ```

--
  + 可以一次放好幾組 `| a b -> c | a c -> b | b c -> a` (當然此例這樣有點怪)

--
  + 也不一定要用到所有的 type variable, `| a -> c | c -> a` 這樣也可以 (當然此例也不適合就是..)

---
## class 裡面也可以定義 data type
```haskell
class Some a where
  data SomeK :: * -> *
  ...
```

```haskell
instance Some MyType where
  SomeK = Maybe
  ...
```

這個關鍵字可以查 type family

---
## 與 C++ template 作類比

 0. Haskell class 有點像 C++ template 裡一個叫作 traits 的技巧<br>
    對泛型的 type 作限制 / 描述

---

layout: false
class: center, middle

# Operator 語法

---

layout: true
.header[Operator 語法]

---
  + 定義 operator (prefix 寫法, 跟函數一樣)
    ```haskell
    (^) a 0 = 1
    (^) a n = a * (^) a (n-1) -- prefix 用法
    -- 或
    (^) a n = a * (a ^ (n-1)) -- infix 用法
    ```

    ```haskell
    (!) 0 = 1
    (!) n = n * (!) (n-1) -- prefix 用法
    -- 或
    (!) n = n * ((n-1) !) -- postfix 用法 (其實是 section)
    ```

--
  + 定義 operator (infix 寫法)
    ```haskell
    a ^ 0 = 1
    a ^ n = a * (a ^ (n-1))
    ```
    (unary operator 只能用 prefix 寫法定義)

---
  + 定義 operator 的 fixity (如果想要流暢地連用的話)
    ```haskell
    infixr 8 ^, ^^, **
    infixl 7 *, /, &#768;div &#768;, &#768;mod &#768;, &#768;rem &#768;, &#768;quot &#768;
    infixl 6 +, -
    infix 4 ==, /=, <, <=, >, >=
    infixr 3 &&
    infixr 2 ||
    ```
     0. 上面是 standard library 裡面定義的部分 operator
     0. 數字是 precedence, 範圍是 0~9, 9 最高
     0. ``div &#768;` 是函數的 infix 用法
     0. 同 precedence 的 infixl 如果連著用,<br>寫 `1 + 2 - 3 + 4`<br>解讀為 `(((1 + 2) - 3) + 4`
     0. 同 precedence 的 infixr 如果連著用,<br>寫 `1 ^ 2 ^ 3`<br>解讀為 `1 ^ (2 ^ 3)`
     0. infix 不能連用, (需要加括號的意思)
     0. 要跟定義該 operator 或函數的地方定義

---
  + 使用 operator (infix 用法, 好像很常見了..)
    ```haskell
    3 - 4
    3  &#768;sub &#768; 4
    ```
    任何函數都可以加 backquote 之後 infix 使用

--
  + 使用 operator (prefix 用法, 用起來像函數)
    ```haskell
    (-) 3 4
    sub 3 4
    ```

--
  + 使用 operator (section 用法, 參數沒給足, 一定要放一組括號)
    ```haskell
    (- 4) 3
    (3 -) 4
    ```
    會變成一個還要再吃一個參數的函數, 剩的是哪一個看原本餵的那一個在哪而定

--
  + postfix 用法的 unary operator 其實是 section, 所以一定要放括號把 operator 和參數括起來

--
  + prefix 用法的 unary operator 就是標準的 operator prefix 用法, 一定要放括號把 operator 自己括起來<br>
    負號是 Haskell 內建寫死的特例, 不是正常的 operator

---
  + `:` 開頭的 operator 是 type 用的, 例如.. List 可以這樣定
    ```haskell
    data List :: * -> * where
      Nil :: List a
      (:-:) :: a -> List a -> List a
    infixr 2 :-:
    ```
    那建立 List value 的時候可以這樣寫, 感覺可能比較開心?
    ```haskell
    aList = 3 :-: 4 :-: 5 :-: Nil
    ```

--
  + Haskell 內建的 list 是這樣用 (反正被內建定義掉了, 我們沒得定義, 只能用 XD)
    ```haskell
    let
      bList = 3 : 4 : 5 : []
      cList = [3,4,5] -- 這個是內建的 syntax sugar, 會自動變成上面那種寫法
    in
      case cList of
        (x : y : zz) -> ... -- 注意 zz 會吃到整個剩下來的尾巴
        [a, b, c] -> ... -- 這個會視為 (a : b : c : []) 所以數量要一樣才會 match
    ```
     + 以 `[a]` 的語法表示 list type 應該是 Haskell 另一個例外 (我不是很確定..) 大家就別追究了 ^^|
     + 另有一組漂亮而且規則漂亮一致的 list comprehension (其實是 monad comprehension 的特例),
       不過不會也不嚴重, 以後再提 ^^|

---
## Operator 小結
  + Operator 在語意功能上沒有額外的用處,
    只是語法上的一種 fu..

--
  + fu 很重要, 要小心用...

--
  + fu 能載舟, 亦能覆舟

---
## Operator 小結
  + 好的例子:
    ```haskell
    ($) :: (a -> b) -> a -> b
    f $ a = f a
    infixr 0 $

    -- f (g (h (k a))) 可以寫成 f $ g $ h $ k a
    ```
    這個在 Haskell 常見的 `$` 用法, 完全不是靠語法定義的東西, 只是一個單純的 operator

--
  + 另一個例子: (這是 `lens` 這個 package 裡的東西)<br>
    我不知道這個算不算好.. 讀熟了很好讀, 不過......
    ```haskell
    (&) :: a -> (a -> b) -> b
    a & f = f a
    infixl 1 &

    ("hello","world") & _1.element 0 .~ 'j' & _1.element 4 .~ 'y'
    -- 結果是 ("jelly","world")
    _1.element 4 .~ 'y' $ _1.element 0 .~ 'j' $ ("hello","world")
    -- 這是一樣的東西
    ```

---
## Operator 小結
  + 壞的例子: benchmarkgame 上面某支計算 pi digit 的程式.. (崩潰大哭)
    ```haskell
    import System.Environment

    pidgits n = 0 % (0 # (1,0,1)) where
     i%ds
      | i >= n = []
      | True = (concat h ++ "\t:" ++ show j ++ "\n") ++ j%t
      where k = i+10; j = min n k
            (h,t) | k > n = (take (n &#768;mod &#768;10) ds ++ replicate (k-n) " ",[])
                  | True = splitAt 10 ds
     j # s | n>a || r+n>=d = k # t
         | True = show q : k # (n*10,(a-(q*d))*10,d)
      where k = j+1; t@(n,a,d)=k&s; (q,r)=(n*3+a) &#768;divMod &#768;d
     j&(n,a,d) = (n*j,(a+n*2)*y,d*y) where y=(j*2+1)

    main = putStr.pidgits.read.head =<< getArgs
    ```

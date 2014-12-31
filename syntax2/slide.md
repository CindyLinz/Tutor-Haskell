class: inverse, center, middle

# Tutor of Haskell - Syntax (2)
## CindyLinz

2014.12.31

---

layout: false
class: center, middle

# Record Syntax

---

layout: true
.header[Record Syntax]

---
## 比較具體不抽象用法的 data type

(`Bool`, `Maybe a`, `Either a b`, `[a]`, `(a, b)`, `(a, b, c)` 是抽象用法的 data type)<br>
--
(你看這些 type variables, 想幫它們取個有意義的名字都取不出來)

--
```haskell
data Player :: * where
  Player
    :: Double -> Double -> Double -- 玩家坐標 x, y, z
    -> Double -- 玩家面向角度, 單位 radius, 水平與 x 軸正向的 CCW 夾角, [0~2π)
    -> Double -- 玩家仰角, 單位 radius, 抬頭為正, 低頭為負, 範圍 [-0.5π ~ 0.5π]
    -> Int16 -- 玩家 HP
    -> Int16 -- 玩家攻擊力
    -> Int16 -- 玩家防禦力
    -> Player
```

--
```haskell
attack
  :: Player -- 發動攻擊的玩家
  -> Player -- 被攻擊的玩家
  -> Player -- 受到攻擊後的玩家
```

---
```haskell
attack
  (Player _ _ _ _ _ _ atk _)
  (Player x y z theta phi hp att0 def) =
    let
      max a b = if a > b then a else b
      hp' = max 0 (hp - max 1 (atk - def))
    in Player x y z theta phi hp' att0 def
```
--
  + 寫出一大排底線讓我感覺不太對勁
  + 寫出一堆沒作什麼事的變數也讓我感覺不舒服

---
## 事先為 `Player` 準備一些 access helper
```haskell
getX, getY, getZ :: Player -> Double
getX (Player a _ _ _ _ _ _ _) = a
getY (Player _ a _ _ _ _ _ _) = a
getZ (Player _ _ a _ _ _ _ _) = a
...
setX, setY, setZ :: Double -> Player -> Player
setX a (Player _ a1 a2 a3 a4 a5 a6 a7) = Player a a1 a2 a3 a4 a5 a6 a7
setY a (Player a1 _ a2 a3 a4 a5 a6 a7) = Player a1 a a2 a3 a4 a5 a6 a7
...
```
--
```haskell
attack p1 p2 =
  let
    max a b = if a > b then a else b
    hp' = max 0 (getHP p2 - max 1 (getAtt p1 - getDef p2))
  in
    setHP hp' p2
```

---
## Haskell 提供 Record Syntax 來取代樣版 accessor
```haskell
data Player :: * where
  Player ::
    { x, y, z :: Double
    , theta, phi :: Double
    , hp, atk, def :: Int
    } -> Player
```
--
```haskell
attack (Player {atk=atk1}) p2 = -- 這一列的括號可以省略
  let
    max a b = if a > b then a else b

    Player {hp=hp2, def=def2} = p2 -- Record Syntax 於 pattern matching 的用法
    hp' = max 0 (hp2 - max 1 (atk1 - def2))
  in
    p2 {hp=hp'} -- Record Syntax 於 data construct 的用法
```

--
 0. `Player {atk=atk, hp=hp}` (變數名剛好取一樣) 可以簡寫為 `Player {atk, hp}`

--
 0. `Player {x, y, z, ..., def}` (全都接) 可以簡寫為 `Player {..}`

--
 0. 對欄位的值作 pattern matching 時這樣寫 `Player {hp=0}`

---
## 省略寫法的 Record Syntax
```haskell
data Player = Player
  { x, y, z :: Double
  , theta, phi :: Double
  , hp, atk, def :: Int
  }
```
---
## 有分支的 type 的 Record Syntax
有分支的 type 學名叫 Sum Type
```haskell
data Player
  = Player1
    { x, y, z :: Double
    , atk :: Int
    }
  | Player2
    { x, y, z :: Double
    , atk, atk2 :: Int
    }
```

--
 0. 不同分支如果出現一樣的 field name, 它們的 type 要一樣<br>
    把這個 field 想成一個 access 函數, 至少它的 type 要寫得出來吧~

--
 0. 只出現在部分的 field name, 如果使用時 data 不是在它有出現的分支, 會出 exception

---

layout: false
class: center, middle

# Existential Type

---

layout: true
.header[Existential Type]

---
```haskell
data Cup :: * where
  Cup
    :: c -- 一個我們外人不知道的 type (第一列沒有列出來的 type)
    -> (c -> c) -- 一個可以處理這個 type 的函數, 想像為倒水的動作
    -> Cup
```
--
```haskell
data GlassCup :: * where
  GlassCup :: Int -> GlassCup -- 玻璃杯可以裝整數單位的水
pourGlassCup :: GlassCup -> GlassCup
pourGlassCup (GlassCup d) = GlassCup (d-1)

data Mug :: * where
  Mug :: Double -> Mug -- 馬克杯可以裝浮點數單位的水
pourMug :: Mug -> Mug
pourMug (Mug d) = Mug (d-1)
```
--
```haskell
let cups =
  [ Cup (GlassCup 10) pourGlassCup
  , Cup (Mug 5.0) pourMug
  ]
in map (\(Cup cup pour) -> Cup (pour cup) pour) cups
```
--
 0. 這種是比較類似物件導向的多型

--
 0. 塞進 `Cup` type 東西, 就會有 `pour` 的方式<br>
    (不過這裡 `pour` 是放進 `Cup` 的時候才指定, 沒有跟死一個特定的杯種)

---
## 配合 `class` 的作法
```haskell
class Pourable a where
  pour :: a -> a
data Cup :: * where
  Cup
    :: Pourable c => c -- 這個 c 唯一對外透明公開的資訊就是它符合 Pourable 的條件
    -> Cup
```
--
```haskell
data GlassCup :: * where
  GlassCup :: Int -> GlassCup -- 玻璃杯可以裝整數單位的水
instance Pourable GlassCup where
  pour (GlassCup d) = GlassCup (d-1)

data Mug :: * where
  Mug :: Double -> Mug -- 馬克杯可以裝浮點數單位的水
instance Pourable Mug where
  pour (Mug d) = Mug (d-1)
```
--
```haskell
let cups = [Cup (GlassCup 10), Cup (Mug 5.0)]
in map (\(Cup cup) -> Cup (pour cup)) cups
```
--
  + 這作法 `pour` 就會跟死一種 `Cup` 了

---
## 配合 Record Syntax
```haskell
data Cup :: * where
  Cup :: { cup :: c, pour :: c -> c } -> Cup

data Cup :: * where
  Cup :: Pourable c => { realCup :: c } -> Cup
```
--
## 配合 ADT 省略寫法
```haskell
data Cup = forall c. Cup c (c -> c)
data Cup = forall c. Cup { cup :: c, pour :: c -> c }
data Cup = forall c. Pourable c => Cup c
data Cup = forall c. Pourable c => Cup { cup :: c }
```
  + ADT 寫法要用 `forall` 關鍵字來寫

--
  + 建議停下來想一想, Haskell 設計用 forall 這樣的字, 是什麼意思.<br>
    forall 的相對面是 exist, 想想是誰對誰 forall, 誰對誰 exist
---
## `forall` 用在 Type Signature 裡面
  + ```haskell
    bracketShow :: forall a. Show a => a -> String
    bracketShow a = "[" ++ show a ++ "]"
    ```
    其實這跟沒加 `forall` 的一樣, 因為「最外層」的 type variable 就是要 forall 的, Haskell 會自動加.<br>
    (只是手動加會有額外啟動 ScopedTypeVariables extension 的效果)
--
  + ```haskell
    pourOnce :: forall c. Pourable c => (c -> c) -> Mug -> Mug
      -- 這邊 forall c. 沒寫也會自動加
    pourOnce p m = p m -- error
    ```
--
  + ```haskell
    pourTwice
      :: (forall c. Pourable c => c -> c)
      -> GlassCup -> Mug -> (GlassCup, Mug)
    pourTwice p g m = (p g, p m) -- ok
    p1 = pourTwice pour -- ok
    p2 = pourTwice pourMug -- error
    ```
---
## `forall` 用在 Type Signature 裡面
  + ```haskell
    pourOnce :: forall c. Pourable c => (c -> c) -> Mug -> Mug
      -- 這邊 forall c. 沒寫也會自動加
    pourOnce p m = p m -- error
    ```
  + ```haskell
    pourTwice
      :: (forall c. Pourable c => c -> c)
      -> GlassCup -> Mug -> (GlassCup, Mug)
    pourTwice p g m = (p g, p m) -- ok
    p1 = pourTwice pour -- ok
    p2 = pourTwice pourMug -- error
    ```
  + ```haskell
    ($) :: forall a b. (a -> b) -> a -> b
    ($) f a = f a
    pp = ($) pourMug -- ok
    ppm = pp (Mug 5) -- ok
    ppmm = ($) pourMug (Mug 5) -- ok
    ```
--
  + ```haskell
    pourMugOnce :: (forall c. Pourable c => c -> Mug) -> Mug -> Mug
    pourMugOnce p m = p m -- ok
    pm = pourMugOnce pour -- error
    mm = pourMugOnce pourMug -- error
    ```
---
## std lib 中用在 async interrupt mask 的實例
[Brain Hurt 注意]
```haskell
mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b

mask $ \restore -> do
  x <- acquire
  restore (do_something_with x)
  release x
```
  + 想想這邊為什麼需要 `forall`
  + 這個 `forall` 是給誰方便, 又限制了誰?
--


```haskell
f1 :: forall a. a -> Int
f2 :: (forall a. a -> Int) -> Int
f3 :: ((forall a. a -> Int) -> Int) -> Int
f4 :: (((forall a. a -> Int) -> Int) -> Int) -> Int
```
--
  + 這是一個: 反服貿, 反反服貿, 反反反服貿, 反反反反服貿... 的節奏
---
## 這種 Brain Hurt 煩惱為什麼以前沒遇過?

--
  + Higher order function 好棒棒

--
  + Static type 好棒棒

--
  + Parametric polymorphism 好棒棒

--
  + Higher order function + static type + parametric polymorphism 就這樣了<br>
--
    Haskell 是目前我所知道兼具這三者的語言裡面, 最簡單好學的....<br>
    ╮(╯_╰)╭<br>
--
      + Lisp, Perl, Python, Ruby, Javascript 不是 static type
      + C, C++, Java 沒有 higher order function<br>
        (Java parametric polymorphism 也不是 static)
      + Agda, Coq, Idris 比 Haskell 難.. (ML 家族我幾乎沒接觸, 無法比較 ^^|)
      + 我不會 Scala, 所以不在我所了解的語言裡面 :p<br>
        (不知道它有沒有; 也不知道它好不好學..)
---
layout: false
class: center, middle

# 其他非必要
# 但可以讓日子更好過的小東西

---
layout: true
.header[其他非必要, 但可以讓日子更好過的小東西]
---

## deriving

原本要這樣寫
```haskell
data Tree a = Leave a | Branch (Tree a) a (Tree a)
instance Show a => Show (Tree a) where
  show (Leave a) = "Leave " ++ show a
  show (Branch l a r) = "Branch " ++ show l ++ " " ++ show a ++ " " ++ show r
instance Eq a => Eq (Tree a) where
  Leave a == Leave b = a == b
  Branch l1 a1 r1 == Branch l2 a2 r2 =
    l1 == l2 && a1 == a2 && r1 == r2
  _ == _ = False

instance Ord a => Eq (Tree a) where ...
```
--
一些常見的 instance, GHC 可以自動產生
```haskell
data Tree a = Leave a | Branch (Tree a) a (Tree a)
  deriving (Show, Eq)
```

--
或類似這樣寫法, 可以稍微手動控制一下的半自動
```haskell
instance Ord (Tree Int) -- 不寫 where, 這邊只自動生成 Tree Int, 不是整個 Tree a
```

---

## deriving

GADT 語法.. (不過 deriving 只能用在用 ADT 就寫得出來的 data type)
```haskell
data Maybe1 :: * -> * where
  Nothing1 :: Maybe1 a
  Just1    :: a -> Maybe1 a
  deriving (Eq, Ord)
```

--

## 可以 deriving 的 class
  + Haskell98 標準規定: Eq, Ord, Enum, Ix, Bounded, Read, Show<br>
  + GHC 追加: Typeable, Data, Generics, Functor, Foldable, Traversable

---

## newtype

```haskell
newtype MyInt = MyInt Int
newtype MyMaybe a = MyMaybe (Maybe a)
```

--
 0. 藉由已有的 type 另外產生一個結構一模一樣的新 type.<br>
    (所以右邊的 data constructor 之後要恰好放一個 type)

--
 0. 結構一樣, 但是在 type check 當成不同的 type 來用<br>
    (由於結構一樣, runtime 完全不加額外資訊, 所以不能用 existential type)

--
 0. 所有原 type instance 的 class 都可以選擇 deriving 過來用<br>
    (反正結構一樣, GHC 就知道怎麼拿一樣的實作來用)

--
 0. 可以給(換用)不一樣的 instance (這其實是 `newtype` 在應用上蠻重要的作用)

--
 0. 用「貌似」data construct 或 pattern matching 來建立新 type 資料或取得原 type 的資料, 但 runtime 沒有 overhead

---

```haskell
a = MyMaybe (Just 3)
...
case a of
  MyMaybe Nothing -> ...
  MyMaybe (Just x) -> ...
```

---

## type

type synonym, 讓複雜的 type 可能可以更好讀 (像 C 的 `typedef`)
```haskell
type Good = Bool
type Complex a = Either String (Maybe a)
```
  + type check 等到這個 synonym 完全展開再作
  + 有人把這個 `type` 當成 type level 的函數來用

---

## Pattern Synonym

pattern synonym, 可以對資料型態的「形狀」作額外的 alias<br>
可以用 alias 來建資料, 也可以用 alias 來作 pattern matching

```haskell
data Type = App String [Type]

pattern Func t1 t2 = App "->" [t1, t2]
pattern I = App "Int" []
pattern B = App "Bool" []

isPositive = Func I B
-- 就是 isPositive = App "->" [App "Int" [], App "Bool" []]
-- 相當於我們可以訂義一堆不同面貌的 data constructor

-- pattern matching 這樣用
case ooo of
  Func a b -> ...
    -- 當 ooo 長得像 App "->" [t1, t2] 的時候會 match
    -- t1, t2 就會裝到這邊的 a, b
  I -> ...
  B -> ...
-- 相當於我們可以為一個 data type 定義一堆不同面貌的可 match 的 pattern
```

---

## Pattern Synonym

單向的 pattern synonym, pattern 裡面列出的變數沒有涵蓋右邊的所有變數. 只能 pattern match 時使用.
拿來 construct 的話, 沒列到的變數不知道要放什麼
```haskell
pattern Head a <- a : _ -- 這邊改用向左箭頭而不是等號

-- match 時這樣用
case [1,3,2,4,5] of
  Head 5 -> ... -- 如果 list 第一項是 5 的話會走這一項
  Head a -> ... -- a 會拿到 1
```
.del[雖然我覺得沒列到的可以全都放 `undefined`, 就一樣可以 construct XD]<br>
不過為了配合別的組合用法, 手動從語法上把語意分出來比較明確,
對未來可能出現的新組合用法也比較不會出現問題

---

## Pattern Synonym

  + 我們可以拆成好幾個 `module` 依情境分別選擇 (re)`export` 不同長相的 pattern<br>
--
    <span class=del>(封裝封裝好像就會比較高級.... XD)</span>
--
  + 後面講到 ViewPatterns 時, 配合著用威力更大

--
  + 註: pattern synonym 作 pattern matching 時, evaluate 的順序和直接寫出它背後的 pattern 的順序是不一樣的,
    不過還沒介紹 Haskell 的 evaluate 順序, 所以先在此略過 (這串是給自己讀投影片的人看的)

---

## Pattern Alias
pattern matching 時可以用 `p2 @ Player {hp, def}` 這樣的寫法, 取出局部的 `hp`, `def` 與整體 `p2`
```haskell
attack Player {atk=atk1} p2 @ Player {hp=hp2, def=def2} = ...
```

```haskell
attack (Player {atk=atk1}) (p2 @ Player {hp=hp2, def=def2}) = ...
```
請考慮可讀性來決定怎麼加括號

---

## where

倒過來寫的 `let ... in ...`, 不過允許使用的地方比較少<br>
(這裡講的不包含 `module`, `data`, `class`, `instance` 等語法所附帶的 `where`)

--

  + 函數定義的地方 (沒參數的函數也算歐).. 每一個 pattern 可以放一組 where
    ```haskell
    f 0 a b = (go1 a, go2 b) where
      go1 = (+1)
      go2 = (+2)
    f k a b = (go a, go b) where
      go a = k * a
    ```

--

  + `case..of` 的每一個 branch 可以放一組 where
    ```haskell
    f k a b = case k of
      0 -> (go1 a, go2 b) where
        go1 = (+1)
        go2 = (+2)
      _ -> (go a, go b) where
        go a = k * a
    ```

--

  + `where` 的後面是縮排式, 或大括號分號式<br>

--
  + 一般是用在希望人家一讀「主 expression」就大概知道怎麼回事的場合

---

## Guard & Pattern Guard & multi-way if

在用到 pattern matching 的地方作為輔助條件

```haskell
f (Just n)
  | n < 0 = ...
  | n > 0 = ...
  | otherwise = ... -- 有時候我會寫 | True = ... 比較短 XD
f Nothing = ...
```

--

```haskell
g x = case x of
  Just n
    | n < 0 -> ...
    | otherwise -> ...
  Nothing -> ...
```

--

```haskell
(msg, numOfRoot)
  | det < 0 = ("no real roots", 0)
  | det == 0 = (show ans1, 1)
  | otherwise = (show ans1 ++ ", " ++ show ans2, 2)
```

--
這裡沒有縮排規則 .del[但是不排好的話應該會被討厭]

---

## Guard & Pattern Guard & multi-way if

guard 裡面可以先 let 一些 guard 右半要用的東西 (最後面的 expr 裡面也可以用)..
```haskell
isPositive :: Int -> Bool
isPositive n
  | let n0 = n - 1, n0 >= 0 = (True, n0)
  | otherwise = (False, -1)
```

--

  + 如果一個 `let` 裡面想放多個定義, 也是選用縮排式或大括號分號來作<br>
    同一組內的定義, 依慣例也是順序無關
    (我猜這種地方想選用大括號的人比較多?)

--
  + 可以有好幾組 `let`, 這幾組 `let` 之間和最後一個 guard expr 以逗點隔開<br>
    前面的 `let` 不能用後面的 `let` 所定義的東西

    ```haskell
    isPositive n
      | let
          n1 = n0 + 2
          n0 = n - 1
        , n0 >= 0 = (True, n0)
      | let { n1 = n0 + 2; n0 = n - 1 }, n0 >= 0 = (True, n0)
      | let n0 = n - 1, let n1 = n0 + 2, n0 >= 0 = (True, n0)
    ```

---

## Guard & Pattern Guard & multi-way if

在 guard 裡面再偷偷(?)作 pattern matching (學名 Pattern Guard)
```haskell
lookup :: FiniteMap -> Int -> Maybe Int

addLookup env var1 var2
  | Just val1 <- lookup env var1
  , Just val2 <- lookup env var2
  = val1 + val2

  | otherwise = ...
```

  + 如果其中一個 match 失敗了, 那麼就是整條 guard 失敗, 繼續試下一條 guard

--
  + 和前頁的 `let` 可以混合使用

---

## Guard & Pattern Guard & multi-way if

multiway if 就是拿 guard 當 if 用, 可以有很多分支..

```haskell
if | a < 3 -> putStrLn "less then 3"
   | a > 3 -> putStrLn "larger than 3"
   | otherwise -> putStrLn "others"
```

--
multiway if 為縮排式, 也可以換為大括號, 而因為有 | 作分隔了所以分號可加可不加.
這需要有排版規則, 是為了巢狀多層使用的時候語法不會混淆.

```haskell
if | a < 3 -> putStrLn "less then 3"
   | a > 3 -> if -- 這邊這個 if 要不要折下去... 依心情決定...
     | b < 4 -> putStrLn "x"
     | b > 5 -> putStrLn "y"
     | otherwise -> putStrLn "z"
   | otherwise -> putStrLn "others"
```

--
它是 guard, 可以混用前面所提的 `let` 和偷偷 pattern match 的用法

---

## Guard & Pattern Guard & multi-way if

我覺得 multiway if 大概只會用在 statement 環境裡面放 statement,
因為如果是要分好幾個 branch 決定要怎麼 bind 資料到變數 (或一些變數所成的 pattern) 上,
用一般 guard 的第三種用法就夠了, 字還比較少.

```haskell
good = if
  | a < 3 -> "X"
  | a > 3 -> "O"
  | otherwise -> "="
```
直接寫成
```haskell
good
  | a < 3 = "X"
  | a > 3 = "O"
  | otherwise = "="
```

---

## View Patterns

在 pattern 要 match 以前, 先幫它加工一下再 match. 這個「加工」就像是某種 view 的意義

```haskell
c = case "CindyLinz" of
  (length -> 7) -> "the length is 7"
  (head -> 'C') -> "the head is C"

addLookup :: FiniteMap -> Int -> Int -> Int
addLookup env (lookup env -> Just a) (lookup env -> Just b) = a + b

example :: Maybe ((String -> Integer,Integer), String) -> Bool
example (Just ((f,_), f -> 4)) = True
-- 要用來當 view 的函數, 可以是在同一個 pattern 裡面的左邊已經 match 到的東西,
-- 不能從右邊拿..
-- 因為這整個 pattern 還沒 match 完, 其 match 的順序是從左邊開始
-- 已經 match 到的部分才能拿來用
```

---

## View Patterns + Pattern Synonym 組合應用

```haskell
module Temperature
  ( fahrenheit, celsius, pattern Fahrenheit, Temperature (Celsius)
  ) where

data Temperature = Celsius Rational
  -- 註: Rational 是0誤差精度無上限(以記憶體為限)有理數喔

c2f, f2c :: Rational -> Rational
c2f c = c / 5 * 9 + 32
f2c f = (f - 32) / 9 * 5

pattern Fahrenheit a <- Celsius (c2f -> a)
  -- view pattern 只能用在單向 pattern synonym
  -- 畢竟 GHC 沒有保證生出反函數的機制
-- 所以反向 (construct 向) 不能用 pattern
-- 只能用一個叫 smart constructor 的技巧
-- (定義函數當 constructor 用)
fahrenheit = Celsius . f2c

-- 為了對稱, 我們也作一個攝氏的 smart constructor
celsius = Celsius

-- 然後(看看最前面), 我們 export 的是:
--   (fahrenheit, celsius, pattern Fahrenheit, Temperature (Celsius))
```

---

## View Patterns + Pattern Synonym 組合應用

用起來像這樣
```haskell
import Temperature -- 假設上一頁的程式碼在這模組裡

todayTemp = celsius 11
isCold temp = case temp of
  Fahrenheit f | f < 50 -> True
  _ -> False
```

--
以下這些是 pattern synonym 的 future work..
```haskell
pattern Fahrenheit a <- Celsius (c2f -> a) where
  Fahrenheit a = Celsius . f2c
-- 這樣 constructor 就不用 smart 了 (咦

pattern Succ n <- n1 | let n = n1-1, n >= 0 where
  Succ n = n + 1
-- 可以混用 guard
```

---

## do-notation

Haskell std lib 有個 class 叫作 `Monad`, 裡頭有個重要的函數 (operator) 叫作 `(>>=)`
```haskell
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  ... -- 其他函數
```
do-notation 是為它量身打造的
--
(縮排式或大括號分號式)

```haskell
do
  putStrLn "Hi"
  line <- getLine
  putStrLn $ "You said " ++ line
```
--
直接譯為下面這樣一段 code
```haskell
putStrLn "Hi" >>= \_ -> getLine >>= \line -> (putStrLn $ "You said " ++ line)
```
--
 0. 用 `(>>=)` 把 statements 串起來

--
 0. 有用 `<-` 接結果的話, 後面的 lambda 的參數就用這邊接的變數名

--
 0. 沒有接的話, 後面的 lambda 用 `\_` 當參數, 也就是接起來丟掉

--
 0. Monad 與其應用以後再說吧~~
--
    有個遞迴版的 `mdo` 也是以後再說吧~~

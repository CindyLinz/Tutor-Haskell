class: center, middle, inverse

# Tutor of Haskell - monad
## CindyLinz

2015.1.14

---

layout: false
class: center, middle

# Monad paradox

---

layout: true
.header[Monad paradox]

---

```nohighlight
每個學 monad 的都說 monad 很難;
  每個教 monad 的都說 monad 很簡單.

          這中間一定是出了什麼誤會(literally).......
```
.right[~~ CindyLinz 2015.1.8]

--

  + 我目前遇過, 一下子就能說 monad 很簡單的, 是學純粹數學的人.

--

  + 所以, 我將試著從純粹數學的角度, 作為介紹 monad 的入口.

--

  + 然後, 我就要請大家教我什麼是 monad. 根據 monad paradox, 大家應該就會覺得 monad 很簡單了.

---

layout: false
class: center, middle

# Monad definition & Monad laws

---

layout: true

.header[Monad definition & Monad laws]

---

## Monad definition

```haskell
class Functor m => Applicative m where
  ... (它不是今天的主角, 先略過) ...

class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  fail :: String -> m a -- 請忽略這一條
```
  + 忽略 `fail` 不看. 數學上的 monad 沒有這一條, 實用上它是 Haskell compiler 實作方便的 internal function,
    正常的程式不該直接使用

--

## Monad laws (良心要求)

```haskell
         return a >>= k    =    k a               -- Left identity  左單位元
           m >>= return    =    m                 -- Right identity 右單位元
m >>= (\x -> k x >>= h)    =    (m >>= k) >>= h   -- Associativity  結合律
```
  + .del[數學家是一種看到定律就會興奮的生物]

---

## 你看到了什麼?

```haskell
class Monad m where
  fmap   :: (a -> b) -> m a -> m b -- 來自 Functor (今天說好不提 Applicative)
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

         return a >>= k    =    k a               -- Left identity  左單位元
           m >>= return    =    m                 -- Right identity 右單位元
m >>= (\x -> k x >>= h)    =    (m >>= k) >>= h   -- Associativity  結合律
```

  + 回想一下 Functor 是什麼?

  + 把 Monad 的函數名稱遮起來不看, 只看 type 的部分, type 說出什麼故事?

  + 想一下在這幾條定律約束之下的世界, 可以演伸出哪些直覺的特性?

  + 想一下你寫過的程式裡面, 遇到過哪些概念可以符合 Monad 的性質?

---

layout: false
class: center, middle

# Monad instances

---

layout: true

.header[Monad instances]

---

  + 以下將列出經典款的 monad instance

--

  + 會先列出 data type

--

  + 請先直接就 data type 的長相告訴我
      + 這 monad 的可能使用情境
      + 各元件的用處或直覺意義
      + monad 大概會怎麼實作

--

  + type 很可怕, 但程式碼更可怕.....

--
      + 當 type 不再可怕, 程式碼也就不可怕了...

--

  + 等 type 的故事說完, 我們才看程式碼

--
      + 已經變成 trivial 的程式碼

---

## Prelude 裡出現的 monad

```haskell
instance Monad Maybe where ..
instance Monad (Either e) where ..
instance Monad [] where ..
instance Monad IO where ..
instance Monad ((->) r) where ..
```

## 生來當 monad 的職業 monad

```haskell
newtype Identity a = Identity a
instance Monad Identity where ..

newtype Reader r a = Reader (r -> a)
instance Monad (Reader r) where ..

newtype Writer w a = Writer (a, w)
instance Monoid w => Monad (Writer w) where ..

newtype State s a = State (s -> (a, s))
instance Monad (State s) where ..

newtype Cont r a = Cont ((a -> r) -> r)
instance Monad (Cont r) where ..
```

---

## Monad Maybe

```haskell
data Maybe a
  = Nothing
  | Just a
```

--

```haskell
instance Monad Maybe where
  return = Just -- return a = Just a

  Just x >>= k = k x
  Nothing >>= _ = Nothing
```

---

## Monad (Either e)

```haskell
data Either e a
  = Left e
  | Right a
```

--

```haskell
instance Monad (Either e) where
  return = Right -- return a = Right a

  Right r >>= k = k r
  Left l >>= _ = Left l
```

---

## Monad []

```haskell
data [a]
  = []
  | a : [a]
```

--

```haskell
instance Monad [] where
  return a = [a]
  m >>= k = concat (map k m)
    -- 令 m = [m1, m2, m3, ...]
    -- m >>= k 就是
    --   k m1 ++ k m2 ++ k m3 ++ ...
```

---

## Monad IO

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

--

```haskell
instance Monad IO where
  return x = IO $ \s -> (# s, x #)
  IO f >>= k = IO $ \s ->
    let
      (# s', a #) = f s
      IO f' = k a
    in
      f' s'
  -- (# , #) 是 unboxed pair/tuple, 有 pair/tuple 的形狀, 不過 runtime 沒有那層殼
```

---

## Monad ((->) r)

--

```haskell
instance Monad ((->) r) where
  return = const -- return a = \_ -> a
  m >>= k = \r -> k (m r) r
```

---

## Monad Identity

--

```haskell
newtype Identity a = Identity a
instance Monad Identity where
  return = id -- return a = a
  m >>= k = k m
```

---

## Monad (Reader r)

--

```haskell
newtype Reader r a = Reader (r -> a)
instance Monad (Reader r) where
  return a = Reader (\_ -> a)
  Reader f >>= k = Reader
    ( \r ->
      let
        Reader f' = k (f r)
      in
        f' r
    )
```

---

## Monad (Writer w)

--

```haskell
newtype Writer w a = Writer (a, w)
instance Monoid w => Monad (Writer w) where
  return a = Writer (a, mempty)

  Writer (a, w) >>= k = Writer (a', w <> w') where
    Writer (a', w') = k a

class Monoid w where
  mempty :: w
  mappend :: w -> w -> w

(<>) = mappend
infixr 6 <>
```

---

## Monad (State s)

--

```haskell
newtype State s a = State (s -> (a, s))
instance Monad (State s) where
  return a = State (\s -> (a, s))

  State f >>= k = State
    ( \s ->
      let
        (a, s') = f s
        State f' = k a
      in
        f' s'
    )
```

---

## Monad (Cont r)

--

```haskell
newtype Cont r a = Cont ((a -> r) -> r)
instance Monad (Cont r) where
  return a = Cont ($ a) -- return a = Cont (\f -> f a)

  Cont c >>= k = Cont
    ( \f ->
      c ( \a ->
        let Cont c' = k a
        in c' f
        )
    )
```

class: center, middle, inverse

# Tutor of Haskell - lazy evaluation
## CindyLinz

2015.1.7

---

layout: false
class: center, middle

# Haskell 傳說

---

layout: true

.header[Haskell 傳說]

---

## Haskell 可以用無窮 list

(順便介紹一下簡單的 list comprehension)
```haskell
[1..3] -- 1:2:3:[]
[1,3..7] -- 1:3:5:7:[]
[1, -1.. -4] -- 1 : -1 : -3 : []
[1..] -- 1:2:3:4:... (無窮)
[1,1..] -- 1:1:1:... (無窮)
[1,3..] -- 1:3:5:... (無窮)
[1,0..] -- 1:0:-1:-2:... (無窮)
```

---

## Haskell 可以用無窮 list

Fibonacci (無窮數列) 可以這樣定義
```haskell
--             fib = 1  1  2  3  5  8 ..
--        tail fib = 1  2  3  5  8 13 ..
-- zipWith (+) . . = 2  3  5  8 13 21 ..
fib = 1 : 1 : zipWith (+) fib (tail fib) -- 1 1 2 3 5 8 ..

tail :: [a] -> [a] -- 摘掉 list 頭
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] -- 把兩個 list 的元素兩兩作用
```

--
這個長度無限的 `fib` list 是真的可以用的..

```haskell
(!!) :: [a] -> Int -> a -- 從 list 中取出一個元素

putStrLn $ show (fib !! 5) -- 印出 8
putStrLn $ show (fib !! 6) -- 印出 13
putStrLn $ show (fib !! 7) -- 印出 21
```

---

layout: false
class: center, middle

# Evaluation strategy

---

layout: true

.header[Evaluation strategy]

---

## Evaluate expression

  + 我們先定義一個函數 `f`
    ```haskell
    f 0 a b = a + b
    f 1 a b = a * b
    f 2 a b = a
    f 3 a b = b
    ```

--
  + 下面是我們要 evaluate 的 expression
    ```haskell
    f 0 (100 + 10) (100 - 10)
    f 1 (100 + 10) (100 - 10)
    f 2 (100 + 10) (100 - 10)
    f 3 (100 + 10) (100 - 10)
    ```

---

## Applicative Order (case 1)

--
  + ```haskell
    f 1 a b = a * b

    f 1 (100 + 10) (100 - 10)
    ```

--
  + ```haskell
    f 1 110 (100 - 10)
    ```

--
  + ```haskell
    f 1 110 90
    ```

--
  + ```haskell
    110 * 90
    ```

--
  + ```haskell
    9900
    ```

---

## Applicative Order (case 2)

--
  + ```haskell
    f 2 a b = a

    f 2 (100 + 10) (100 - 10)
    ```

--
  + ```haskell
    f 2 110 (100 - 10)
    ```

--
  + ```haskell
    f 2 110 90
    ```

--
  + ```haskell
    110
    ```

---

## Normal Order (case 2)

--
  + ```haskell
    f 2 a b = a

    f 2 (100 + 10) (100 - 10)
    ```

--
  + ```haskell
    100 + 10
    ```

--
  + ```haskell
    110
    ```

---

## Normal Order (case 1)

--
  + ```haskell
    f 1 a b = a * b

    f 1 (100 + 10) (100 - 10)
    ```

--
  + ```haskell
    (100 + 10) * (100 - 10)
    ```

--
  + ```haskell
    110 * (100 - 10)
    ```

--
  + ```haskell
    110 * 90
    ```

--
  + ```haskell
    9900
    ```

---

## Human Order (case 1)

--
  + ```haskell
    f 1 a b = a * b

    f 1 (100 + 10) (100 - 10)
    ```

--
  + ```haskell
    (100 + 10) * (100 - 10)
    ```

--
  + ```haskell
    100^2 - 10^2
    ```

--
  + ```haskell
    10000 - 10^2
    ```

--
  + ```haskell
    10000 - 100
    ```

--
  + ```haskell
    9900
    ```

---

## Human Order (case extra)

--
  + ```haskell
    f 1 a b = a * b

    f 1 (f 1 10 20) (100 - 100)
    ```

--
  + ```haskell
    (f 1 10 20) * (100 - 100)
    ```

--
  + ```haskell
    (f 1 10 20) * 0
    ```

--
  + ```haskell
    0
    ```

---

## Evaluation strategy 的主要分類

  + Strict Evaluation -- 以 applicative order 為代表, 一般常見程式語言是這個分類<br>
    (有小差異, 例如說 C 的參數之間沒有規定順序)

--

  + Non-strict Evaluation -- 以 normal order 為代表, lazy evaluation 是在這個分類

--

  + Nondeterministic strategy -- .del[以人類為代表] 一些 concurrent 計算的模型, 或是會找最短步驟的算在此類

---

layout: false
class: center, middle

# Strict &amp; Non-strict evaluation

---

layout: true

.header[Strict &amp; Non-strict evaluation]

---

## 定義

  + Strict - 任何參數爆, 結果一定爆 (exception 或 crash 或 undefined behavior..)<br>
    無法完成整個運算, 學名叫作 bottom, 學術符號是 ⊥ 或 \_|\_
    ```haskell
    f :: a -> b -> b
    f a b = b
    f 1 爆 = 爆
    f 爆 2 = 爆
    ```

--
  + Non-strict - 參數爆, 結果不一定會爆
    ```haskell
    f :: a -> b -> b
    f a b = b
    f 1 爆 = 爆 -- 還是爆
    f 爆 2 = 2 -- 沒爆
    ```

---
  + Haskell spec 規定採用 non-strict evaluation

--

  + 不過 Non-strict 是一個否定定義的辭

      + 國中時學到的「無理數」的定義: 數線上面, 不是有理數的那些數
      + No-SQL 的定義: ...

--
  + 不是建構性的定義, 我們只知道它不是什麼, 不知道它是什麼, 無從討論它的性質

---

layout: false
class: center, middle

# Lazy evaluation

---

layout: true

.header[Lazy evaluation]

---

## Lazy evaluation 作為學習材料

--

  + 這是符合 Non-strict evaluation 的一種 evaluation

--

  + 可以把 GHC 的作法視為:<br>
    以 lazy evaluation 為湯底, 再加上一堆效能最佳化的料

--

  + GHC 加上了最佳化, 所以運行過程和 lazy evaluation 也不完全一樣,
    只是會確保會被觀察到的東西表現要一致, 然後會比較快.<br>
    (除了記憶體用量不一樣所造成的不一致之外, 應該要同進同退, 該爆的會一起爆, 不該爆的會一起不爆.. 不然應該列為實作的 bug)

--

  + 我覺得用 lazy evaluation 來理解 Haskell evaluation 是個不錯的進入點<br>
    因為它蠻具體的, 而且是建構性的<br>
    (雖然它理論上只是 spec 允許的 non-strict 之中一種特例)

---

## 原則

  + 還能拖著不算就千萬不要算. 沒被外界觀察到的一團運算式就擺在那邊處於爆與不爆之間的糾纏態 (學名叫 thunk)

--

  + 就算非不得已要被觀察到了, 也只算一步, 盡可能吝嗇地要被看到幾分就只算幾分

---

## 糾纏態的一小步．程式的一大步

  + 崩塌(解開)的糾纏態只有兩種狀態
      + 爆
      + 沒爆

--

  + 解開糾纏態的一步, 只到確認它表面上有沒有爆, 就收工了

--

  + 只靠這樣一小步 (確認整個程式的最表面會不會爆)<br>
    就可以把整個程式執行完了

--

  + 雖然上面是這樣說成明確的兩種狀態, 但這不一定是有限時間內能作出的判斷<br>
    (學名 undecidable, halting problem)

--

  + 我們也只能列出什麼叫作結果有出來 (沒爆), 無法建構性地列出所有的爆掉狀態,
    它只是一個虛擬的: 結果出不來的概念

---

## 糾纏態的一小步．程式的一大步

  + 表面已解開 (沒爆) 的情況: (學名叫 weak head normal form)

--

      + 單純的值
        ```haskell
        1, 2, 3, 'A', 'b' -- 單純的值
        .... -- 指標, external buffer 等等外部值.. (用 Haskell 寫不出建構式 ^^|)
        ```

--
      + 表面經確認, 是一個 data constructor (裡面也許還有未解糾纏態就不管了)
        ```haskell
        let a = a in (3+4, a, undefined)
          -- 雖然有還沒算完的 3+4, 有一算下去就沒完的 a, 還有一沾就爆的 undefined
          -- 但是表面經確認是一個 (,,), 所以表面上不爆確認
        ```

--
      + 或, 一個沒還沒吃夠參數的函數
        ```haskell
        let f a b = a + b in f 2
          -- 還少吃一個, 所以這一團糾纏態算是確認表面上沒爆的
        ```

---

## 糾纏態的一小步．程式的一大步

  + 必須繼續往前解的情況 (都是從面對的糾纏態的最表面來看)

--

      + 吃飽的函數, 必須繼續把函數內容展開
        ```haskell
        let f a b = a + b in f 1 2 --> 1 + 2 --> 3
        ```

--
      + 是一個 `case` 或 `if` 或是拆開寫成好幾條的函數, 這種需要決定該往哪邊解的

--
          + 解開它的 target expression, 直到能判斷出要往哪條路走為限

--
          + `case` 或拆開的函數定義.. 從第一條 branch 開始看, 解開要判斷的 expression, 直到能判斷出確定要走這一條, 或確定不能走這一條 (那就再看第二條..)

--
          + `if` 就是把要檢查的 expression 的 `Bool` 糾纏態解到 `True` / `False` 為止<br>
            `if a then b else c` 其實就是 `case a of { True -> b; False -> c }`

--
      + pattern matching 是我們把外部矛盾轉化為內部矛盾的手法<br>
        利用解開糾纏態表面這一小步的動力, 層層接力解開整個程式

---

## 糾纏態的一小步．程式的一大步

  + 正常情況下, 只有要被觀察的糾纏態才有必要解開, 使用 pattern matching 即可.<br>
--
    不過如果我們想在 polymorphism 這種我們根本不知道實際 type 的時候精控計算順序,
    因為不知道 type 所以無法寫下 pattern, Haskell 提供一個例外的 `seq` 函數
    ```haskell
    seq :: a -> b -> b
    ```
    它會把傳進的 `a` 先解開一層, 然後才輸出 `b` 的值 (`a` 解開以後, `b` 才成為表面)<br>
--
  + 有本 [Purely Functional Data Structure by Okasaki](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)
    大量運用這個技巧來實作 real time persistent data structure<br>
    (他用的語法是 Standard ML 而不是 Haskell, 不過他用的是 lazy eval 的特別版,
    行為反而跟 Haskell 一樣, (語法上)不會太難讀)


---

layout: true

.header[Lazy evaluation]

## Lazy Evaluation 實例

```haskell
cfold f z [] = z
cfold f z (x:xs) = f x z (\y -> cfold f y xs)
-- continuation passing style 的 fold
```

---

```haskell
cfold (\a b go -> go (a+b)) 0 (1:2:3:[])
```
---
```haskell
(\a b go -> go (a+b)) 1 0 (\y -> cfold (\a b go -> go (a+b)) y (2:3:[]))
```
---
```haskell
(\y -> cfold (\a b go -> go (a+b)) y (2:3:[])) (1+0)
```
---
```haskell
cfold (\a b go -> go (a+b)) (1+0) (2:3:[])
```
---
```haskell
(\a b go -> go (a+b)) 2 (1+0) (\y -> cfold (\a b go -> go (a+b)) y (3:[]))
```
---
```haskell
(\y -> cfold (\a b go -> go (a+b)) y (3:[])) (2+(1+0))
```
---
```haskell
cfold (\a b go -> go (a+b)) (2+(1+0)) (3:[])
```
---
```haskell
(\a b go -> go (a+b)) 3 (2+(1+0)) (\y -> cfold (\a b go -> go (a+b)) y [])
```
---
```haskell
(\y -> cfold (\a b go -> go (a+b)) y []) (3+(2+(1+0)))
```
---
```haskell
cfold (\a b go -> go (a+b)) (3+(2+(1+0))) []
```
---
```haskell
3+(2+(1+0)) -- (+) 是 strict function, 它的實作會先要求把參數的糾纏態解開
```
---
```haskell
3+(2+1)
```
---
```haskell
3+3
```
---
```haskell
6
```

---

layout: true

.header[Lazy evaluation]

## Lazy Evaluation 實例

```haskell
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _ _ = []

tail (a:as) = as

take 0 _ = []
take _ [] = []
take n (a:as) = a : take (n-1) as

fib = 1 : 1 : zipWith (+) fib (tail fib)
```

---

```haskell
take 4 fib
```
---
```haskell
take 4
  ( let fib = <.in.> -- 這是我自己亂發明的符號, 表示跟 in 後面一串一樣
    in 1 : 1 : zipWith (+) fib (tail fib)
  )
```
---
```haskell
take 4
  ( let fib = <.in.>
    fib1 = 1 : zipWith (+) fib (tail fib)
    in 1 : fib1
  )
```
---
```haskell
1 : take (4-1)
  ( let
    fib = 1 : fib1
    fib1 = 1 : zipWith (+) fib (tail fib)
    in fib1
  )
```
---
```haskell
1 : take 3
  ( let
    fib = 1 : fib1
    fib1 = 1 : zipWith (+) fib (tail fib)
    in fib1
  )
```
---
```haskell
1 : take 3
  ( let
    fib = 1 : fib1
    fib1 = <.in.>
    in 1 : zipWith (+) fib (tail fib)
  )
```
---
```haskell
1 : take 3
  ( let
    fib = 1 : fib1
    fib1 = <.in.>
    fib2 = zipWith (+) fib (tail fib)
    in 1 : fib2
  )
```
---
```haskell
1 : 1 : take (3-1)
  ( let
    fib = 1 : fib1
    fib1 = 1 : fib2
    fib2 = zipWith (+) fib (tail fib)
    in fib2
  )
```
---
```haskell
1 : 1 : take 2
  ( let
    fib = 1 : fib1
    fib1 = 1 : fib2
    fib2 = zipWith (+) fib (tail fib)
    in fib2
  )
```
---
```haskell
1 : 1 : take 2
  ( let
    fib = 1 : fib1
    fib1 = 1 : fib2
    fib2 = <.in.>
    in zipWith (+) fib (tail fib)
  )
```
---
```haskell
1 : 1 : take 2
  ( let
    fib = 1 : fib1
    fib1 = 1 : fib2
    fib2 = <.in.>
    in zipWith (+) (1 : fib1) (tail fib)
  )
```
---
```haskell
1 : 1 : take 2
  ( let
    fib = 1 : fib1 -- fib 沒用了會被 GC 掉
    fib1 = 1 : fib2
    fib2 = <.in.>
    in zipWith (+) (1 : fib1) (tail (1 : fib1))
  )
```
---
```haskell
1 : 1 : take 2
  ( let
    fib1 = 1 : fib2
    fib2 = <.in.>
    in zipWith (+) (1 : fib1) fib1
  )
```
---
```haskell
1 : 1 : take 2
  ( let
    fib1 = 1 : fib2
    fib2 = <.in.>
    in zipWith (+) (1 : fib1) (1 : fib2)
  )
```
---
```haskell
1 : 1 : take 2
  ( let
    fib1 = 1 : fib2
    fib2 = <.in.>
    in (+) 1 1 : zipWith (+) fib1 fib2
  )
```
---
```haskell
1 : 1 : take 2
  ( let
    fib1 = 1 : fib2
    fib2 = <.in.>
    fib3 = zipWith (+) fib1 fib2
    in (+) 1 1 : fib3
  )
```
---
```haskell
1 : 1 : (+) 1 1 : take (2-1)
  ( let
    fib1 = 1 : fib2
    fib2 = (+) 1 1 : fib3
    fib3 = zipWith (+) fib1 fib2
    in fib3
  )
```
---
```haskell
1 : 1 : 2 : take (2-1)
  ( let
    fib1 = 1 : fib2
    fib2 = 2 : fib3 -- 是 ref 同一個 (+) 1 1, 會連動
    fib3 = zipWith (+) fib1 fib2
    in fib3
  )
```
---
```haskell
1 : 1 : 2 : take 1
  ( let
    fib1 = 1 : fib2
    fib2 = 2 : fib3
    fib3 = zipWith (+) fib1 fib2
    in fib3
  )
```
---
```haskell
1 : 1 : 2 : take 1
  ( let
    fib1 = 1 : fib2
    fib2 = 2 : fib3
    fib3 = <.in.>
    in zipWith (+) fib1 fib2
  )
```
---
```haskell
1 : 1 : 2 : take 1
  ( let
    fib1 = 1 : fib2 -- GC
    fib2 = 2 : fib3
    fib3 = <.in.>
    in zipWith (+) (1 : fib2) fib2
  )
```
---
```haskell
1 : 1 : 2 : take 1
  ( let
    fib2 = 2 : fib3
    fib3 = <.in.>
    in zipWith (+) (1 : fib2) (2 : fib3)
  )
```
---
```haskell
1 : 1 : 2 : take 1
  ( let
    fib2 = 2 : fib3
    fib3 = <.in.>
    in (+) 1 2 : zipWith (+) fib2 fib3
  )
```
---
```haskell
1 : 1 : 2 : take 1
  ( let
    fib2 = 2 : fib3
    fib3 = <.in.>
    fib4 = zipWith (+) fib2 fib3
    in (+) 1 2 : fib4
  )
```
---
```haskell
1 : 1 : 2 : (+) 1 2 : take (1-1)
  ( let
    fib2 = 2 : fib3
    fib3 = (+) 1 2 : fib4
    fib4 = zipWith (+) fib2 fib3
    in fib4
  )
```
---
```haskell
1 : 1 : 2 : 3 : take (1-1)
  ( let
    fib2 = 2 : fib3
    fib3 = 3 : fib4
    fib4 = zipWith (+) fib2 fib3
    in fib4
  )
```
---
```haskell
1 : 1 : 2 : 3 : take 0
  ( let
    fib2 = 2 : fib3
    fib3 = 3 : fib4
    fib4 = zipWith (+) fib2 fib3
    in fib4
  )
```
---
```haskell
1 : 1 : 2 : 3 : []
```

---

layout: false
class: center, middle

# Löb

---

layout: true

.header[Löb]

---

One of those functions in Haskell that are amazing, crazy, simple and complicated in equal parts.

 0. The implementation is very simple to write.
 0. The implementation is hard to understand.
 0. It is easy to use.
 0. It is explainable.


  + [參考教學文](https://github.com/quchen/articles/blob/master/loeb-moeb.md)

---

## 先看一眼.. löb..

這是個試算表 (spreadsheet) 的計算引擎

--

(看一眼就好)
```haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x
```

---

  + 假設我們的試算表的計算結果可由 `[a]` 表示 (用比較熟悉的 type 來舉例)

--

  + 我們在格子裡填的東西, 每一格放的是一個 `[a] -> a` 類型的函數

--
      + 通常大部分的格子放的是忽略參數的常數函數 (如下例是直接放 `25`)
        ```haskell
        \_ -> 25
        ```

--
      + 有些會去讀取別的格子來計算
        ```haskell
        \(_ : a1 : a2 : a3 : _) -> (a1 + a2 + a3) / 3
        ```

--

  + 所有在格子裡填的東西合起來用 `fs` 表示
    ```haskell
    fs :: [[a] -> a]
    fs = [...]
    ```

--
  + 所有的運算結果合起來用 `xs` 表示
    ```haskell
    xs :: [a]
    xs = [...]
    ```

---
  + 我們所填的所有東西 `fs` 讀取整個試算表的所有結果 `xs` 算出來的就是整個試算表的結果 `xs`
    ```haskell
    xs = map (\f -> f xs) fs
    ```
    (通常寫成)
    ```haskell
    xs = map ($ xs) fs
    ```

--
  + 我們把上面這個式子包一層皮, 讓它成為 `fs` 的函數<br>
    (我們要寫一個通用的引擎, 是拿 `fs` 當參數, 因為 `fs` 是使用者的輸入)
    ```haskell
    loeb :: [[a] -> a] -> [a]
    loeb fs = xs where
      xs = map ($ xs) fs
    ```

--
  + 我們要用的試算表不一定希望是 `[a]` (它有點慢, 而且一維的試算表很奇怪)<br>
    我們的試算表引擎也沒用到 `[a]` 的所有功能<br>
    (只用到 `map`, 沒有計算長度, 或只對其中一些元素操作等等)<br>
    所以可以把實作再一般化一點

---

## Functor

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

  + 這是個相當抽象的 class (比 `Monad` 抽象)

--
  + 直覺解讀意義很多種

--
  + 這裡我們把它視為「可能用某種形式住著任意不預設限制類型的 `a` 的容器」

--
      + 如果我們只把某個 `f` 當成 `Functor f` 來用<br>
--
        我們只能知道這個 `f a` 裡面可能用某種形式存在著 `a`<br>
--
        可能真的有個 `a`, 可能有很多個, 可能暫時不在了, 也可能它只是精神上的存在, 當你需要它而向它禱告的時候, 會及時地顯靈一下 (等會有例子)
--
      + 我們能作的, 就是只憑信心相信那裡有個 `a`<br>
--
        然後透過這個 `fmap` 把我們準備的函數 (`:: a -> b`) 交給它, 也許它就會吃到傳說中的 `a`..<br>
--
        不過無論如何, 最後我們會拿到由 `f a` 轉換來的 `f b`, 相信原本住在裡面的 `a` 已經依函數轉換為 `b` 了
--
      + 我們只能用相信的, 因為只透過 `Functor f`, 我們根本無從驗證這個 `a` 或是後來的 `b` 是不是真的在那裡.. 也沒機會真的把它拿出來看.<br>
--
        只能留待知道它不僅僅是個 `Functor f` 的人, 才有機會作額外的動作

---

## Functor 的良心要求

```haskell
fmap id [全等] id
fmap (k . j) [全等] fmap k . fmap j
```

  + 這不是 Haskell 的 type system 能確保的事情, 只能列作良心要求

--

  + 使用 `Functor` 的人通常會假設這性質存在, 如果 `instance` `Functor` 的人沒有保持這性質的話, 可能會造成有問題的程式<br>
    (除非只有知道祕密的你自己會用)<br>
--
    (不過三個月後的你也不會記得的... UCCU~)

--
  + 用比較簡單的角度看待 `Functor` 對 `fmap` 的良心要求就是:
      + 在新生出的 `f b` 中, 不能增減 `f a` 裡面 `a` 的數量, 生出的 `b` 要跟原有的 `a` 一樣多
      + 新生出的 `f b`, 其結構要跟原本的 `f a` 一樣, 唯一可能不一樣的, 就是被傳入的函數動到的內容物 `a`, 而容納放置的方式要一樣

---

## 一些 Functor 常見 (Prelude) 的 instances

```haskell
instance Functor [] where
  fmap = map
  -- 或
  fmap _ [] = []
  fmap f (b:bs) = f b : fmap f bs
  -- 有時會看到這樣寫的, 為了節省傳遞 f 的動作
  fmap f = go where
    go [] = []
    go (b:bs) = f b : go bs
```

--
```haskell
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)
```

--
```haskell
instance Functor ((->) r) where
  -- 吃某不特定 r 的函數, 此 (->) 是 type level 的 binary op
  fmap f k = f . k
    -- 利用 f :: a -> b 從一個 k :: r -> a 生出 r -> b
```
這一個就是精神上存在 `a`, 只有向它禱告的時候才會及時顯靈一下的 `Functor`

--

常見還有 `Functor IO`, `Functor (Either a)`, `Functor ((,) a)` 沒新意就不提細節了

---

## 無聊到有趣的 Functor instance

```haskell
data Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
```
相當於只有 `Just a` 的 `Maybe a`

--
## 詐騙型的 Functor instance

```haskell
data Constant a b = Constant a
instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a
  -- 或 fmap _ = id 或 fmap = flip const 或 fmap = const id
```
相當於只有 `Nothing` 的 `Maybe a`<br>

我們一直相信的 `a` (這邊程式碼裡面對應的是 `b`) 根本從來就不曾存在...<br>
我們向 `fmap` 的禱告一直都只是幻覺....

---
## 無聊與詐騙型的 Functor instances

  + 無聊或詐騙型的設計, 通常不會直接拿來用 (根本無用), 但是可以點綴這個世界

--

      + 補齊數學世界失落的一角 (不過工程師不在意這個..)
--

      + 讓我們別處作更諧調的設計, 採取漂亮對稱的通則 (就會 highly reusable),
        然後使用時適時地塞進這種極端小物, 作出特例效果

---

## 實際試算表會用到的 Functor instance

```haskell
instance Functor (Array (Int, Int))
  -- 以 (Int, Int) 為坐標的二維 random access array
```

--

## Functor 之下的 löb

```haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs

-- 對照一下一般化以前的樣子
-- loeb :: [[a] -> a] -> [a]
-- loeb fs = xs where xs = map ($ xs) fs

-- 那篇教學連結裡用的寫法, 可能他想耍一下神祕..
-- loeb x = go where go = fmap ($ go) x
```

---

## 應用實例

```haskell
import Data.Array
import Data.List
import Control.Monad
import Text.Printf

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

-- Empty cell
e = val 0

-- Simple cell value
val = const -- const a b = a

-- VAT of a cell's contents (10 %)
vat ix = (* 0.1) . (! ix) -- (!) 是 array 取元素的函數, 這邊 ix 是坐標

-- Sum of the values at a list of indices
sum' ixs = \arr -> foldl' (\acc ix -> acc + arr ! ix) 0 ixs

printArr :: Array (Int, Int) Double -> IO ()
printArr arr =
  forM_ [0..4] $ \i -> do -- 我覺得這樣用蠻像迴圈的
    forM_ [0..4] $ \j ->
      printf "%4.1f   " (arr ! (i,j))
    printf "\n"
```

---

## 應用實例

```haskell
spreadsheet = listArray ((0,0), (4,4))
-- Prices | VAT        | Effective prices + total
  [ val 1,   vat (0,0),   sum' [(0,i) | i <- [0..1]],   e,   e
  , val 3,   vat (1,0),   sum' [(1,i) | i <- [0..1]],   e,   e
  , val 5,   vat (2,0),   sum' [(2,i) | i <- [0..1]],   e,   e
  , val 2,   vat (3,0),   sum' [(3,i) | i <- [0..1]],   e,   e
  ,     e,           e,   sum' [(i,2) | i <- [0..3]],   e,   e
  ]

main = printArr $ loeb spreadsheet
```

## Output

```nohighlight
 1.0    0.1    1.1    0.0    0.0
 3.0    0.3    3.3    0.0    0.0
 5.0    0.5    5.5    0.0    0.0
 2.0    0.2    2.2    0.0    0.0
 0.0    0.0   12.1    0.0    0.0
```

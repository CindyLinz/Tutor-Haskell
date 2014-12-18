class: center, middle, inverse

# Tutor of Haskell - start
## CindyLinz

2014.12.17

---

layout: false
class: center, middle

# 安裝 GHC - Glasgow Haskell Compiler

---

layout: true

.header[安裝 GHC - Glasgow Haskell Compiler]

---

## 建議版本 7.8

+ (今天是 2014.12.17)

+ 建議從 7.8 開始用..

+ GHC 7.8 有幾個我覺得重要的新 extension, 使 Haskell 語言環境更完備

  (可能更好學)


---

## 各 Distro 現狀

+ Linux Ubuntu
  + .del[current version 14.10 Utopic 是 7.6.3]
  + .del[next version 15.04 Vivid 是 7.6.3]

+ Linux Debian
  + .del[experimental] 是 7.8
  + .del[testing, unstable 是 7.6.3]
  + .del[stable 是 7.4.1]

+ Linux Arch 是 7.8.3

+ .del[MacPorts 是 7.6.3]

+ Mac homebrew 從 [braumeister](http://braumeister.org/formula/ghc) 查是 7.8.3

--

(其實我目前是跟 7.6 比較熟.....)

---

## 從 GHC 官網安裝

+ 從 [GHC 官網下載頁](https://www.haskell.org/ghc/download) 下載適用自己平台的 GHC binary (建議用 7.8 或以上)
+ 依下載的格式 tar.bz2 或 tar.xz 解開
+ 進入解開的目錄裡面安裝<br>
  (可以不用 root, 除非你想裝在 /usr/local 之類需要 root 權限的地方)
  ```shell
  $ ./configure --prefix=[想安裝的目錄]
  $ make install
  ```
+ 應該會獲得下列檔案
  + `[想安裝的目錄]/bin/ghc`
  + `[想安裝的目錄]/bin/runghc`
  + `[想安裝的目錄]/bin/ghci`
+ 把 `[想安裝的目錄]/bin` 加到環境變數 `PATH` 裡面 (可以要用的時候再設)

---

layout: false

class: center, middle

# Hello World

---

layout: true

.header[Hello World]

---

## source

+ `hello_world.hs`
  ```haskell
  main = putStrLn "Hello, world!"
  ```

---

## 先編譯再執行

+ compile
  ```shell
  $ ghc --make hello_world
  ```
  或
  ```shell
  $ ghc --make hello_world.hs
  ```

+ 產生的檔案
  + `hello_world.hi`
  + `hello_world.o`

+ execute
  ```shell
  $ ./hello_world
  ```

+ output
  ```text
  Hello, world!
  ```

---

## 或直接執行

+ run
  ```shell
  $ runghc hello
  ```
  或
  ```shell
  $ runghc hello.hs
  ```

+ output
  ```text
  Hello, world!
  ```

+ 不會額外產生檔案

---

## 也可以用中文 (UTF-8)

+ source
  ```haskell
  main = putStrLn "哈囉, 握的！"
  ```

+ output
  ```text
  哈囉, 握的！
  ```

---

layout: false

class: center, middle

# Basic Scripting

---

layout: true

.header[Basic Scripting]

---

## 加法程式

+ source (縮排式, 較常見)
  ```haskell
  main = do
      a <- getLine
      b <- getLine
      putStrLn (a ++ " + " ++ b ++ " = " ++ show (read a + read b))
  ```
  + operator priority 比函數 apply 低
  + read a + read b 相當於 (read a) + (read b)

+ source (大括弧+分號式)
  ```haskell
  main = do {
      a <- getLine;
      b <- getLine;
      putStrLn (a ++ " + " ++ b ++ " = " ++ show (read a + read b));
  }
  ```
  (最後一個分號可省略)

---

## Input / Output

+ `putStr`, `putStrLn` 印出字串到 `stdout`
  ```haskell
  putStrLn "字串" -- 會換行
  putStr "字串" -- 不會換行 (注意, 印到 console default 是 line buffer)
  ```

--

+ `getLine` 從 `stdin` 讀取一行
  ```haskell
  line <- getLine
  ```

--

+ `show` 把能印的 data 變成字串
  ```haskell
  putStrLn (show 12345)
  ```

+ `read` 把字串變成 data
  ```haskell
  line <- getLine
  putStrLn (show (123 + read line))
  ```

---

## Statement / Expression (我自己掰的講法)

+ Statement 是一種環境裡的運算, 要擺在環境中使用, 它可能會跟環境互動, 獲取一些它需要的資訊, 並且改變環境, 運算結束後會有一個新環境

  + 除了影響環境以外, 可以從 Statement 取出運算的結果, 方便組裝出更大的 Statement
  + 有的 Statement 只需要影響環境, 我們不在意它的結果

--

+ Statement 的例子
  + `putStrLn "Hello"` -- 從沒有顯示 Hello 的環境變成顯示 Hello 的環境
  + `a <- getLine` -- 從環境讀取一行字 (而且改變 read offset), 結果命名為 `a`
  + `main` -- 整個 `main` 是一個 Statement. 程式開始執行的時候 Haskell runtime 會把它放到整個大環境裡面運算

--

+ 多個同環境的 Statement 依序組合在一起用 `do` block 來寫<br>
  (縮排式 or 大括弧+分號式)

---

## Statement / Expression (我自己掰的講法)

+ Expression 不需要環境, 也不影響環境. 無論什麼時候取用它, 它都是同一個結果.

--

+ Expression 的例子
  + `"Hello"` -- 字串 Hello
  + `read a` -- a 字串所對應的值
  + `read a + read b` -- (a 字串的值與 b 字串的值) 的和

--

+ Expression 可以直接組裝成更大的 Expression<br>
  (像上面的 `read a + read b`)

---

## Statement / Expression (我自己掰的講法)

+ Statement 是 Expression 的一種:

--

  + 將 Statement 運算前的環境化作 Expression 的一個參數

--

  + 運算後的環境化作 Expression 結果中的一個項目

--

  + Statement 就是一種 Expression 的特例

--

  + Haskell 就是這樣定義 Statement 的

--

  + 以後會看到各種官方或民間定義的 Statement 環境 (學名叫作 monad)

---

## 呼叫函數

呼叫函數 f, 參數 a

+ Haskell, CoffeeScript, LiveScript, Perl
  ```haskell
  f a
  ```

+ C, C++, Java, Javascript, Python
  ```c
  f(a)
  ```

---

## 呼叫函數

呼叫函數 f, 參數 a 和 b

+ Haskell
  ```haskell
  f a b
  ```

+ CoffeeScript, LiveScript, Perl
  ```perl
  f a, b
  ```

+ C, C++, Java, Javascript, Python
  ```c
  f(a, b)
  ```

---

## 呼叫函數

呼叫函數 f, 沒有參數

+ Haskell, Perl
  ```haskell
  f
  ```

+ LiveScript
  ```livescript
  f!
  ```

+ C, C++, Java, Javascript, CoffeeScript, Python
  ```c
  f()
  ```

---

## 呼叫函數

呼叫函數 f, 複雜一點的參數

+ Haskell
  ```haskell
  f a (g b)
  h (k c) d
  ```

+ CoffeeScript, LiveScript, Perl
  ```perl
  f a, g b
  h k(c), d
  ```

+ C, C++, Java, Javascript, Python
  ```c
  f(a, g(b))
  h(k(c), d)
  ```

---

## 呼叫函數

呼叫函數 (給參數 a), 以及它所 return 的函數 (給參數 b)

+ Haskell
  ```haskell
  (f a) b
  ```
  或 (較常見, 但容易卡住新手的寫法)
  ```haskell
  f a b
  ```

+ Perl
  ```perl
  f(a)->(b)
  ```

+ C, C++, Java, Javascript, CoffeeScript, LiveScript, Python
  ```c
  f(a)(b)
  ```

---

## 呼叫函數

先給一部分參數, 剩下的以後再給 (假設 f 是一個可以吃 2 個參數的函數, 先給 1 個)

+ Haskell
  ```haskell
  f a
  ```

+ C, C++, Java, Javascript, CoffeeScript, LiveScript, Perl, Python
  ```perl
  ...
  ```
  基本上不行, 只能發揮想像力模擬;<br>
  通常會在一般 function 之外額外發明一個叫作 curry function 的東西專門用來作 partial application

---

## 解一元二次方程式

```haskell
  main = do
    putStrLn "input a b c for a*x^2 + b*x + c = 0"

    line <- getLine
    let [a, b, c] = words line

    putStrLn ("got a=" ++ a ++ " b=" ++ b ++ " c=" ++ c)

    let
      a0 = read a
      b0 = read b
      c0 = read c
      det0 = b0 * b0 - 4 * a0 * c0
      r0 = -b0 / (2 * a0)

    if det0 < 0
      then putStrLn "沒有實根"
      else if det0 == 0
        then putStrLn (show r0)
        else putStrLn
          (  show (r0 + sqrt det0 / (2 * a0))
          ++ ", "
          ++ show (r0 - sqrt det0 / (2 * a0))
          )
```

---

## 條件判斷

+ if [一團結果為 Bool 的 expression]<br>
  then [一團結果為 某a型別 的 expression]<br>
  else [一團結果也為 某a型別 的 expression]

--

+ statement 也是 expression, 由我們看待它的角度決定

--

+ 一定要有 else, 而且其結果的型別要跟 then 一樣

--

+ 排版不重要

--

  + 不過我們常常看到 then 和 else 會往後縮, 那是舊版 Haskell 的 .del[bug] feature.

--

+ .del[if..then..else 在 Haskell 其實是個可有可無的功能]

---

## 定義函數

+ 定義一個給一個 Expression 用
  ```haskell
  let sqr x = x * x in sqr a + sqr b
  ```
  這邊整個是一個 expression, 可以用括號把整坨裝起來塞在任何應該放 expression 的地方

--

+ 定義一組給一個 Expression 用 (縮排式)
  ```haskell
  let
      a = ...
      b = ...
  in ...
  ```
  定義一組給一個 Expression 用 (大括弧+分號式) 最後一個分號可有可無
  ```haskell
  let {
      a = ...;
      b = ...;
  } in ...
  ```

---

## 定義函數

+ 在 Statement 環境裡面, 定義一個給後半環境的 Statement 使用
  ```haskell
  let add a b = a + b
  ```
+ 一次定義多個 (縮排式)
  ```haskell
  let
      add a b = a + b
      y = 5
  ```
+ 一次定義多個 (大括弧+分號式)
  ```haskell
  let {
      add a b = a + b;
      y = 5;
  } -- 最後一個分號可有可無
  ```

---

## 定義函數

+ lambda 定義方式
  ```haskell
  let add = \a b -> a + b in ...

  let add a = \b -> a + b in ...

  let add = \a -> \b -> a + b in ...

  let add = \a -> let ad b = a + b in ad in ...
  ```

--

+ partial application 定義方式
  ```haskell
  let add a b = a + b
  in
      let ad b = add 1
      in ad 2
  -- 3
  ```

--

+ 不用吃參數的函數就是值, 就是 (數學意義的) 變數 / (程式意義的) 常數

--

+ 組內定義可互相引用, 也可以引用自己. 定義與引用的順序無限制

---

## Script 風的 Haskell

+ 這不是 Haskell 的全貌

--

+ 只有隨手亂寫的 Haskell 程式, 或是學術論文裡的範例才會整個程式都長這樣

--

+ 在大程式裡面, 仍會大量出現在字裡行間 (維護時不具有關鍵提示資訊的部分, 就可以亂寫)

--

+ 參考一下 [Yesod router 的 source](https://github.com/yesodweb/yesod/blob/master/yesod-core/Yesod/Routes/Parse.hs)

  + Yesod 之於 Haskell; 猶如 Rails 之於 Ruby
  + 不一樣的是 Yesod 與外部 library 的相容性很好, 而且社群很和善不具攻擊性, 同質性的競爭的 framework author 甚至互相具體建議你考慮對方的優點, 依使用者自己的個性作破碎型混搭
  + [StackOverflow 討論串: Snap 與 Yesod 的比較](http://stackoverflow.com/questions/5645168/comparing-haskells-snap-and-yesod-web-frameworks)

---

layout: false

class: center, middle

# The Next Step

---

layout: true

.header[The Next Step]

---

## 重要拼圖

+ 標 type

+ 定義資料結構

+ 分解資料結構 (pattern matching)

+ 多型 (好幾種多型.....)

+ class/instance (跟 Java/C++ 的 class 不一樣, 只是名字一樣)

+ import / export module

--

## 次要 (但實用) 的拼圖

+ 一些方便的簡寫 (syntax sugar)

+ Template Haskell (可自訂語法的巨集.. 且可以讀取已經 parse 過的程式碼)

+ hackage / cabal - third party library 集合

---

## 兩條 track

+ 對正確性比較有興趣?

<!--
  + type theory
  + 抽象結構 - monoid, foldable, functor, traversable, applicative, monad, arrow
-->

+ 對執行過程與效能比較有興趣?

<!--
  + lazy evaluation
  + infinite list
  + GHC runtime
  + FFI
  r rewrite rule
-->


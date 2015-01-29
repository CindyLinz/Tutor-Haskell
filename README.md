# Tutor of Haskell

在某個 study group 準備的教材.

歡迎批評指教, 也歡迎問問題 (可以開 issue 當留言板.. XD)

**一邊準備, 一邊使用, 一邊還在摸索怎麼作比較好.**

**想拿來自學的話, 如果覺得讀起來很吃力.. 可能是我的問題不是你的問題 囧**

  + [2014.12.17 start](http://cindylinz.github.io/Tutor-Haskell?volume=start)
      + 最新版 (現在是 7.8) GHC 安裝
      + 一些會動的, hello world 等級的 haskell 範例程式碼, 與部分(不完整的)解釋

  + [2014.12.24 syntax](http://cindylinz.github.io/Tutor-Haskell?volume=syntax)
      + type signature 語法 (物件多型 (ad hoc polymorphism) 與泛型多型 (parametric polymorphism) 比較)
      + 定義 data type 語法 (GADT, 含一點點 data kind)
      + data construct / destruct (pattern matching) 語法
      + class / instance 語法 (含 functional dependency, 和一點點 type family)
      + operator 語法
      + 這一次開始想儘快把最必要的語法掃過一遍, 將枯燥的東西趕快帶過去, 以後再來主題性地看組合應用.

  + [2014.12.31 syntax2](http://cindylinz.github.io/Tutor-Haskell?volume=syntax2)
      + record syntax
      + existential type
      + pattern synonym
      + view patterns
      + guard, pattern guard, multiway if
      + do-notation

  + [2015.1.7 lazy](http://cindylinz.github.io/Tutor-Haskell?volume=lazy)
      + Haskell 無窮 List 展示
      + Evaluation Strategy 介紹 (Applicative order, Normal order.. / Strict, Non-strict)
      + Lazy evaluation 介紹, 展示
      + Löb 介紹, 建構原理, 用法展示
      + Functor 介紹, instance 展示

  + [2015.1.14 monad](http://cindylinz.github.io/Tutor-Haskell?volume=monad)
      + monad paradox, monad definition, monad laws
      + 經典款 monad instance, 資料結構與實作
      + (是以討論互動的方式進行, 直接閱讀可能用處不大)

  + [2015.1.28 lens (begining)](http://cindylinz.github.io/Tutor-Haskell?volume=lens)
      + 從問題情境介紹

        ```haskell
        type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
        ```

      + 介紹部分 helper, 使用情境
      + 沿伸閱讀材料

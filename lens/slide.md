class: center, middle, inverse

# Tutor of Haskell - lens (beginning)
## CindyLinz

2015.1.28

---

layout: false
class: center, middle

# Lens 是..

---
layout: false
.header[Lens 是..]

  + 是一種 design pattern

--

  + 一個概念, 一種看待資料的觀點

--

      + lens 字譯叫作「透鏡」

--
      + 可以串接: 有的負責高倍率, 有的負責擴大光圈, 有的利用繞射, 有的是黑洞重力透鏡 (一次看到 N 重虛像)

--
      + 我們可以 zoom-in 觀察/修改一個整體的局部

--
      + 有時也可以從局部看到整體....ww<br>
        現實物理的類比... 想像我們拿望遠鏡往天空看, 應該只能看到天空的一小部分,
        結果剛好看到一顆蟲洞的出口, 並且從這個出口看進去看到了我們的地球...

--
  + lens 的數學特性很強 (XX律, YY律很多), 如果我們一大堆各種奇怪的 lens 都設計成同一個「形狀」, 其組合連續技非常厲害

--

      + 如果精通這些 XX律, 培養出對它們的強烈直覺, 會進入另一個境界. 看程式的感覺會不一樣, 會看到別人看不到的東西..

---
layout: false
class: center, middle

# 具體到抽象
# 我們來建構 lens

---
layout: true
.header[具體到抽象 我們來建構 lens]

---

## 一個資料存取的情境

--

  + ```haskell
    getUserName :: User -> Text
    setUserName :: User -> Text -> User

    originName = getUserName user
    userNamedCindy = setUserName user "Cindy"
    ```

--

  + ```haskell
    getRoomUser :: Room -> Int -> User
    setRoomUser :: Room -> Int -> User -> Room

    originUser2 = getRoomUser room 2
    roomWithAnotherUser2 = setRoomUser room 2 anotherUser
    ```

--

  + ```haskell
    getLobbyRoom :: Lobby -> Int -> Room
    setLobbyRoom :: Lobby -> Int -> Room -> Lobby

    originRoom5 = getLobbyRoom lobby 5
    lobbyWithAnotherRoom5 = setLobbyRoom lobby 5 anotherRoom
    ```

---

  + 如果 lobby 中某房間裡的一個 user 想改名字...

--

      + 不額外想變數名的寫法...
        ```haskell
        setLobbyRoomUserName :: Lobby -> Int -> Int -> Text -> Lobby
        setLobbyRoomUserName initLobby roomPos userPos newName =
          setLobbyRoom initLobby roomPos
            ( setRoomUser (getLobbyRoom roomPos) userPos
              ( setUserName
                (getRoomUser (getLobbyRoom roomPos) userPos)
                newName
              )
            )
        ```

--
      + 額外想變數名的寫法...
        ```haskell
        setLobbyRoomUserName :: Lobby -> Int -> Int -> Text -> Lobby
        setLobbyRoomUserName initLobby roomPos userPos newName =
          let
            initUser = getRoomUser initRoom userPos
            finalUser = setUserName initUser newName
            initRoom = getLobbyRoom initLobby roomPos
            finalRoom = setRoomUser initRoom userPos finalUser
            finalLobby = setLobbyRoom initLobby roomPos finalRoom
          in
            finalLobby
        ```

--
      + 不知道該說哪個比較糟.. 前者有很多重複程式, 後者 `init` 和 `final` 容易填錯

---
## The Functional Way..

  + 剛剛如此悲劇, 是發生什麼事?
    XD

--
  + 我們剛剛定義 `get` 與 `set`, 然後用它們組合出 `update` 的效果<br>
    (room 與 lobby 的部分)

--
  + 這是 non-functional way..<br>
    `get` 與 `set` 所拿到的參教都是目標物「東西」(data), 而不是一個「改變」(function)

--
  + 而 functional way 是定義 `update`, 然後拿 `update` 作 `get` 與 `set` 之用. 這 `update` 吃的是一個「改變」(function)

--
  + ```haskell
    updateUserName :: (Text -> Text) -> User -> User
    updateRoomUser :: Int -> (User -> User) -> Room -> Room
    updateLobbyRoom :: Int -> (Room -> Room) -> Lobby -> Lobby

    setLobbyRoomUserName :: Lobby -> Int -> Int -> Text -> Lobby
    setLobbyRoomUserName initLobby roomPos userPos newName =
      updateLobbyRoom roomPos
        ( updateRoomUser userPos
          (updateUserName (const newName))
        )
        initLobby
    ```

---
  + 剛剛說... 如果我們注意「形狀」的話, 可以變成厲害的組合連續技..
    ```haskell
    setLobbyRoomUserName :: Int -> Int -> Text -> Lobby -> Lobby
    setLobbyRoomUserName roomPos userPos newName =
      updateLobbyRoom roomPos .
      updateRoomUser userPos .
      updateUserName $
      const newName
    ```

--
  + 這邊的
    ```haskell
    updateLobbyRoom roomPos :: (Room -> Room) -> Lobby -> Lobby
    updateRoomUser userPos :: (User -> User) -> Room -> Room
    updateUserName :: (Text -> Text) -> User -> User
    ```
    就是 lens, 它們的 type 長相有某種共通性..

---
## 小結一下我們得到了什麼:

  + lens 可以串起來變成複雜的 lens (還是 lens)
    ```haskell
    updateRoomUserName :: Int -> (Text -> Text) -> User -> User
    updateRoomUserName userPos = updateRoomUser userPos . updateUserName

    updateLobbyRoomUser :: Int -> Int -> (User -> User) -> Lobby -> Lobby
    updateLobbyRoomUser roomPos userPos =
      updateLobbyRoom roomPos . updateRoomUser userPos
    ```

--
  + 透過 lens, 讓我們可以 focus 處理局部 (例如 user),<br>
    而整體會自動反應 (例如 lobby)

--
  + 可以當 `set` 用, 我們放進的「改變」無視原始值即可

--
  + 也可以當 `get` 用, 我們放進的「改變」可以從參數看到這個「局部」<br>
    但是只能當場調閱, 不能影印帶出來..

--
  + 我們的「改變」只能是 pure 的, 不能帶小抄, 帶手錶進去參考, 不能帶骰子進去輔助猜答案, 也不能從裡面對外打 pass

---
layout: false
class: center, middle

# 把成像投影到另一個世界的 lens

---
layout: true

.header[把成像投影到另一個世界的 lens]

## Functor 裡的影子

```haskell
updateLobbyRoomF :: Functor f => Int -> (Room -> f Room) -> Lobby -> f Lobby
updateRoomUserF :: Functor f => Int -> (User -> f User) -> Room -> f Room
updateUserNameF :: Functor f => (Text -> f Text) -> User -> f User
```

---

(用 `set` 和 `get` 來舉例實作, 因為我沒有舉例 `Room` 的實際資料結構, 沒辦法直接拿它的結構實作)
```haskell
updateRoomUserF userPos change room =
  fmap (setRoomUser room userPos) (change (getRoomUser room userPos))
```

--
  + 這是個可以有 side-effect 的
    「改變」

--
  + Haskell 的 side-effect 是相對的. 如果我們視原值與影子的對映關係為 primary-effect, 那麼 `Functor` 的長相就是 side-effect

--
  + 如果我們把整個 `Functor` 的長相也視為 primary-effect, 那麼它就不是
    side-effect

--
  + 從絕對 side-effect 的角度來討論, 根據我們放入的 `Functor` 是誰, 有的是 impure (例 `IO`), 有的是 pure (例 `Maybe`, `Identity`)

---
  + 偷看手錶的「改變」
    ```haskell
    setUserNameAsTime :: Int -> Int -> Lobby -> IO Lobby
    setUserNameAsTime roomPos userPos =
      updateLobbyRoomF roomPos .
      updateRoomUserF userPos .
      updateUserNameF $ \_ -> do
        now <- getCurrentTime
        return $ pack (show now)
    ```

---
  + 對外打 pass 的「改變」
    ```haskell
    printUserName :: Int -> Int -> Lobby -> IO Lobby
    printUserName roomPos userPos =
      updateLobbyRoomF roomPos .
      updateRoomUserF userPos .
      updateUserNameF $ \name -> do
        putStrLn $ unpack name
        return name
    ```

---
  + 仍可以當 `set` 使用, 把成像投影回原來的世界
    ```haskell
    newtype Identity a = Identity {runIdentity :: a}
      -- Identity :: a -> Identity a
      -- runIdentity :: Identity a -> a

    setUserName :: Int -> Int -> Text -> Lobby -> Lobby
    setUserName roomPos userPos newName =
      (runIdentity .) $
        updateLobbyRoomF roomPos .
        updateRoomUserF userPos .
        updateUserNameF (const $ Identity newName)
    ```

---
  + 這次可以真的像 `get` 一樣使用, 把從 lens 裡看到的東西傳出來..
    ```haskell
    newtype Const a b = Const {getConst :: a}
      -- Const :: a -> Const a b
      -- getConst :: Const a b -> a

    getUserName :: Int -> Int -> Lobby -> Text
    getUserName roomPos userPos =
      (getConst .) $
        updateLobbyRoomF roomPos .
        updateRoomUserF userPos .
        updateUserNameF Const
    ```
    (注意, 從外部看, 這是 pure 的..)

--
    這是我第一次看到 `Const a b` 這種 `Functor` 正經應用的地方<br>
    以前以為它只是個幽默一下的玩具

---
  + 前面這些不一樣的用法, 中間用的是完全一樣的「透鏡」組合, 只有最前面和最後面的「物鏡」「目鏡」不一樣<br>
    (不過「物鏡」「目鏡」不算在這邊特殊定義的 lens 的範圍)

--
  + 這是一個很棒的分權 / 再利用 / DRY 的設計:

--
      + 各 lens 的實作跟著自己針對的那一截資料結構走, 不須考慮會被怎麼利用或前後會跟誰接<br>
        (例如 `updateRoomUserF` 只須考慮怎麼從 `Room` 到 `User`,<br>
        不用考慮這個 `Room` 會被裝在哪裡, 也不用考慮 `User` 裡面會有什麼;<br>
        也不用考慮會被用在 pure 的地方還是 impure 的地方, 或是什麼詭異的地方)

--
      + 使用 lens 的部分不用考慮這 lens 行經什麼途徑, 只要認得頭跟尾, 還有使用的環境就可以了

---
layout: false
class: center, middle

# 成像扭曲的 lens

---
layout: true

.header[成像扭曲的 lens]

---

## 「改變」的結果不一定要跟原本的東西同 type

```haskell
_2 :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
_2 g (c, a) = fmap (c,) (g a)
```
--

  + 局部: `a -> b` (映像的部分)

  + 整體: `(c, a) -> (c, b)` (映像的部分)

--

```haskell
secondToLength :: (a, String) -> (a, Int)
secondToLength =
  runIdentity . _2 (Identity . length)
```

---

## 更推廣的 lens type 全貌

```haskell
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
```

--

  + 「整體」與「部分」是一個方便想像的譬喻,<br>
    而實際上是只要可以對應得出來的東西都行..

---
layout: false
class: center, middle

# 望遠鏡鏡架
# lens 用的輔助工具

---

layout: true

.header[望遠鏡鏡架 - lens 用的輔助工具]

---

## 組合完整的望遠鏡

  + 我們現在可以有許許多多的 lens 透鏡.<br>
    直接使用裸著的一疊透鏡.... 能用, 但不夠方便

--
  + Lens 有一致的標準規格, 所以可以準備好一組萬用的望遠鏡鏡架

---

## 部分鏡架零件展示

```haskell
set :: Lens s t a b -> b -> s -> t
set lens value =
  runIdentity . lens (const (Identity value))
(.~) = set
infixr 4 .~

get :: Lens s t a b -> s -> a
get lens = getConst . lens Const
(^.) = get
infixl 8 ^.

update :: Lens s t a b -> (a -> b) -> s -> t
update lens f =
  runIdentity . lens (Identity . f)
(%~) = update
infixr 4 (%~)

(&) :: a -> (a -> b) -> b
a & f = f a
infixl 1 &
```

---

## 使用範例

(沒有提到過的 lens 就... 麻煩意會一下 ^^|)
```haskell
("hello", "world") & _2 .~ 42 -- ("hello", 42)
("hello", "world") & _2 .~ 42 & _1 .~ "abc" -- ("abc", 42) 一次並用兩組在不同部分
("hello", "world") & both .~ "XD" -- ("XD", "XD") 這個 both 是雙筒繞射透鏡 XD
("hello", "world") & both %~ head -- ('h', 'w')
("hello", "world") ^. _2 -- "world"
("hello",("world","!!!"))^._2._1 -- "world" 一次串用兩個 lens

body & find "form" . eq 0 . name "password" . val .~ "pa55w0rd" -- 偽 jQuery
```

---
layout: false
class: center, middle

# More..

---
layout: true

.header[More..]

## 加強表面功夫

---

--

  + 讀 [Edward Kmett](https://github.com/ekmett) 的
    [lens package](https://hackage.haskell.org/package/lens)
    除了讀說明, 也要讀程式碼

--
  + Edward Kmett 不是嫡系學院理論派, 他原本是工程師背景, 後來才學 FP / Haskell 並走進理論世界.<br>
    所以他設計的 library 不會只注重理論正確<br>
    (computable / decidable ... in finite time)<br>
    而會用起來很流暢, 熟悉上手以後, 寫東西就像在寫詩一樣....<br>
    (<span style=text-decoration:line-through>啦啦我也是..</span>)

---
  + 為每一個常見 Monad 環境打造的
    helper

--
  + 更多方便的 [operator](https://github.com/ekmett/lens/wiki/Operators)<br>
    (priority 設計巧妙, 用起來才順; 形狀設計巧妙, 才直覺記得住, 用起來美得像幅畫)

--
  + 其他類似 lens 這樣概念的東西...
    ```haskell
    type Traversal s t a b =
      forall f. Applicative f => (a -> f b) -> s -> f t

    type Prism s t a b =
      forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

    type Iso s t a b =
      forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

    type IndexedLens i s t a b =
      forall f p. (Indexable i p, Functor f) => p a (f b) -> s -> f t

    ...
    ```

---
  + 各種對這些 lens 或資料結構或使用方式作形容描述的 class (超多...)
    ```haskell
    class Contravariant f
    class Choice p,
    class Profunctor p
    class Conjoined p,
    class Representable p
    ...
    ```

--
  + 這些表面表面功夫.. 把它們翻一翻看一看記一記, 很快就可以看起來很厲害.

      + 可以寫看起來很厲害的程式寫法

      + 可以拿一堆專有名辭去嚇唬人.. (X<br>
        一句話裡面 4 個關鍵字, 很有氣勢...ww

        ```html
        A monad is just a monoid in the category of endofunctors
        ```

      + 對自己開發程式的幫助不會太大.. (X

      + 用起來會像別的語言的 optional, promise, coroutine.<br>
        本身有用處, 但是只能用到這裡.

---
layout: true

.header[More..]

## 加強心法

---

  + 培養自己對這些性質,
    數學結構的強烈直覺

      + 看出隱身在茫茫資料結構裡面的 lens

--
  + 培養 lens 之眼

      + 面對要處理的目標問題, 從 lens 的角度來想.
        只考慮我們需要怎樣意義的東西, 不用再在 record 結構, 語法層次, 甚至 big/little endian 打轉.<br>
        (面對低階的資料結構, 依然還是考慮東西的原貌, bit 要怎麼排列.. 以及把實際要用的最低階 lens 實作出來)

--
  + 看什麼都是函數

      + 從 `get`+`set` → `update` 變成 `update` → `get`+`set`

      + 程式是許多 `update` 接起來的

      + 開始去考慮這些 `update` 有什麼性質, 然後把這種直覺變敏銳

---
  + 如果表面功夫花很多時間, 這邊可能會花更多時間..
    但是值得 ^^

--
  + 我通常是拿表面功夫的東西當作素材, 然後開始參......<br>
    讀一些參一些, 再讀一些再參一些...

--
  + 然後再來想程式寫程式, 會漸漸有異樣的感覺..

--
      + 可以寫出很短的程式 (不是為了故意寫短硬擠的那種 XD)

--
      + 會看到可以 reuse 的東西.. 發現以前覺得不一樣的東西明明就是一樣的

--
      + 會更容易講出程式碼之間的故事, 可以說得出正確性, 會比較好維護

---
layout: true

.header[More..]

## 我還在路上.. XD

---

(幻想 &amp; 自我期許文... XD)
<center>
```html
我初學程式, 那可意氣風發了
說什麼 Object Oriented, Design Patterns, Agile programming
當真是不要臉的胡吹法螺
直到後來修習 Haskell, 才慢慢悟到了人生妙諦
其後勤修 Type Theory, 數年之後, 終於明白了天人化生, 萬物滋長的要道
```
</center>
.right[~~ CindyLinz 2013.12.15]

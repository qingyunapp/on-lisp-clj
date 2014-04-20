(ns on-lisp.chapter-8
  "On Lisp サンプルマクロの Clojure 実装。
   野田開さんが翻訳された草稿の PDF 版をベースとする。
   PDF 版配布先: http://www.asahi-net.or.jp/~kc7k-nd/

   ここには
        第 8 章 変数捕捉
   に掲載されているサンプルの Clojure 実装を置く。")

;;;
;;; 8.1 マクロ引数の捕捉
;;;

;; p75
"ここで挙げられている for はリスト内包表記な Clojure の for でなく、
命令プログラミング的な for をマクロ実装してみたもの。"

"Clojure でサンプルを素直に読み替えて定義してみた場合だと実際は使えない。"

(defmacro for-a [[sym start stop] & body]
  `(loop [~sym ~start
          limit ~stop]
     (when (<= ~sym limit)
       ~@body
       (recur (inc ~sym) limit))))

(for-a [x 1 5]
  (println x))
;=> CompilerException java.lang.RuntimeException: Can't let qualified name: on-lisp.core/limit, compiling:~

"少し前でもあったが、Clojure でバッククォート中にシンボルが出てきた場合
名前空間を付けて解決しようとする。この場合マクロ展開するとこうなる。"

(clojure.core/loop [x 1
                    user.core/limit 5]
  (clojure.core/when (clojure.core/<= x user.core/limit)
    (println x)
    (recur (clojure.core/inc x) user.core/limit)))

"展開形見ると分かるが user.core/limit なぞ未定義なのでそれでコケる。

正直な所 loop マクロを使ってもっと素直に書くと今回の悪い例示として出てる limit はそもそも
不要になる。"

(defmacro for-b [[sym start stop] & body]
  `(loop [~sym ~start]
     (when (<= ~sym ~stop)
       ~@body
       (recur (inc ~sym)))))

"けどこのままでは例示にならないのでこう定義すれば Clojure でも変数捕捉は起こせる。"
(defmacro for-c [[sym start stop] & body]
  `(loop [~sym ~start
          ~'limit ~stop]
     (when (<= ~sym ~'limit)
       ~@body
       (recur (inc ~sym) ~'limit))))

"~' はクォート・アンクォートの組み合わせで特に新しいマクロ展開文字ではないのだが、
こいつを並べてシンボルの前に付けると名前修飾が行われなくなる。

これでおかしな動きにできる。ただこの実装例だと延々と 5 を出し続けるので実行注意。"
(for-c [limit 1 5]
  (println limit))

"展開形を見ると、不等号を変えた影響でずっと limit = limit を比較し続けてる為なのが分かる。"
(clojure.core/loop [limit 1 limit 5]
  (clojure.core/when (clojure.core/<= limit limit)
    (println limit)
    (recur (clojure.core/inc limit) limit)))

"こちらは On Lisp と同様 nil しか返らなくなる。"
(let [limit 5]
  (for-c [i 1 10]
    (when (> i limit)
      (println i))))


;;;
;;; 8.2 フリーシンボルの捕捉
;;;

;; p76

"Clojure は基本 Immutable なので、Atom を使って似たような事できるかなとまずは gripe を書いてみた。"
(def w (atom nil))

(defmacro gripe [warning]
  `(do
     (reset! ~w (concat @w (list ~warning)))
     nil))

"が、この段階で例外になる。"
(gripe "sample < 2")
;=> CompilerException java.lang.RuntimeException: Can't embed object in code, maybe print-dup not defined: clojure.lang.Atom@4f2d14, compiling:~

"Atom どーのと書かれてるので試しに reset! を生で呼んでみたが問題ない。"
(reset! w (concat @w (list "sample < 2")))
;=> ("sample < 2")

"正直良く分からないが、 # での auto gensym と名前空間での修飾に頼れば
きっと大丈夫じゃないかなと楽観視して次へ行くことにする。"

;;更新ここまで~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;;; 8.3 捕捉はいつ起きるのか
;;;

;;;
;;; 8.4 適切な名前によって捕捉を避ける
;;;

"8.3 に書いてはいけないコード例があるが、利用目的がさっぱり分からないので飛ばす。"


;;;
;;; 8.5 優先評価によって捕捉を避ける
;;;

;; p79
"図 40 の before 例。Clojure だといずれにせよ定義はできても
``RuntimeException: Can't let qualified name:``と言う let のエラーでコケる。"

(defmacro before [x y coll]
  `(let [coll ~coll]
     (< (.indexOf coll ~x)
        (.indexOf coll ~y))))

(defmacro before [x y coll]
  `(let [xval ~x
         yval ~y
         coll ~coll]
     (< (.indexOf coll xval)
        (.indexOf coll yval))))

"ただ let で先に評価してしまう手法は Clojure でも時々見かける。
Clojure のどのマクロで見たかはちょっと忘れたが、before 例だとこんな感じか。"
(defmacro before [x y coll]
  `(let [xval# ~x
         yval# ~y
         coll# ~coll]
     (< (.indexOf coll# xval#)
        (.indexOf coll# yval#))))


;; p80
"図 41 クロージャによる変数捕捉の回避方法.
ここでは間違った for マクロ として Clojure 版のを再掲。
再度書くが、Clojure のこの例は無理やり捕捉を起こしてるもので
Java のメソッド名を修飾なしで展開したいなどの明確な意図がない限り
無闇に ``~'`` のコンボは使うべきではないと思う。"
(defmacro for-c [[sym start stop] & body]
  `(loop [~sym ~start
          ~'limit ~stop]
     (when (<= ~sym ~'limit)
       ~@body
       (recur (inc ~sym) ~'limit))))

"On Lisp のようにクロージャで包み込んでみた例。"
(defmacro for-d [[sym start stop] & body]
  `(let [b# (fn [~sym] ~@body)]
     (loop [~sym ~start]
       (when (<= ~sym ~stop)
         (b# ~sym)
         (recur (inc ~sym))))))
"うーん、作ってはみたがいまひとつピンと来ない。
body に sym で渡されたシンボルが被っても大丈夫って事か？"

;;;
;;; 8.6 Gensym によって捕捉を避ける
;;;

;; p81
"Clojure にも # の他に gensym もあるので無理やり使ってみた例。"
(defmacro for-e [[sym start stop] & body]
  (let [gstop (gensym)]
    `(loop [~sym ~start
            ~gstop ~stop]
       (when (<= ~sym ~gstop)
         ~@body
         (recur (inc ~sym) ~gstop)))))
"ただ for-b を見れば分かるが loop を使った場合 gstop は全く必要ない。
あくまで gensym を生で使ってみましたと言うだけの例。"

;;;
;;; 8.7 パッケージによって捕捉を避ける
;;;
"デフォルトで修飾されるかされないかと言うだけで
Clojure で言う名前空間で修飾すると言う話と同じなので割愛。"

;;;
;;; 8.8 異なる名前空間での捕捉
;;;

;; p82
"ここで言う名前空間は Common Lisp での変数と関数の名前空間が分かれている事を指す。
Clojure の名前空間とは意味合いが異なる。"

"とは言え Clojure はこのパターンの場合は気にしなくても良さそう？"
(defn f [x] (+ x 1))
;=> #'user/f
(defmacro mac [x] `(f ~x))
;=> #'user/mac
(mac 10)
;=> 11
(letfn [(f [y] (- y 1))]
  (mac 10))
;=> 11

"この後 block に展開される時のラベルの注意など書かれてるが、Clojure では
存在しない話なので割愛。"

;;;
;;; 8.9 変数捕捉にこだわる理由
;;;
"コード例無し。

最後に私見だが Clojure のマクロはデフォルトでの名前空間修飾と auto gensym で
捕捉が起こりにくくするよう配慮されてると思う。
意図して捕捉を起こしたい場合は``~'``を使うなどの手も残されているし。"

"第 8 章 変数捕捉"

"はここまで。"

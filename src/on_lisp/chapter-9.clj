(ns on-lisp.chapter-9
  "On Lisp サンプルマクロの Clojure 実装。
   野田開さんが翻訳された草稿の PDF 版をベースとする。
   PDF 版配布先: http://www.asahi-net.or.jp/~kc7k-nd/

   ここには
        第 9 章 マクロのその他の落し穴
   に掲載されているサンプルの Clojure 実装を置く。")

;;;
;;; 9.1 評価の回数
;;;

;; p84

"On Lisp で言う複数回の評価を起こし得るマクロが Clojure 版にあるかどうか
良く分からん。形式的に近いのとなると for-b なんだが、"
(defmacro for-b [[sym start stop] & body]
  `(loop [~sym ~start]
     (when (<= ~sym ~stop)
       ~@body
       (recur (inc ~sym)))))

"特に問題ない。"
(let [x 2]
  (for-b [i 1 (inc x)]
    (println i)))
;=> 1
;   2
;   3
;   nil

"複数回評価を起こし得る状況を Clojure マクロで思いつかないので次へは進むが、
Clojure でも有り得るんじゃないかと気には留めておきたい。

ところで例示とは言え On Lisp のマクロ呼び出しで incf 使ってるのもあまり良くないんじゃないかな。
これは変数を変更しない 1+ (Clojure での inc) を使えば延々と数字を出し続ける現象は解消できる。

Mutable な変更を割と許している Common Lisp あまりいいマクロの書き方じゃないのは確かだろうけど。"


;;;
;;; 9.2 評価の順番
;;;

;; p84
"これも一応 stop が先に評価されるようなマクロを定義してみたが、"
(defmacro for-f [[sym start stop] & body]
  `(let [gstop# ~stop]
     (loop [~sym ~start]
       (when (<= ~sym gstop#)
         ~@body
         (recur (inc ~sym))))))

"問題ない。"
(let [x 1]
  (for-f [i x (+ x 13)]
     (println i)))
;=> 1
;   2
;   ...
;   13
;   nil

"急いで書いてる時にはこんな書き方するかもしれないからこれも注意はしておきたい。
でも Clojure 的な感覚から言うと破壊的変更をデフォルトで許している事もやっぱり問題な気がする。"

;;;
;;; 9.3 関数によらないマクロ展開
;;;

;; p85

"Clojure でも誤った定義できると言えばできる。alter-var-root とか初めて使ったけど。"
(def ^:dynamic *nil!s* 0)

(defmacro nil! [x]
  (alter-var-root (var *nil!s*) (partial inc))
  `(def ~x nil))
;=> #'user/nil!
(nil! a)
;=> #'user/a
*nil!s*
;=> 1

"文字列での呼び出し。"
(defmacro string-call [opstring & args]
  `(~(symbol opstring) ~@args))
;=> #'user/string-call
(defn our+ [x y] (+ x y))
;=> #'user/our+
(string-call "our+" 2 3)
;=> 5

"いずれにせよあまり使う方法ではないと思う。

この後 nconc(破壊的 concat) を利用したマクロにおいての危険性についての例示がある。
Clojure だとありそうなのは transient 使った時かもしれないが、自分は今の所使わないので
読み流すだけにした。"

;;;
;;; 9.4 再帰
;;;

;; p87
"Common Lisp の length は Clojure で言う count。"

(defn our-count [x]
  (if-not x
    0
    (inc (our-count (next x)))))

"但し Clojure は JVM の制限だったか深い再帰をすると StackOverflowError が出る。"
(our-count (range 1 10000))
;=> StackOverflowError   clojure.lang.ChunkedCons.next (ChunkedCons.java:41)

"これは末尾再帰な内部関数作って呼ぶようにすれば回避できる。"
(defn our-count [x]
  (letfn [(counter [n coll]
            (if (seq coll)
              (recur (inc n) (next coll))
              n))]
    (counter 0 x)))

(our-count (range 1 10000))
;=> 9999

"ついでに Clojure の反復版。"
(defn our-count [x]
  (loop [n 0
         y x]
    (if (seq y)
      (recur (inc n) (next y))
      n)))

"nth 例。まずは関数版。引数順は Clojure 版準拠だが :not-found オプション引数はここでは実装しない。
また図らずも標準と違って IndexOutOfBoundsException は出なくなってる。"
(defn ntha [coll index]
  (if (zero? index)
    (first coll)
    (recur (rest coll) (dec index))))

"間違った定義である nthb。Clojure でも定義はできるものの実際使うと無限ループになってしまう。"
(defmacro nthb [coll index]
  `(if (zero? ~index)
     (first ~coll)
     (nthb (rest ~coll) (dec ~index))))

;; p88

"マクロ反復版。あれ、マクロ中でも recur 使えた。loop との対だと大丈夫って事か？"
(defmacro nthc [coll index]
  `(loop [c# ~coll
          i# ~index]
     (if (zero? i#)
       (first c#)
       (recur (next c#) (dec i#)))))

"再帰関数呼び出し版 nthd と、呼び出す関数を内蔵した nthe。
サンプルと比較して nth-fn が上なのは単に Clojure が上から下への流れで評価される為。"

(defn nth-fn [coll index]
  (if (zero? index)
    (first coll)
    (recur (next coll) (dec index))))

(defmacro nthd [coll index]
  `(nth-fn ~coll ~index))

(defmacro nthe [coll index]
  (letfn [(nth-fn [coll index]
            (if (zero? index)
              (first coll)
              (recur (next coll) (dec index))))]
    `(nth-fn ~coll ~index)))


;; p89
(defn or-expand [args]
  (if (nil? args)
    nil
    (let [sym (gensym)]
      `(let [~sym ~(first args)]
         (if ~sym
           ~sym
           ~(or-expand (next args)))))))

(defmacro ora [& args]
  (or-expand args))

"Clojure だとこちらは定義はできても使えず。理由は良く分からんが、
nthc と違って loop 使ってないからそこの違い？"
(defmacro orb [& args]
  (if (nil? args)
    nil
    (let [sym (gensym)]
      `(let [~sym ~(first args)]
         (if ~sym
           ~sym
           (recur ~@(next args)))))))

(orb nil 1)
;=> CompilerException java.lang.IllegalArgumentException: Mismatched argument count to recur, expected: 0 args, got: 1, compiling:~

(orb nil)
;=> 無限ループ

"第 9 章 マクロのその他の落し穴"

"はここまで。"

(ns on-lisp.chapter-6
  "On Lisp サンプルマクロの Clojure 実装。
   野田開さんが翻訳された草稿の PDF 版をベースとする。
   PDF 版配布先: http://www.asahi-net.or.jp/~kc7k-nd/

   ここには
        第 6 章 マクロ
   に掲載されているサンプルの Clojure 実装を置く。")

;;;
;;; 6.1 マクロはどのように動作するか
;;;

;; p54
(defmacro nil! [x]
  (list 'def x nil))


;;;
;;; 6.2 逆クォート
;;;

;; p56
"逆クォート版 nil!"
(defmacro nil! [x]
  `(def ~x nil))

;; p57
"図 29 マクロ定義に逆クォートを使った例と使わない例．"
(defmacro nif [expr pos zero neg]
  `(cond
    (pos? ~expr) ~pos
    (zero? ~expr) ~zero
    (neg? ~expr) ~neg))

(defmacro nif [expr pos zero neg]
  (list
   'cond
   (list 'pos? expr) pos
   (list 'zero? expr) zero
   (list 'neg? expr) neg))
"Common Lisp の truncate や signum と全く同じものはないので別実装。
 テストコード。"
(map #(nif % 'p 'z 'n) [0 2.5 -8])
;=> (z p n)

"when。どうでもいいんだけど Clojure の when はなぜか逆クォートを使わない版で書かれてる。"
(defmacro our-when [test & body]
  `(if ~test (do ~@body)))

;; p58
(defn greet [name]
  `(hello ~name))

"一つ Clojure で注意する点があるとすると
バッククォート中でクォートされてないシンボルがあった場合まず名前解決しようとする。
何気なく定義した上の関数の結果を見るとそれが分かる。"
(greet 'Clojure)
;=> (user/hello Clojure)

"動きとしてはまず既に定義済みの名前がないか探し、なければ現在の名前空間で修飾して
展開するようだ。この展開された式を評価してみても user 名前空間に hello なんてない
よと怒られる。"

"既に定義済みの関数を使ったものをバッククォート中に含めてみる。"
(defn mac-factorial [n]
  `(* ~@(range 1 (inc n))))

(mac-factorial 10)
;=> (clojure.core/* 1 2 3 4 5 6 7 8 9 10)

"も一つ Clojure 特有と言うわけじゃないんだろうが defn と defmacro の違いは
defn は定義した式を評価すると言うのに対して defmacro は展開できるところまで
式を展開した後に評価すると言う点が違うと言う事でいいのかな。"

"先と中身が全く同じものを defmacro で定義して評価してみる。"
(defmacro mac-factorial [n]
  `(* ~@(range 1 (inc n))))

(mac-factorial 10)
;=> 3628800


;;;
;;; 6.3 単純なマクロの定義
;;;

"member の定義だが、同等の事は Clojure なら drop-while で可能。"
(defn member [item col & {:keys [test] :or {test =}}]
  (seq (drop-while #(not (test item %)) col)))

"Clojure では nil と () が違うものなので一応 seq をかましておく。
Common Lisp 標準だとキーワード引数として他にも取れるものがあるのだが
ここでは省略する。"

"また、Common Lisp の同一性チェックは案外色々あって面倒なので、
eql は Clojure の =, eq は Clojure の identical? と置き換えたものとして memq 定義をする。"

;; p59
(defmacro memq [x choices]
  `(member ~x ~choices :test identical?))

"それと本書の方では #' が出てるが Clojure のそれとは異なり、
Common Lisp では関数呼び出しをするリードマクロな点に注意するくらいか。
これは Common Lisp では言語仕様として変数と関数の名前空間が異なる事に起因する。
他の Lisp との違いについては http://clojure.org/lisps に説明がある。"

"次に while マクロの例があるが、これまた Common Lisp の do はマクロでありそのままのものはない。
が、while は Clojure なら条件付きの loop として展開させてやれば良い。"

(defmacro while [test & body]
  `(loop []
     (if ~test
       (do
         ~@body
         (recur)))))

"後で Clojure の方のソース見て when の存在を忘れてた事に気づいたw"

;;;
;;; 6.4 マクロ展開の確認
;;;

;; p60

"図 33 マクロ展開確認用のマクロ"
(defmacro mac [expr]
  `(clojure.pprint/pprint (macroexpand-1 '~expr)))

"cider なら C-c C-m すればいいんだが一応。"


;;;
;;; 6.5 引数リストの構造化代入
;;;

;; p61

"最初サンプルほぼそのまま書いてみたのだが、"
(defmacro our-dolist [[v coll & result] & body]
  `(do
     (map (fn [~v] ~@body) ~coll)
     (let [~v nil]
       ~result)))

"nil しか返らない。"
(our-dolist [x '(a b c)]
  (print x))
;=> nil

"もしや map って lazy-seq だから何もしてないのかと思い doall をかましてみると出た。"
(defmacro our-dolist [[v coll & result] & body]
  `(do
     (doall (map (fn [~v] ~@body) ~coll))
     (let [~v nil]
       ~result)))
;=> abc
;   nil

"when-bind サンプル。"
(defmacro when-bind [[v expr] & body]
  `(let [~v ~expr]
     (when ~v ~@body)))

"ちなみに Clojure のソースを見ると入れ子の destructuring は使わずに実装してる。
1.0 の頃だとまだなかったのかな？"
(when-let [bindings & body]
  ;; 途中省略
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (when temp#
         (let [~form temp#]
           ~@body)))))
  
;;;
;;; 6.6 マクロのモデル
;;;

;; p62
"図 34 defmacro の概形のコードがあるがここは一旦飛ばす。
何となくわからん気がしないでもないが、destructuring-bind 辺りや
gemsym した g を仮引数に関数作ってる辺りが今の所理解できん。"


"次にあるのはレキシカル環境を読めるはずだというサンプル。"
(let [op 'def]
  (defmacro our-def [sym init]
    (list op sym init)))

"一見使えそうだが、On Lisp で例示している事と全く同じかどうかは自信なし。"
(our-def a 1)
;=> #'user/a
a
;=> 1


;;;
;;; 6.7 プログラムとしてのマクロ
;;;

;; p63
"まず Common Lisp がやってる do マクロについて軽く読み解く。
これはすごく大雑把に言えば Clojure の loop が少し複雑になったものと言える。"

(do
  ;; 初期化ブロック。
  ;; sym [init] [step] と言った最大 3 つをペアとするリストを並べる。
  ;; sym がローカル変数、init が初期値、step がループ更新毎に変数をどう変化させるかの式。
  ;; init, step は任意で、init のみの場合はループ毎の更新無し、
  ;; init も step もなければ変数は nil で初期化される。
  ((w 3)
   (x 1 (1+ x))
   (y 2 (1+ y))
   (z))
    ;; ループ終了条件と条件満たした時の処理。
    ;; 最後の式が do マクロが返す値になる。
    ((> x 10) (princ z) y)

    ;; ループ本体。初期化ブロックでのローカル変数更新式で
    ;; ローカル変数を更新しながら終了条件を満たすまで実行される。
    (princ x)
    (princ y))

"やや限定的になるが、初期値と変数変化を必須にすれば割とすんなり loop に展開できる。"
(loop [w 3
       x 1
       y 2
       z nil]
  (if (> x 10)
    (do
      (println z)
      y)
    (do
      (println x)
      (println y)
      (recur w (inc x) (inc y) z))))

"上の式に展開するマクロはこんな感じにしてみる。不格好だが初期化ブロック、
終了条件・終了時処理ブロック、ループ本体ブロックは何となく [] で囲んだ。"
(cl-like-do [w 3 w
             x 1 (inc x)
             y 2 (inc y)
             z nil z]
  [(> x 10) (println z) y]
  [(println x)
   (println y)])

"一応本書の少し先にある図 36 実装例を見ないで書いてみたもの。"
(defmacro cl-like-do [inits end-clause & body]
  (let [binds (mapcat butlast (partition 3 inits))
        steps (take-nth 3 (subvec inits 2))]
    `(loop [~@binds]
       (if ~(first end-clause)
         (do ~@(rest end-clause))
         (do ~@body
             (recur ~@steps))))))

"実装例見た後、ああ end-clause は destructuring してしまってもいいなと思い
少し書き直したもの。"
(defmacro cl-like-do [inits [test & result] & body]
  (let [binds (mapcat butlast (partition 3 inits))
        steps (take-nth 3 (subvec inits 2))]
    `(loop [~@binds]
       (if ~test
         (do ~@result)
         (do ~@body
             (recur ~@steps))))))

"本当は make-initforms と make-stepforms をちゃんと作った方がいいんだろうけど
do 自体にあまり利便性を見出せないのでこのくらいで次に行く。"

;;;
;;; 6.8 マクロのスタイル
;;;

;; p64

"On Lisp 実装例 2 種(図 37 and と等価なマクロの例 2 個．)。
Common Lisp と違い t と言う唯一のシンボルはないので、全て true なら
最後の式の値を返す。"

(defmacro our-and [& args]
  (case (count args)
    0 args
    1 (first args)
    `(when ~(first args) (our-and ~@(rest args)))))

(defmacro our-andb [& args]
  (if (nil? (seq args))
    args
    (letfn [(expander [rest]
              (if (next rest)
                `(if ~(first rest)
                   ~(expander (next rest)))
                (first rest)))]
      (expander args))))

"え、our-andb ってちゃんと展開されるのと見たが展開されとる。"
(macroexpand-1 '(our-andb 1 2 3 nil 5))
;=> (if 1 (if 2 (if 3 (if nil 5))))
(our-andb 1 2 3 nil 5)
;=> nil

"うーん、後のは今後思いつくパターン出るんだろうか。ただ、前者はマクロには
recur 使えないから Clojure で再帰的なマクロ書きたい場合はこうなるのかなあ。"

"最後に Clojure の and を見てみる。"

(defmacro and
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

"引数での多重定義できる Clojure ならこの形がいいな。
``(if ~x (and ~@next) ~x)`` とせずに let かましてるのは
評価が複数回されるのを避けるためなのかな？"


;;;
;;; 6.9 マクロへの依存
;;;

;; p66
"Clojure も事情は同じと考えていいのかな。"
(defmacro mac [x] `(inc ~x))
;=> #'user/mac
(def f (fn [y] (mac y)))
;=> #'user/f
(defmacro mac [x] `(+ ~x 100))
;=> #'user/mac
(f 1)
;=> 2


;;;
;;; 6.10 関数からマクロへ
;;;

;; p67
"ここでは Clojure 定義に合わせてみる。"
(defn second [x]
  (first (next x)))

(defmacro second [x]
  `(first (next ~x)))

(defn noisy-second [x]
  (println "Someone is taking a second!")
  (first (next x)))

(defmacro noisy-second [x]
  `(do
     (println "Someone is taking a second!")
     (first (next ~x))))

; sum マクロ。この場合 reduce でも同じ
(defn sum [& args]
  (apply + args))

(defmacro sum [& args]
  `(+ ~@args))

;; p68
"foo のまずは関数版。"
(defn foo [x y z]
  (list x (let [x y]
            (list x z))))

"foo のマクロ版は Clojure の場合この定義で使おうとしても、"
(defmacro foo [x y z]
  `(list ~x (let [x ~y]
             (list x ~z))))
;=> #'user/foo

"let で修飾名は使えないよと怒られる。"
(foo 1 2 3)
;=> CompilerException java.lang.RuntimeException: Can't let qualified name: user/x, compiling:~

"なので foo 関数の Clojure マクロ版を作るなら仮引数を参照しない let 以降の x は
auto gensym させてやるべきか。On Lisp ではまだ紹介されてない技法だけども。"

(defmacro foo [x y z]
  `(list ~x (let [x# ~y]
              (list x# ~z))))

"その前にせめて自分で定義した関数内で仮引数とマクロ内変数の名前衝突はさせないようにした方が
無難だとは思うけど。"


;;;
;;; 6.11 シンボル・マクロ
;;;
"symbol-macrolet は Clojure でも clojure/tools.macro 外部ライブラリを導入すれば使えるように
なるみたいだけど、実際の導入は使われていると言う 15 章と 18 章に持ち越す。"

"第 6 章 マクロ"

"はここまで。"

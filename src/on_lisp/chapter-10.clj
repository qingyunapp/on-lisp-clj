(ns on-lisp.chapter-10
  "On Lisp サンプルマクロの Clojure 実装。
   野田開さんが翻訳された草稿の PDF 版をベースとする。
   PDF 版配布先: http://www.asahi-net.or.jp/~kc7k-nd/

   ここには
        第 10 章 古典的なマクロ
   に掲載されているサンプルの Clojure 実装を置く。")

;;;
;;; 10.1 コンテキストの生成
;;;

;; p90

"図 47 let のマクロによる実装。

binds の奇数 index はローカル変数のシンボルなので、それを関数の仮引数として渡し、body を実行。
で、binds の偶数 index を関数の実引数として後半部で渡す、と言った感じか。

元サンプルの mapcar は Clojure で言う map なんだが、binding form を Clojure と同じカッコ無し
にするならこれで良い。"
(defmacro our-let [binds & body]
  `((fn [~@(take-nth 2 binds)]
      ~@body)
    ~@(take-nth 2 (rest binds))))


"bind のフォームだけは Clojure の let と同じものの、動作としては Common Lisp の let のようになる。"
; ケース1
(our-let [a 1
          b 2
          c 3]
  (+ a b c))
;=> 6

; ケース2
(our-let [a 1
          b a
          c 3]
  (+ a b c))
;=> CompilerException java.lang.RuntimeException: Unable to resolve symbol: a in this context, compiling:~

"と言った感じで前の変数束縛を参照できない。"

"Clojure の let っぽく前の定義も参照するバージョンも書いてみた。"

(defmacro clj-let [binds & body]
  (cond
   (= (count binds) 2)
   `(our-let ~binds ~@body)
   
   (empty? binds)
   `(do ~@body)

   :else
   `(our-let ~(subvec binds 0 2)
      (clj-let ~(subvec binds 2) ~@body))))

"clj-let なら our-let では通らなかったケース 2 も動作する。"
(clj-let [a 1
          b a
          c 3]
  (+ a b c))
; => 5

;; p.90

"図 48 変数を束縛するマクロの例。
確か既出だったはずだが when-bind* との対比の為再掲。"
(defmacro when-bind [[v expr] & body]
  `(let [~v ~expr]
     (when ~v ~@body)))

(defmacro when-bind* [binds & body]
  (if (empty? binds)
    `(do ~@body)
    `(when-let ~(subvec binds 0 2)
       (when-bind* ~(subvec binds 2) ~@body))))
  
"ソース見ると分かるのだが when-let が取れるシンボル、式のペアは 1 つまでなので
ここでは when-let を流用した。意味合いは if let を組み合わせた式と変わらない。"


"with-gensym。"
(defmacro with-gensyms [syms & body]
  `(let ~(vec (mapcat (fn [s] `(~s (gensym)))
                      syms))
     ~@body))

"condlet を移植する前に割と複雑なマクロなので動作を読み解く。
On Lisp でのマクロ動作説明読んでスッと入る人にはいいんだろうけど
自分は一発ではよー分からなかったので、自分が分り易い言葉に置き換えてみる。

condlet の最初のブロックは [test bind1...] が 1 個以上並んだもので、
順に test を実行し、最初に test が true の節のみ変数 bind を実施。
で、最後の body を実行する。"

"次にマクロ実装例。ここでは On Lisp に書かれてる項目を再掲する。
もちろんこのままでは Clojure では動作しない。"
(condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
          ((= 1 1) (y (princ 'c)) (x (princ 'd)))
          (t (x (princ 'e)) (z (princ 'f))))
         (list x y z))

"最初 bind form で princ 使っておいてシンボルちゃんと入ってるのなんでだよと思ったが
princ って標準出力もするが、関数の戻り値としても引数で渡されたのを返す動作するのか。
勘違いしてた。"

"試してみた例。"
(println 'a)
;=> a
;   nil
"と、標準出力に出された a と println の戻り値として nil が返るが、
Common Lisp の princ だとこうなる。"
(princ 'a)
;=> a
;   a

"Clojure っぽく置き換えてみるとこんな感じか。"
(condlet [(= 1 2)  [x (str 'a) y (str 'b) z nil]
          (= 1 1)  [y (str 'c) x (str 'd) z nil]
          :default [x (str 'e) z (str 'f) y nil]]
  (list x y z))

"cl-like-do を書いてみた時にもあったのだが、Clojure の let と
Common Lisp の let の違いとして、後のローカル変数が前のローカル変数を
参照できる事の他に、変数シンボルのみの宣言もできる点がある。Clojure で例えると、
デフォルト nil とする declare を let 中で可能と言った感じ。
(実際の declare ではデフォルト値設定はできないが)

上のマクロ使用イメージコードはローカル declare ライクな機能はナシにしたもので、
そうすると今回も割と楽にマクロ化できる。"
(defn cond-clause [test bind body]
  `(~test
    (let ~bind
      ~@body)))

(defmacro condlet [clauses & body]
  (let [tests (take-nth 2 clauses)
        binds (take-nth 2 (next clauses))]
    `(cond
      ~@(mapcat cond-clause tests binds (repeat body)))))

"これでマクロ使用イメージコードを評価すると (\"d\" \"c\" nil) が返るようになる。"

;;;
;;; 10.2 with-系マクロ
;;;

"ここのサンプルは擬似コードで移植する面白みがないので飛ばす。

Clojure の with 系マクロはちょうど例示されてた with-open が参考になる。
On Lisp の with-db では見られない複数 binding に対応させる時の書き方や
再帰マクロを使っててこれはこれで面白い。"


;;;
;;; 10.3 条件付き評価
;;;




;;更新ここまで~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

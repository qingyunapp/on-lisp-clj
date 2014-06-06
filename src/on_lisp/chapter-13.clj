(ns on-lisp.chapter-13
  "On Lisp サンプルマクロの Clojure 実装。
   野田開さんが翻訳された草稿の PDF 版をベースとする。
   PDF 版配布先: http://www.asahi-net.or.jp/~kc7k-nd/

   ここには
        第 13 章 アナフォリックマクロ
   に掲載されているサンプルの Clojure 実装を置く。")

;;;
;;; 13.1 アナフォリックな変種オペレータ
;;;

;; p116

"aif を初めとした図 72 ひと通り。"
(defmacro aif [test-form then-form & [else-form]]
  `(let [~'it ~test-form]
     (if ~'it ~then-form ~else-form)))

(defmacro awhen [test-form & body]
  `(aif ~test-form
     (do ~@body)))

(defmacro awhile [expr & body]
  `(loop [~'it ~expr]
     (if (not ~'it)
       ~@body
       (recur ~expr))))

(defmacro aand [& args]
  (cond
   (empty? args) true
   (empty? (next args)) (first args)
   :default `(aif ~(first args) (aand ~@(next args)))))

(defmacro acond [& clauses]
  (if (empty? clauses)
    nil
    (let [cl1 (subvec clauses 0 2)
          sym (gensym)]
      `(let [~sym ~(cl1 0)]
         (if ~sym
           (let [~'it ~sym] ~@(cl1 1))
           (acond ~@(subvec clauses 2)))))))

"aand はややこしいので本書の擬似コードを使って展開形を見てみる。"
(pprint
 (clojure.walk/macroexpand-all 
  '(aand (owner x) (address it) (town it))))
;=> (let* [it (owner x)]
;     (if it (let* [it (address it)]
;              (if it (town it) nil))
;         nil))

"it の束縛関係がよく分からんので、展開形しか確認できない本書の擬似コードでなく
実際に動くコードで確かめてみる。"
(aand (inc x) (- it 10) (- it 100))
;=> 99
"it は直近の式に束縛されるみたい。

acond は一応 Clojure の cond に近くなるよう subvec で展開するように書いてみたが
これの動作は未確認。"

"続いて図 74 にあるやつ。alambda は Clojure に合わせて afn と言う名前にした。
block は Clojure で言う名前が付けられる do みたいなもんだが、標準では用意されてない。
ここでは名前指定する tag は省略した。"

(defmacro afn [parms & body]
  `(letfn [(~'self ~parms ~@body)]
     ~'self))

(defmacro ablock [& args]
  `(do
    ~((afn [args]
        (case (count args)
          0 nil
          1 (first args)
          `(let [~'it ~(first args)]
             ~(self (next args)))))
      args)))

"afn はよりそれらしい再帰は確かに書けるが、"
((afn [x] (if (= x 0) 1 (* x (self (dec x))))) 10)
;=> 3628800

"ここでも JVM の制限で recur を使わないとすぐ StackOverflowError になる。"
((afn [x] (if (= x 0) 1 (* x (self (dec x))))) 100M)
;=> StackOverflowError   clojure.lang.PersistentHashMap$BitmapIndexedNode.index (PersistentHashMap.java:580)

"と言うか、Clojure だと標準の fn で名前を指定する事もできる為、
 afn のようなマクロを特別に用意する必要もなかったりする。
 依然 StackOverflowError はつきまとうがそれはまた別の話。"
((fn fact [x] (if (= x 0) 1 (* x (fact (dec x))))) 10)

"なので本文に出ている count-instances は Clojure だと afn 無しにこう書ける。"
(defn count-instances [obj lists]
  (map (fn self [list]
         (if list
           (+ (if (= (first list) obj) 1 0)
              (self (next list)))
           0))
       lists))

"ablock は、、、名前が付けられず、return-from も使えないので
 Clojure では定義する意味がまるでないかな。一応動くが。"
;;更新ここまで~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;;; 13.2 失敗
;;;

;; p120

"いきなり相違点。Clojure では nil = 空リストではない。移植でさっきから使ってる
cdr と似たような next を使ってみると、一見同じじゃないかと思える。"
(next '(a))
;=> nil

"となるが、それは next が nil を返してるだけで、比較してみれば分かる。"
(= () nil)
;=> false

"Clojure には next とほぼ似たような rest もあるが、こちらは () が返る。"
(rest '(a))
;=> ()

"この辺りは Clojure 界隈では falsy なんちゃらと言われたりしてて、
Joy of Clojure でも確か見た気がする。

正直使い分け方はよー分からん。自分は falsy になる nil が欲しい事が多いので
もっぱら next を使うけどあんまり明確な基準はない。

また、() は falsy ではなく、次を実行してみればその事が分かる。"
(if () 'a)
;=> a

"nil の他に真偽値も別にあり、他言語と同様に true/false となる。"
(= 1 0)
;=> false

"nil と false を分けたのはおそらく nil と言う値が欲しい場合と
偽値としての nil が欲しい場合を分けたくてこうしたのかな？
On Lisp でもちょうどここでこの件について語られてるが
解決法は Clojure とは異なる。詳しくは本書でどうぞ。"

;; p122
"図 74 は 図 72 にあるアナフォリックマクロの多値版があるが、前にも書いたように
個人的に多値にあまり意味を見出だせないので飛ばす。

図 75 も特段何が便利になるのか今ひとつピンと来ないので飛ばす。"


;;;
;;; 13.3 参照の透明性
;;;

"ここはほぼ読むだけでサンプルは無し。"

"第 13 章 アナフォリックマクロ"

"はここまで。"

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

"複数回評価を起こし得る状況を Clojure マクロで思いつかないので次へ。
とは言え Clojure でも有り得るんじゃないかと気には留めておきたい。

だた例示とは言え On Lisp のマクロ呼び出しで incf 使ってるのもあまり良くない。
これは変数を変更しない 1+ を使えば延々と数字を print する現象は回避できる。

まあ Mutable な変更を割と許している Common Lisp だとマクロ自体も
あまりいい書き方じゃないんだろう。"


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

"まあ急いで書いてる時にはこんな書き方するかもしれないから
注意はしておいた方がいいかな。
ただ Clojure 的な感覚から言うと破壊的変更をデフォルトで許している事も
やっぱり問題な気がする。"

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
(string-call "clojure.core/our+" 2 3)
;=> 5

"いずれにせよあまり使う方法ではないと思う。

この後 nconc(破壊的 concat) を利用したマクロにおいての危険性についての例示がある。
Clojure だとありそうなのは transient 使った時かもしれないが、自分は使わないので
読み流すだけにした。"

;;;
;;; 9.4 再帰
;;;


;~~~~ 更新ここまで ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(ns on-lisp.chapter-16
  "On Lisp サンプルマクロの Clojure 実装。
   野田開さんが翻訳された草稿の PDF 版をベースとする。
   PDF 版配布先: http://www.asahi-net.or.jp/~kc7k-nd/

   ここには
        第 16 章 リードマクロ
   に掲載されているサンプルの Clojure 実装を置く。")
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ここまで更新

;;;
;;; 15.1 省略
;;;

;; p130

"マクロの省略形を定義するマクロの例が出てるの見て、
そういやマクロって単純に def で別名設定できないんだっけ？と試してみた。"
(def ift when)
;=> Unhandled clojure.lang.Compiler$CompilerException java.lang.RuntimeException: Can't take value of a macro: #'clojure.core/when

"Clojure でそれなりに使いつつも長い名前のマクロがパッと思いつかなかったのはご容赦を。
とにかく単純 def じゃダメみたいなのでそれじゃあと図 83 にある
abbrev, abbrevs を移植してみる。"

(defmacro abbrev [short long]
  `(defmacro ~short [& ~'args]
     `(~'~long ~@~'args)))

(defmacro abbrevs [& names]
  `(do
     ~@(map (fn [pair] `(abbrev ~@pair))
            (partition 2 names))))

"abbrev が少し手こずった。特に ``~@~'args`` の部分。``~'~long`` の方に関しては本書にも
説明があるのでここでは省くが、結局 Clojure 特有のマクロ展開時の名前空間修飾を回避する
には ``~'`` がワンセットでやり、そうやって名前空間修飾が回避された ``args`` が
スプライシングアンクオートでまた展開されると。そんな感じの流れ。

ともあれ試してみる。"
(abbrev ift when)
;=> #'user/ift

(ift () "yeah")
;=> "yeah"

(abbrevs ift when
         wlvars with-local-vars)
;=> #'user/wlvars

(ift () "yeah")
;=> "yeah"

(wlvars [x 1 y 2]
  (println (var-get x) (var-get y)))
;=> 1 2
;   nil

"docstring やらのメタデータが引き継がれないので実際には使う事はないと思うが、
ここまでくると Clojure マクロの名前空間修飾回避は ``~'`` をセットで使えってのが
そろそろ染みこんでくるのかなw"


"第 16 章 リードマクロ"

"はここまで。"

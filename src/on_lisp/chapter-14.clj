(ns on-lisp.chapter-10
  "On Lisp サンプルマクロの Clojure 実装。
   野田開さんが翻訳された草稿の PDF 版をベースとする。
   PDF 版配布先: http://www.asahi-net.or.jp/~kc7k-nd/

   ここには
        第 14 章 関数を返すマクロ
   に掲載されているサンプルの Clojure 実装を置く。")

;;;
;;; 14.1 関数の構築
;;;

"関数合成をマクロ化する意味が結局良く分からない。
funcall を暗に呼んでいて省略できるのがメリットくらい？それにしてもまだ呼び方が"
(fn (compose list 1+ truncate))

"と、Clojure の comp を知っていると使う側が fn を改めて呼ぶ必要があんのか
と言う気もしてマクロ版作る気にならない。

ただ comp を作ってみるのは面白そうなので関数版で見ないで作ってみた。
4Clojure であったようななかったような、忘れた。"
(defn compose
  ([] identity)
  ([f] f)
  ([f g] (fn [& args] (f (apply g args))))
  ([f g h] (fn [& args] (f (g (apply h args)))))
  ([f1 f2 f3 & fs]
     (reduce (fn [g f] (comp f g))
             (concat (reverse fs) [f3 f2 f1]))))

"では大元の comp はどうなんだろと見てみたらちょっと長めだったので
(ソースのリンク)[https://github.com/clojure/clojure/blob/028af0e0b271aa558ea44780e5d951f4932c7842/src/clj/clojure/core.clj#L2391]
だけ載せる。

関数返す所で設定してる仮引数も引数多重定義してる他はそう外れた作り方じゃない感じだったかなと。

この後見ると、On Lisp で作ってる fn マクロはもう少し柔軟な合成の仕方ができるみたいだけど
やっぱりマクロ版はあんま作る気にならん。comp だってそうしょっちゅう使ってるわけじゃないしなあ。"

;;;
;;; 14.2 Cdr 部での再帰
;;;

;;;
;;; 14.3 部分ツリーでの再帰
;;;

"すんません、これらは自分にゃ難し過ぎたのでギブ。
一つ敢えて言うとするなら、ちょっと抽象化しすぎじゃね？って気が。"

;;;
;;; 14.4 遅延評価
;;;

"そういや Clojure にも標準で delay/force あったなあ。
とりあえず図 82 にあるものを移植してみる。"

(defrecord Delay [forced closure])

(defmacro delay [& expr]
  (let [self (gensym)]
    `(let [~self (Delay. :unforced (fn [] ~@expr))]
       ~self)))
  
(defn force [x]
  (if (= (type x) Delay)
    (if (= (:forced x) :unforced)
      ((:closure x))
      (:forced x))
    x))

"defconst は ``(def ^:const unforced (gensym))`` としても良かったが、
Clojure だと Keyword を定数のように扱えるので特に設けなかった。

defstruct は defrecord で。まんま defstruct もあるんだが、
使ってるサンプルあんま見かけないし Clojure の方では試した事もない。
ちなみに defrecord で定義した名前は Clojure 標準も Delay と同じ名前。
標準の方は defrecord で作ったものではないみたいだが。

えーと、項目的に豪快にすっ飛ばしたけど、"

"第 14 章 関数を返すマクロ"

"はここまで。"

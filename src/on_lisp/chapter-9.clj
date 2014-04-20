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

;~~~~ 更新ここまで ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"ここで挙げられている for はリスト内包表記な Clojure の for でなく、
命令プログラミング的な for をマクロ実装してみたもの。"

"Clojure でサンプルを素直に読み替えて定義してみた場合だと実際は使えない。"

(defmacro for-a [[sym start stop] & body]
  `(loop [~sym ~start
          limit ~stop]
     (when (<= ~sym limit)
       ~@body
       (recur (inc ~sym) limit))))

(defmacro for-b [[sym start stop] & body]
  `(loop [~sym ~start]
     (when (<= ~sym ~stop)
       ~@body
       (recur (inc ~sym)))))

(defmacro for-c [[sym start stop] & body]
  `(loop [~sym ~start
          ~'limit ~stop]
     (when (<= ~sym ~'limit)
       ~@body
       (recur (inc ~sym) ~'limit))))

(defmacro for-d [[sym start stop] & body]
  `(let [b# (fn [~sym] ~@body)]
     (loop [~sym ~start]
       (when (<= ~sym ~stop)
         (b# ~sym)
         (recur (inc ~sym))))))

(defmacro for-e [[sym start stop] & body]
  (let [gstop (gensym)]
    `(loop [~sym ~start
            ~gstop ~stop]
       (when (<= ~sym ~gstop)
         ~@body
         (recur (inc ~sym) ~gstop)))))

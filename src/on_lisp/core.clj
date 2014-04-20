(ns on-lisp.core)

;;; 10.1 コンテキストの生成

;; on-lispp.90 図 47 let のマクロによる実装

(defmacro our-let [binds & body]
  `((fn [~@(take-nth 2 binds)]
      ~@body)
    ~@(take-nth 2 (rest binds))))

;; 元のサンプルを見た時自分はすぐには分からなかったので理解を整理する意味も込めて少し説明。
;; 前半の fn は binds の奇数インデックスがシンボルなので、そいつを取って関数の仮引数リストとして展開する。
;; 後半が binds の偶数インデックスを取って body を処理する関数を呼び出し引数に適用する値として展開する、で通じるかな？

;; bind のフォームだけは Clojure の let と同じものの
;; 動作としては Common Lisp の let のようになる。

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
;=> CompilerException java.lang.RuntimeException: Unable to resolve symbol: a in this context, compiling:...

;; と言った感じで前の変数束縛を参照できない。

;; Clojure の let, Common Lisp の let* と同様な動きにする場合
;; すぐ後の図 48 に出てくる when-bind* のように書くと良さそう。

(defmacro clj-let [binds & body]
  (if (empty? binds)
    `(do ~@body)
    `(our-let ~(subvec binds 0 2)
       (clj-let ~(subvec binds 2) ~@body))))

;; our-let では通らなかったケース 2 も動作するようになった。
(clj-let [a 1
          b a
          c 3]
  (+ a b c))
; => 5

;; p.90 図 48 変数を束縛するマクロの例

(defmacro when-bind [[v expr] & body]
  `(let [~v ~expr]
     (when ~v ~@body)))

;; Clojure の when-let と似てるが
;; when-let の方の source 見ると↑で言う v, expr の部分を一旦 let でローカル変数に入れてる。
;; 何でだろ？

(defmacro when-bind* [binds & body]
  (if (empty? binds)
    `(do ~@body)
    `(when-let ~(subvec binds 0 2)
       (when-bind* ~(subvec binds 2) ~@body))))
  
;; ソース見ると分かるのだが when-let が取れるシンボル、式のペアは 1 つまでなので
;; ここでは when-let を流用した。意味合いは if let を組み合わせた式と変わらない。

(defmacro with-gensyms [syms & body]
  `(let ~(vec (interleave syms (mapv gensym syms)))
     ~@body))
;; 変数衝突しないよう let 中で declare するようなもんか？

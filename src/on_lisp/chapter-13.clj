(ns on-lisp.chapter-10
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

"続いて図 74 にあるやつ。alambda は Clojure に合わせて afn と言う名前にした。"

(defmacro afn [parms & body]
  `(labels ((self ,parms ,@body))
           #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
          ,(funcall (alambda (args)
                             (case (length args)
                               (0 nil)
                               (1 (car args))
                               (t `(let ((it ,(car args)))
                                     ,(self (cdr args))))))
                    args)))

;;更新ここまで~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

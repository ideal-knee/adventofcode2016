(ns adventofcode2016.utils)

(defmacro <-> [expr-1 expr-2 & exprs]
  (let [expr-2-with-expr-1-let `(let ~['<- expr-1] ~(if (seq? expr-2)
                                                      expr-2
                                                      `(~expr-2 ~'<-) ))]
    (if exprs
      `(<-> ~expr-2-with-expr-1-let ~@exprs)
      expr-2-with-expr-1-let ) ) )

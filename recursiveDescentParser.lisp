(defun Ifn (str)
  "I -> i E S X"
  (print '(in Ifn))
  (print str)
  (if (or (null str) (not (eql (car str) 'i)))
      (list 'err)
      (let ((e (Efn (cdr str))))
        (if (and (consp e) (eq (car e) 'err))
            e
            (let ((s (Sfn e)))
              (if (and (consp s) (eq (car s) 'err))
                  s
                  (Xfn s)))))))

(defun Xfn (str)
  "X -> ε | e S"
  (print '(in Xfn))
  (print str)
  (if (and str (eql (car str) 'e))
      (Sfn (cdr str))  ; consume 'e' and parse S
      str))           ; epsilon production: return the input unchanged

(defun Efn (str)
  "E -> G E'"
  (print '(in Efn))
  (print str)
  (let ((g (Gfn str)))
    (if (and (consp g) (eq (car g) 'err))
        g
        (Eprimefn g))))

(defun Eprimefn (str)
  "E' -> o G E' | ε"
  (print '(in Eprimefn))
  (print str)
  (if (and str (eql (car str) 'o))
      (let ((g (Gfn (cdr str))))
        (if (and (consp g) (eq (car g) 'err))
            g
            (Eprimefn g)))
      str))  ; epsilon production

(defun Ofn (str)
  "O -> o G O | ε"
  (print '(in Ofn))
  (print str)
  (if (and str (eql (car str) 'o))
      (let ((g (Gfn (cdr str))))
        (if (and (consp g) (eq (car g) 'err))
            g
            (Ofn g)))
      str))  ; epsilon production

(defun Gfn (str)
  "G -> x | y | z | w"
  (print '(in Gfn))
  (print str)
  (if (null str)
      (list 'err)
      (case (car str)
        ((x y z w) (cdr str))
        (otherwise (list 'err)))))

(defun Sfn (str)
  "S -> s | d L b"
  (print '(in Sfn))
  (print str)
  (cond 
    ((and str (eql (car str) 's))
     (cdr str))  ; S -> s: consume s
    ((and str (eql (car str) 'd))
     (let ((l (Lfn (cdr str))))
       (if (and l (not (and (consp l) (eq (car l) 'err))) ; l parsed OK
                (and l (eql (car l) 'b)))
           (cdr l)  ; consume the trailing b
           (list 'err))))
    (t (list 'err))))

(defun Lfn (str)
  "L -> s L'"
  (print '(in Lfn))
  (print str)
  (if (and str (eql (car str) 's))
      (Lprimefn (cdr str))
      (list 'err)))

(defun Lprimefn (str)
  "L' -> s L' | ε"
  (print '(in Lprimefn))
  (print str)
  (if (and str (eql (car str) 's))
      (Lprimefn (cdr str))
      str))  ; epsilon production


(defun parse-input (tokens)
  (let ((result (Ifn tokens)))
    (if (or (null result)  ; completely consumed input
            (and (consp result) (null result))) ; if result is an empty list
        (format t "Parse successful!~%")
        (if (and (consp result) (eq (car result) 'err))
            (format t "Parse error. Parse Failed.")
            (format t "Parse error. Parse Failed.")))))




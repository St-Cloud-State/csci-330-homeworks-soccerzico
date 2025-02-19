(defun swap (list1) 
  
  (if (and (= (length list1) 2) (> (car list1) (cadr list1)))  ;; Swap only if out of order
      (list (cadr list1) (car list1))
      list1))  ;; Otherwise, return as is

(defun break-into-sorted-pairs (list1)
 
  (cond
    ((null list1) '())  ;; Base case: empty list
    ((null (cdr list1)) (list (list (car list1))))  ;; Single element case
    (t (cons (swap (list (car list1) (cadr list1)))  ;; Sort the pair before adding
             (break-into-sorted-pairs (cddr list1))))))  ;; Recursively process the rest

(defun merge-lists(list1 list2)
  
  (cond
    ((null list1) list2)
    ((null list2) list1) 
    ((< (car list1) (car list2))  
     (cons (car list1) (merge-lists(cdr list1) list2)))
    (t (cons (car list2) (merge-lists list1 (cdr list2))))))

(defun merge-adjacent (lists)
  
  (cond
    ((null lists) '())  ;; If list is empty, return empty list
    ((null (cdr lists)) lists)  ;; If only one sublist remains, return it
    (t (cons (merge-lists (car lists) (cadr lists))  ;; Merge first two lists
             (merge-adjacent (cddr lists)))))))  ;; Recurse on the rest

(defun merge-sort-passes (lists)
  "Repeatedly merges pairs of lists until a single sorted list remains."
  (if (null (cdr lists))  ;; If only one list remains, return it
      (car lists)
      (merge-sort-passes (merge-adjacent lists))))  ;; Keep merging until one list remains

(defun merge-sort (list1)
  
  (merge-sort-passes (break-into-sorted-pairs list1)))  


(merge-sort '(1 3 4 5 9 1 2 7 6 8 3 4)) 



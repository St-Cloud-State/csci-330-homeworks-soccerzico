
(defun partition-list (baselist list1 list2)
  (cond
    ;; Base case: when the list is empty, return the two halves (reversed for correct order)
    ((null baselist) (list (reverse list1) (reverse list2)))  
    (t (progn
         ;; Add the first element of baselist to list1
         (setf list1 (cons (car baselist) list1))
         (setf baselist (cdr baselist)) ;; Move to the next element

         ;; If there's another element, add it to list2
         (when baselist
           (setf list2 (cons (car baselist) list2))
           (setf baselist (cdr baselist))) ;; Move to the next element

         ;; Recursively continue partitioning
         (partition-list baselist list1 list2)))))


(defun merge-lists(list1 list2)
  (cond
    ;; If one list is empty, return the other list
    ((null list1) list2)
    ((null list2) list1) 
    ;; If the first element of list1 is smaller, keep it and merge the rest
    ((< (car list1) (car list2))  
     (cons (car list1) (merge-lists(cdr list1) list2)))
    ;; Otherwise, keep the first element of list2 and merge the rest
    (t (cons (car list2) (merge-lists list1 (cdr list2)))))) 


(defun mergesort (list1)
  ;; Base case
  (if (or (null list1) (null (cdr list1)))
      list1
      ;; Otherwise, split the list into two halves and sort them recursively
      (let ((first-half (car (partition-list list1 '() '()))) ;; Get first half
            (second-half (cadr (partition-list list1 '() '())))) ;; Get second half
        
        ;; Recursively sort each half
        (setf first-half (mergesort first-half))
        (setf second-half (mergesort second-half))
       
        ;; Merge the sorted halves back together
        (merge-lists first-half second-half))))


(mergesort '(7 8 9 2 3 6 5 1 0))

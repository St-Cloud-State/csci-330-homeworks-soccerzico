(defun insert-sorted (item sorted-list)
  
  (cond
    ((null sorted-list) (list item))  ;; If list is empty, return a list with just the item
    ((<= item (car sorted-list)) (cons item sorted-list))  ;; If the item is smaller or equal to the first item, insert at the front
    ((> item (car (last sorted-list))) ;; If the item is greater than the last item, append it to the end
     (append sorted-list (list item)))
    (t (cons (car sorted-list) (insert-sorted item (cdr sorted-list)))))) ;; Otherwise, recursively find the right place in the middle of the sorted list


(defun insertion-sort (sorted-list unsorted-list)
  "Sorts a list using insertion sort."
  (cond
    ((null unsorted-list) sorted-list)  ;; Base case
    (t (insertion-sort 
         (insert-sorted (car unsorted-list) sorted-list)  ;; Insert the item 
         (cdr unsorted-list)))))  ;; Continue with remaining items

(insertion-sort '() '(3 1 4 4 1 1 0 19 5 9 2 6))
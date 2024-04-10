(defvar cola '())
(defvar lista-colas (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))
(defvar lista-colas-neg (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))


(defun RADIX-Sort (cola multiplicador)
    (if (< multiplicador 100000)
        (progn
            (mapcar #'(lambda (x) (llenar-lista-colar x multiplicador)) cola)
            (mapcar #'llenar-colas-neg lista-colas-neg)
            (mapcar #'llenar-colas lista-colas)
            (setq lista-colas (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))
            (setq lista-colas-neg (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))
            (radix-sort cola (* multiplicador 10))
        )
        (print (reverse cola))
    )
)

(defun llenar-lista-colar (numero multiplicador)   
    (if (and (> (mod (truncate (/ (car cola) multiplicador)) 10) 0) (< (car cola) 0))
        (push (car cola)
            (nth (funcall (lambda (x) (+ (mod (truncate (/ x multiplicador)) 10) 0)) (car cola)) lista-colas-neg)
        )
        (push (car cola)
            (nth (funcall (lambda (x) (+ (mod (truncate (/ x multiplicador)) 10) 0)) (car cola)) lista-colas)
        )
    )
    (pop cola)
)

(defun llenar-colas (lista)
    (if (null lista)
        (pop lista-colas)
        (progn 
            (push (car lista) cola)
            (llenar-colas (cdr lista))
        )
    )
)

(defun llenar-colas-neg (lista)
    (if (null lista)
        (pop lista-colas-neg)
        (progn 
            (push (car lista) cola)
            (llenar-colas-neg (cdr lista)) 
        )
    )
)


(defun llamar-radix-sort (list)
    (radix-sort list 1)
)

(llamar-radix-sort '(1 2 3 4 5))
(llamar-radix-sort '(5 4 3 2 1))
(llamar-radix-sort '())
(llamar-radix-sort '(5))
(llamar-radix-sort '(1 10 100 1000))
(llamar-radix-sort '(9 89 789 6789))
(llamar-radix-sort '(6789 789 89 9))
(llamar-radix-sort '(0 0 0 0))
(llamar-radix-sort '(1 20 300 4000))
(llamar-radix-sort '(4000 300 20 1))
(llamar-radix-sort '(1 1 1 1 1))
(llamar-radix-sort '(00001 0002 03 004))
(llamar-radix-sort '(9999 1))
(llamar-radix-sort '(5 12 8 1 98 76 6 2))
(llamar-radix-sort '(-1 2 -10 -78 65 -9))
(llamar-radix-sort '(-1 2 -10 -78 65 -9 0))




;Solución: Crear una cola con 20 colar, llenar esas 20 colas y reordenar
;Esta listo al 100% :D  
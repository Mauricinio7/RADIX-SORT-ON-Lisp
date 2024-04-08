(defvar cola '())
(defvar lista-colas (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))


(defun RADIX-Sort (cola multiplicador)
    (if (< multiplicador 100000)
        (progn
            (mapcar #'(lambda (x) (llenar-lista-colar x multiplicador)) cola)
            (mapcar #'llenar-colas lista-colas)
            (setq lista-colas (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))
            (radix-sort cola (* multiplicador 10))
        )
        (print (reverse cola))
    )
)

(defun llenar-lista-colar (numero multiplicador)    
    (push (car cola)
            (nth (funcall (lambda (x) (+ (mod (truncate (/ x multiplicador)) 10) 0)) (car cola)) lista-colas)
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

(defun llamar-radix-sort (list)
    (radix-sort list 1)
)

(llamar-radix-sort '(1 2 4 9 3))




;Solución: Crear una cola con 20 colar, llenar esas 20 colas y reordenar
;Esta casi listo 
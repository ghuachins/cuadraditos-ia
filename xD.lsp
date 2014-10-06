;configuracion de cada nodo
; nodo= (estado turno regla nivel)
;f_eval=(0.5*(nro. de cuad. con 2 lineas)+0.8*(nro. de cuad. mios))-(0.2*(nro. de cuad. con 3 lineas)+0.2*(nro. de cuad. de adversario))

(defun sucesores (nodo)
	(let ((suc '()) (aux 0))
		(dotimes (i 4)
			(setq aux (random (nth 0 nodo)))
			(if (evenp aux) (setq suc (append suc (list (list aux (* -1 (nth 1 nodo)) i (+ 1 (nth 3 nodo)))))) 
							(setq suc (append suc (list (list aux (nth 1 nodo) i (+ 1 (nth 3 nodo)))))))
		)
                (format t "~%Sucesores del nodo ~s: ~s" nodo suc)
		suc
	)
)

(defun val_minimax (nodo)
	(let ((maximo -1) (minimo 30) (valor '()) (aux '()))
		(format t "~%Nodo: ~s Turno:~s Regla:~s Nivel:~s" (nth 0 nodo) (nth 1 nodo) (nth 2 nodo) (nth 3 nodo))
		(if (nodo_terminal nodo)
			(f_utilidad nodo)
			;else
			(progn
				(print "No terminal")
				(if (>= (nth 3 nodo) 3)
					(f_eval nodo)
					;else
					(progn
						(cond 
							((= (nth 1 nodo) 1) 
								(dolist (x (sucesores nodo))
									(setq aux (val_minimax x))
									(if (>= (nth 1 aux) maximo)
										(progn 
											(print "hola que hace")
                                                                                        (setq valor (list (nth 2 x) (nth 1 aux)))
                                                                                        (print valor)
											(setq maximo (nth 1 aux))
										)
									)
								)
							)
							;para el min
							((= (nth 1 nodo) -1) 
								(dolist (x (sucesores nodo))
									(setq aux (val_minimax x))
									(if (< (nth 1 aux) minimo)
										(progn 
											(setq valor (list (nth 2 x) (nth 1 aux)))
											(setq minimo (nth 1 aux))
                                                                                        (print valor)
										)
									)
								)
							)						
						)
						valor
					)
				)
			)
		)
	)
)

(defun nodo_terminal (nodo)
	(if (< (nth 0 nodo) 3) t nil)
)

(defun f_utilidad (nodo)
	(cond ((= (nth 0 nodo) 0) (list (nth 2 nodo) 0))
		((= (nth 0 nodo) 1) (list (nth 2 nodo) 10))
		((= (nth 0 nodo) 2) (list (nth 2 nodo) 20))
	)
)

(defun f_eval (nodo)
	(cond ((< (nth 0 nodo) 5) (list (nth 2 nodo) 0))
		((and (>= (nth 0 nodo) 5) (< (nth 0 nodo) 10)) (list (nth 2 nodo) 10))
		((>= (nth 0 nodo) 10) (list (nth 2 nodo) 20))
	)
)

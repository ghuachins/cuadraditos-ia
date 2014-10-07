;configuracion de cada nodo
; nodo= (estado turno regla nivel)
;f_eval=(0.5*(nro. de cuad. con 2 lineas)+0.8*(nro. de cuad. mios))-(0.2*(nro. de cuad. con 3 lineas)+0.2*(nro. de cuad. de adversario))

(setq *Tablero* '(  nil -1 nil nil nil
	                -1 nil nil nil nil
	                nil 1 nil nil nil
	                nil nil nil nil nil
	                nil nil nil nil nil)

      *Cuad* '((1 5 7 11) (3 7 9 13) (11 15 17 21) (13 17 19 23))

      *Turno* 1

      *NroHumano* 0
                *NroOrdenador* 0
)

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

(defun posicionOrig (linea)
		 (- (* (+ linea 1) 2) 1 ) 
)


(defun sucesores2 (nodo)
	(setq listaSucesor '())
	(setq turnoHijo *turno*)
    (setq tablero (nth 0 nodo))

	( dotimes (contador 12)
           
             (if (equal (nth (posicionOrig contador) tablero ) nil) 
               (progn

               	(setq tableroaux (editar-tablero (posicionOrig contador) (nth 1 nodo) tablero  ))
               	(setq tableroaux (verificarCuad (posicionOrig contador)  *turno* tableroaux) )
               	 

				(setq nodoaux  (list tableroaux *Turno* (+ contador 1) (+ (nth 3 nodo) 1) ) )
				(print nodoaux)
				(setq listaSucesor (append listaSucesor (list nodoaux) ) )
               	)    
			

			 )
			 (setq *turno* turnoHijo)
             	
    )
    listaSucesor
)


(defun pintar (pos tur tablero)
  (editar-tablero pos tur tablero)
  
)
;funcion para probar si un elemento pertenece a una lista
(defun probar (elemento lista)
  (if (equal nil (member elemento lista))
      (setq valor nil)   ;no esta
      (setq valor t)     ;si esta
  )
)
;funcion para identificar el cuadrado a pintar
(defun llenarCuad (numCuad)
  (cond ((equal numCuad 0) 6)
        ((equal numCuad 1) 8)
        ((equal numCuad 2) 16)
        ((equal numCuad 3) 18)
  )
)
;funcion para verificar si se llenó algún cuadrado
(defun verificarCuad (linea turno tablero)
	(setq tabPaint tablero)
	(setq tab tablero)
  (setq bandera nil)
  (dotimes (i 4)
     (setq cnt 0)
     (setq cuadradito (nth i *Cuad*))
     (if (probar linea cuadradito)
         (progn
              (dolist (x cuadradito)
                  (if (not(equal (nth x tab) nil))
                      (setq cnt (+ 1 cnt))
                  )
               )
               (if (= cnt 4)
                   (progn
                       (setq tabPaint (pintar (llenarCuad i) turno tab))
                       (setq bandera t)
                       (if (= turno 1)
                         (setq *NroHumano* (+ 1 *NroHumano*))
                         (setq *NroOrdenador* (+ 1 *NroOrdenador*))
                       )
                   )
               )
         )     
     )
  )
  (if (equal bandera nil) (setq *turno* (* -1 *turno*)))

   tabPaint
   ;si no cerramos un cuadrado cambiamos de turno
)



(defun editar-tablero (posicion elemento tablero )
  (setq tab tablero)
  (setq listaux '())  ;lista auxiliar
  (setq cnt 0)        ;inicializamos el contador
  (if (< posicion (length tab))
      (progn
         ;(print "caso correcto")
         ;aqui vamos a empezar a recorrer la lista
         (dolist (x tab)
            ;(print x)
            ;(print cnt)
            (if (or (< cnt posicion) (> cnt posicion))
                (progn
                  (setq listaux (append listaux (list x)))
                  ;(format t "Lista=~s ~&" listaux)
                )
            )
            (if (= cnt posicion) 
                (progn
                    (setq listaux (append listaux (list elemento)))
                    ;(format t "Lista=~s ~&" listaux)
                 )
            )
            (setq cnt (+ cnt 1))
         )
         listaux
      )
  ;si no cumple
      (print "malo")
  )
)


(setq *Tab* '(  nil	-1 	nil	1	nil
				-1 	1	1	1	1
				nil	1	nil	-1	nil
				-1	nil	nil	nil	-1
				nil	-1	nil	1	nil)
			)

; tablero 	0	1	2	3	4
;			5	6	7	8	9	
; 			10	11	12	13	14	
;			15	16	17	18	19
;			20	21	22	23	24
					
; f_eval=(0.5*(nro. de cuad. con 2 lineas)+0.8*(nro. de cuad. mios))-(0.2*(nro. de cuad. con 3 lineas)+0.2*(nro. de cuad. de adversario))

(defun feval (tablero turno)
	
	(setq num_cuad_2 0) ; numero de cuadrados con 2 lineas
	(setq num_cuad_3 0) ; numero de cuadrados con 3 lineas
	(setq num_lineas 0) ; variable que va ir contando el numero de lineas de cada cuadrado
	(setq fun_eval 0) ; valor de la funcion de evaluacion
	
	(dotimes (i (length tablero))

		(if (equal i 6)
			(progn
				(if (not (equal nil (nth 1 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 5 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 7 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 11 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
			)
		)

		(if (= num_lineas 2)
			(setq num_cuad_2 (+ num_cuad_2 1))
			(if (= num_lineas 3)
				(setq num_cuad_3 (+ num_cuad_3 1))
			)
		)
		
		(setq num_lineas 0)
		
		(if (equal i 8)
			(progn
				(if (not (equal nil (nth 3 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 7 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 9 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 13 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
			)
		)
		
		(if (= num_lineas 2)
			(setq num_cuad_2 (+ num_cuad_2 1))
			(if (= num_lineas 3)
				(setq num_cuad_3 (+ num_cuad_3 1))
			)
		)
		
		(setq num_lineas 0)
		
		(if (equal i 16)
			(progn
				(if (not (equal nil (nth 11 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 15 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 17 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 21 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
			)
		)
		
		(if (= num_lineas 2)
			(setq num_cuad_2 (+ num_cuad_2 1))
			(if (= num_lineas 3)
				(setq num_cuad_3 (+ num_cuad_3 1))
			)
		)
		
		(setq num_lineas 0)
		
		(if (equal i 18)
			(progn
				(if (not (equal nil (nth 13 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 17 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 19 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 23 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
			)
		)
		
		(if (= num_lineas 2)
			(setq num_cuad_2 (+ num_cuad_2 1))
			(if (= num_lineas 3)
				(setq num_cuad_3 (+ num_cuad_3 1))
			)
		)
		
		(setq num_lineas 0)
	)
	
	(setq cnt1 0) ; numero de cuadrados de 1
	(setq cnt_1 0) ; numero de cuadrados de -1
	
	(if (equal 1 (nth  6 tablero))
		(setq cnt1 (+ cnt1 1))
		(if (equal -1 (nth 6 tablero))
			(setq cnt_1 (+ cnt_1 1))
		)
	)
	(if (equal 1 (nth  8 tablero))
		(setq cnt1 (+ cnt1 1))
		(if (equal -1 (nth 8 tablero))
			(setq cnt_1 (+ cnt_1 1))
		)
	)
	(if (equal 1 (nth  16 tablero))
		(setq cnt1 (+ cnt1 1))
		(if (equal -1 (nth 16 tablero))
			(setq cnt_1 (+ cnt_1 1))
		)
	)
	(if (equal 1 (nth  18 tablero))
		(setq cnt1 (+ cnt1 1))
		(if (equal -1 (nth 18 tablero))
			(setq cnt_1 (+ cnt_1 1))
		)
	)
	
	; aplicando la formula tenemos
	(format t "~%Numero de cuadrados de 1: ~S" cnt1);
	(format t "~%Numero de cuadrados de -1: ~S" cnt_1);
	(format t "~%Numero de cuadrados con 2 lineas ~S" num_cuad_2)
	(format t "~%Numero de cuadrados con 3 lineas ~S" num_cuad_3)
	
	;f_eval=(0.5*(nro. de cuad. con 2 lineas)+0.8*(nro. de cuad. mios))-(0.2*(nro. de cuad. con 3 lineas)+0.2*(nro. de cuad. de adversario))
	(if (= turno 1)
		(setq fun_eval (- (+ (* 0.5 num_cuad_2) (* 0.8 cnt1)) (+ (* 0.2 num_cuad_3) (* 0.2 cnt_1))))
		(setq fun_eval (- (+ (* 0.5 num_cuad_2) (* 0.8 cnt_1)) (+ (* 0.2 num_cuad_3) (* 0.2 cnt1))))
	)
	
	(format t "~%Valor de la funcion de evaluacion: ~S" fun_eval)
)
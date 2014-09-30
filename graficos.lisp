(load "ltk")
(in-package :ltk)

;---------------------------------------------------
	; CARGAMOS EL ARCHIVO INTERFAZ.
(load "interfaz.lisp")



;---------------------------------------------------

(inicializar-juego2)

;---------------------------------------------------


(defvar *Posiciones*)
(defvar *colorFicha*)
(defvar *colorHumano*)
(defvar *colorOrdenador*)
(defvar *jugada*)
(defvar *turno*)

(setq 
  *colorLinea* '"#ccd2c8"
  *colorFicha* '"#394034"
  *colorHumano* '"#ff2f70"
  *colorOrdenador* '"#0083e8" )

(defun tablero()
	(with-ltk ()
		(let* (
					(sc (make-instance 'canvas :height 520 :width 520))
					(lienzo (canvas sc))
					; Caminos por el tablero
					
					;(linea2 (create-line lienzo (list 60 360 360 360)))

                    (ovalo1 (create-oval lienzo 50 50 70 70))
					(ovalo2 (create-oval lienzo 250 50 270 70))
					(ovalo3 (create-oval lienzo 450 50 470 70))

					(linea1 (create-line lienzo (list 80 60 240 60) ))
					(linea2 (create-line lienzo (list 280 60 440 60) ))

					(linea3 (create-line lienzo (list 60 80 60 240) ))
					(linea4 (create-line lienzo (list 260 80 260 240) ))
					(linea5 (create-line lienzo (list 460 80 460 240) ))

 					(ovalo4 (create-oval lienzo 50 250 70 270))
					(ovalo5 (create-oval lienzo 250 250 270 270))
					(ovalo6 (create-oval lienzo 450 250 470 270))

					(linea6 (create-line lienzo (list 80 260 240 260) ))
					(linea7 (create-line lienzo (list 280 260 440 260) ))

					(linea8 (create-line lienzo (list 60 280 60 440) ))
					(linea9 (create-line lienzo (list 260 280 260 440) ))
					(linea10 (create-line lienzo (list 460 280 460 440) ))

					(ovalo7 (create-oval lienzo 50 450 70 470))
					(ovalo8 (create-oval lienzo 250 450 270 470))
					(ovalo9 (create-oval lienzo 450 450 470 470))

					(linea11 (create-line lienzo (list 80 460 240 460) ))
					(linea12 (create-line lienzo (list 280 460 440 460) ))
		
			 )

		(pack sc :expand 1 :fill :both)
		(wm-title *tk* "Cuadraditos 3X3")

		(bind lienzo "<ButtonPress-1>"
			(lambda (evento)
				
				(itemconfigure lienzo linea1 :width 20)
			(itemconfigure lienzo linea1 :fill *colorHumano*)		

			)

		)
				    

		; Grosor de los caminos
		(itemconfigure lienzo linea1 :width 8)
		(itemconfigure lienzo linea1 :fill *colorLinea*)
	    (itemconfigure lienzo linea2 :width 8)
		(itemconfigure lienzo linea2 :fill *colorLinea*)
         
        (itemconfigure lienzo linea3 :width 8)
		(itemconfigure lienzo linea3 :fill *colorLinea*)
	    (itemconfigure lienzo linea4 :width 8)
		(itemconfigure lienzo linea4 :fill *colorLinea*)
		(itemconfigure lienzo linea5 :width 8)
		(itemconfigure lienzo linea5 :fill *colorLinea*)

         
		(itemconfigure lienzo ovalo1 :fill *colorFicha*)
		(itemconfigure lienzo ovalo1 :width 3)
		(itemconfigure lienzo ovalo2 :fill *colorFicha*)
		(itemconfigure lienzo ovalo2 :width 3)
		(itemconfigure lienzo ovalo3 :fill *colorFicha*)
		(itemconfigure lienzo ovalo3 :width 3)

		(itemconfigure lienzo ovalo4 :fill *colorFicha*)
		(itemconfigure lienzo ovalo4 :width 3)
		(itemconfigure lienzo ovalo5 :fill *colorFicha*)
		(itemconfigure lienzo ovalo5 :width 3)
		(itemconfigure lienzo ovalo6 :fill *colorFicha*)
		(itemconfigure lienzo ovalo6 :width 3)


		(itemconfigure lienzo linea6 :width 8)
		(itemconfigure lienzo linea6 :fill *colorLinea*)
	    (itemconfigure lienzo linea7 :width 8)
		(itemconfigure lienzo linea7 :fill *colorLinea*)
         
        (itemconfigure lienzo linea8 :width 8)
		(itemconfigure lienzo linea8 :fill *colorLinea*)
	    (itemconfigure lienzo linea9 :width 8)
		(itemconfigure lienzo linea9 :fill *colorLinea*)
		(itemconfigure lienzo linea10 :width 8)
		(itemconfigure lienzo linea10 :fill *colorLinea*)





		(itemconfigure lienzo ovalo7 :fill *colorFicha*)
		(itemconfigure lienzo ovalo7 :width 3)
		(itemconfigure lienzo ovalo8 :fill *colorFicha*)
		(itemconfigure lienzo ovalo8 :width 3)
		(itemconfigure lienzo ovalo9 :fill *colorFicha*)
		(itemconfigure lienzo ovalo9 :width 3)

		(itemconfigure lienzo linea11 :width 8)
		(itemconfigure lienzo linea11 :fill *colorLinea*)
	    (itemconfigure lienzo linea12 :width 8)
		(itemconfigure lienzo linea12 :fill *colorLinea*)

         )
      )

)

(tablero)


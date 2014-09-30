(load "ltk")
(in-package :ltk)

;---------------------------------------------------
	; CARGAMOS EL ARCHIVO INTERFAZ.
(load "interfaz.lisp")



;---------------------------------------------------

;(inicializar-juego)

;---------------------------------------------------

(defvar *Posiciones*)
(defvar *colorFicha*)
(defvar *colorHumano*)
(defvar *colorOrdenador*)
(defvar *jugada*)
(defvar *turno*)

(setq *Posiciones* '(
  ( (10 10) (110 110) ) ; 1
  ( (160 10) (260 110) ) ; 2
  ( (310 10) (410 110) ) ; 3
  ( (10 160) (110 260) ) ; 4
  ( (160 160) (260 260) ) ; 5
  ( (310 160) (410 260) ) ; 6
  ( (10 310) (110 410) ) ; 7
  ( (160 310) (260 410) ) ; 8
  ( (310 310) (410 410) ) ; 9
  )

  *colorFicha* '"#394034"
  *colorHumano* '"#ff2f70"
  *colorOrdenador* '"#0083e8" )

(defun tablero()
	(with-ltk ()
		(let* (
			(sc (make-instance 'canvas :height 420 :width 420))
			(lienzo (canvas sc))
			; Caminos por el tablero
			(linea1 (create-line lienzo (list 60 60 360 60)))
		
			 )

		(pack sc :expand 1 :fill :both)
		(wm-title *tk* "Cuadraditos 3X3")

		
	    

		; Grosor de los caminos
		(itemconfigure lienzo linea1 :width 8)
		
         )
      )

)
(tablero)


(load "ltk")
(in-package :ltk)


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
		(format-wish "package require Img")
		(let* (
					(img (make-image))
					(sc (make-instance 'canvas :height 520 :width 520))
					(lienzo (canvas sc))	
			 )

		(pack sc :expand 1 :fill :both)
		(image-load img "iron-man.png")
		(create-image sc 0 0 :image img)
		(wm-title *tk* "Cuadraditos 3X3")

		  

         )
      )

)

(tablero)


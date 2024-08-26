(ql:quickload "cl-raylib")

(defstruct posicao
  x y)

(defstruct cobra
  posicoes direcao)

(setq cobrinha 
  (make-cobra :posicoes (list (make-posicao :x 100 :y 100)) :direcao 'baixo))

(setq comida 
  (make-posicao :x 15 :y 15))

(defun draw-cobra ()
  (dolist (posicao (cobra-posicoes cobrinha)) 
    (cl-raylib:draw-rectangle (posicao-x posicao) 
                              (posicao-y posicao) 
                              15 15 :red)))

(defun draw-comida ()
  (cl-raylib:draw-rectangle (posicao-x comida)
                            (posicao-y comida)
                            15 15 :green))

; não pode ir na direção contrária (cobra indo pra baixo não pode rastejar pra cima)
(defun input ()
  (let ((direcao-atual (cobra-direcao cobrinha)))
    (cond
      ((and (cl-raylib:is-key-down :key-up) (not (eq direcao-atual 'baixo)))
       (setf (cobra-direcao cobrinha) 'cima))
      ((and (cl-raylib:is-key-down :key-down) (not (eq direcao-atual 'cima)))
       (setf (cobra-direcao cobrinha) 'baixo))
      ((and (cl-raylib:is-key-down :key-left) (not (eq direcao-atual 'direita)))
       (setf (cobra-direcao cobrinha) 'esquerda))
      ((and (cl-raylib:is-key-down :key-right) (not (eq direcao-atual 'esquerda)))
       (setf (cobra-direcao cobrinha) 'direita)))))

(defun adiciona-segmento ()
  (let* ((direcao (cobra-direcao cobrinha))
         (posicoes (cobra-posicoes cobrinha))
         (cabeça (car posicoes))
         (nova-cabeça (make-posicao :x (posicao-x cabeça) :y (posicao-y cabeça)))
         (x-delta (case direcao
                    ('cima 0)
                    ('baixo 0)
                    ('direita 15)
                    ('esquerda -15)))
         (y-delta (case direcao
                    ('cima -15)
                    ('baixo 15)
                    ('esquerda 0)
                    ('direita 0))))
    (setf (posicao-x nova-cabeça) (+ (posicao-x nova-cabeça) x-delta))
    (setf (posicao-y nova-cabeça) (+ (posicao-y nova-cabeça) y-delta))
    (setf (cobra-posicoes cobrinha) 
          (cons nova-cabeça (cobra-posicoes cobrinha)))))

; adiciona nova cabeça e remove o rabo
(defun mover-cobra ()
  (adiciona-segmento)
  (setf (cobra-posicoes cobrinha) (butlast (cobra-posicoes cobrinha))))

(defun colisao-comida ()
  (let* ((posicoes (cobra-posicoes cobrinha))
         (cabeça (car posicoes))
         (rec1 (cl-raylib:make-rectangle :x (posicao-x cabeça) :y (posicao-y cabeça) :width 15 :height 15))
         (rec2 (cl-raylib:make-rectangle :x (posicao-x comida) :y (posicao-y comida) :width 15 :height 15)))
    (when (cl-raylib:check-collision-recs rec1 rec2)
      (adiciona-segmento)
      (setf (posicao-x comida) (* 15 (cl-raylib:get-random-value 0 52)))
      (setf (posicao-y comida) (* 15 (cl-raylib:get-random-value 0 39))))))

(defun colisao-corpo ()
  (let* ((posicoes (cobra-posicoes cobrinha))
         (cabeça (car posicoes))
         (rec1 (cl-raylib:make-rectangle :x (posicao-x cabeça) 
                                         :y (posicao-y cabeça) 
                                         :width 15 
                                         :height 15)))
    (dolist (segmento (cdr posicoes))
      (let ((rec2 (cl-raylib:make-rectangle :x (posicao-x segmento) 
                                            :y (posicao-y segmento) 
                                            :width 15 
                                            :height 15)))
        (when (cl-raylib:check-collision-recs rec1 rec2)
          (cl-raylib:close-window)
          (sb-ext:quit :unix-status 0))))))

(defun createWindow ()
  (cl-raylib:init-window 800 600 "Snake Game")
  (cl-raylib:set-target-fps 15))

(defun main () 
  (createWindow)
  (loop
    until (cl-raylib:window-should-close)
    do
      (input)
      (mover-cobra)
      (colisao-comida)
      (colisao-corpo)
      (cl-raylib:begin-drawing)
      (cl-raylib:clear-background :raywhite)
      (draw-comida)
      (draw-cobra)
      (cl-raylib:end-drawing))
  
  (cl-raylib:close-window))

(main)

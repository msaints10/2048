(require graphics/graphics)
;;ingreso de la ip a utilizar
(display "Coloque la IP que utilizará: ")
;;se definen valores de entrada y salida recibidos por el server
(define IPdress (read-line))
(define-values (entrada2 salida2) (tcp-connect IPdress 1155))
;;se crea el plano
(open-graphics)
(define plano (open-viewport "2048 - Blood Dragon - 1 vs 1 online - Jugador 1" 1000 700))
(((draw-pixmap-posn "fondo_cliente_2048.jpg") plano) (make-posn 0 0) "white")
;;procedimiento que ejecuta el juego del cliente
(define (client_player_2048 base_user)
  (define pool 0)
  ;Base que va a ser recibida por el usuario
  (define Base base_user)

  ;Definicion de variables maximas
  (define maximo1 0)
  (define maximo2 0)
  (define maximo3 0)
  (define maximo4 0)
  (define maximo 0)
  ;Se crea el plano con un vector de vectores creado una matriz de 4 x 4
  (define col (make-vector 4))
  (vector-set! col 0 (make-vector 4 0))
  (vector-set! col 1 (make-vector 4 0))
  (vector-set! col 2 (make-vector 4 0))
  (vector-set! col 3 (make-vector 4 0))


  ;;variable contadora de movimientos
  (define vcontador_movimientos 0)

  ;Retorna la pocision en la que se colocará el número
  (define (numero-posicion posicion)
    (define retorno 0)
    (cond
      [(= posicion 0) (set! retorno 115)]
      [(= posicion 1) (set! retorno 267)]
      [(= posicion 2) (set! retorno 425)]
      [(= posicion 3) (set! retorno 570)]
      )
    retorno
    )

  ;Retorna la pocision en la cual se dibujará la fila
  (define (reglamento-dibujo fila)
    (define fila0 50)
    (define fila1 200)
    (define fila2 350)
    (define fila3 500)
    (define retorno 0)
    (cond
      [(= fila 0)(set! retorno fila0)]
      [(= fila 1)(set! retorno fila1)]
      [(= fila 2)(set! retorno fila2)]
      [(= fila 3)(set! retorno fila3)]
      )
    retorno
    )

  ;Dibuja el tablero del juego
  (define (grafico)
    (((draw-pixmap-posn "fondo_cliente_2048.jpg") plano) (make-posn 0 0) "white")
    ((draw-solid-rectangle plano ) (make-posn 50 50) 600 600 "White")
    ((draw-rectangle plano ) (make-posn 50 50) 600 600 "black")
    (do ((contfila 0 (+ contfila 1)))
      ((= contfila 4))
      (do ((contcolumna 0 (+ contcolumna 1)))
        ((= contcolumna 4))
        (set! pool (reglamento-dibujo contcolumna))
        (cond
          [(= contfila 0)((draw-rectangle plano ) (make-posn pool 50) 150 150 "black")]
          [(= contfila 1)((draw-rectangle plano ) (make-posn pool  200) 150 150 "black")]
          [(= contfila 2)((draw-rectangle plano ) (make-posn pool  350) 150 150 "black")]
          [(= contfila 3)((draw-rectangle plano ) (make-posn pool  500) 150 150 "black")]
          )
        )
      )    
    )
  (grafico)

  ;Muestra la tabla del juego por cada movimiento en consola
  (define (console-plano)
    (define contador 0)
    (define (ciclo)
      (if (not (= (vector-length col) contador))
          (begin
            (display (vector-ref col contador))(newline)
            (set! contador (+ contador 1))         
            (ciclo))
          )
      )
    (ciclo)
    ((draw-solid-rectangle plano) (make-posn 840  200) 100 20  "white")
    ((draw-string plano) (make-posn 840 215) (number->string (buscarMaximo)) "black")
    (write (string-append "1-" (number->string(buscarMaximo))) salida2)
    (flush-output salida2)
    )

  ;hace efectivo el contador de movimientos
  (define (contador_movimientos)
    (set! vcontador_movimientos (+ vcontador_movimientos 1))
    ((draw-solid-rectangle plano) (make-posn 840  166) 100 20  "white")
    ((draw-string plano) (make-posn 840 181) (number->string vcontador_movimientos) "black")
    (write (string-append "2-" (number->string vcontador_movimientos)) salida2)
    (flush-output salida2)
    )

  ;;verifica las casillas vacias del plano
  (define (verifica_vacias)
    (define celda 0)
    (define aux 0)
    (do ((contfila 0 (+ contfila 1)))
      ((= contfila 4))
      (do ((contcolumna 0 (+ contcolumna 1)))
        ((= contcolumna 4) ())
        (set! celda (vector-ref (vector-ref col contfila) contcolumna))
        (if (= celda 0)
            (begin
              (set! aux (+ aux 1))
              )
            )            
        )
      )
    ((draw-solid-rectangle plano) (make-posn 840  134) 100 20  "white")
    ((draw-string plano) (make-posn 840 147) (number->string aux) "black")
    (write (string-append "3-" (number->string aux)) salida2)
    (flush-output salida2)
    )

  ;;verifica el tiro realizado con el teclado o flechas
  (define (verifica_tiro opcion)
    (cond
      [(= opcion 1)
       (begin
         ((draw-solid-rectangle plano) (make-posn 840 103) 100 20  "white")
         ((draw-string plano) (make-posn 840 117) "Izquierda" "black")
         (write (string-append "4-" "Izquierda") salida2)
         (flush-output salida2)
         )]
      [(= opcion 2)
       (begin
         ((draw-solid-rectangle plano) (make-posn 840 103) 100 20  "white")
         ((draw-string plano) (make-posn 840 117) "Derecha" "black")
         (write (string-append "4-" "Derecha") salida2)
         (flush-output salida2)
         )]
      [(= opcion 3)
       (begin
         ((draw-solid-rectangle plano) (make-posn 840 103) 100 20  "white")
         ((draw-string plano) (make-posn 840 117) "Abajo" "black")
         (write (string-append "4-" "Abajo") salida2)
         (flush-output salida2)
         )]
      [(= opcion 4)
       (begin
         ((draw-solid-rectangle plano) (make-posn 840 103) 100 20  "white")
         ((draw-string plano) (make-posn 840 117) "Arriba" "black")
         (write (string-append "4-" "Arriba") salida2)
         (flush-output salida2)
         )]
      )
    )
  
  ; devuelve el maximo de todas las filas // para eso utilizo el "buscarMaximoF"
  (define (buscarMaximo)
    (define valor1 (buscarMaximoF 0))
    (define valor2 (buscarMaximoF 1))
    (define valor3 (buscarMaximoF 2))
    (define valor4 (buscarMaximoF 3))
    (define nada 0)
    (if (integer? valor1) (begin (set! nada 1) ) (begin
                                                   (set! valor1 0)
                                                   ))
    (if (integer? valor2) (begin (set! nada 1)) (begin
                                                  (set! valor2 0)
                                                  ))
    (if (integer? valor3) (begin (set! nada 1)) (begin
                                                  (set! valor3 0)
                                                  ))
    (if (integer? valor4) (begin (set! nada 1)) (begin
                                                  (set! valor4 0)
                                                  ))
    (cond
      [(and (>= valor1 valor2)(>= valor1 valor3)(>= valor1 valor4)) (begin
                                                                      valor1)]
      [(and (>= valor2 valor1)(>= valor2 valor3)(>= valor2 valor4)) (begin
                                                                      valor2)]
      [(and (>= valor3 valor1)(>= valor3 valor2)(>= valor3 valor4)) (begin
                                                                      valor3)]
      [(and (>= valor4 valor1)(>= valor4 valor2)(>= valor4 valor3)) (begin
                                                                      valor4)]
      )
    )
  ; devuelve el maximo pero de cada fila del vector
  (define (buscarMaximoF fila)
    (define contador 0)
    (define contadorAux 0)
    (define valor1 0)
    (define valor2 0)
    (define valor3 0)
    (define valor4 0)
    (if (< contador 5)
        (begin
          (set! valor1 (vector-ref (vector-ref col fila) 0))
          (set! valor2 (vector-ref (vector-ref col fila) 1))
          (set! valor3 (vector-ref (vector-ref col fila) 2))
          (set! valor4 (vector-ref (vector-ref col fila) 3))
          (cond
            [(and (>= valor1 valor2)(>= valor1 valor3)(>= valor1 valor4)) (begin
                                                                            valor1)]
            [(and (>= valor2 valor1)(>= valor2 valor3)(>= valor2 valor4)) (begin
                                                                            valor2)]
            [(and (>= valor3 valor1)(>= valor3 valor2)(>= valor3 valor4)) (begin
                                                                            valor3)]
            [(and (>= valor4 valor1)(>= valor4 valor2)(>= valor4 valor3)) (begin
                                                                            valor4)]
            )
          )
        )
    )

  (define (tablero-lleno?); verifica si el tablero esta lleno
    (define celda 0)
    (define aux 0)
    (do ((contfila 0 (+ contfila 1)))
      ((= contfila 4))
      (do ((contcolumna 0 (+ contcolumna 1)))
        ((= contcolumna 4) ())
        (set! celda (vector-ref (vector-ref col contfila) contcolumna))
        (if (= celda 0)
            (begin
              (set! aux (+ aux 1))
              )
            )
            
        )
      )
    aux
    )

  ;Construye un par con valores de "x","y" aleatorios y devuelve una posicion 
  (define (posicion_retorno)
    (define x (random 4))
    (define y (random 4))
    (define pos (cons x y))
    (newline)(display pos)(newline)
    pos
    )

  ;Regresa el valor aleatorio cada que el jugador haga un movimiento.
  (define (dar-valor)
    (define aux (random Base))
    (define numero 0)
    (define pos (posicion_retorno))
    (if (= aux 0)
        (set! numero Base)
        (set! numero (* Base 2))
        )
    (define vacio (vector-ref (vector-ref col (car pos)) (cdr pos)))
    (if (= vacio 0)
        (begin
          (vector-set! (vector-ref col (car pos)) (cdr pos) numero)
          ((draw-string plano) (make-posn (numero-posicion (cdr pos)) (numero-posicion (car pos))) (number->string numero) "black")
          (verifica_vacias)
          )
        (begin
          (do ((cont 2 (+ cont 1)))
            ((or (<= cont 1) (= cont 20)))
            (set! pos (posicion_retorno))
            (set! vacio (vector-ref (vector-ref col (car pos)) (cdr pos)))
            (if (= vacio 0)
                (begin
                  (vector-set! (vector-ref col (car pos)) (cdr pos) numero)
                  ((draw-string plano) (make-posn (numero-posicion (cdr pos)) (numero-posicion (car pos))) (number->string numero) "black")
                  (verifica_vacias)
                  (set! cont 0)
                  )
                )
            )
          )
        )
    )

  ;Primeros valores
  (define (principio)    
    (do((contador 0 (+ contador 1)))
      ((= contador 2))
      (dar-valor)
      )    
    ((draw-solid-rectangle plano) (make-posn 840  200) 100 20  "white")
    ((draw-string plano) (make-posn 840 215) (number->string (buscarMaximo)) "black")
    (write (string-append "1-" (number->string(buscarMaximo))) salida2)
    (flush-output salida2)
    ((draw-solid-rectangle plano) (make-posn 840  166) 100 20  "white")
    ((draw-string plano) (make-posn 840 181) (number->string 0) "black")
    (write (string-append "2-" (number->string 0)) salida2)
    (flush-output salida2)
    (verifica_vacias)
    ((draw-string plano) (make-posn 855 240) (number->string Base) "red")
    )

  ; Procedimiento que mueve los numeros dependiendo del movimiento
  (define (movimiento accion)
    (define actual 0)
    (define anterior 0)
    (define indice 0)
    (if (= accion 0)
        (begin
          (do ((contfila 0 (+ contfila 1)))
            ((= contfila 4))
            (do ((contcolumna 0 (+ contcolumna 1)))
              ((= contcolumna 4))      
              (set! actual (vector-ref (vector-ref col contfila) contcolumna))
              (if (not(= actual 0))           
                  (begin
                    (set! indice (do ((cont 0 (+ cont 1)))
                                   ((or (= cont 4)(= (vector-ref (vector-ref col contfila) cont) 0)) cont)                 
                                   ))
                    (if (> contcolumna indice)
                        (begin
                          (vector-set! (vector-ref col contfila) indice actual)                
                          ((draw-string plano) (make-posn (numero-posicion indice) (numero-posicion contfila)) (number->string actual) "black")
                          )
                        )            
                    (if (and (not(= contcolumna 0)) (> contcolumna indice))
                        (begin
                          (vector-set! (vector-ref col contfila) contcolumna 0)
                          ((clear-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string actual))
                          )
                        )
                    )
                  )
              )
            )
          (dar-valor)
          (console-plano)
          (set! actual 0)
          (set! anterior 0)
          (set! indice 0))
        )
    (if (= accion 1)
        (begin
          (do ((contfila 3 (- contfila 1)))
            ((= contfila -1))
            (do ((contcolumna 3 (- contcolumna 1)))
              ((= contcolumna -1))      
              (set! actual (vector-ref (vector-ref col contfila) contcolumna))
              (if (not(= actual 0))          
                  (begin
                    (set! indice (do ((cont 3 (- cont 1)))
                                   ((or (= cont -1)(= (vector-ref (vector-ref col contfila) cont) 0)) cont)                 
                                   ))
                    (if (< contcolumna indice)
                        (begin
                          (vector-set! (vector-ref col contfila) indice actual)
                          ((draw-string plano) (make-posn (numero-posicion indice) (numero-posicion contfila)) (number->string actual) "black")
                          )
                        )            
                    (if (and (not(= contcolumna 3)) (< contcolumna indice))
                        (begin
                          (vector-set! (vector-ref col contfila) contcolumna 0)
                          ((clear-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string actual))
                          )
                        )
                    )
                  )
              )
            )
          (dar-valor)
          (console-plano)
          (set! actual 0)
          (set! anterior 0)
          (set! indice 0))
        )
    (if (= accion 2)
        (begin
          (do ((contcolumna 0 (+ contcolumna 1)))
            ((= contcolumna 4))
            (do ((contfila 0 (+ contfila 1)))
              ((= contfila 4))      
              (set! actual (vector-ref (vector-ref col contfila) contcolumna))
              (if (not(= actual 0))          
                  (begin
                    (set! indice (do ((cont 0 (+ cont 1)))
                                   ((or (= cont 4)(= (vector-ref (vector-ref col cont) contcolumna) 0)) cont)                 
                                   ))
                    (if (> contfila indice)
                        (begin
                          (vector-set! (vector-ref col indice) contcolumna actual)
                          ((draw-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion indice)) (number->string actual) "black")
                          )
                        )            
                    (if (and (not(= contfila 0)) (> contfila indice))
                        (begin
                          (vector-set! (vector-ref col contfila) contcolumna 0)
                          ((clear-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string actual))
                          )
                        )
                    )
                  )
              )
            )
          (dar-valor)
          (console-plano)
          (set! actual 0)
          (set! anterior 0)
          (set! indice 0))
        )
    (if (= accion 3)
        (begin
          (do ((contcolumna 3 (- contcolumna 1)))
            ((= contcolumna -1))
            (do ((contfila 3 (- contfila 1)))
              ((= contfila -1))      
              (set! actual (vector-ref (vector-ref col contfila) contcolumna))
              (if (not(= actual 0))          
                  (begin
                    (set! indice (do ((cont 3 (- cont 1)))
                                   ((or (= cont -1) (= (vector-ref (vector-ref col cont) contcolumna) 0)) cont)                 
                                   ))
                    (if (< contfila indice)
                        (begin                  
                          (vector-set! (vector-ref col indice) contcolumna actual)
                          ((draw-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion indice)) (number->string actual) "black")
                          )
                        )            
                    (if (and (not(= contfila 3)) (< contfila indice))
                        (begin
                          (vector-set! (vector-ref col contfila) contcolumna 0)
                          ((clear-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string actual))
                          )
                        )
                    )
                  )
              )
            )
          (dar-valor)
          (console-plano)
          (set! actual 0)
          (set! anterior 0)
          (set! indice 0))
        )
    )

  ;Suma los valores a la izquierda
  (define (suma-izquierda)
    (define actual 0)
    (define siguiente 0)
    (define indice 0)
    (do ((contfila 0 (+ contfila 1)))
      ((= contfila 4))
      (do ((contcolumna 0 (+ contcolumna 1)))
        ((= contcolumna 4))      
        (set! actual (vector-ref (vector-ref col contfila) contcolumna))
        (if (and (not(= actual 0)) (< contcolumna 3))
            (begin
              (set! siguiente (vector-ref (vector-ref col contfila) (+ contcolumna 1)))
              (if (= actual siguiente)
                  (begin (vector-set! (vector-ref col contfila) contcolumna (+ actual siguiente))
                         ((clear-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string actual))
                         ((draw-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string (+ actual siguiente)) "black")
                         (verifica_vacias)
                         (if (< contcolumna 3)
                             (begin (vector-set! (vector-ref col contfila) (+ contcolumna 1) 0)
                                    ((clear-string plano) (make-posn (numero-posicion (+ contcolumna 1)) (numero-posicion contfila)) (number->string siguiente))
                                    )
                             )
                         )
                  )
              )
            )      
        )
      )
    (movimiento 0)
    )

  ;Suma los numeros a la derecha
  (define (suma-derecha)
    (define actual 0)
    (define siguiente 0)
    (define indice 0)
    (do ((contfila 3 (- contfila 1)))
      ((= contfila -1))
      (do ((contcolumna 3 (- contcolumna 1)))
        ((= contcolumna -1))      
        (set! actual (vector-ref (vector-ref col contfila) contcolumna))
        (if (and (not(= actual 0)) (> contcolumna 0))
            (begin          
              (set! siguiente (vector-ref (vector-ref col contfila) (- contcolumna 1)))
              (if (= actual siguiente)
                  (begin (vector-set! (vector-ref col contfila) contcolumna (+ actual siguiente))
                         ((clear-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string actual))
                         ((draw-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string (+ actual siguiente)) "black")
                         (verifica_vacias)
                         (if (> contcolumna 0)
                             (begin
                               (vector-set! (vector-ref col contfila) (- contcolumna 1) 0)
                               ((clear-string plano) (make-posn (numero-posicion (- contcolumna 1)) (numero-posicion contfila)) (number->string siguiente))
                               )
                             )
                         )
                  )
              )
            )      
        )
      )
    (movimiento 1)
    )

  ;Suma los numeros hacia arriba
  (define (suma-arriba)
    (define actual 0)
    (define siguiente 0)
    (define indice 0)
    (do ((contcolumna 0 (+ contcolumna 1)))
      ((= contcolumna 4))
      (do ((contfila 0 (+ contfila 1)))
        ((= contfila 4))      
        (set! actual (vector-ref (vector-ref col contfila) contcolumna))
        (if (and (not(= actual 0)) (< contfila 3))
            (begin
              (set! siguiente (vector-ref (vector-ref col (+ contfila 1)) contcolumna))
              (if (= actual siguiente)
                  (begin (vector-set! (vector-ref col contfila) contcolumna (+ actual siguiente))
                         ((clear-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string actual))
                         ((draw-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string (+ actual siguiente)) "black")
                         (verifica_vacias)
                         (if (< contfila 3)
                             (begin
                               (vector-set! (vector-ref col (+ contfila 1)) contcolumna 0)
                               ((clear-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion (+ contfila 1))) (number->string siguiente))
                               )
                             )
                         )
                  )
              )
            )      
        )
      )
    (movimiento 2)
    )

  ;Suma los numeros hacia abajo
  (define (suma-abajo)
    (define actual 0)
    (define siguiente 0)
    (define indice 0)
    (do ((contcolumna 3 (- contcolumna 1)))
      ((= contcolumna -1))
      (do ((contfila 3 (- contfila 1)))
        ((= contfila -1))      
        (set! actual (vector-ref (vector-ref col contfila) contcolumna))
        (if (and (not(= actual 0)) (> contfila 0))
            (begin          
              (set! siguiente (vector-ref (vector-ref col (- contfila 1)) contcolumna))
              (if (= actual siguiente)
                  (begin (vector-set! (vector-ref col contfila) contcolumna (+ actual siguiente))
                         ((clear-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string actual))
                         ((draw-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion contfila)) (number->string (+ actual siguiente)) "black")
                         (verifica_vacias)
                         (if (> contfila 0)
                             (begin
                               (vector-set! (vector-ref col (- contfila 1 )) contcolumna 0)
                               ((clear-string plano) (make-posn (numero-posicion contcolumna) (numero-posicion (- contfila 1))) (number->string siguiente))
                               )
                             )
                         )
                  )
              )
            )      
        )
      )
    (movimiento 3)
    )

  ;Ejecuta el procedimiento de suma correspondiente dependiendo de la acción del teclado
  (define (evento-teclado)
    (define key "")
    (define lleno 0)
    (define ganador 0)
    (do ((cont 2 (+ cont 1)))
      ((<= cont 1))
      (set! key (get-key-press plano))
      (set! lleno (tablero-lleno?))    
      (if (> lleno 0)
          (begin
            (cond
              [(or (equal? (key-value key) 'left)(equal? (key-value key) #\a)) (begin (contador_movimientos) (verifica_tiro 1) (suma-izquierda))]
              [(or (equal? (key-value key) 'right)(equal? (key-value key) #\d)) (begin (contador_movimientos) (verifica_tiro 2) (suma-derecha))]
              [(or (equal? (key-value key) 'down)(equal? (key-value key) #\s)) (begin (contador_movimientos) (verifica_tiro 3) (suma-abajo))]
              [(or (equal? (key-value key) 'up)(equal? (key-value key) #\w )) (begin (contador_movimientos) (verifica_tiro 4) (suma-arriba))])
            )
          (begin
            (((draw-pixmap-posn "endgamecliente_2048.jpg") plano) (make-posn 50 125))
            (kill-thread thread_verifica_rendicion)
            )
          )
      )
    )

  (define (verifica_rendicion)
    (let ciclo
      ()
      (define opcionmenu (get-mouse-click plano))
      (define x (posn-x (mouse-click-posn opcionmenu)))
      (define y (posn-y (mouse-click-posn opcionmenu)))
      (cond
        ((and (< 758 x 910) (< 271 y 307))
         (begin
           (kill-thread tarea-paralela)
           (((draw-pixmap-posn "endgamecliente_2048.jpg") plano) (make-posn 50 125))
           )
         )   
        (else
         (ciclo)          
         )
        )
      )
    )

  ;Se define tareas paralelas
  (define tarea-paralela  (thread evento-teclado))
  (define thread_verifica_rendicion (thread verifica_rendicion))
  (principio)
  )
;;la base recibida por el server y el inicio del juego
(define base_recibida (read entrada2))
(client_player_2048 (string->number base_recibida))

(define (escuchar_server)
  (let ciclo_escuchar
    ()
    (define msg (read entrada2))
    (cond
      ((equal? (substring msg 0 2) "1-") ;; 1- nos indicara el numero mayor del contrincante
       (begin
         ((draw-solid-rectangle plano) (make-posn 840  596) 100 20  "white")
         ((draw-string plano) (make-posn 840 607) (substring msg 2 (string-length msg)) "black")
         (ciclo_escuchar)
         )
       )
      ((equal? (substring msg 0 2) "2-")  ;; 2- nos indicara el numero de movimientos realizados por el contrincante
       (begin
         ((draw-solid-rectangle plano) (make-posn 840  549) 100 20  "white")
         ((draw-string plano) (make-posn 840 560) (substring msg 2 (string-length msg)) "black")
         (ciclo_escuchar)
         )
       )
      ((equal? (substring msg 0 2) "3-")  ;; 3- nos indicara el numero de casillas vacias del contrincante
       (begin
         ((draw-solid-rectangle plano) (make-posn 840  494) 100 20  "white")
         ((draw-string plano) (make-posn 840 505) (substring msg 2 (string-length msg)) "black")
         (ciclo_escuchar)
         )
       )
      ((equal? (substring msg 0 2) "4-")  ;; 4- nos indicara el movimiento escogido por el contrincante
       (begin
         ((draw-solid-rectangle plano) (make-posn 840  444) 100 20  "white")
         ((draw-string plano) (make-posn 840 455) (substring msg 2 (string-length msg)) "black")
         (ciclo_escuchar)
         )
       )
      (else
       (begin
         (ciclo_escuchar)
         )
       )
      )
    )
  )

(define tarea-paralela1 (thread escuchar_server))
    .text
    .global _start

_start:
    // Inicializamos los registros con valores
    mov X0, #5              // Inicializamos X0 con 5 (contador)
    mov X1, #0              // Inicializamos X1 con 0 (acumulador)
    
    // Salto hacia adelante
    b forward_jump          // Salta a la etiqueta forward_jump

backward_jump:              // Etiqueta para salto hacia atrás
    // Aquí solo se ejecuta si se vuelve de un salto hacia atrás
    add X1, X1, X0          // Suma el valor de X0 al acumulador X1

forward_jump:               // Etiqueta para salto hacia adelante
    // Aquí se ejecuta después del salto hacia adelante
    subs X0, X0, #1          // Decrementa X0
    cmp X0, #0              // Compara X0 con 0
    beq end_program         // Si X0 == 0, termina el programa

    // Salto hacia atrás
    b backward_jump         // Salta hacia la etiqueta backward_jump

end_program:                // Fin del programa
    hlt #0                  // Detiene la ejecución del programa

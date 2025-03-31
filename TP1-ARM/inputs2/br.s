    .text
    .global _start

_start:
    mov X0, #0x4   // Cargar la dirección de la etiqueta 'skip_here' en X0
    lsl X0, X0, 20
    adds x0, x0, 8

    // Salto a la dirección contenida en X0
    br X0               // Saltar a la dirección en X0

    // Esta parte nunca se ejecutará, ya que saltamos antes de llegar a ella
    mov X1, #0x1234     // Esto nunca se ejecuta

skip_here:               // Etiqueta de destino del salto
    mov X2, #0x5678     // Si llegamos aquí, movemos un valor a X2
    mov X3, #0x9ABC     // También cargamos un valor en X3

    // Finalización del programa
    hlt #0               // Terminamos la ejecución del programa    

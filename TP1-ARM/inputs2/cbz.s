    .text
    .global _start

_start:
    // Caso 1: CBZ con un registro que es cero, debe saltar
    mov X0, #0              // Poner 0 en X0
    cbz X0, label_1         // Si X0 es cero, saltar a label_1

    // Código después de CBZ (no se ejecutará si el salto es tomado)
    mov X1, #1              // Poner un valor en X1
    mov X2, #2              // Poner un valor en X2
    b end_test              // Ir al final

label_1:
    // Este código se ejecutará solo si X0 es cero (debe saltar aquí)
    mov X1, #100            // Poner valor en X1 para verificar que saltó
    mov X2, #200            // Poner valor en X2 para verificar que saltó

    // Caso 2: CBZ con un registro no cero, no saltará
    mov X3, #1              // Poner 1 en X3 (no cero)
    cbz X3, label_2         // Si X3 es cero, saltará a label_2 (no se ejecutará)

    // Este código se ejecutará porque X3 no es cero
    mov X4, #300            // Poner valor en X4 para verificar que no saltó
    mov X5, #400            // Poner valor en X5 para verificar que no saltó
    b end_test              // Ir al final

label_2:
    // Este código se ejecutará solo si X3 es cero
    mov X4, #500            // Poner valor en X4 para verificar que no saltó
    mov X5, #600            // Poner valor en X5 para verificar que no saltó

end_test:
    // Instrucción HLT para terminar el programa
    hlt #0                  // Detener la ejecución del programa

// Test de LDURB: Carga un byte (0xFF) en un registro cuando el valor de la dirección es grande.
.text
.global _start

_start:
    // Inicializar registros para pruebas
    mov X0, 0x100        // Cargar la dirección base de memoria en X0 (registro base)
    mov X1, 0x400         // Cargar un valor grande en X1 (registro base)
    lsl X0, x0, 20
    lsl x1, x1, 20
    
    // Escribir un valor grande en la memoria
    mov X2, 0xDEAD        // Valor de ejemplo para la memoria
    stur X2, [X0, #4]          // Almacenar en la memoria, en la dirección 0x10000004
    stur X2, [X1, #8]          // Almacenar en la memoria, en la dirección 0x40000008

    // LDURB - Test 1: Lectura de un byte desde X0 + 4
    ldur x3, [X0, #4]         // Cargar un byte desde la memoria en X0 + 4
    
    // LDURB - Test 2: Lectura de un byte desde X1 + 8
    ldur x4, [X1, #8]         // Cargar un byte desde la memoria en X1 + 8

    lsl x3, x3, 20

    ldur x3, [x0, #4]

    // Hacer un HALT para finalizar la ejecución
    hlt 0x0

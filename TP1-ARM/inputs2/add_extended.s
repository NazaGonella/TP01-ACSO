// Test para ADD (Extended Register)
.text
.global _start

_start:
    // Test 1: ADD (Extended Register) que da 0
    mov X5, 0x0              // Cargar 0 en X5
    mov X6, 0x0              // Cargar 0 en X6
    add X5, X5, X6           // Sumar X6 a X5, el resultado debe ser 0

    // Test 2: ADD (Extended Register) con números que suman entre 0 y 2^30
    mov X7, 0x2000           // Cargar 8,192 en X7
    mov X8, 0x1000           // Cargar 4,096 en X8
    add X7, X7, X8           // Sumar ambos registros, el resultado debe estar entre 0 y 2^30

    // Test 3: ADD (Extended Register) que causa Overflow (Más de 64 bits)
    mov X9, 0x7FFF           // Cargar un número cercano a 2^16 en X9
    lsl X9, X9, #48          // Desplazar 48 bits a la izquierda para crear un valor de 64 bits
    mov X10, 0x1             // Cargar 1 en X10
    add X9, X9, X10          // Sumar 1 a X9, lo que debería causar overflow

    // Test 4: ADD (Extended Register) con números negativos
    mov X11, 0x8000 // Cargar el valor más bajo de 64 bits en X11
    lsl X11, x11, 48
    mov X12, 0x1000 // Cargar un valor positivo grande en X12
    lsl x12, x12, 48
    add X11, X11, X12            // Sumar ambos registros, el resultado debe ser un número positivo

    // Test 5: ADD (Extended Register) con números pequeños que no causan Overflow
    mov X13, 0x30            // Cargar 48 en X13
    mov X14, 0x10            // Cargar 16 en X14
    add X13, X13, X14        // Sumar X14 a X13, el resultado debe ser 64

    // Hacer un HALT para finalizar la ejecución
    hlt 0x0

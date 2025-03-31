// Batería de Tests para ADD (Immediate) y ADD (Extended Register)
.text
.global _start

_start:
    // Test 1: ADD (Immediate) que da 0
    mov X0, 0x0              // Cargar 0 en X0
    add X0, X0, #0           // Sumar 0 a X0, el resultado debe ser 0

    // Test 2: ADD (Immediate) que da un número entre 0 y 2^30
    mov X1, 0x4000           // Cargar 16,384 en X1
    add X1, X1, #0x1000      // Sumar 4,096 a X1, el resultado debe estar entre 0 y 2^30

    // Test 3: ADD (Immediate) que causa Overflow (Mayor que 64 bits)
    mov X2, 0x7FFF           // Cargar un número cercano a 2^16 en X2 (máximo para MOV)
    lsl X2, X2, #48          // Desplazar 48 bits a la izquierda para crear un valor de 64 bits
    add X2, X2, #1           // Sumar 1, lo que debería causar overflow

    // Test 4: ADD (Immediate) con número negativo (Dará un valor entre 0 y 2^30)
    mov X3, 0x8000           // Cargar un número negativo en X3
    add X3, X3, #0x1000      // Sumar 4,096 a X3, el resultado debe ser positivo

    // Test 5: ADD (Immediate) con números pequeños que no causan Overflow
    mov X4, 0x10             // Cargar 16 en X4
    add X4, X4, #0x20        // Sumar 32 a X4, el resultado debe ser 48

    // Hacer un HALT para finalizar la ejecución
    hlt 0x0

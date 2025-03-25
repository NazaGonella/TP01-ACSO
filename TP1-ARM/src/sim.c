#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <inttypes.h>
#include "shell.h"

#define R_OPCODE 0xFFE00000
#define I_OPCODE 0xFFC00000
#define D_OPCODE 0xFFE00000
#define B_OPCODE 0xF8000000
#define CB_OPCODE 0xFE000000
#define IW_OPCODE 0xFFE00000
#define MASCARA 0xFFFFFFFF

#define ADDS_CODE 0xb1000000            //ninguna es prefijo de otra --> esto es lo que nos permite identificar correctamente la inst iterando por todos los tipos
#define SUBS_CODE
#define HLT_CODE
#define CMP_CODE
#define ANDS_CODE
#define EOR_CODE
#define ORR_CODE
#define CMP_CODE
#define B_CODE
#define BR_CODE
#define BCOND_CODE
#define LSL_CODE
#define LSR_CODE
#define STUR_CODE
#define STURB_CODE
#define STURH_CODE
#define LDUR_CODE
#define LDURH_CODE
#define LDURB_CODE
#define MOVZ_CODE
#define ADD_CODE
#define MUL_CODE
#define CBZ_CODE
#define CBNZ_CODE

uint32_t get_R_opcode(uint32_t instruction) {
    return (instruction & R_OPCODE_MASK) >> 21;
}

uint32_t get_I_opcode(uint32_t instruction) {
    return (instruction & I_OPCODE_MASK) >> 22;
}

uint32_t get_instruction_bit_field(uint32_t instruction, int size, int shift){
    uint32_t bit_mask = ((1 << (size))-1)<<shift;
    printf("ESTO: %x\n", bit_mask);
    return 0;
}

void adds(uint32_t instruction, int shifted) {

}

void process_instruction()
{
    /* execute one instruction here. You should use CURRENT_STATE and modify
     * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
     * access memory. 
     * */
    uint64_t PC = CURRENT_STATE.PC;
    uint32_t instruction = mem_read_32(PC);

    switch(get_R_opcode(instruction)){
        case (0b10101011001) : printf("INST ADDS (extended register)\n\n"); break;
        case (0b11101011001) : printf("INST SUBS (extended register)\n\n"); break;
        case (0b11101010000) : printf("INST ANDS (shifted register, shift '00')\n\n"); break;
        case (0b11001010000) : printf("INST EOR (shifted register, shift '00')\n\n"); break;
        case (0b10101010000) : printf("INST ORR (shifted register, shift '00')\n\n"); break;
        case (0b11010010100) : printf("INST MOVZ (hw '00')\n\n"); break;
        case (0b10001011001) : printf("INST ADD (extended register)\n\n"); break;
        case (0b10110001000) : printf(":D\n\n"); break;
    }
    // printf("INSTRUCTION: %x\n", instruction);
    // printf("OPCODE: %x\n", get_I_opcode(instruction));
    switch(get_I_opcode(instruction)){
        case (0b1011000100) : 
            printf("INST ADDS (immediate, shift '00')\n\n");
            // adds(instruction);
            get_instruction_bit_field(1, 3, 4);
            break;
        case (0b1011000101) : printf("INST ADDS (immediate, shift '01')\n\n"); break;
        case (0b1111000100) : printf("INST SUBS (immediate, shift '00')\n\n"); break;
        case (0b1111000101) : printf("INST SUBS (immediate, shift '01')\n\n"); break;
        case (0b1001000100) : printf("INST ADD (immediate, shift '00')\n\n"); break;
        case (0b1001000101) : printf("INST ADD (immediate, shift '01')\n\n"); break;
    }
}

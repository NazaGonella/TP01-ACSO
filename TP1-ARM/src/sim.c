#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <inttypes.h>
#include "shell.h"

#define R_OPCODE_MASK 0xFFE00000
#define I_OPCODE_MASK 0xFFC00000
#define D_OPCODE_MASK 0xFFE00000
#define B_OPCODE_MASK 0xF8000000
#define CB_OPCODE_MASK 0xFE000000
#define IW_OPCODE_MASK 0xFFE00000
#define MASCARA_MASK 0xFFFFFFFF

#define OPCODE_INTERVAL_A 11, 21
#define OPCODE_INTERVAL_B 10, 22
#define OPCODE_INTERVAL_C 22, 10
#define OPCODE_INTERVAL_D 6, 26
#define OPCODE_INTERVAL_E 8, 24

#define ADDS_IMM_00_CODE 0xB1000000            //ninguna es prefijo de otra --> esto es lo que nos permite identificar correctamente la inst iterando por todos los tipos
#define ADDS_IMM_01_CODE 0xB1C00000
#define ADDS_EXT_CODE 0xAB200000
#define SUBS_IMM_00_CODE 0x3C400000
#define SUBS_IMM_01_CODE 0x3C500000
#define SUBS_EXT_CODE 0x3AC80000  // es igual que cmp ext
#define HLT_CODE 0x35100000
#define CMP_EXT_CODE 0x3AC80000 // es igual que sub ext
#define CMP_IMM_00_CODE 0x3C400000  // es igual que subs imm 00
#define CMP_IMM_01_CODE 0x3C500000  // es igual que subs imm 01
#define ANDS_CODE 0x3A800000
#define EOR_CODE 0x32800000
#define ORR_CODE 0x2A800000
#define B_CODE 0x14000000
#define BR_CODE
#define BCOND_CODE 0x54000000
#define LSL_IMM_CODE 0xD3000000 //?
#define LSR_IMM_CODE 0x         //?
#define STUR_CODE
#define STURB_CODE
#define STURH_CODE
#define LDUR_CODE
#define LDURH_CODE
#define LDURB_CODE
#define MOVZ_CODE 0xD2800000
#define ADD_IMM_00_CODE 0x91000000
#define ADD_IMM_01_CODE 0x91400000
#define ADD_EXT_CODE
#define MUL_CODE
#define CBZ_CODE 0xB4000000
#define CBNZ_CODE 0xB5000000

typedef struct {
    const char *name;
    uint32_t opcode;
    void (*run)(uint32_t);
} Instruction;

// Instruction instructions[] = {
//     {"ADDS (immediate, shift '00')", 0xB1000000, adds_immediate}
// };

uint32_t get_instruction_bit_field(uint32_t instruction, int size, int shift) {
    uint32_t bit_mask = ((1 << (size))-1)<<shift;
    return (instruction & bit_mask) >> shift;
}

uint32_t get_Rn(uint32_t instruction) {
    return get_instruction_bit_field(instruction, 5, 5);
    // return get_instruction_bit_field(instruction, BIT_MASK_INTERVAL);
}

uint32_t get_Rd(uint32_t instruction) {
    return get_instruction_bit_field(instruction, 5, 0);
}

uint32_t get_Rm(uint32_t instruction) {
    return get_instruction_bit_field(instruction, 5, 16);
}

int is_shifted(uint32_t instruction) {
    return get_instruction_bit_field(instruction, 2, 22);
}

void adds_immediate(uint32_t instruction) {
    uint32_t imm12 = get_instruction_bit_field(instruction, 12, 10);
    if (is_shifted(instruction)){
        imm12 = imm12 << 12;
    }
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    // printf("\n");
    // printf("Instruction: %x\n", instruction);
    // printf("imm12: %x\n", imm12);
    // printf("Rn   : %x\n", Rn);
    // printf("Rd   : %x\n", Rd);
    // printf("\nRESULT: %ld\n\n", result);
    int64_t result = CURRENT_STATE.REGS[Rn] + imm12;
    NEXT_STATE.REGS[Rd] = result;
    if (result == 0) {
        NEXT_STATE.FLAG_Z = 1;
    }
    NEXT_STATE.PC += 4;
}

void adds_extended(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] + CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;
    if (result == 0) {
        NEXT_STATE.FLAG_Z = 1;
    }
    NEXT_STATE.PC += 4;
}

void subs_immediate(uint32_t instruction) {
    uint32_t imm12 = get_instruction_bit_field(instruction, 12, 10);
    if (is_shifted(instruction)){
        imm12 = imm12 << 12;
    }
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] - imm12;
    NEXT_STATE.REGS[Rd] = result;
    if (result == 0) {
        NEXT_STATE.FLAG_Z = 1;
    } else if (result < 0) {
        NEXT_STATE.FLAG_N = 1;
    }
    NEXT_STATE.PC += 4;
}

void subs_extended(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] - CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;
    if (result == 0) {
        NEXT_STATE.FLAG_Z = 1;
    } else if (result < 0){
        NEXT_STATE.FLAG_N = 1;
    }
    NEXT_STATE.PC += 4;
}

void cmp_extended(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] - CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[31] = result;
    if (result == 0) {
        NEXT_STATE.FLAG_Z = 1;
    } else if (result < 0){
        NEXT_STATE.FLAG_N = 1;
    }
}

void ands_extended(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] & CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;
    NEXT_STATE.PC += 4;
}

void eor_extended(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] ^ CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;
    NEXT_STATE.PC += 4;
}

// Está bien???
void logical_shift_left_immediate(uint32_t instruction) {
    uint32_t immr = get_instruction_bit_field(instruction, 5, 16);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    // printf("IMMR: %x", immr);
    NEXT_STATE.REGS[Rd] = CURRENT_STATE.REGS[Rn] << immr;
    NEXT_STATE.PC += 4;
}

void movz(uint32_t instruction) {
    uint32_t imm16 = get_instruction_bit_field(instruction, 16, 5);
    uint32_t Rd = get_Rd(instruction);
    NEXT_STATE.REGS[Rd] = imm16;
    NEXT_STATE.PC += 4;
}

// ESTA BIEN??
void stur(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    mem_write_32(CURRENT_STATE.REGS[Rn] + imm9, CURRENT_STATE.REGS[Rt]);
    // printf("imm9: %x\n", imm9);
    // printf("Rn: %x\n", Rn);
    // printf("Rt: %x\n", Rt);
    NEXT_STATE.PC += 4;
}

void halt(uint32_t instruction) {
    RUN_BIT = 0;
    NEXT_STATE.PC += 4;
}

void process_instruction(){
    uint32_t instruction = mem_read_32(CURRENT_STATE.PC);
    printf("INSTRUCTION: %x\n", instruction);
    // switch(get_R_opcode(instruction)){
    switch (get_instruction_bit_field(instruction, OPCODE_INTERVAL_A)){
        // case (0b10101011001) : printf("INST ADDS (extended register)\n\n"); adds_extended(instruction); break;
        // case (0b11101011001) : printf("INST SUBS (extended register)\n\n"); subs_extended(instruction); break;
        case (0b10101011000) : printf("INST ADDS (extended register)\n\n");             adds_extended(instruction); break;
        case (0b11101011000) :
            if (get_Rd(instruction) == 0b11111) {
                printf("INST CMP (extended register)\n\n");
                cmp_extended(instruction);
            } else {
                printf("INST SUBS (extended register)\n\n");
                subs_extended(instruction); break;
            }
        case (0b11101010000) : printf("INST ANDS (shifted register, shift '00')\n\n");  ands_extended(instruction); break;
        case (0b11001010000) : printf("INST EOR (shifted register, shift '00')\n\n");    eor_extended(instruction); break;
        case (0b10101010000) : printf("INST ORR (shifted register, shift '00')\n\n"); break;
        case (0b11010010100) : printf("INST MOVZ (hw '00')\n\n");                                movz(instruction); break;
        case (0b10001011001) : printf("INST ADD (extended register)\n\n"); break;
        case (0b11010100010) : printf("INST HALT\n\n"); halt(instruction); break;
        // CREO QUE MAL
        // case (0b11101011001) : printf("INST CMP (extended register)\n\n");              cmp_extended(instruction); break;
        case (0b11111000000) : printf("INST STUR\n\n");                                 stur(instruction); break;
        case (0b10111000000) : printf("INST STUR\n\n");                                 stur(instruction); break;
    }
    // printf("INSTRUCTION: %x\n", instruction);
    // printf("OPCODE: %x\n", get_I_opcode(instruction));
    // switch(get_I_opcode(instruction)){
    switch (get_instruction_bit_field(instruction, OPCODE_INTERVAL_B)){
        case (0b1011000100) : printf("INST ADDS (immediate, shift '00')\n\n"); adds_immediate(instruction); break;
        case (0b1011000101) : printf("INST ADDS (immediate, shift '01')\n\n"); adds_immediate(instruction); break;
        case (0b1111000100) : printf("INST SUBS (immediate, shift '00')\n\n"); subs_immediate(instruction); break;
        case (0b1111000101) : printf("INST SUBS (immediate, shift '01')\n\n"); subs_immediate(instruction); break;
        case (0b1001000100) : printf("INST ADD (immediate, shift '00')\n\n");  break;
        case (0b1001000101) : printf("INST ADD (immediate, shift '01')\n\n");  break;
        case (0b1101001101) : printf("INST LSL (immediate)\n\n"); logical_shift_left_immediate(instruction); break;
                // 11010011011100001011110000100001
    }
}

#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <inttypes.h>
#include "shell.h"

#define OPCODE_INTERVAL_11 11, 21
#define OPCODE_INTERVAL_10 10, 22
#define OPCODE_INTERVAL_C 22, 10
#define OPCODE_INTERVAL_D 6, 26
#define OPCODE_INTERVAL_8 8, 24

typedef struct {
    const char *name;
    uint32_t opcode;
    void (*run)(uint32_t);
} Instruction;


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

uint32_t get_cond(uint32_t instruction) {
    return get_instruction_bit_field(instruction, 4, 0);
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

void add_immediate(uint32_t instruction) {
    uint32_t imm12 = get_instruction_bit_field(instruction, 12, 10);
    if (is_shifted(instruction)){
        imm12 = imm12 << 12;
    }
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
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
    NEXT_STATE.PC += 4;
}

void ands_shifted(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] & CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;
    NEXT_STATE.PC += 4;
}

void eor_shifted(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] ^ CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;
    NEXT_STATE.PC += 4;
}

// Está bien???
void logical_shift_left_immediate(uint32_t instruction) {
    // uint32_t immr = get_instruction_bit_field(instruction, 5, 16);
    uint32_t immr = 64 - get_instruction_bit_field(instruction, 6, 16);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    printf("IMMR: %x", immr);
    NEXT_STATE.REGS[Rd] = CURRENT_STATE.REGS[Rn] << immr;
    NEXT_STATE.PC += 4;
}

void movz(uint32_t instruction) {
    uint32_t imm16 = get_instruction_bit_field(instruction, 16, 5);
    uint32_t Rd = get_Rd(instruction);
    NEXT_STATE.REGS[Rd] = imm16;
    NEXT_STATE.PC += 4;
}

void beq(uint32_t instruction) {
    uint32_t imm19 = get_instruction_bit_field(instruction, 19, 4);
    int64_t offset = (int64_t)(imm19 << 2);
    if (imm19 & (1 << 18)) {
        offset |= 0xFFFFFFFFFFE00000;
    }
    if (CURRENT_STATE.FLAG_Z == 1) {
        NEXT_STATE.PC += offset;
    } else {
        NEXT_STATE.PC += 4;
    }
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

void orr_shifted(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result= CURRENT_STATE.REGS[Rn] | CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;
    NEXT_STATE.PC += 4;
}

void b(uint32_t instruction) {
    int offset=0;
    NEXT_STATE.PC += offset;
}

void br(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    NEXT_STATE.PC = CURRENT_STATE.REGS[Rn];
}

void bcond(uint32_t instruction) {
    NEXT_STATE.PC += 4;
}

void cmp_immediate(uint32_t instruction){

}

void logical_shift_immediate(uint32_t instruction){

}

void mul(uint32_t instruction){

}

void add_extended(uint32_t instruction){

}


Instruction instructions[] = {
    {"INST ADDS (extended register)", 0b10101011000, adds_extended},
    {"INST ADDS (immediate, shift 00)", 0b1011000100, adds_immediate},
    {"INST ADDS (immediate, shift 01)", 0b1011000101, adds_immediate},
    {"INST SUBS (extended register)",0b11101011000, subs_extended},     //hacer funcion en comun para subs extended y cmp extended
    {"INST SUBS (immediate, shift 00)", 0b1111000100, subs_immediate},
    {"INST SUBS (immediate, shift 01)", 0b1111000101, subs_immediate},
    {"INST HLT", 0b11010100010, halt},
    {"INST CMP (extended register)", 0b11101011000,cmp_extended},
    {"INST CMP (immediate, shift 00)", 0b1111000100, cmp_immediate},
    {"INST ANDS (shifted register, shift '00')", 0b11010010100, ands_shifted},
    {"INST EOR (shifted register, shift '00')", 0b11001010000, eor_shifted},
    {"INST ORR (shifted register, shift '00')",0b10101010000,orr_shifted},
    {"INST B", 0b01010100, b},
    {"INST BR", 0b01010100, br},
    {"INST BCOND", 0b01010100, bcond},
    {"INST LSL (immediate)", 0b1101001101, logical_shift_immediate},
    {"INST LSR (immediate)", 0b1101001101, logical_shift_immediate},
    {"INST STUR", 0b11111000000, stur},
    {"INST STUR", 0b10111000000, stur},
    {"INST STURB", 0b00111000000, stur},
    {"INST STURH", 0b01111000000,stur}, //cambiar
    {"INST LDUR", 0b11111000010,stur},
    {"INST LDURH",0b01111000010,stur},
    {"INST LDURB", 0b00111000010,stur},
    {"INST MOVZ", 0b11010010100, movz},
    {"INST ADD (immediate, shift '00')", 0b1001000100, add_immediate},
    {"INST ADD (immediate, shift '01')", 0b1001000101, add_immediate},
    {"INST ADD (extended register)",0b1001000100, add_extended},
    {"INST MUL", 0b10011011000, mul},
    {"INST CBZ", 0b10101011001, cmp_extended},
    {"INST CBNZ", 0b11101011001, cmp_extended},

    {}
};

void process_instruction(){
    uint32_t instruction = mem_read_32(CURRENT_STATE.PC);
    printf("INSTRUCTION: %x\n", instruction);
    // switch(get_R_opcode(instruction)){
    switch (get_instruction_bit_field(instruction, OPCODE_INTERVAL_11)){
        case (0b10101011000) : printf("INST ADDS (extended register)\n\n");             adds_extended(instruction); break;
        case (0b11101011000) :
            if (get_Rd(instruction) == 0b11111) {
                printf("INST CMP (extended register)\n\n");
                cmp_extended(instruction);
            } else {
                printf("INST SUBS (extended register)\n\n");
                subs_extended(instruction);
            } break;
        case (0b11101010000) : printf("INST ANDS (shifted register, shift '00')\n\n");  ands_shifted(instruction); break;
        case (0b11001010000) : printf("INST EOR (shifted register, shift '00')\n\n");    eor_shifted
    (instruction); break;
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
    switch (get_instruction_bit_field(instruction, OPCODE_INTERVAL_10)){
        case (0b1011000100) : printf("INST ADDS (immediate, shift '00')\n\n"); adds_immediate(instruction); break;
        case (0b1011000101) : printf("INST ADDS (immediate, shift '01')\n\n"); adds_immediate(instruction); break;
        case (0b1111000100) : printf("INST SUBS (immediate, shift '00')\n\n"); subs_immediate(instruction); break;
        case (0b1111000101) : printf("INST SUBS (immediate, shift '01')\n\n"); subs_immediate(instruction); break;
        case (0b1001000100) : printf("INST ADD (immediate, shift '00')\n\n");  break;
        case (0b1001000101) : printf("INST ADD (immediate, shift '01')\n\n");  break;
        case (0b1101001101) : printf("INST LSL (immediate)\n\n"); logical_shift_left_immediate(instruction); break;
                // 11010011011100001011110000100001
    }

    switch (get_instruction_bit_field(instruction, OPCODE_INTERVAL_8)){
        case (0b01010100) :
            // printf("COND: %x\n", get_cond(instruction));
            if (get_cond(instruction) == 0b0000) {
                printf("INST BEQ\n\n");
                beq(instruction);
            } else if (get_cond(instruction) == 0b1011) {
                printf("INST BLT\n\n");
            } break;
    }
}

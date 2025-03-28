#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <inttypes.h>
#include "shell.h"

#define OPCODE_INTERVAL_11 11, 21
#define OPCODE_INTERVAL_10 10, 22
#define OPCODE_INTERVAL_22 22, 10
#define OPCODE_INTERVAL_6 6, 26
#define OPCODE_INTERVAL_8 8, 24

typedef struct {
    int size;
    int shift;
} OpcodeInterval;

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

void update_flags(int64_t result) {
    if (result == 0) {
        NEXT_STATE.FLAG_Z = 1;
        NEXT_STATE.FLAG_N = 0;
    } else if (result > 0) {
        NEXT_STATE.FLAG_Z = 0;
        NEXT_STATE.FLAG_N = 0;
    } else if (result < 0) {
        NEXT_STATE.FLAG_Z = 0;
        NEXT_STATE.FLAG_N = 1;
    }
}

void adds_immediate(uint32_t instruction) {
    uint32_t imm12 = get_instruction_bit_field(instruction, 12, 10);
    if (is_shifted(instruction)){
        imm12 = imm12 << 12;
    }
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] + imm12;
    NEXT_STATE.REGS[Rd] = result;
    update_flags(result);
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
    NEXT_STATE.PC += 4;
}

void adds_extended(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] + CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;
    update_flags(result);
    NEXT_STATE.PC += 4;
}

void add_extended(uint32_t instruction){
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] + CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;
    NEXT_STATE.PC += 4;
}

void subs_cmp_immediate(uint32_t instruction) {
    uint32_t imm12 = get_instruction_bit_field(instruction, 12, 10);
    if (is_shifted(instruction)){
        imm12 = imm12 << 12;
    }
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] - imm12;
    if (Rd == 0b11111) {
        printf("INST CMP (immediate, shift '00')\n\n");
    } else {
        printf("INST SUBS (immediate, shift '00')\n\n");
        NEXT_STATE.REGS[Rd] = result;
    }
    update_flags(result);
    NEXT_STATE.PC += 4;
}

void subs_cmp_extended(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] - CURRENT_STATE.REGS[Rm];
    if (Rd == 0b11111) {
        printf("INST CMP (extended register)\n\n");
    } else {
        printf("INST SUBS (extended register)\n\n");
        NEXT_STATE.REGS[Rd] = result;
    }
    update_flags(result);
    NEXT_STATE.PC += 4;
}

void cmp_extended(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] - CURRENT_STATE.REGS[Rm];
    // NEXT_STATE.REGS[31] = result;
    update_flags(result);
    NEXT_STATE.PC += 4;
}

void cmp_immediate(uint32_t instruction){
    uint32_t imm12 = get_instruction_bit_field(instruction, 12, 10);
    uint32_t Rn = get_Rn(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] - imm12;
    // NEXT_STATE.REGS[31] = result;
    update_flags(result);
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

void logical_shift_left_immediate(uint32_t instruction) {
    uint32_t immr = 64 - get_instruction_bit_field(instruction, 6, 16);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    // printf("IMMR: %x", immr);
    NEXT_STATE.REGS[Rd] = CURRENT_STATE.REGS[Rn] << immr;
    NEXT_STATE.PC += 4;
}

void logical_shift_right_immediate(uint32_t instruction) {
    uint32_t immr = get_instruction_bit_field(instruction, 6, 16);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    // printf("IMMR: %x", immr);
    NEXT_STATE.REGS[Rd] = CURRENT_STATE.REGS[Rn] >> immr;
    NEXT_STATE.PC += 4;
}

void movz(uint32_t instruction) {
    uint32_t imm16 = get_instruction_bit_field(instruction, 16, 5);
    uint32_t Rd = get_Rd(instruction);
    NEXT_STATE.REGS[Rd] = imm16;
    NEXT_STATE.PC += 4;
}

void beq(uint32_t instruction) {
    uint32_t imm19 = get_instruction_bit_field(instruction, 19, 5);
    int64_t offset = (int64_t)(imm19 << 2);
    if (imm19 & (1 << 18)) {
        offset |= 0xFFFFFFFFFFE00000;
    }
    // printf("%ld\n", offset);
    if (CURRENT_STATE.FLAG_Z == 1) {
        NEXT_STATE.PC += offset;
    } else{
        NEXT_STATE.PC += 4;
    }
}

void blt(uint32_t instruction) {
    uint32_t imm19 = get_instruction_bit_field(instruction, 19, 5);
    int64_t offset = (int64_t)(imm19 << 2);
    if (imm19 & (1 << 18)) {
        offset |= 0xFFFFFFFFFFE00000;
    }
    if (CURRENT_STATE.FLAG_N == 1) {
        NEXT_STATE.PC += offset;
    } else{
        NEXT_STATE.PC += 4;
    }
}

void ble(uint32_t instruction) {
    uint32_t imm19 = get_instruction_bit_field(instruction, 19, 5);
    int64_t offset = (int64_t)(imm19 << 2);
    if (imm19 & (1 << 18)) {
        offset |= 0xFFFFFFFFFFE00000;
    }
    if (!(CURRENT_STATE.FLAG_Z == 1 || CURRENT_STATE.FLAG_N == 0)) {
        NEXT_STATE.PC += offset;
    } else{
        NEXT_STATE.PC += 4;
    }
}

void bne(uint32_t instruction) {
    uint32_t imm19 = get_instruction_bit_field(instruction, 19, 5);
    int64_t offset = (int64_t)(imm19 << 2);
    if (imm19 & (1 << 18)) {
        offset |= 0xFFFFFFFFFFE00000;
    }
    if (CURRENT_STATE.FLAG_Z == 0) {
        NEXT_STATE.PC += offset;
    } else{
        NEXT_STATE.PC += 4;
    }
}

void bge(uint32_t instruction) {
    uint32_t imm19 = get_instruction_bit_field(instruction, 19, 5);
    int64_t offset = (int64_t)(imm19 << 2);
    if (imm19 & (1 << 18)) {
        offset |= 0xFFFFFFFFFFE00000;
    }
    if (CURRENT_STATE.FLAG_N == 0) {
        NEXT_STATE.PC += offset;
    } else{
        NEXT_STATE.PC += 4;
    }
}

void bgt(uint32_t instruction) {
    uint32_t imm19 = get_instruction_bit_field(instruction, 19, 5);
    int64_t offset = (int64_t)(imm19 << 2);
    if (imm19 & (1 << 18)) {
        offset |= 0xFFFFFFFFFFE00000;
    }
    if (CURRENT_STATE.FLAG_Z == 0 && CURRENT_STATE.FLAG_N == 0) {
        NEXT_STATE.PC += offset;
    } else{
        NEXT_STATE.PC += 4;
    }
}

void cbz(uint32_t instruction) {
    uint32_t imm19 = get_instruction_bit_field(instruction, 19, 5);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = (int64_t)(imm19 << 2);
    if (imm19 & (1 << 18)) {
        offset |= 0xFFFFFFFFFFE00000;
    }
    if (CURRENT_STATE.REGS[Rt] == 0){
        NEXT_STATE.PC += offset;
    } else{
        NEXT_STATE.PC += 4;
    }
}

void cbnz(uint32_t instruction) {
    uint32_t imm19 = get_instruction_bit_field(instruction, 19, 5);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = (int64_t)(imm19 << 2);
    if (imm19 & (1 << 18)) {
        offset |= 0xFFFFFFFFFFE00000;
    }
    if (CURRENT_STATE.REGS[Rt] != 0){
        NEXT_STATE.PC += offset;
    } else{
        NEXT_STATE.PC += 4;
    }
}

void stur(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = (int64_t)(imm9);
    if (imm9 & (1 << 8)) {
        offset |= 0xFFFFFFFFFFFFFE00;
    }
    mem_write_32(CURRENT_STATE.REGS[Rn] + offset, CURRENT_STATE.REGS[Rt]);
    NEXT_STATE.PC += 4;
}

void sturb(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = (int64_t)(imm9);
    if (imm9 & (1 << 8)) {
        offset |= 0xFFFFFFFFFFFFFE00;
    }
    uint32_t Rt_8 = CURRENT_STATE.REGS[Rt] & 0b11111111;                        // Agarro los primeros 8 bits
    uint32_t mem = mem_read_32(CURRENT_STATE.REGS[Rn] + offset);
    uint32_t Rt_8_or_mem = (mem & (((1 << (24))-1)<<8)) | Rt_8;                 // Lleno de 0s los primeros 8 bits y hago OR con los primeros 8 bits del registro Rt
    mem_write_32(CURRENT_STATE.REGS[Rn] + offset, Rt_8_or_mem);
    NEXT_STATE.PC += 4;
}

void sturh(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = (int64_t)(imm9);
    if (imm9 & (1 << 8)) {
        offset |= 0xFFFFFFFFFFFFFE00;
    }
    uint32_t Rt_16 = CURRENT_STATE.REGS[Rt] & 0b1111111111111111;                        // Agarro los primeros 16 bits
    uint32_t mem = mem_read_32(CURRENT_STATE.REGS[Rn] + offset);
    uint32_t Rt_16_or_mem = (mem & (((1 << (16))-1)<<16)) | Rt_16;                 // Lleno de 0s los primeros 16 bits y hago OR con los primeros 16 bits del registro Rt
    mem_write_32(CURRENT_STATE.REGS[Rn] + offset, Rt_16_or_mem);
    NEXT_STATE.PC += 4;
}

void ldur(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = (int64_t)(imm9);
    if (imm9 & (1 << 8)) {
        offset |= 0xFFFFFFFFFFFFFE00;
    }
    uint64_t lower = (uint64_t)mem_read_32(CURRENT_STATE.REGS[Rn] + offset);
    uint64_t upper = (uint64_t)mem_read_32(CURRENT_STATE.REGS[Rn] + offset + 4);
    NEXT_STATE.REGS[Rt] = (upper << 32) | lower;
    NEXT_STATE.PC += 4;
}

void ldurb(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = (int64_t)(imm9);
    if (imm9 & (1 << 8)) {
        offset |= 0xFFFFFFFFFFFFFE00;
    }
    uint32_t mem_8 = mem_read_32(CURRENT_STATE.REGS[Rn] + imm9) & 0b11111111;                        // Agarro los primeros 8 bits
    uint32_t mem_8_or_Rt = (NEXT_STATE.REGS[Rt] & (((1 << (24))-1)<<8)) | mem_8;                 // Lleno de 0s los primeros 8 bits y hago OR con los primeros 8 bits del registro Rt
    NEXT_STATE.REGS[Rt] = mem_8_or_Rt;
    NEXT_STATE.PC += 4;
}

void ldurh(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = (int64_t)(imm9);
    if (imm9 & (1 << 8)) {
        offset |= 0xFFFFFFFFFFFFFE00;
    }
    uint32_t mem_16 = mem_read_32(CURRENT_STATE.REGS[Rn] + imm9) & 0b1111111111111111;                        // Agarro los primeros 16 bits
    uint32_t mem_16_or_Rt = (NEXT_STATE.REGS[Rt] & (((1 << (16))-1)<<16)) | mem_16;                 // Lleno de 0s los primeros 16 bits y hago OR con los primeros 16 bits del registro Rt
    NEXT_STATE.REGS[Rt] = mem_16_or_Rt;
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
    uint32_t imm26 = get_instruction_bit_field(instruction, 26, 0);
    int64_t offset = (int64_t)(imm26 << 2);
    if (imm26 & (1 << 25)) {
        offset |= 0xFFFFFFFFFC000000;
    }
    NEXT_STATE.PC += offset;
}

void br(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    NEXT_STATE.PC = CURRENT_STATE.REGS[Rn];
}

void logical_shift_immediate(uint32_t instruction){
    uint32_t imms = get_instruction_bit_field(instruction, 6, 10);
    if (imms == 0b111111 || imms == 0b011111){
        printf("INST LSR (immediate)\n\n");
        logical_shift_right_immediate(instruction);
    } else {
        printf("INST LSL (immediate)\n\n");
        logical_shift_left_immediate(instruction);
    }
}

void mul(uint32_t instruction){
    uint32_t Rm = get_Rm(instruction);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    NEXT_STATE.REGS[Rd] = CURRENT_STATE.REGS[Rn] * CURRENT_STATE.REGS[Rm];
    NEXT_STATE.PC += 4;
}

void bcond(uint32_t instruction){
    if (get_cond(instruction) == 0b0000) {
        printf("INST BEQ\n\n");
        beq(instruction);
    } else if (get_cond(instruction) == 0b1011) {
        printf("INST BLT\n\n");
        blt(instruction);
    } else if (get_cond(instruction) == 0b0001){
        printf("INST BNE");
        bne(instruction);
    } else if (get_cond(instruction) == 0b1100) {
        printf("INST BGT");
        bgt(instruction);
    } else if (get_cond(instruction) == 0b1010) {
        printf("INST BGE");
        bge(instruction);
    } else if (get_cond(instruction) == 0b1101) {
        printf("INST BLE");
        ble(instruction);
    }
}

Instruction instructions[] = {
    {"INST ADDS (extended register)", 0b10101011000, adds_extended},
    {"INST ADDS (immediate, shift 00)", 0b1011000100, adds_immediate},
    {"INST ADDS (immediate, shift 01)", 0b1011000101, adds_immediate},
    {"INST SUBS / CMP (extended register)",0b11101011000, subs_cmp_extended},     //hacer funcion en comun para subs extended y cmp extended
    {"INST SUBS / CMP (immediate, shift 00)", 0b1111000100, subs_cmp_immediate},
    {"INST SUBS / CMP (immediate, shift 01)", 0b1111000101, subs_cmp_immediate},
    {"INST HLT", 0b11010100010, halt},
    {"INST ANDS (shifted register, shift '00')", 0b11101010000, ands_shifted},
    {"INST EOR (shifted register, shift '00')", 0b11001010000, eor_shifted},
    {"INST ORR (shifted register, shift '00')",0b10101010000,orr_shifted},
    {"INST B", 0b000101, b},
    {"INST BR", 0b1101011000011111000000, br},
    {"INST BCOND", 0b01010100, bcond},
    {"INST LSL (immediate)", 0b1101001101, logical_shift_immediate},
    {"INST LSR (immediate)", 0b1101001101, logical_shift_immediate},
    {"INST STUR", 0b11111000000, stur},
    {"INST STURB", 0b00111000000, sturb},
    {"INST STURH", 0b01111000000,sturh}, //cambiar
    {"INST LDUR", 0b11111000010,ldur},
    {"INST LDURH",0b01111000010,ldurh},
    {"INST LDURB", 0b00111000010,ldurb},
    {"INST MOVZ", 0b11010010100, movz},
    {"INST ADD (immediate, shift '00')", 0b1001000100, add_immediate},
    {"INST ADD (immediate, shift '01')", 0b1001000101, add_immediate},
    {"INST ADD (extended register)",0b10001011001, add_extended},
    {"INST MUL", 0b10011011000, mul},
    {"INST CBZ", 0b10101011001, cbz},
    {"INST CBNZ", 0b11101011001, cbnz},
};

OpcodeInterval opcode_intervals[] = {
    {.size=11, .shift=21},
    {.size=10, .shift=22},
    {.size=22, .shift=10},
    {.size=6, .shift=26},
    {.size=8, .shift=24},
};

#define NUM_INTERVALS (sizeof(opcode_intervals) / sizeof(opcode_intervals[0]))
#define NUM_INSTRUCTIONS    (sizeof(instructions) / sizeof(instructions[0]))

void process_instruction(){
    uint32_t whole_instruction = mem_read_32(CURRENT_STATE.PC);
    printf("INSTRUCTION: %x\n", whole_instruction);

    for (int i = 0; i < NUM_INSTRUCTIONS; i++){
        Instruction instruction = instructions[i];
        for (int j = 0; j < NUM_INTERVALS; j++){
            OpcodeInterval opcode_interval = opcode_intervals[j];
            uint32_t opcode = get_instruction_bit_field(whole_instruction, opcode_interval.size, opcode_interval.shift);
            if (opcode == instruction.opcode){
                instruction.run(whole_instruction);
                printf("%s\n\n", instruction.name);
            }
        }
    }
}

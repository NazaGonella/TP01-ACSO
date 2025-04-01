#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <inttypes.h>
#include "shell.h"

typedef struct {
    int size;
    int shift;
} OpcodeInterval;

typedef struct {
    const char *name;
    uint32_t opcode;
    void (*run)(uint32_t);
} Instruction;


uint32_t create_bit_mask(int size, int shift) {
    return ((1 << (size))-1)<<shift;
}

uint32_t get_instruction_bit_field(uint32_t instruction, int size, int shift) {
    uint32_t bit_mask = create_bit_mask(size, shift);
    return (instruction & bit_mask) >> shift;
}

uint32_t get_Rn(uint32_t instruction) {
    return get_instruction_bit_field(instruction, 5, 5);
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
    NEXT_STATE.FLAG_Z = (result == 0);
    NEXT_STATE.FLAG_N = (result < 0);
}

int64_t sign_extend(uint32_t bit_field, int size) {
    int64_t offset = (int64_t) bit_field;
    if (bit_field & (1 << (size - 1))) {
        offset |= ((int64_t)-1 << size);
    }
    return offset;
}

int64_t add_immediate_base(uint32_t instruction) {
    uint32_t imm12 = get_instruction_bit_field(instruction, 12, 10);
    if (is_shifted(instruction)){
        imm12 = imm12 << 12;
    }
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] + imm12;
    NEXT_STATE.REGS[Rd] = result;
    NEXT_STATE.PC += 4;
    return result;
}

void adds_immediate(uint32_t instruction) {
    int64_t result = add_immediate_base(instruction);
    update_flags(result);
}

void add_immediate(uint32_t instruction) {
    add_immediate_base(instruction);
}

int64_t add_extended_base(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] + CURRENT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;
    NEXT_STATE.PC += 4;
    return result;
}

void adds_extended(uint32_t instruction) {
    int64_t result = add_extended_base(instruction);
    update_flags(result);
}

void add_extended(uint32_t instruction){
    add_extended_base(instruction);
}

void subs_cmp_immediate(uint32_t instruction) {
    uint32_t imm12 = get_instruction_bit_field(instruction, 12, 10);
    if (is_shifted(instruction)){
        imm12 = imm12 << 12;
    }
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    int64_t result = CURRENT_STATE.REGS[Rn] - imm12;
    if (Rd != 0b11111) {
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
    if (Rd != 0b11111) {
        NEXT_STATE.REGS[Rd] = result;
    }
    update_flags(result);
    NEXT_STATE.PC += 4;
}

int64_t bitwise_operation(uint32_t instruction, char op) {
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    uint32_t Rm = get_Rm(instruction);
    int64_t result;
    switch (op) {
        case '&': result = CURRENT_STATE.REGS[Rn] & CURRENT_STATE.REGS[Rm]; break;
        case '^': result = CURRENT_STATE.REGS[Rn] ^ CURRENT_STATE.REGS[Rm]; break;
        case '|': result = CURRENT_STATE.REGS[Rn] | CURRENT_STATE.REGS[Rm]; break;
        default: return 1;
    }
    NEXT_STATE.REGS[Rd] = result;
    NEXT_STATE.PC += 4;
    return result;
}

void ands_shifted(uint32_t instruction) {
    int64_t result = bitwise_operation(instruction, '&');
    update_flags(result);
}

void eor_shifted(uint32_t instruction) {
    bitwise_operation(instruction, '^');
}

void orr_shifted(uint32_t instruction) {
    bitwise_operation(instruction, '|');
}

void logical_shift_immediate(uint32_t instruction){
    uint32_t immr;
    uint32_t imms = get_instruction_bit_field(instruction, 6, 10);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    if (imms == 0b111111 || imms == 0b011111){
        // printf("R\n");
        immr = get_instruction_bit_field(instruction, 6, 16);
        NEXT_STATE.REGS[Rd] = CURRENT_STATE.REGS[Rn] >> immr;
    } else {
        // printf("L\n");
        immr = 64 - get_instruction_bit_field(instruction, 6, 16);
        NEXT_STATE.REGS[Rd] = CURRENT_STATE.REGS[Rn] << immr;
    }
    NEXT_STATE.PC += 4;
}

void movz(uint32_t instruction) {
    uint32_t imm16 = get_instruction_bit_field(instruction, 16, 5);
    uint32_t Rd = get_Rd(instruction);
    NEXT_STATE.REGS[Rd] = imm16;
    NEXT_STATE.PC += 4;
}

void conditional_branch(uint32_t instruction, int branch_cond) {
    uint32_t imm19 = get_instruction_bit_field(instruction, 19, 5);
    int64_t offset = sign_extend(imm19 << 2, 19);
    if (branch_cond){
        NEXT_STATE.PC += offset;
    } else{
        NEXT_STATE.PC += 4;
    }
}

void beq(uint32_t instruction) {
    conditional_branch(instruction, CURRENT_STATE.FLAG_Z == 1);
}

void blt(uint32_t instruction) {
    conditional_branch(instruction, CURRENT_STATE.FLAG_N == 1);
}

void ble(uint32_t instruction) {
    conditional_branch(instruction, !(CURRENT_STATE.FLAG_Z == 1 || CURRENT_STATE.FLAG_N == 0));
}

void bne(uint32_t instruction) {
    conditional_branch(instruction, CURRENT_STATE.FLAG_Z == 0);
}

void bge(uint32_t instruction) {
    conditional_branch(instruction, CURRENT_STATE.FLAG_N == 0);
}

void bgt(uint32_t instruction) {
    conditional_branch(instruction, CURRENT_STATE.FLAG_Z == 0 && CURRENT_STATE.FLAG_N == 0);
}

void cbz(uint32_t instruction) {
    uint32_t Rt = get_Rd(instruction);
    conditional_branch(instruction, CURRENT_STATE.REGS[Rt] == 0);
}

void cbnz(uint32_t instruction) {
    uint32_t Rt = get_Rd(instruction);
    conditional_branch(instruction, CURRENT_STATE.REGS[Rt] != 0);
}

void stur(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = sign_extend(imm9, 9);
    mem_write_32(CURRENT_STATE.REGS[Rn] + offset, CURRENT_STATE.REGS[Rt]);
    NEXT_STATE.PC += 4;
}

void sturb(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = sign_extend(imm9, 9);
    uint32_t Rt_8 = CURRENT_STATE.REGS[Rt] & 0xFF;
    mem_write_32(CURRENT_STATE.REGS[Rn] + offset, Rt_8);
    NEXT_STATE.PC += 4;
}

void sturh(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = sign_extend(imm9, 9);
    uint32_t Rt_16 = CURRENT_STATE.REGS[Rt] & 0xFFFF;
    mem_write_32(CURRENT_STATE.REGS[Rn] + offset, Rt_16);
    NEXT_STATE.PC += 4;
}

void ldur(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = sign_extend(imm9, 9);
    uint64_t lower = (uint64_t)mem_read_32(CURRENT_STATE.REGS[Rn] + offset);
    uint64_t upper = (uint64_t)mem_read_32(CURRENT_STATE.REGS[Rn] + offset + 4);
    NEXT_STATE.REGS[Rt] = (upper << 32) | lower;
    NEXT_STATE.PC += 4;
}

void ldurb(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = sign_extend(imm9, 9);
    uint32_t mem_8 = mem_read_32(CURRENT_STATE.REGS[Rn] + offset) & 0xFF;                        // Agarro los primeros 8 bits
    NEXT_STATE.REGS[Rt] = mem_8;
    NEXT_STATE.PC += 4;
}

void ldurh(uint32_t instruction) {
    uint32_t imm9 = get_instruction_bit_field(instruction, 9, 12);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rt = get_Rd(instruction);
    int64_t offset = sign_extend(imm9, 9);
    uint32_t mem_16 = mem_read_32(CURRENT_STATE.REGS[Rn] + offset) & 0xFFFF;                        // Agarro los primeros 16 bits
    NEXT_STATE.REGS[Rt] = mem_16;
    NEXT_STATE.PC += 4;
}

void halt(uint32_t instruction) {
    RUN_BIT = 0;
    NEXT_STATE.PC += 4;
}

void b(uint32_t instruction) {
    uint32_t imm26 = get_instruction_bit_field(instruction, 26, 0);
    int64_t offset = sign_extend(imm26 << 2, 26);
    NEXT_STATE.PC += offset;
}

void br(uint32_t instruction) {
    uint32_t Rn = get_Rn(instruction);
    NEXT_STATE.PC = CURRENT_STATE.REGS[Rn];
}

void mul(uint32_t instruction){
    uint32_t Rm = get_Rm(instruction);
    uint32_t Rn = get_Rn(instruction);
    uint32_t Rd = get_Rd(instruction);
    NEXT_STATE.REGS[Rd] = CURRENT_STATE.REGS[Rn] * CURRENT_STATE.REGS[Rm];
    NEXT_STATE.PC += 4;
}

void bcond(uint32_t instruction){
    switch (get_cond(instruction)) {
        case (0b0000):
            beq(instruction);
            break;
        case (0b1011):
            blt(instruction);
            break;
        case (0b0001):
            bne(instruction);
            break;
        case (0b1100):
            bgt(instruction);
            break;
        case (0b1010):
            bge(instruction);
            break;
        case (0b1101):
            ble(instruction);
            break;
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
    {"INST LS (immediate)", 0b1101001101, logical_shift_immediate},
    {"INST STUR", 0b11111000000, stur},
    {"INST STURB", 0b00111000000, sturb},
    {"INST STURH", 0b01111000000,sturh},
    {"INST LDUR", 0b11111000010,ldur},
    {"INST LDURH",0b01111000010,ldurh},
    {"INST LDURB", 0b00111000010,ldurb},
    {"INST MOVZ", 0b11010010100, movz},
    {"INST ADD (immediate, shift '00')", 0b1001000100, add_immediate},
    {"INST ADD (immediate, shift '01')", 0b1001000101, add_immediate},
    {"INST ADD (extended register)",0b10001011000, add_extended},
    {"INST MUL", 0b10011011000, mul},
    {"INST CBZ", 0b10110100, cbz},
    {"INST CBNZ", 0b10110101, cbnz},
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
    // printf("INSTRUCTION: %x\n", whole_instruction);
    for (int i = 0; i < NUM_INSTRUCTIONS; i++){
        Instruction instruction = instructions[i];
        for (int j = 0; j < NUM_INTERVALS; j++){
            OpcodeInterval opcode_interval = opcode_intervals[j];
            uint32_t opcode = get_instruction_bit_field(whole_instruction, opcode_interval.size, opcode_interval.shift);
            if (opcode == instruction.opcode){
                instruction.run(whole_instruction);
                NEXT_STATE.REGS[31] = 0;
                // printf("%s\n\n", instruction.name);
            }
        }
    }
}

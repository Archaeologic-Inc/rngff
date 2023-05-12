// Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
// This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
// "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
#include <stdint.h>
#include <math.h>

const uint64_t golden_gamma = 0x9e3779b97f4a7c15;
const double doubleUlp = 1.0 / (1ULL << 53);

typedef struct SMState {
    uint64_t seed;
    uint64_t gamma;  // always odd
} SMState;

uint64_t shift_xor(int n, uint64_t w) {
    return w ^ (w >> n);
}

uint64_t shift_xor_multiply(int n, uint64_t k, uint64_t w) {
    return shift_xor(n, w) * k;
}

uint64_t mix64(uint64_t z0) {
    uint64_t z1 = shift_xor_multiply(33, 0xc4ceb9fe1a85ec53, z0);
    uint64_t z2 = shift_xor_multiply(33, 0xff51afd7ed558ccd, z1);
    return shift_xor(33, z2);
}

uint64_t mix64variant13(uint64_t z0) {
    uint64_t z1 = shift_xor_multiply(30, 0xbf58476d1ce4e5b9, z0);
    uint64_t z2 = shift_xor_multiply(27, 0x94d049bb133111eb, z1);
    return shift_xor(31, z2);
}

int pop_count(uint64_t x) {
    int c = 0;
    for (c = 0; x; c++)
    {
        x &= x - 1; // clear the least significant bit set
    }
    return c;
}

uint64_t mix_gamma(uint64_t z0) {
    uint64_t z1 = mix64variant13(z0) | 1;
    int n = pop_count(z1 ^ (z1 >> 1));
    if (n >= 24) {
        return z1 ^ 0xaaaaaaaaaaaaaaaa;
    } else {
        return z1;
    }
}

void set_state_c(SMState* state, uint64_t seed, uint64_t gamma) {
    state->seed = seed;
    state->gamma = gamma | 1;
}

SMState construct_from_seed_c(uint64_t seed) {
    SMState state;
    set_state_c(&state, mix64(seed), mix_gamma(seed + golden_gamma));
    return state;
}

SMState split_c(SMState* state) {
    uint64_t seed_ = state->seed + state->gamma;
    uint64_t seed__ = seed_ + state->gamma;
    SMState split_state;
    set_state_c(&split_state, mix64(seed_), mix_gamma(seed__));
    state->seed = seed__;
    return split_state;
}

void next_word(SMState* state, uint64_t* harvest) {
    uint64_t seed_ = state->seed + state->gamma;
    state->seed = seed_;
    *harvest = mix64(seed_);
}

void next_double(SMState* state, double* harvest) {
    uint64_t int_harvest;
    next_word(state, &int_harvest);
    *harvest = (int_harvest >> 11) * doubleUlp;
}


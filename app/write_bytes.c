// Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
// This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
// "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
#include <stdint.h>
#include <stdio.h>

void write_32bits(int32_t * bits) {
    fwrite(bits, sizeof(int32_t), 1, stdout);
}

void write_64bits(int64_t * bits) {
    fwrite(bits, sizeof(int64_t), 1, stdout);
}
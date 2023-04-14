#include <stdint.h>
#include <stdio.h>

void write_32bits(int32_t * bits) {
    fwrite(bits, sizeof(int32_t), 1, stdout);
}

void write_64bits(int64_t * bits) {
    fwrite(bits, sizeof(int64_t), 1, stdout);
}
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "arch/x86/include/generated/uapi/asm/unistd_64.h"

int main(int argc, char ** argv) {
    // Check for user error
    if(argc != 2) {printf("Provide ONE argument.\n"); return -1;}

    // Init length
    int length = strlen(argv[1]);

    // Put messages and return exit code
    return syscall(__NR_dm510_msgbox_put, argv[1], sizeof(char) * length);
}

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "arch/x86/include/generated/uapi/asm/unistd_64.h"

int main(int argc, char ** argv) {
    // Check for user error
    if(argc != 2) {printf("Provide ONE argument.\n"); return -1;}

    int length = atoi(argv[1]);
    // Init buffers
    char* test = malloc(sizeof(char) * length);

    // Get messages
    syscall(__NR_dm510_msgbox_get, test, sizeof(char) * length);

    // Print messages
    printf("%s\n", test);

    return 0;
}

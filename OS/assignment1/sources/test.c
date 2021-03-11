#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include "arch/x86/include/generated/uapi/asm/unistd_64.h"

int main() {
    // Initializing messages and length
    char *msg1 = "We want the whole string";
    int length1 = sizeof(char) * strlen(msg1);

    char *msg2 = "We want the first part, and not the second";
    int length2 = sizeof(char) * strlen(msg2);

    char *msg3 = "We try to get twice the length";
    int length3 = sizeof(char) * strlen(msg3);
    
    char *msg4 = "We try to put a negative length";
    int length4 = sizeof(char) * -strlen(msg4);
    
    char *msg5 = "We try to get a negative length";
    int length5 = sizeof(char) * strlen(msg5);
    
    char *msg6 = "We try to put from a bad address";
    int length6 = sizeof(char) * strlen(msg6);
    
    char *msg7 = "We try to get to a bad address";
    int length7 = sizeof(char) * strlen(msg7);
    
    // Defining ints for return values
    int returnVal1; int returnVal2; int returnVal3; int returnVal4;
    int returnVal5; int returnVal6; int returnVal7;

    // Putting messages
    printf("=== Putting ===\n");
    
    returnVal1 = syscall(__NR_dm510_msgbox_put, msg1, length1); 
    printf("Putting: %s\nReturn value: %d\n\n", msg1, returnVal1);
    
    returnVal2 = syscall(__NR_dm510_msgbox_put, msg2, length2);
    printf("Putting: %s\nReturn value: %d\n\n", msg2, returnVal2);
    
    returnVal3 = syscall(__NR_dm510_msgbox_put, msg3, length3);
    printf("Putting: %s\nReturn value: %d\n\n", msg3, returnVal3);
    
    returnVal4 = syscall(__NR_dm510_msgbox_put, msg4, length4);
    printf("Putting: %s\nReturn value: %d\n\n", msg4, returnVal4);
    
    returnVal5 = syscall(__NR_dm510_msgbox_put, msg5, length5); 
    printf("Putting: %s\nReturn value: %d\n\n", msg5, returnVal5);

    returnVal6 = syscall(__NR_dm510_msgbox_put, 0x012345678910, length6); 
    printf("Putting: %s\nReturn value: %d\n\n", msg6, returnVal6);

    returnVal7 = syscall(__NR_dm510_msgbox_put, msg7, length7); 
    printf("Putting: %s\nReturn value: %d\n\n", msg7, returnVal7);

    // Allocating memory for buffers
    char *buffer1 = malloc(length1);
    char *buffer2 = malloc(sizeof(char) * 20);
    char *buffer3 = malloc(length3 * 2);
    char *buffer5 = malloc(length5);
    char *trashcan = malloc(sizeof(char) * 42);

    // Getting messages
    printf("=== Getting ===\n");

    returnVal7 = syscall(__NR_dm510_msgbox_get, 0x012345678910, sizeof(char) * 123);
    printf("Original message: %s\nReturn value: %d\n\n",
            msg7, returnVal7);
    syscall(__NR_dm510_msgbox_get, trashcan, sizeof(char) * 42); // Removing the msg from the stack

    returnVal5 = syscall(__NR_dm510_msgbox_get, buffer5, -42);
    printf("Original message: %s\nReturn value: %d\n\n",
            msg5, returnVal5);
    syscall(__NR_dm510_msgbox_get, trashcan, sizeof(char) * 42); // Removing the msg from the stack

    returnVal3 = syscall(__NR_dm510_msgbox_get, buffer3, sizeof(char) * strlen(msg3));
    printf("Original message: %s\nGot message: %s\nRequested length:%ld\nReturn value: %d\n\n",
            msg3, buffer3, (sizeof(char)*strlen(msg3)*2), returnVal3);

    returnVal2 = syscall(__NR_dm510_msgbox_get, buffer2, sizeof(char) * 22);
    printf("Original message: %s\nGot message: %s\nOriginal length:%d\nReturn value: %d\n\n",
            msg2, buffer2, length2, returnVal2);

    returnVal1 = syscall(__NR_dm510_msgbox_get, buffer1, sizeof(char) * strlen(msg1));
    printf("Original message: %s\nGot message: %s\nOriginal length:%d\nReturn value: %d\n",
            msg1, buffer1, length1, returnVal1);

    // Freeing memory allocations
    free(buffer1);
    free(buffer2);
    free(buffer3);
    free(buffer5);
    free(trashcan);

    return 0;
}

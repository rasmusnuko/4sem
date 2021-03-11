/*rasjo19@student.sdu.dk*/
#include "linux/kernel.h"
#include "linux/unistd.h"
#include "linux/slab.h"
#include "linux/uaccess.h"

typedef struct _msg_t msg_t;

struct _msg_t{
    msg_t* previous;    // Previous msg_t in the stack
    int length;         // Length of the message being stored
    char* message;      // The message being stored
};

// top points to the top of the stack
static msg_t *top = NULL;

int sys_dm510_msgbox_put( char *buffer, int length ) {
    // Return error code, if buffer or length is unsafe
    if ( length < 0 ) {
        printk(KERN_ERR "Length < 0. Error in dm510_msgbox_put\n");
        return -1;      // Operation not permitted
    }
    if ( !access_ok(buffer, length) ){
        printk(KERN_ERR "Bad address. Error in dm510_msgbox_put\n");
        return -14;     // Bad address
    }

    // local irq save
    unsigned long flags;
    local_irq_save(flags);

    // Allocate memory, and initialize values for msg
    msg_t* msg = kmalloc(sizeof(msg_t), GFP_KERNEL);
    msg->previous = NULL;
    msg->length = length;
    msg->message = kmalloc(length, GFP_KERNEL);

    // Make sure memory allocation was successfull
    if (msg == NULL || msg->message == NULL){
        printk(KERN_ERR "Try again. kmalloc failed in dm510_msgbox_put\n");
        local_irq_restore(flags);
        return -11;     // Try again
    }
 
    // Copy memory from user-space buffer to kernel-space msg
    copy_from_user(msg->message, buffer, length);
 
    // Arrange previous and top
    msg->previous = top;
    top = msg;

    // local irq restore
    local_irq_restore(flags);

    // Return 0, to indicate everything's good
    return 0;
}


int sys_dm510_msgbox_get( char* buffer, int length ) {
    // Return error code, if bad address || length is negative || top is NULL
    if ( length < 0 ){
        printk(KERN_ERR "Length < 0. Error in dm510_msgbox_get\n");
        return -1;      // Operation not permitted
    }    
    if ( !access_ok(buffer, length) || buffer == NULL){
        printk(KERN_ERR "Bad address. Error in dm510_msgbox_put\n");
        return -14;     // Bad address
    }
    if ( top == NULL ) {
        printk(KERN_ERR "Empty stack. Error in dm510_msgbox_get\n");
        return -61;     // No data available
    }

    // local irq save
    unsigned long flags;
    local_irq_save(flags);
    
    // Init msg & length, and rearrange top
    msg_t* msg = top;
    int mlength = msg->length;
    top = msg->previous;

    // Make sure length <= mlength, if not, set length = mlength
    if ( length > mlength) { length = mlength; }

    // copy message
    copy_to_user(buffer, msg->message, length);

    // free memory
    kfree(msg->message);
    kfree(msg);

    // local irq restore
    local_irq_restore(flags);

    // Return the length of buffer copied
    return length;
}

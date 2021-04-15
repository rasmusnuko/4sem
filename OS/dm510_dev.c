/* Prototype module for second mandatory DM510 assignment */
#ifndef __KERNEL__
#  define __KERNEL__
#endif
#ifndef MODULE
#  define MODULE
#endif

#undef PDEBUG
#ifdef DM510_DEBUG
#  ifdef __KERNEL__
#    define PDEBUG(fmt, args...) printk(KERN_DEBUG "DM510: " fmt, ## args)
#  else
#    define PDEBUG(fmt, args...) fprintf(stderr, fmt, ## args)
#  endif
#else
#  define PDEBUG(fmt, args...)
#endif

#include <linux/module.h>
#include <linux/init.h>
#include <linux/slab.h> 
#include <linux/kernel.h>
#include <linux/fs.h>
#include <linux/errno.h>
#include <linux/types.h>
#include <linux/wait.h>
#include <linux/cdev.h>
/* #include <asm/uaccess.h> */
#include <linux/uaccess.h>
#include <linux/semaphore.h>
/* #include <asm/system.h> */
#include <asm/switch_to.h>
/* Prototypes - this would normally go in a .h file */
static int dm510_open( struct inode*, struct file* );
static int dm510_release( struct inode*, struct file* );
static ssize_t dm510_read( struct file*, char*, size_t, loff_t* );
static ssize_t dm510_write( struct file*, const char*, size_t, loff_t* );
long dm510_ioctl(struct file *filp, unsigned int cmd, unsigned long arg);

#define DEVICE_NAME "dm510_dev" /* Dev name as it appears in /proc/devices */
#define MAJOR_NUMBER 254
#define MIN_MINOR_NUMBER 0
#define MAX_MINOR_NUMBER 1
#define DEVICE_COUNT 2
/* end of what really should have been in a .h file */
struct dm510_pipe {
        wait_queue_head_t inQueue, outQueue;
        char *buffer, *end;
        int bufferSize;
        char *rLoc, *wLoc;
        int nReaders, nWriters;
        struct mutex mutex;
        struct cdev cdev;
        struct dm510_pipe *otherDev;
};

/* Parameters */
static struct dm510_pipe *dm510_p_devices;
dev_t dm510_p_devno[2];

/* file operations struct */
static struct file_operations dm510_fops = {
        .owner          = THIS_MODULE,
        .read           = dm510_read,
        .write          = dm510_write,
        .open           = dm510_open,
        .release        = dm510_release,
        .unlocked_ioctl = dm510_ioctl
};

static void dm510_p_setup_cdev(struct dm510_pipe *dev, int index) {
        int err, devno = dm510_p_devno[index];

        cdev_init(&dev->cdev, &dm510_fops);
        dev->cdev.owner = THIS_MODULE;
        err = cdev_add(&dev->cdev, devno, 1);

        if(err)
                printk(KERN_NOTICE "Error %d adding DM510 pipe%d", err, index);
}

static int spacefree(struct dm510_pipe *dev) {
        if(dev->rLoc == dev->wLoc) {
                return dev->bufferSize - 1;
        } else {
                return ((dev->rLoc + dev->bufferSize - dev->wLoc) % dev->bufferSize) - 1;
        }

        return 0;
}


static int dm510_getwritespace(struct dm510_pipe *dev, struct file *filp) {
        while(spacefree(dev) == 0) {
                DEFINE_WAIT(wait);

                mutex_unlock(&dev->mutex);
                if(filp->f_flags & O_NONBLOCK)
                        return -EAGAIN;
                PDEBUG("\"%s\" writing: going to sleep\n", current->comm);
                prepare_to_wait(&dev->outQueue, &wait, TASK_INTERRUPTIBLE);
                if(spacefree(dev) == 0)
                        schedule();
                finish_wait(&dev->outQueue, &wait);
                if(signal_pending(current))
                        return -ERESTARTSYS;
                if(mutex_lock_interruptible(&dev->mutex))
                        return -ERESTARTSYS;
        }
        return 0;
}

/* called when module is loaded */
int dm510_init_module( void ) {
        /* initialization code belongs here */
        int i, result;
        dm510_p_devno[0] = MKDEV(MAJOR_NUMBER, MIN_MINOR_NUMBER);
        dm510_p_devno[1] = MKDEV(MAJOR_NUMBER, MAX_MINOR_NUMBER);
        result = register_chrdev_region(MAJOR_NUMBER, DEVICE_COUNT, DEVICE_NAME);
        if(result < 0) {
                printk(KERN_ALERT "DM510: Failed to load module\n");
                return result;
        } else {
                printk(KERN_INFO "DM510: Hello from your device!\n");
        }

        dm510_p_devices = kmalloc(DEVICE_COUNT * sizeof(struct dm510_pipe), GFP_KERNEL);
        if(dm510_p_devices == NULL) {
                unregister_chrdev_region(MAJOR_NUMBER, DEVICE_COUNT);
                return 0;
        }

        memset(dm510_p_devices, 0, DEVICE_COUNT * sizeof(struct dm510_pipe));
        for(i = 0; i < DEVICE_COUNT; i++) {
                init_waitqueue_head(&(dm510_p_devices[i].inQueue));
                init_waitqueue_head(&(dm510_p_devices[i].outQueue));
                mutex_init(&dm510_p_devices[i].mutex);
                dm510_p_setup_cdev(dm510_p_devices + i, i);
        }

        dm510_p_devices[0].otherDev = &(dm510_p_devices[1]);
        dm510_p_devices[1].otherDev = &(dm510_p_devices[0]);

        return 0;
}

/* Called when module is unloaded */
void dm510_cleanup_module( void ) {
        /* clean up code belongs here */
        int i;

        if(!dm510_p_devices)
                return;

        for(i = 0; i < DEVICE_COUNT; i++) {
                cdev_del(&dm510_p_devices[i].cdev);
                kfree(dm510_p_devices[i].buffer);
        }
        kfree(dm510_p_devices);
        unregister_chrdev_region(MAJOR_NUMBER, DEVICE_COUNT);
        dm510_p_devices = NULL;

        printk(KERN_INFO "DM510: Module unloaded.\n");
}

/* Called when a process tries to open the device file */
static int dm510_open( struct inode *inode, struct file *filp ) {
        /* device claiming code belongs here */
        struct dm510_pipe *dev;
        struct dm510_pipe *dev2;
        //printk(KERN_INFO "--OPEN START\n");   
        dev = container_of(inode->i_cdev, struct dm510_pipe, cdev);
        dev2 = dev->otherDev;
        filp->private_data = dev;

        if(mutex_lock_interruptible(&dev->mutex)) {
                return -ERESTARTSYS;
        }
        if(!dev->buffer) {
                dev->buffer = kmalloc(4000, GFP_KERNEL);
                if(!dev->buffer) {
                        mutex_unlock(&dev->mutex);
                        return -ENOMEM;
                }
        }

        if(!dev2->buffer) {
                dev2->buffer = kmalloc(4000, GFP_KERNEL);
                if(!dev2->buffer) {
                        mutex_unlock(&dev2->mutex);
                        return -ENOMEM;
                }
        }

        dev->bufferSize = 4000;
        dev->end = dev->buffer + dev->bufferSize;
        dev->rLoc = dev->buffer;
        dev->wLoc = dev2->buffer;

        dev2->bufferSize = 4000;
        dev2->end = dev2->buffer + dev2->bufferSize;
        dev2->rLoc = dev2->buffer;
        dev2->wLoc = dev->buffer;

        if(filp->f_mode & FMODE_READ)
                dev->nReaders++;
        if(filp->f_mode & FMODE_WRITE)
                dev->nWriters++;
        mutex_unlock(&dev->mutex);
        //printk(KERN_INFO "--OPEN END\n");
        return nonseekable_open(inode, filp);
}

/* Called when a process closes the device file. */
static int dm510_release( struct inode *inode, struct file *filp ) {

        /* device release code belongs here */
        struct dm510_pipe *dev = filp->private_data;

        mutex_lock(&dev->mutex);
        if(filp->f_mode & FMODE_READ)
                dev->nReaders--;
        if(filp->f_mode & FMODE_WRITE)
                dev->nWriters--;
        if(dev->nReaders + dev->nWriters == 0) {
                kfree(dev->buffer);
                dev->buffer = NULL;
        }
        mutex_unlock(&dev->mutex);
        return 0;
}

/* Called when a process, which already opened the dev file, attempts to read from it. */
static ssize_t dm510_read( struct file *filp, char *buf, size_t count, loff_t *f_pos)
{
        /* read code belongs here */
        struct dm510_pipe *dev = filp->private_data;
        //printk(KERN_INFO "--READ START\n");
        if(mutex_lock_interruptible(&dev->mutex))
                return -ERESTARTSYS;

        while(dev->rLoc == dev->wLoc) {
                mutex_unlock(&dev->mutex);
                if(filp->f_flags & O_NONBLOCK)
                        return -EAGAIN;
                PDEBUG("\"%s\" reading: going to sleep\n", current->comm);
                if(wait_event_interruptible(dev->inQueue, (dev->rLoc != dev->wLoc)))
                        return -ERESTARTSYS;
                if(mutex_lock_interruptible(&dev->mutex))
                        return -ERESTARTSYS;
        }

        if(dev->wLoc > dev->rLoc)
                count = min(count, (size_t)(dev->wLoc - dev->rLoc));
        else
                count = min(count, (size_t)(dev->end - dev->rLoc));
        if(copy_to_user(buf, dev->rLoc, count)) {
                mutex_unlock(&dev->mutex);
                return -EFAULT;
        }

        dev->rLoc += count;
        if(dev->rLoc == dev->end)
                dev->rLoc = dev->buffer;
        mutex_unlock(&dev->mutex);

        wake_up_interruptible(&dev->outQueue);
        PDEBUG("\"%s\" did read %li bytes\n", current->comm, (long)count);
        //printk(KERN_INFO "--READ END\n");
        return count;
}

/* Called when a process writes to dev file */
static ssize_t dm510_write( struct file *filp, const char *buf, size_t count, loff_t *f_pos)
{
        /* write code belongs here */
        struct dm510_pipe *dev = filp->private_data;
        int result;
        //printk(KERN_INFO "--WRITE START\n");
        if(mutex_lock_interruptible(&dev->mutex))
                return -ERESTARTSYS;

        result = dm510_getwritespace(dev, filp);
        if(result)
                return result;

        count = min(count, (size_t)spacefree(dev));
        if(dev->wLoc >= dev->rLoc)
                count = min(count, (size_t)(dev->end - dev->wLoc));
        else
                count = min(count, (size_t)(dev->rLoc - dev->wLoc - 1));
        PDEBUG("Going to accept %li bytes to %p from %p\n", (long)count, dev->wLoc, buf);
        if(copy_from_user(dev->wLoc, buf, count)) {
                mutex_unlock(&dev->mutex);
                return -EFAULT;
        }
        dev->wLoc += count;
        if(dev->wLoc == dev->end)
                dev->wLoc = dev->buffer;
        mutex_unlock(&dev->mutex);

        wake_up_interruptible(&dev->inQueue);
        //printk(KERN_INFO "--WRITE END\n");
        return count; //return number of bytes written
}

/* called by system call icotl */
long dm510_ioctl(
                    struct file *filp,
                        unsigned int cmd,   /* command passed from the user */
                            unsigned long arg ) /* argument of the command */
{
                /* ioctl code belongs here */
                printk(KERN_INFO "DM510: ioctl called.\n");

                        return 0; //has to be changed
}



module_init( dm510_init_module );
module_exit( dm510_cleanup_module );

MODULE_AUTHOR( "...Anders Nis Herforth Larsen / anla119." );
MODULE_LICENSE( "GPL" );
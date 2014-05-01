/* this is in locks.c  */
   
   /* usage: lockset(string with pathname of file to be locked, 
                     string with type of lock "Read" or "Write") */

#include  <stdio.h>
#include  <fcntl.h>
#include  <errno.h>

/*  The error codes used in this routine are for an HP-UX system  */
/*   but may not be defined elsewhere ... to make sure they have  */
/*   some value, they may be given a dummy 999 value here.        */

#ifndef EAGAIN
#define EAGAIN   999      /* 11 No more processes   */
#endif

#ifndef ENFILE
#define ENFILE   999      /* 23 File table overflow */
#endif

#ifndef EMFILE
#define EMFILE   999      /* 24 Too many open files */
#endif

#define MAXHOLDS 10
int  oflag;
 
/* the following struct is the third arguement to fcntl. it is defined
   in /usr/include/sys/flock.h as follows:


 
struct	flock	{
	short	l_type;    !*F_RDLCK, F_WRLCK, F_UNLCK *!
	short	l_whence;  !* flag to choose starting offset 
                              0=beginning of file, 1= current position,
                              2 = end of file *!
	off_t	l_start;   !* relative offset from l_whence in bytes *!

	off_t	l_len;	   !* len = 0 means until end of file *!
	unsigned long	l_sysid; !* system id returned with F_GETLK *!
	pid_t	l_pid;     !* process id          "      "     "    *! 
	int	l_vfs;     !* i dunno, hs, 2!4!93                    *!
};  */


/* file segment locking set data type - information passed to system by user */

/* file segment locking types 
#define	F_RDLCK	01	!* Read lock *!
#define	F_WRLCK	02	!* Write lock *!
#define	F_UNLCK	03	!* Remove lock(s) *!
*/



static struct flock args =
   {
    (short)0, (short)0, (off_t)0, (off_t)0, 
(pid_t)0
   };

static int file_id, nholds=0;

static struct      /*  array of structures to hold the pathname   */
  {                /*  and the fileid of the locked file, when it */
   int id;         /*  opened.  the file needs to be opened as    */
   char * pathn;   /*  read or write before it can be locked!     */
  } holdem[MAXHOLDS];

int lockset( char * in_name, int *in_len, char * type)
{
   /* usage: lockset(string with pathname of file to be locked, string length,
                     string with type of lock "Read" or "Write") */
 char c;
 int retval, mskh;
 int i;
 char name[240];

 /* put input name local string and force a '\0' onto the end */

    for(i=0; i<*in_len; i++) name[i] = in_name[i];
    name[*in_len] = '\0';

 /* convert type request to lower case and check for read or write.
    if neither, open file as read&write.  OR in O_NODELAY to avoid
    blocking while system waits other action on this file: cf System V
    Interface Definition, ATT&T,1985, pg 114  */

 oflag = ((c=tolower(type[0]))=='r' ? O_RDONLY :
              (c=='w' ? O_WRONLY : O_RDWR)) | O_NDELAY;

 file_id=-1; /* inductive step for while loop */
 while( file_id <0) 
    /* try to open the file accroding to lock type */
  if ((file_id=open(name,oflag, 0))== -1)
    /* if failure, check if neither the maximum file descriptors are 
       open nor the system file table is full */
    if ( !(errno==EMFILE | errno==ENFILE))
     {
          /* open failed not because system tables too full 
             in case the file did not exist, try to create it */
      mskh = umask(0);    /* save file mode creation mask, 
                             usually = 022, it is XORed with
                             mode in creat. so set it to 0000, 
                             for now */
      file_id = creat(name, 0666 /* mode = -rw-rw-rw- */);
      umask(mskh);        /* restore file mode creation mask */
      if(file_id ==-1)
          /* if file create fails, return with error code -2 */return(-2);
      close(file_id);  /* close the newly created file.  it is opened */
      file_id=-1;      /* to write.  reset file_id for while loop.    */
     }
    else return (-3); /* a system table too full */


 args.l_type = (c=='w' ? F_WRLCK : F_RDLCK);
 retval = fcntl(file_id,F_SETLK,&args);
 if(retval >=0 && nholds<MAXHOLDS)
  { /* successful lock and not too many of them, so get some room to
       hold pathname */
   if ((holdem[nholds].pathn=(char *) malloc(strlen(name)+1))==NULL)
    { /* failed to get space, return with error code -2, same was when
         not able to create new file, above */
     /*fprintf(stderr,"NULL return from malloc in lockit, file: %s", name);*/
     close(file_id);
     return(-2);
    }
    /* all copasetic, copy path name and file id into saving array */
   strcpy(holdem[nholds].pathn,name);
   holdem[nholds++].id=file_id;
   return (0);
  }
 if (retval>=0)
  { /* too many files being locked in this program, return with err code -2, 
       change MAXHOLDS and recompile */
   /*fprintf(stderr,
           "exceeded lock file limit of %d for file %s\n",
            MAXHOLDS, name);*/
   close(file_id);
   return (-2);
  }
 else if (errno == EAGAIN)
  { /* lock permission is denied because of prior blocking locks, 
       return code -1 */
   /* fprintf(stderr,
           "lock permission denied for file %s errno = %d retval = -1\n",
	    name,errno); */
   close(file_id);
   return (-1);
  }
 else
  { /* lock permission denied because 
          1. file_id is not a valid open file descriptor (not likely), or
          2. the command (F_GETLK, F_SETLK, of F_SETLKW [not used]) and
             the &arg or the data it points ot is not valid (also not
             likely), or
          3. the command is F_SETLK or F_SETLKW [not used], the type of
             lock is a read or write lock and there are no more file locks
             available (too many segments are locked) (this can happen in
             our case if there are many read locks). */
   /* fprintf(stderr,
           "lock permission denied for file %s errno = %d retval = -2\n",
	    name,errno); */
   close(file_id);
   return (-1);
  }
                
}


int lockfree(char * in_name, int *in_len)
{
 int i,j;
 char name[240];

 /* put input name local string and force a '\0' onto the end */

    for(i=0; i<*in_len; i++) name[i] = in_name[i];
    name[*in_len] = '\0';

 for (i=0; i<nholds; i++)
  if (!(strcmp(holdem[i].pathn,name)))
   {  /* delete found path from list and its fileid, and
         then close ranks so array is dense, as far as it
         goes */ 
    close(holdem[i].id);
    free(holdem[i].pathn);
    if (i < (nholds-1))
     for (j=i; j<nholds-1; j++)
      {
       holdem[j].id = holdem[j+1].id;
       holdem[j].pathn = holdem[j+1].pathn;
      }
    nholds--;
    return(0);
   } 
 /*fprintf(stderr," nothing of %d elements matched %s \n",nholds,name);*/
 return (-1);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/locks.c,v $";
 static char rcs_id2[] = "$Id: locks.c,v 1.4 2000/12/18 15:04:22 jgofus Exp $";}
/*  ===================================================  */

}


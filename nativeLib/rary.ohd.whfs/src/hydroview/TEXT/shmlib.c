/*
	File:           shmlib.c
	Date:           January 2000
			Author: Sung Vo

	Purpose:        Provide support for sharing info b/w 
			shared window server and Hydroview
*/

#include <stdio.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/types.h>

#define SHMKEY         123456
#define SHMSIZE        100



int  create_shared_memory();
int  remove_shared_memory();
int  *locate_shared_memory();


/*
extern void  *shmat(int shmid, void *shmaddr, int shmflg);
extern int   shmget(key_t key, size_t size, int shmflg);
extern int   shmctl(int shmid, int cmd, struct shmid_ds *buf);
*/

extern void  *shmat();
extern int   shmget();
extern int   shmctl();

int create_shared_memory()
{
	int 	shmid;

	/* 
		remove shared memory segment if already exist then
		allocate  new shared memory and return shmid 
	*/

	shmid = shmget (SHMKEY, SHMSIZE,  0 );
	if ( shmid > 0)
		remove_shared_memory( shmid );

	shmid  = shmget (SHMKEY, SHMSIZE,  0666 | 0x0200);

	if ( shmid < 0) 
	{
		printf("Shared memory create has been denied.......\n");
		return (shmid);
	}
	
	printf("created shm id %d\n", shmid);
	
	return (shmid);

}


int  *locate_shared_memory()
{
	int 	shmid;
	char  	*shmptr = NULL;

	/* Get shmid from shared_memory_create () 
	   and do not attach to shared memory segment 
	   at this point. */

	shmid  = shmget (SHMKEY, SHMSIZE,  0 ); 
	if ( shmid < 0 )
	{
		printf("Error: locate_shared_memory ( %d )\n", shmid);
		return ( (int *)shmptr );

	}
	else
	{
		shmptr = (char  *)shmat( shmid, (char *)0, 0 );

		if ( shmptr == NULL ) 
		{
			printf("Error: shmat %d\n", *shmptr);
			return ( (int *)shmptr );
		}
	}

	printf("located shm ptr %p\n", shmptr);

	return ( (int *)shmptr );

}

int remove_shared_memory( )
{
	int	shmid,
		status;

	/*
		Remove shared memory segments when done 
	*/

	shmid  = shmget (SHMKEY, SHMSIZE,  0 );
	status = shmctl(shmid, 0, 0);
	
	printf("remove shm %d; status =%d\n", shmid, status);
	return (status);
}



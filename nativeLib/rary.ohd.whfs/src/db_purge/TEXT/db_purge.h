#include "DbmsDefs.h"

#ifndef DB_CLEANUP_MAIN_H
#define DB_CLEANUP_MAIN_H


/* defines */

#define SQLBUFFERSIZE 250
#define TIMESTRINGBUFFERSIZE 100   
#define VTEC_PURGE_TIME_HOST  30
#define VTEC_PURGE_TIME_BACKUP 14

/* include files */

#include "DbmsAccess.h"
#include "DbmsUtils.h"


/* prototypes */

int main();


void delete_file(char	* dir_envstr,
		 char	* file,
		 char	* where);
void delete_files();
void delete_daa_files(char * table, char * timestring);
void lower_trim(char* string);


void ProcessPurgeProduct();

void PurgeVTECevent(int backup_use);

void PurgeVTECpractice(int backup_use);

void wr2perflog();

/*  structure definitions */


typedef struct  lochsa {
                 char lid[LOC_ID_LEN+1] ;
                 char hsa[HYD_SERV_LEN+1] ;
               } lochsa;

#endif

/*
	File:		DbmsUtils.h
	
	Date:		4/13/94
	
	Purpose:	Utility routines to work with the 
			database.
*/

#ifndef DbmsUtils_h
#define DbmsUtils_h

#include <stdio.h>
#include "GeneralUtil.h"
#include "ecpgtype.h"
#include "ecpglib.h"


typedef enum SqlCommand
{
    INSERT = 0,
    UPDATE,
    DELETE,
    SELECT,
    SQL_COMMAND_COUNT
} SqlCommand;


typedef struct DbStatus
{
    int rows_affected;
    int sql_code;
    
    SqlCommand command;
      
    char sql_state[10];
    char error_msg[256];

} DbStatus;

void initDbStatus(DbStatus *dbStatus);
void setDbStatusRowsAffected(DbStatus *dbStatus, int rowCount);
void setDbStatusSqlCommand(DbStatus *dbStatus, SqlCommand command);
int getIndicator(int type, void *field);
int indicatesNull(int indicator);
void setNullIfIndicated(int indicator, int type, void *field);

/*
	Return values
*/
#define OK		 0
#define NOTNULL		 0	
#define ISNULL	 	 1
#define UNK_TYPE	-1


/*
	Type specifiers (corresponding to Informix types)
*/	
#define CHAR         	ECPGt_char
#define SHORT        	ECPGt_short
#define INT          	ECPGt_int
#define LONG		ECPGt_long
#define DOUBLE       	ECPGt_double
#define FLOAT        	ECPGt_float
#define REAL		ECPGt_real
#define DATETIME	ECPGt_timestamp
#define DATE            ECPGt_date

#define MAXTYPES        ECPGt_timestamp+1


#define TABLE_NAME_LEN 18


/*
	SQL Wrapper prototypes.
*/
long	recordCount(const char *table, const char *where);

int     execFunction( char *funcName);
int     execStatement( char *origStatement);
long 	dynamicDelete(char* cpSql, long* lpDeleted);
long 	executeImmediate(char* cpSql);



/*
	Error message functions
*/
char * DbErrorString(int errorCode);
void   DbErrorPrint(int errorCode, FILE* fp);
 

/*
	BLOB functions
*/
/*
void initBlobForDbInput(loc_t *locator, void * buffer,
		      long bufferSize, long dataSize, int isNull);
*/

/*
	Prototypes
*/
int	IsNull(int type, void *field);
int	SetNull(int type, void *field);	

void	getTableName(const char *pe, const char *ts, char *tablename);

#endif	

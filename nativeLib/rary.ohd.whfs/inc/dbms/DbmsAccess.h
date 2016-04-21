#ifndef DbmsAccess_h
#define DbmsAccess_h

#define STMT_LEN	512

typedef enum {
	Ok,
	errNoDatabaseName,
	errSqlFail,
	errCloseFail
} dbmsResult;


int	OpenDbms ( const char * name ) ;
int	CloseDbms ( );

#endif


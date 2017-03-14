/*
    File: ShefTs.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ShefTs_h
#define ShefTs_h


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>
#include "DbmsAccess.h"
#include "DbmsUtils.h"
#include "List.h"
#include "GeneralUtil.h"
#include "dbmserrs.h"
#include "datetime.h"
#include "time_convert.h"



typedef struct _ShefTs
{
    Node		node;
    char		ts[3];
    char		name[21];
    List		list;
} ShefTs;
/*
    Function Prototypes
*/
    ShefTs* GetShefTs(const char * where);
    ShefTs* SelectShefTs(const char * where);
    int SelectShefTsCount(const char * where);
    int PutShefTs(const ShefTs * structPtr);
    int InsertShefTs(const ShefTs * structPtr);
    int UpdateShefTs(const ShefTs* structPtr, const char *where);
    int DeleteShefTs(const char *where);
    int UpdateShefTsByRecord (const ShefTs * newStructPtr, const ShefTs * oldStructPtr);
    int InsertOrUpdateShefTs(const ShefTs * structPtr);
    int InsertIfUniqueShefTs(const ShefTs * structPtr, bool *isUnique);
    bool ShefTsExists(const ShefTs * structPtr);
    int DeleteShefTsByRecord(const ShefTs * structPtr);
    void GetShefTsPrimaryKeyWhereString (const ShefTs * structPtr, char returnWhereString[] );
    void FreeShefTs(ShefTs * structPtr);
    DbStatus * GetShefTsDbStatus();
    void SetShefTsErrorLogging(int value);
#endif

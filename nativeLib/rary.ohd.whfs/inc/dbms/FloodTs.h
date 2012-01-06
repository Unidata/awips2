/*
    File: FloodTs.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FloodTs_h
#define FloodTs_h


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



typedef struct _FloodTs
{
    Node		node;
    char		lid[9];
    dtime_t		obstime;
    long		flood_event_id;
    double		value;
    List		list;
} FloodTs;
/*
    Function Prototypes
*/
    FloodTs* GetFloodTs(const char * where);
    FloodTs* SelectFloodTs(const char * where);
    int SelectFloodTsCount(const char * where);
    int PutFloodTs(const FloodTs * structPtr);
    int InsertFloodTs(const FloodTs * structPtr);
    int UpdateFloodTs(const FloodTs* structPtr, const char *where);
    int DeleteFloodTs(const char *where);
    int UpdateFloodTsByRecord (const FloodTs * newStructPtr, const FloodTs * oldStructPtr);
    int InsertOrUpdateFloodTs(const FloodTs * structPtr);
    int InsertIfUniqueFloodTs(const FloodTs * structPtr, bool *isUnique);
    bool FloodTsExists(const FloodTs * structPtr);
    int DeleteFloodTsByRecord(const FloodTs * structPtr);
    void GetFloodTsPrimaryKeyWhereString (const FloodTs * structPtr, char returnWhereString[] );
    void FreeFloodTs(FloodTs * structPtr);
    DbStatus * GetFloodTsDbStatus();
    void SetFloodTsErrorLogging(int value);
#endif

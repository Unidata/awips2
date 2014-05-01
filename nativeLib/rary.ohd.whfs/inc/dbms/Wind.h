/*
    File: Wind.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Wind_h
#define Wind_h


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



typedef struct _Wind
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    dtime_t		obstime;
    double		value;
    char		shef_qual_code[2];
    long		quality_code;
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} Wind;
/*
    Function Prototypes
*/
    Wind* GetWind(const char * where);
    Wind* SelectWind(const char * where);
    int SelectWindCount(const char * where);
    int PutWind(const Wind * structPtr);
    int InsertWind(const Wind * structPtr);
    int UpdateWind(const Wind* structPtr, const char *where);
    int DeleteWind(const char *where);
    int UpdateWindByRecord (const Wind * newStructPtr, const Wind * oldStructPtr);
    int InsertOrUpdateWind(const Wind * structPtr);
    int InsertIfUniqueWind(const Wind * structPtr, bool *isUnique);
    bool WindExists(const Wind * structPtr);
    int DeleteWindByRecord(const Wind * structPtr);
    void GetWindPrimaryKeyWhereString (const Wind * structPtr, char returnWhereString[] );
    void FreeWind(Wind * structPtr);
    DbStatus * GetWindDbStatus();
    void SetWindErrorLogging(int value);
#endif

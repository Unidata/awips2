/*
    File: LatestObsValue.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LatestObsValue_h
#define LatestObsValue_h


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



typedef struct _LatestObsValue
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    dtime_t		obstime;
    double		value;
    short		revision;
    char		shef_qual_code[2];
    long		quality_code;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} LatestObsValue;
/*
    Function Prototypes
*/
    LatestObsValue* GetLatestObsValue(const char * where);
    LatestObsValue* SelectLatestObsValue(const char * where);
    int SelectLatestObsValueCount(const char * where);
    int PutLatestObsValue(const LatestObsValue * structPtr);
    int InsertLatestObsValue(const LatestObsValue * structPtr);
    int UpdateLatestObsValue(const LatestObsValue* structPtr, const char *where);
    int DeleteLatestObsValue(const char *where);
    int UpdateLatestObsValueByRecord (const LatestObsValue * newStructPtr, const LatestObsValue * oldStructPtr);
    int InsertOrUpdateLatestObsValue(const LatestObsValue * structPtr);
    int InsertIfUniqueLatestObsValue(const LatestObsValue * structPtr, bool *isUnique);
    bool LatestObsValueExists(const LatestObsValue * structPtr);
    int DeleteLatestObsValueByRecord(const LatestObsValue * structPtr);
    void GetLatestObsValuePrimaryKeyWhereString (const LatestObsValue * structPtr, char returnWhereString[] );
    void FreeLatestObsValue(LatestObsValue * structPtr);
    DbStatus * GetLatestObsValueDbStatus();
    void SetLatestObsValueErrorLogging(int value);
#endif

/*
    File: Snow.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Snow_h
#define Snow_h


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



typedef struct _Snow
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
} Snow;
/*
    Function Prototypes
*/
    Snow* GetSnow(const char * where);
    Snow* SelectSnow(const char * where);
    int SelectSnowCount(const char * where);
    int PutSnow(const Snow * structPtr);
    int InsertSnow(const Snow * structPtr);
    int UpdateSnow(const Snow* structPtr, const char *where);
    int DeleteSnow(const char *where);
    int UpdateSnowByRecord (const Snow * newStructPtr, const Snow * oldStructPtr);
    int InsertOrUpdateSnow(const Snow * structPtr);
    int InsertIfUniqueSnow(const Snow * structPtr, bool *isUnique);
    bool SnowExists(const Snow * structPtr);
    int DeleteSnowByRecord(const Snow * structPtr);
    void GetSnowPrimaryKeyWhereString (const Snow * structPtr, char returnWhereString[] );
    void FreeSnow(Snow * structPtr);
    DbStatus * GetSnowDbStatus();
    void SetSnowErrorLogging(int value);
#endif

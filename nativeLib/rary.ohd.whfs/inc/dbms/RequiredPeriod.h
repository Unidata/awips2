/*
    File: RequiredPeriod.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RequiredPeriod_h
#define RequiredPeriod_h


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



typedef struct _RequiredPeriod
{
    Node		node;
    char		period_req[31];
    List		list;
} RequiredPeriod;
/*
    Function Prototypes
*/
    RequiredPeriod* GetRequiredPeriod(const char * where);
    RequiredPeriod* SelectRequiredPeriod(const char * where);
    int SelectRequiredPeriodCount(const char * where);
    int PutRequiredPeriod(const RequiredPeriod * structPtr);
    int InsertRequiredPeriod(const RequiredPeriod * structPtr);
    int UpdateRequiredPeriod(const RequiredPeriod* structPtr, const char *where);
    int DeleteRequiredPeriod(const char *where);
    int UpdateRequiredPeriodByRecord (const RequiredPeriod * newStructPtr, const RequiredPeriod * oldStructPtr);
    int InsertOrUpdateRequiredPeriod(const RequiredPeriod * structPtr);
    int InsertIfUniqueRequiredPeriod(const RequiredPeriod * structPtr, bool *isUnique);
    bool RequiredPeriodExists(const RequiredPeriod * structPtr);
    int DeleteRequiredPeriodByRecord(const RequiredPeriod * structPtr);
    void GetRequiredPeriodPrimaryKeyWhereString (const RequiredPeriod * structPtr, char returnWhereString[] );
    void FreeRequiredPeriod(RequiredPeriod * structPtr);
    DbStatus * GetRequiredPeriodDbStatus();
    void SetRequiredPeriodErrorLogging(int value);
#endif

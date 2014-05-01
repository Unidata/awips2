/*
    File: TimeZone.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef TimeZone_h
#define TimeZone_h


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



typedef struct _TimeZone
{
    Node		node;
    char		tzone[9];
    char		name[31];
    List		list;
} TimeZone;
/*
    Function Prototypes
*/
    TimeZone* GetTimeZone(const char * where);
    TimeZone* SelectTimeZone(const char * where);
    int SelectTimeZoneCount(const char * where);
    int PutTimeZone(const TimeZone * structPtr);
    int InsertTimeZone(const TimeZone * structPtr);
    int UpdateTimeZone(const TimeZone* structPtr, const char *where);
    int DeleteTimeZone(const char *where);
    int UpdateTimeZoneByRecord (const TimeZone * newStructPtr, const TimeZone * oldStructPtr);
    int InsertOrUpdateTimeZone(const TimeZone * structPtr);
    int InsertIfUniqueTimeZone(const TimeZone * structPtr, bool *isUnique);
    bool TimeZoneExists(const TimeZone * structPtr);
    int DeleteTimeZoneByRecord(const TimeZone * structPtr);
    void GetTimeZonePrimaryKeyWhereString (const TimeZone * structPtr, char returnWhereString[] );
    void FreeTimeZone(TimeZone * structPtr);
    DbStatus * GetTimeZoneDbStatus();
    void SetTimeZoneErrorLogging(int value);
#endif

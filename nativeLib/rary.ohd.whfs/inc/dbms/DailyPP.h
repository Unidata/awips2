/*
    File: DailyPP.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DailyPP_h
#define DailyPP_h


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



typedef struct _DailyPP
{
    Node		node;
    char		lid[9];
    char		ts[3];
    dtime_t		obstime;
    double		value;
    char		qc[2];
    dtime_t		postingtime;
    List		list;
} DailyPP;
/*
    Function Prototypes
*/
    DailyPP* GetDailyPP(const char * where);
    DailyPP* SelectDailyPP(const char * where);
    int SelectDailyPPCount(const char * where);
    int PutDailyPP(const DailyPP * structPtr);
    int InsertDailyPP(const DailyPP * structPtr);
    int UpdateDailyPP(const DailyPP* structPtr, const char *where);
    int DeleteDailyPP(const char *where);
    int UpdateDailyPPByRecord (const DailyPP * newStructPtr, const DailyPP * oldStructPtr);
    int InsertOrUpdateDailyPP(const DailyPP * structPtr);
    int InsertIfUniqueDailyPP(const DailyPP * structPtr, bool *isUnique);
    bool DailyPPExists(const DailyPP * structPtr);
    int DeleteDailyPPByRecord(const DailyPP * structPtr);
    void GetDailyPPPrimaryKeyWhereString (const DailyPP * structPtr, char returnWhereString[] );
    void FreeDailyPP(DailyPP * structPtr);
    DbStatus * GetDailyPPDbStatus();
    void SetDailyPPErrorLogging(int value);
#endif

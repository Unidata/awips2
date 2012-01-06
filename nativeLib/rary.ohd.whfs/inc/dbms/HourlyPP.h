/*
    File: HourlyPP.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef HourlyPP_h
#define HourlyPP_h


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



typedef struct _HourlyPP
{
    Node		node;
    char		lid[9];
    char		ts[3];
    date_t		obsdate;
    char		minute_offset[25];
    char		hourly_qc[25];
    short		hour1;
    short		hour2;
    short		hour3;
    short		hour4;
    short		hour5;
    short		hour6;
    short		hour7;
    short		hour8;
    short		hour9;
    short		hour10;
    short		hour11;
    short		hour12;
    short		hour13;
    short		hour14;
    short		hour15;
    short		hour16;
    short		hour17;
    short		hour18;
    short		hour19;
    short		hour20;
    short		hour21;
    short		hour22;
    short		hour23;
    short		hour24;
    short		sixhr06;
    short		sixhr12;
    short		sixhr18;
    short		sixhr24;
    char		sixhrqc[5];
    char		sixhroffset[5];
    List		list;
} HourlyPP;
/*
    Function Prototypes
*/
    HourlyPP* GetHourlyPP(const char * where);
    HourlyPP* SelectHourlyPP(const char * where);
    int SelectHourlyPPCount(const char * where);
    int PutHourlyPP(const HourlyPP * structPtr);
    int InsertHourlyPP(const HourlyPP * structPtr);
    int UpdateHourlyPP(const HourlyPP* structPtr, const char *where);
    int DeleteHourlyPP(const char *where);
    int UpdateHourlyPPByRecord (const HourlyPP * newStructPtr, const HourlyPP * oldStructPtr);
    int InsertOrUpdateHourlyPP(const HourlyPP * structPtr);
    int InsertIfUniqueHourlyPP(const HourlyPP * structPtr, bool *isUnique);
    bool HourlyPPExists(const HourlyPP * structPtr);
    int DeleteHourlyPPByRecord(const HourlyPP * structPtr);
    void GetHourlyPPPrimaryKeyWhereString (const HourlyPP * structPtr, char returnWhereString[] );
    void FreeHourlyPP(HourlyPP * structPtr);
    DbStatus * GetHourlyPPDbStatus();
    void SetHourlyPPErrorLogging(int value);
#endif

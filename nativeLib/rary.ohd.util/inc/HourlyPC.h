/*
    File: HourlyPC.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef HourlyPC_h
#define HourlyPC_h


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



typedef struct _HourlyPC
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
    List		list;
} HourlyPC;
/*
    Function Prototypes
*/
    HourlyPC* GetHourlyPC(const char * where);
    HourlyPC* SelectHourlyPC(const char * where);
    int SelectHourlyPCCount(const char * where);
    int PutHourlyPC(const HourlyPC * structPtr);
    int InsertHourlyPC(const HourlyPC * structPtr);
    int UpdateHourlyPC(const HourlyPC* structPtr, const char *where);
    int DeleteHourlyPC(const char *where);
    int UpdateHourlyPCByRecord (const HourlyPC * newStructPtr, const HourlyPC * oldStructPtr);
    int InsertOrUpdateHourlyPC(const HourlyPC * structPtr);
    int InsertIfUniqueHourlyPC(const HourlyPC * structPtr, bool *isUnique);
    bool HourlyPCExists(const HourlyPC * structPtr);
    int DeleteHourlyPCByRecord(const HourlyPC * structPtr);
    void GetHourlyPCPrimaryKeyWhereString (const HourlyPC * structPtr, char returnWhereString[] );
    void FreeHourlyPC(HourlyPC * structPtr);
    DbStatus * GetHourlyPCDbStatus();
    void SetHourlyPCErrorLogging(int value);
#endif

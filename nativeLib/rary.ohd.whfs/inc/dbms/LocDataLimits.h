/*
    File: LocDataLimits.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LocDataLimits_h
#define LocDataLimits_h


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



typedef struct _LocDataLimits
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		monthdaystart[6];
    char		monthdayend[6];
    double		gross_range_min;
    double		gross_range_max;
    double		reason_range_min;
    double		reason_range_max;
    double		roc_max;
    double		alert_upper_limit;
    double		alert_roc_limit;
    double		alarm_upper_limit;
    double		alarm_roc_limit;
    double		alert_lower_limit;
    double		alarm_lower_limit;
    double		alert_diff_limit;
    double		alarm_diff_limit;
    List		list;
} LocDataLimits;
/*
    Function Prototypes
*/
    LocDataLimits* GetLocDataLimits(const char * where);
    LocDataLimits* SelectLocDataLimits(const char * where);
    int SelectLocDataLimitsCount(const char * where);
    int PutLocDataLimits(const LocDataLimits * structPtr);
    int InsertLocDataLimits(const LocDataLimits * structPtr);
    int UpdateLocDataLimits(const LocDataLimits* structPtr, const char *where);
    int DeleteLocDataLimits(const char *where);
    int UpdateLocDataLimitsByRecord (const LocDataLimits * newStructPtr, const LocDataLimits * oldStructPtr);
    int InsertOrUpdateLocDataLimits(const LocDataLimits * structPtr);
    int InsertIfUniqueLocDataLimits(const LocDataLimits * structPtr, bool *isUnique);
    bool LocDataLimitsExists(const LocDataLimits * structPtr);
    int DeleteLocDataLimitsByRecord(const LocDataLimits * structPtr);
    void GetLocDataLimitsPrimaryKeyWhereString (const LocDataLimits * structPtr, char returnWhereString[] );
    void FreeLocDataLimits(LocDataLimits * structPtr);
    DbStatus * GetLocDataLimitsDbStatus();
    void SetLocDataLimitsErrorLogging(int value);
#endif

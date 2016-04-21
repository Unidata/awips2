/*
    File: DataLimits.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DataLimits_h
#define DataLimits_h


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



typedef struct _DataLimits
{
    Node		node;
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
} DataLimits;
/*
    Function Prototypes
*/
    DataLimits* GetDataLimits(const char * where);
    DataLimits* SelectDataLimits(const char * where);
    int SelectDataLimitsCount(const char * where);
    int PutDataLimits(const DataLimits * structPtr);
    int InsertDataLimits(const DataLimits * structPtr);
    int UpdateDataLimits(const DataLimits* structPtr, const char *where);
    int DeleteDataLimits(const char *where);
    int UpdateDataLimitsByRecord (const DataLimits * newStructPtr, const DataLimits * oldStructPtr);
    int InsertOrUpdateDataLimits(const DataLimits * structPtr);
    int InsertIfUniqueDataLimits(const DataLimits * structPtr, bool *isUnique);
    bool DataLimitsExists(const DataLimits * structPtr);
    int DeleteDataLimitsByRecord(const DataLimits * structPtr);
    void GetDataLimitsPrimaryKeyWhereString (const DataLimits * structPtr, char returnWhereString[] );
    void FreeDataLimits(DataLimits * structPtr);
    DbStatus * GetDataLimitsDbStatus();
    void SetDataLimitsErrorLogging(int value);
#endif

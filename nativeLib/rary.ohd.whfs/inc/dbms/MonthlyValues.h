/*
    File: MonthlyValues.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef MonthlyValues_h
#define MonthlyValues_h


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



typedef struct _MonthlyValues
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    char		adjustment[2];
    dtime_t		postingtime;
    double		jan_value;
    double		feb_value;
    double		mar_value;
    double		apr_value;
    double		may_value;
    double		jun_value;
    double		jul_value;
    double		aug_value;
    double		sep_value;
    double		oct_value;
    double		nov_value;
    double		dec_value;
    List		list;
} MonthlyValues;
/*
    Function Prototypes
*/
    MonthlyValues* GetMonthlyValues(const char * where);
    MonthlyValues* SelectMonthlyValues(const char * where);
    int SelectMonthlyValuesCount(const char * where);
    int PutMonthlyValues(const MonthlyValues * structPtr);
    int InsertMonthlyValues(const MonthlyValues * structPtr);
    int UpdateMonthlyValues(const MonthlyValues* structPtr, const char *where);
    int DeleteMonthlyValues(const char *where);
    int UpdateMonthlyValuesByRecord (const MonthlyValues * newStructPtr, const MonthlyValues * oldStructPtr);
    int InsertOrUpdateMonthlyValues(const MonthlyValues * structPtr);
    int InsertIfUniqueMonthlyValues(const MonthlyValues * structPtr, bool *isUnique);
    bool MonthlyValuesExists(const MonthlyValues * structPtr);
    int DeleteMonthlyValuesByRecord(const MonthlyValues * structPtr);
    void GetMonthlyValuesPrimaryKeyWhereString (const MonthlyValues * structPtr, char returnWhereString[] );
    void FreeMonthlyValues(MonthlyValues * structPtr);
    DbStatus * GetMonthlyValuesDbStatus();
    void SetMonthlyValuesErrorLogging(int value);
#endif

/*
    File: AlertAlarmVal.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef AlertAlarmVal_h
#define AlertAlarmVal_h


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



typedef struct _AlertAlarmVal
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    float		probability;
    dtime_t		validtime;
    dtime_t		basistime;
    double		value;
    double		suppl_value;
    char		shef_qual_code[2];
    long		quality_code;
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    dtime_t		action_time;
    char		aa_categ[7];
    char		aa_check[7];
    List		list;
} AlertAlarmVal;
/*
    Function Prototypes
*/
    AlertAlarmVal* GetAlertAlarmVal(const char * where);
    AlertAlarmVal* SelectAlertAlarmVal(const char * where);
    int SelectAlertAlarmValCount(const char * where);
    int PutAlertAlarmVal(const AlertAlarmVal * structPtr);
    int InsertAlertAlarmVal(const AlertAlarmVal * structPtr);
    int UpdateAlertAlarmVal(const AlertAlarmVal* structPtr, const char *where);
    int DeleteAlertAlarmVal(const char *where);
    int UpdateAlertAlarmValByRecord (const AlertAlarmVal * newStructPtr, const AlertAlarmVal * oldStructPtr);
    int InsertOrUpdateAlertAlarmVal(const AlertAlarmVal * structPtr);
    int InsertIfUniqueAlertAlarmVal(const AlertAlarmVal * structPtr, bool *isUnique);
    bool AlertAlarmValExists(const AlertAlarmVal * structPtr);
    int DeleteAlertAlarmValByRecord(const AlertAlarmVal * structPtr);
    void GetAlertAlarmValPrimaryKeyWhereString (const AlertAlarmVal * structPtr, char returnWhereString[] );
    void FreeAlertAlarmVal(AlertAlarmVal * structPtr);
    DbStatus * GetAlertAlarmValDbStatus();
    void SetAlertAlarmValErrorLogging(int value);
#endif

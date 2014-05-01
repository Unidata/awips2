/*
    File: Power.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Power_h
#define Power_h


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



typedef struct _Power
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
} Power;
/*
    Function Prototypes
*/
    Power* GetPower(const char * where);
    Power* SelectPower(const char * where);
    int SelectPowerCount(const char * where);
    int PutPower(const Power * structPtr);
    int InsertPower(const Power * structPtr);
    int UpdatePower(const Power* structPtr, const char *where);
    int DeletePower(const char *where);
    int UpdatePowerByRecord (const Power * newStructPtr, const Power * oldStructPtr);
    int InsertOrUpdatePower(const Power * structPtr);
    int InsertIfUniquePower(const Power * structPtr, bool *isUnique);
    bool PowerExists(const Power * structPtr);
    int DeletePowerByRecord(const Power * structPtr);
    void GetPowerPrimaryKeyWhereString (const Power * structPtr, char returnWhereString[] );
    void FreePower(Power * structPtr);
    DbStatus * GetPowerDbStatus();
    void SetPowerErrorLogging(int value);
#endif

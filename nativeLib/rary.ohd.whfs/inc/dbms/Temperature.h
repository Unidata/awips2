/*
    File: Temperature.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Temperature_h
#define Temperature_h


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



typedef struct _Temperature
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
} Temperature;
/*
    Function Prototypes
*/
    Temperature* GetTemperature(const char * where);
    Temperature* SelectTemperature(const char * where);
    int SelectTemperatureCount(const char * where);
    int PutTemperature(const Temperature * structPtr);
    int InsertTemperature(const Temperature * structPtr);
    int UpdateTemperature(const Temperature* structPtr, const char *where);
    int DeleteTemperature(const char *where);
    int UpdateTemperatureByRecord (const Temperature * newStructPtr, const Temperature * oldStructPtr);
    int InsertOrUpdateTemperature(const Temperature * structPtr);
    int InsertIfUniqueTemperature(const Temperature * structPtr, bool *isUnique);
    bool TemperatureExists(const Temperature * structPtr);
    int DeleteTemperatureByRecord(const Temperature * structPtr);
    void GetTemperaturePrimaryKeyWhereString (const Temperature * structPtr, char returnWhereString[] );
    void FreeTemperature(Temperature * structPtr);
    DbStatus * GetTemperatureDbStatus();
    void SetTemperatureErrorLogging(int value);
#endif

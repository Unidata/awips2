/*
    File: Moisture.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Moisture_h
#define Moisture_h


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



typedef struct _Moisture
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
} Moisture;
/*
    Function Prototypes
*/
    Moisture* GetMoisture(const char * where);
    Moisture* SelectMoisture(const char * where);
    int SelectMoistureCount(const char * where);
    int PutMoisture(const Moisture * structPtr);
    int InsertMoisture(const Moisture * structPtr);
    int UpdateMoisture(const Moisture* structPtr, const char *where);
    int DeleteMoisture(const char *where);
    int UpdateMoistureByRecord (const Moisture * newStructPtr, const Moisture * oldStructPtr);
    int InsertOrUpdateMoisture(const Moisture * structPtr);
    int InsertIfUniqueMoisture(const Moisture * structPtr, bool *isUnique);
    bool MoistureExists(const Moisture * structPtr);
    int DeleteMoistureByRecord(const Moisture * structPtr);
    void GetMoisturePrimaryKeyWhereString (const Moisture * structPtr, char returnWhereString[] );
    void FreeMoisture(Moisture * structPtr);
    DbStatus * GetMoistureDbStatus();
    void SetMoistureErrorLogging(int value);
#endif

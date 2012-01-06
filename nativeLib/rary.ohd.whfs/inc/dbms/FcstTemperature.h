/*
    File: FcstTemperature.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstTemperature_h
#define FcstTemperature_h


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



typedef struct _FcstTemperature
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
    char		shef_qual_code[2];
    long		quality_code;
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} FcstTemperature;
/*
    Function Prototypes
*/
    FcstTemperature* GetFcstTemperature(const char * where);
    FcstTemperature* SelectFcstTemperature(const char * where);
    int SelectFcstTemperatureCount(const char * where);
    int PutFcstTemperature(const FcstTemperature * structPtr);
    int InsertFcstTemperature(const FcstTemperature * structPtr);
    int UpdateFcstTemperature(const FcstTemperature* structPtr, const char *where);
    int DeleteFcstTemperature(const char *where);
    int UpdateFcstTemperatureByRecord (const FcstTemperature * newStructPtr, const FcstTemperature * oldStructPtr);
    int InsertOrUpdateFcstTemperature(const FcstTemperature * structPtr);
    int InsertIfUniqueFcstTemperature(const FcstTemperature * structPtr, bool *isUnique);
    bool FcstTemperatureExists(const FcstTemperature * structPtr);
    int DeleteFcstTemperatureByRecord(const FcstTemperature * structPtr);
    void GetFcstTemperaturePrimaryKeyWhereString (const FcstTemperature * structPtr, char returnWhereString[] );
    void FreeFcstTemperature(FcstTemperature * structPtr);
    DbStatus * GetFcstTemperatureDbStatus();
    void SetFcstTemperatureErrorLogging(int value);
#endif

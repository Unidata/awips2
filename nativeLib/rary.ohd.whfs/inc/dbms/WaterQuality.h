/*
    File: WaterQuality.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef WaterQuality_h
#define WaterQuality_h


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



typedef struct _WaterQuality
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
} WaterQuality;
/*
    Function Prototypes
*/
    WaterQuality* GetWaterQuality(const char * where);
    WaterQuality* SelectWaterQuality(const char * where);
    int SelectWaterQualityCount(const char * where);
    int PutWaterQuality(const WaterQuality * structPtr);
    int InsertWaterQuality(const WaterQuality * structPtr);
    int UpdateWaterQuality(const WaterQuality* structPtr, const char *where);
    int DeleteWaterQuality(const char *where);
    int UpdateWaterQualityByRecord (const WaterQuality * newStructPtr, const WaterQuality * oldStructPtr);
    int InsertOrUpdateWaterQuality(const WaterQuality * structPtr);
    int InsertIfUniqueWaterQuality(const WaterQuality * structPtr, bool *isUnique);
    bool WaterQualityExists(const WaterQuality * structPtr);
    int DeleteWaterQualityByRecord(const WaterQuality * structPtr);
    void GetWaterQualityPrimaryKeyWhereString (const WaterQuality * structPtr, char returnWhereString[] );
    void FreeWaterQuality(WaterQuality * structPtr);
    DbStatus * GetWaterQualityDbStatus();
    void SetWaterQualityErrorLogging(int value);
#endif

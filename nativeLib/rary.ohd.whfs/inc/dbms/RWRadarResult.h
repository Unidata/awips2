/*
    File: RWRadarResult.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RWRadarResult_h
#define RWRadarResult_h


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



typedef struct _RWRadarResult
{
    Node		node;
    char		radid[4];
    dtime_t		obstime;
    short		num_gages;
    char		rad_avail[2];
    double		rw_bias_val_used;
    double		mem_span_used;
    char		edit_bias[2];
    char		ignore_radar[2];
    List		list;
} RWRadarResult;
/*
    Function Prototypes
*/
    RWRadarResult* GetRWRadarResult(const char * where);
    RWRadarResult* SelectRWRadarResult(const char * where);
    int SelectRWRadarResultCount(const char * where);
    int PutRWRadarResult(const RWRadarResult * structPtr);
    int InsertRWRadarResult(const RWRadarResult * structPtr);
    int UpdateRWRadarResult(const RWRadarResult* structPtr, const char *where);
    int DeleteRWRadarResult(const char *where);
    int UpdateRWRadarResultByRecord (const RWRadarResult * newStructPtr, const RWRadarResult * oldStructPtr);
    int InsertOrUpdateRWRadarResult(const RWRadarResult * structPtr);
    int InsertIfUniqueRWRadarResult(const RWRadarResult * structPtr, bool *isUnique);
    bool RWRadarResultExists(const RWRadarResult * structPtr);
    int DeleteRWRadarResultByRecord(const RWRadarResult * structPtr);
    void GetRWRadarResultPrimaryKeyWhereString (const RWRadarResult * structPtr, char returnWhereString[] );
    void FreeRWRadarResult(RWRadarResult * structPtr);
    DbStatus * GetRWRadarResultDbStatus();
    void SetRWRadarResultErrorLogging(int value);
#endif

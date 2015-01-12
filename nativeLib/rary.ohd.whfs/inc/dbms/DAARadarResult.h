/*
    File: DAARadarResult.h
    Author  : CDBGEN
    Created : Thu Dec 05 19:07:00 EST 2013 using database hd_ob9eempty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DAARadarResult_h
#define DAARadarResult_h


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



typedef struct _DAARadarResult
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
} DAARadarResult;
/*
    Function Prototypes
*/
    DAARadarResult* GetDAARadarResult(const char * where);
    DAARadarResult* SelectDAARadarResult(const char * where);
    int SelectDAARadarResultCount(const char * where);
    int PutDAARadarResult(const DAARadarResult * structPtr);
    int InsertDAARadarResult(const DAARadarResult * structPtr);
    int UpdateDAARadarResult(const DAARadarResult* structPtr, const char *where);
    int DeleteDAARadarResult(const char *where);
    int UpdateDAARadarResultByRecord (const DAARadarResult * newStructPtr, const DAARadarResult * oldStructPtr);
    int InsertOrUpdateDAARadarResult(const DAARadarResult * structPtr);
    int InsertIfUniqueDAARadarResult(const DAARadarResult * structPtr, bool *isUnique);
    bool DAARadarResultExists(const DAARadarResult * structPtr);
    int DeleteDAARadarResultByRecord(const DAARadarResult * structPtr);
    void GetDAARadarResultPrimaryKeyWhereString (const DAARadarResult * structPtr, char returnWhereString[] );
    void FreeDAARadarResult(DAARadarResult * structPtr);
    DbStatus * GetDAARadarResultDbStatus();
    void SetDAARadarResultErrorLogging(int value);
#endif

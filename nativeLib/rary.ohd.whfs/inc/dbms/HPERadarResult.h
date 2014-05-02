/*
    File: HPERadarResult.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef HPERadarResult_h
#define HPERadarResult_h


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


typedef struct _HPERadarResult
{
    Node		node;
    char		hpe_productname[31];
    dtime_t     producttime;
    int 		num_radar_avail;
    char 		bias_source[21];
    char        radar_data_source[2];
    List		list;
} HPERadarResult;
/*
    Function Prototypes
*/
    HPERadarResult* GetHPERadarResult(const char * where);
    HPERadarResult* SelectHPERadarResult(const char * where);
    int SelectHPERadarResultCount(const char * where);
    int PutHPERadarResult(const HPERadarResult * structPtr);
    int InsertHPERadarResult(const HPERadarResult * structPtr);
    int UpdateHPERadarResult(const HPERadarResult* structPtr, const char *where);
    int DeleteHPERadarResult(const char *where);
    int UpdateHPERadarResultByRecord (const HPERadarResult * newStructPtr, const HPERadarResult * oldStructPtr);
    int InsertOrUpdateHPERadarResult(const HPERadarResult * structPtr);
    int InsertIfUniqueHPERadarResult(const HPERadarResult * structPtr, bool *isUnique);
    bool HPERadarResultExists(const HPERadarResult * structPtr);
    int DeleteHPERadarResultByRecord(const HPERadarResult * structPtr);
    void GetHPERadarResultPrimaryKeyWhereString (const HPERadarResult * structPtr, char returnWhereString[] );
    void FreeHPERadarResult(HPERadarResult * structPtr);
    DbStatus * GetHPERadarResultDbStatus();
    void SetHPERadarResultErrorLogging(int value);
#endif

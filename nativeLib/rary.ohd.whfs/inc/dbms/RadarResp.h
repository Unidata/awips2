/*
    File: RadarResp.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RadarResp_h
#define RadarResp_h


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



typedef struct _RadarResp
{
    Node		node;
    char		radid[4];
    char		site_id[6];
    List		list;
} RadarResp;
/*
    Function Prototypes
*/
    RadarResp* GetRadarResp(const char * where);
    RadarResp* SelectRadarResp(const char * where);
    int SelectRadarRespCount(const char * where);
    int PutRadarResp(const RadarResp * structPtr);
    int InsertRadarResp(const RadarResp * structPtr);
    int UpdateRadarResp(const RadarResp* structPtr, const char *where);
    int DeleteRadarResp(const char *where);
    int UpdateRadarRespByRecord (const RadarResp * newStructPtr, const RadarResp * oldStructPtr);
    int InsertOrUpdateRadarResp(const RadarResp * structPtr);
    int InsertIfUniqueRadarResp(const RadarResp * structPtr, bool *isUnique);
    bool RadarRespExists(const RadarResp * structPtr);
    int DeleteRadarRespByRecord(const RadarResp * structPtr);
    void GetRadarRespPrimaryKeyWhereString (const RadarResp * structPtr, char returnWhereString[] );
    void FreeRadarResp(RadarResp * structPtr);
    DbStatus * GetRadarRespDbStatus();
    void SetRadarRespErrorLogging(int value);
#endif

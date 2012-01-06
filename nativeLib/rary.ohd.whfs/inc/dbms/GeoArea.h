/*
    File: GeoArea.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef GeoArea_h
#define GeoArea_h


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



typedef struct _GeoArea
{
    Node		node;
    char		area_id[9];
    char		name[41];
    char		boundary_type[7];
    double		interior_lat;
    double		interior_lon;
    List		list;
} GeoArea;
/*
    Function Prototypes
*/
    GeoArea* GetGeoArea(const char * where);
    GeoArea* SelectGeoArea(const char * where);
    int SelectGeoAreaCount(const char * where);
    int PutGeoArea(const GeoArea * structPtr);
    int InsertGeoArea(const GeoArea * structPtr);
    int UpdateGeoArea(const GeoArea* structPtr, const char *where);
    int DeleteGeoArea(const char *where);
    int UpdateGeoAreaByRecord (const GeoArea * newStructPtr, const GeoArea * oldStructPtr);
    int InsertOrUpdateGeoArea(const GeoArea * structPtr);
    int InsertIfUniqueGeoArea(const GeoArea * structPtr, bool *isUnique);
    bool GeoAreaExists(const GeoArea * structPtr);
    int DeleteGeoAreaByRecord(const GeoArea * structPtr);
    void GetGeoAreaPrimaryKeyWhereString (const GeoArea * structPtr, char returnWhereString[] );
    void FreeGeoArea(GeoArea * structPtr);
    DbStatus * GetGeoAreaDbStatus();
    void SetGeoAreaErrorLogging(int value);
#endif

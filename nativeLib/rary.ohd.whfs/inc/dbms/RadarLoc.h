/*
    File: RadarLoc.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RadarLoc_h
#define RadarLoc_h


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



typedef struct _RadarLoc
{
    Node		node;
    char		radid[4];
    char		name[21];
    char		radid_prefix[2];
    short		radar_num;
    char		state[3];
    double		lat;
    double		lon;
    double		elev;
    double		tower_ht;
    char		use_radar[2];
    char		office_id[6];
    List		list;
} RadarLoc;
/*
    Function Prototypes
*/
    RadarLoc* GetRadarLoc(const char * where);
    RadarLoc* SelectRadarLoc(const char * where);
    int SelectRadarLocCount(const char * where);
    int PutRadarLoc(const RadarLoc * structPtr);
    int InsertRadarLoc(const RadarLoc * structPtr);
    int UpdateRadarLoc(const RadarLoc* structPtr, const char *where);
    int DeleteRadarLoc(const char *where);
    int UpdateRadarLocByRecord (const RadarLoc * newStructPtr, const RadarLoc * oldStructPtr);
    int InsertOrUpdateRadarLoc(const RadarLoc * structPtr);
    int InsertIfUniqueRadarLoc(const RadarLoc * structPtr, bool *isUnique);
    bool RadarLocExists(const RadarLoc * structPtr);
    int DeleteRadarLocByRecord(const RadarLoc * structPtr);
    void GetRadarLocPrimaryKeyWhereString (const RadarLoc * structPtr, char returnWhereString[] );
    void FreeRadarLoc(RadarLoc * structPtr);
    DbStatus * GetRadarLocDbStatus();
    void SetRadarLocErrorLogging(int value);
#endif

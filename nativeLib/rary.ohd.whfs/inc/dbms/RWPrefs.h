/*
    File: RWPrefs.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RWPrefs_h
#define RWPrefs_h


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



typedef struct _RWPrefs
{
    Node		node;
    char		userid[33];
    char		state_overlay[4];
    char		city_overlay[4];
    char		county_overlay[4];
    char		river_overlay[4];
    char		basin_overlay[4];
    char		radar_overlay[4];
    short		num_hours_wind;
    char		def_display_type[11];
    List		list;
} RWPrefs;
/*
    Function Prototypes
*/
    RWPrefs* GetRWPrefs(const char * where);
    RWPrefs* SelectRWPrefs(const char * where);
    int SelectRWPrefsCount(const char * where);
    int PutRWPrefs(const RWPrefs * structPtr);
    int InsertRWPrefs(const RWPrefs * structPtr);
    int UpdateRWPrefs(const RWPrefs* structPtr, const char *where);
    int DeleteRWPrefs(const char *where);
    int UpdateRWPrefsByRecord (const RWPrefs * newStructPtr, const RWPrefs * oldStructPtr);
    int InsertOrUpdateRWPrefs(const RWPrefs * structPtr);
    int InsertIfUniqueRWPrefs(const RWPrefs * structPtr, bool *isUnique);
    bool RWPrefsExists(const RWPrefs * structPtr);
    int DeleteRWPrefsByRecord(const RWPrefs * structPtr);
    void GetRWPrefsPrimaryKeyWhereString (const RWPrefs * structPtr, char returnWhereString[] );
    void FreeRWPrefs(RWPrefs * structPtr);
    DbStatus * GetRWPrefsDbStatus();
    void SetRWPrefsErrorLogging(int value);
#endif

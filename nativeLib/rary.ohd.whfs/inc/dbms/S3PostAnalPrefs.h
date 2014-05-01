/*
    File: S3PostAnalPrefs.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef S3PostAnalPrefs_h
#define S3PostAnalPrefs_h


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



typedef struct _S3PostAnalPrefs
{
    Node		node;
    char		userid[33];
    char		state_overlay[4];
    char		city_overlay[4];
    char		river_overlay[4];
    char		basin_overlay[4];
    char		radar_overlay[4];
    short		num_hours_wind;
    List		list;
} S3PostAnalPrefs;
/*
    Function Prototypes
*/
    S3PostAnalPrefs* GetS3PostAnalPrefs(const char * where);
    S3PostAnalPrefs* SelectS3PostAnalPrefs(const char * where);
    int SelectS3PostAnalPrefsCount(const char * where);
    int PutS3PostAnalPrefs(const S3PostAnalPrefs * structPtr);
    int InsertS3PostAnalPrefs(const S3PostAnalPrefs * structPtr);
    int UpdateS3PostAnalPrefs(const S3PostAnalPrefs* structPtr, const char *where);
    int DeleteS3PostAnalPrefs(const char *where);
    int UpdateS3PostAnalPrefsByRecord (const S3PostAnalPrefs * newStructPtr, const S3PostAnalPrefs * oldStructPtr);
    int InsertOrUpdateS3PostAnalPrefs(const S3PostAnalPrefs * structPtr);
    int InsertIfUniqueS3PostAnalPrefs(const S3PostAnalPrefs * structPtr, bool *isUnique);
    bool S3PostAnalPrefsExists(const S3PostAnalPrefs * structPtr);
    int DeleteS3PostAnalPrefsByRecord(const S3PostAnalPrefs * structPtr);
    void GetS3PostAnalPrefsPrimaryKeyWhereString (const S3PostAnalPrefs * structPtr, char returnWhereString[] );
    void FreeS3PostAnalPrefs(S3PostAnalPrefs * structPtr);
    DbStatus * GetS3PostAnalPrefsDbStatus();
    void SetS3PostAnalPrefsErrorLogging(int value);
#endif

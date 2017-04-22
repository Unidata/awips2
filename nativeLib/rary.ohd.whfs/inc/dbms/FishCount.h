/*
    File: FishCount.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FishCount_h
#define FishCount_h


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



typedef struct _FishCount
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
} FishCount;
/*
    Function Prototypes
*/
    FishCount* GetFishCount(const char * where);
    FishCount* SelectFishCount(const char * where);
    int SelectFishCountCount(const char * where);
    int PutFishCount(const FishCount * structPtr);
    int InsertFishCount(const FishCount * structPtr);
    int UpdateFishCount(const FishCount* structPtr, const char *where);
    int DeleteFishCount(const char *where);
    int UpdateFishCountByRecord (const FishCount * newStructPtr, const FishCount * oldStructPtr);
    int InsertOrUpdateFishCount(const FishCount * structPtr);
    int InsertIfUniqueFishCount(const FishCount * structPtr, bool *isUnique);
    bool FishCountExists(const FishCount * structPtr);
    int DeleteFishCountByRecord(const FishCount * structPtr);
    void GetFishCountPrimaryKeyWhereString (const FishCount * structPtr, char returnWhereString[] );
    void FreeFishCount(FishCount * structPtr);
    DbStatus * GetFishCountDbStatus();
    void SetFishCountErrorLogging(int value);
#endif

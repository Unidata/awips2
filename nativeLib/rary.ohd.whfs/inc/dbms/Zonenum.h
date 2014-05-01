/*
    File: Zonenum.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Zonenum_h
#define Zonenum_h


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



typedef struct _Zonenum
{
    Node		node;
    char		lid[9];
    char		state[3];
    char		zonenum[4];
    List		list;
} Zonenum;
/*
    Function Prototypes
*/
    Zonenum* GetZonenum(const char * where);
    Zonenum* SelectZonenum(const char * where);
    int SelectZonenumCount(const char * where);
    int PutZonenum(const Zonenum * structPtr);
    int InsertZonenum(const Zonenum * structPtr);
    int UpdateZonenum(const Zonenum* structPtr, const char *where);
    int DeleteZonenum(const char *where);
    int UpdateZonenumByRecord (const Zonenum * newStructPtr, const Zonenum * oldStructPtr);
    int InsertOrUpdateZonenum(const Zonenum * structPtr);
    int InsertIfUniqueZonenum(const Zonenum * structPtr, bool *isUnique);
    bool ZonenumExists(const Zonenum * structPtr);
    int DeleteZonenumByRecord(const Zonenum * structPtr);
    void GetZonenumPrimaryKeyWhereString (const Zonenum * structPtr, char returnWhereString[] );
    void FreeZonenum(Zonenum * structPtr);
    DbStatus * GetZonenumDbStatus();
    void SetZonenumErrorLogging(int value);
#endif

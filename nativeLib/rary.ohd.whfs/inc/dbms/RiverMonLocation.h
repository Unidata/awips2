/*
    File: RiverMonLocation.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RiverMonLocation_h
#define RiverMonLocation_h


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



typedef struct _RiverMonLocation
{
    Node		node;
    char		lid[9];
    char		group_id[9];
    long		ordinal;
    List		list;
} RiverMonLocation;
/*
    Function Prototypes
*/
    RiverMonLocation* GetRiverMonLocation(const char * where);
    RiverMonLocation* SelectRiverMonLocation(const char * where);
    int SelectRiverMonLocationCount(const char * where);
    int PutRiverMonLocation(const RiverMonLocation * structPtr);
    int InsertRiverMonLocation(const RiverMonLocation * structPtr);
    int UpdateRiverMonLocation(const RiverMonLocation* structPtr, const char *where);
    int DeleteRiverMonLocation(const char *where);
    int UpdateRiverMonLocationByRecord (const RiverMonLocation * newStructPtr, const RiverMonLocation * oldStructPtr);
    int InsertOrUpdateRiverMonLocation(const RiverMonLocation * structPtr);
    int InsertIfUniqueRiverMonLocation(const RiverMonLocation * structPtr, bool *isUnique);
    bool RiverMonLocationExists(const RiverMonLocation * structPtr);
    int DeleteRiverMonLocationByRecord(const RiverMonLocation * structPtr);
    void GetRiverMonLocationPrimaryKeyWhereString (const RiverMonLocation * structPtr, char returnWhereString[] );
    void FreeRiverMonLocation(RiverMonLocation * structPtr);
    DbStatus * GetRiverMonLocationDbStatus();
    void SetRiverMonLocationErrorLogging(int value);
#endif

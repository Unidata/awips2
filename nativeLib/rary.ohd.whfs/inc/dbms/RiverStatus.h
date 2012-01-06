/*
    File: RiverStatus.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RiverStatus_h
#define RiverStatus_h


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



typedef struct _RiverStatus
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    float		probability;
    dtime_t		validtime;
    dtime_t		basistime;
    double		value;
    List		list;
} RiverStatus;
/*
    Function Prototypes
*/
    RiverStatus* GetRiverStatus(const char * where);
    RiverStatus* SelectRiverStatus(const char * where);
    int SelectRiverStatusCount(const char * where);
    int PutRiverStatus(const RiverStatus * structPtr);
    int InsertRiverStatus(const RiverStatus * structPtr);
    int UpdateRiverStatus(const RiverStatus* structPtr, const char *where);
    int DeleteRiverStatus(const char *where);
    int UpdateRiverStatusByRecord (const RiverStatus * newStructPtr, const RiverStatus * oldStructPtr);
    int InsertOrUpdateRiverStatus(const RiverStatus * structPtr);
    int InsertIfUniqueRiverStatus(const RiverStatus * structPtr, bool *isUnique);
    bool RiverStatusExists(const RiverStatus * structPtr);
    int DeleteRiverStatusByRecord(const RiverStatus * structPtr);
    void GetRiverStatusPrimaryKeyWhereString (const RiverStatus * structPtr, char returnWhereString[] );
    void FreeRiverStatus(RiverStatus * structPtr);
    DbStatus * GetRiverStatusDbStatus();
    void SetRiverStatusErrorLogging(int value);
#endif

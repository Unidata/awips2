/*
    File: RpfFcstPoint.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RpfFcstPoint_h
#define RpfFcstPoint_h


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



typedef struct _RpfFcstPoint
{
    Node		node;
    char		lid[9];
    char		group_id[9];
    long		ordinal;
    double		chg_threshold;
    char		rec_type[4];
    char		primary_back[4];
    char		secondary_back[4];
    long		backhrs;
    long		forwardhrs;
    double		adjustendhrs;
    List		list;
} RpfFcstPoint;
/*
    Function Prototypes
*/
    RpfFcstPoint* GetRpfFcstPoint(const char * where);
    RpfFcstPoint* SelectRpfFcstPoint(const char * where);
    int SelectRpfFcstPointCount(const char * where);
    int PutRpfFcstPoint(const RpfFcstPoint * structPtr);
    int InsertRpfFcstPoint(const RpfFcstPoint * structPtr);
    int UpdateRpfFcstPoint(const RpfFcstPoint* structPtr, const char *where);
    int DeleteRpfFcstPoint(const char *where);
    int UpdateRpfFcstPointByRecord (const RpfFcstPoint * newStructPtr, const RpfFcstPoint * oldStructPtr);
    int InsertOrUpdateRpfFcstPoint(const RpfFcstPoint * structPtr);
    int InsertIfUniqueRpfFcstPoint(const RpfFcstPoint * structPtr, bool *isUnique);
    bool RpfFcstPointExists(const RpfFcstPoint * structPtr);
    int DeleteRpfFcstPointByRecord(const RpfFcstPoint * structPtr);
    void GetRpfFcstPointPrimaryKeyWhereString (const RpfFcstPoint * structPtr, char returnWhereString[] );
    void FreeRpfFcstPoint(RpfFcstPoint * structPtr);
    DbStatus * GetRpfFcstPointDbStatus();
    void SetRpfFcstPointErrorLogging(int value);
#endif

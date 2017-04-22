/*
    File: FcstHorizon.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstHorizon_h
#define FcstHorizon_h


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



typedef struct _FcstHorizon
{
    Node		node;
    char		fcst_horizon[31];
    List		list;
} FcstHorizon;
/*
    Function Prototypes
*/
    FcstHorizon* GetFcstHorizon(const char * where);
    FcstHorizon* SelectFcstHorizon(const char * where);
    int SelectFcstHorizonCount(const char * where);
    int PutFcstHorizon(const FcstHorizon * structPtr);
    int InsertFcstHorizon(const FcstHorizon * structPtr);
    int UpdateFcstHorizon(const FcstHorizon* structPtr, const char *where);
    int DeleteFcstHorizon(const char *where);
    int UpdateFcstHorizonByRecord (const FcstHorizon * newStructPtr, const FcstHorizon * oldStructPtr);
    int InsertOrUpdateFcstHorizon(const FcstHorizon * structPtr);
    int InsertIfUniqueFcstHorizon(const FcstHorizon * structPtr, bool *isUnique);
    bool FcstHorizonExists(const FcstHorizon * structPtr);
    int DeleteFcstHorizonByRecord(const FcstHorizon * structPtr);
    void GetFcstHorizonPrimaryKeyWhereString (const FcstHorizon * structPtr, char returnWhereString[] );
    void FreeFcstHorizon(FcstHorizon * structPtr);
    DbStatus * GetFcstHorizonDbStatus();
    void SetFcstHorizonErrorLogging(int value);
#endif

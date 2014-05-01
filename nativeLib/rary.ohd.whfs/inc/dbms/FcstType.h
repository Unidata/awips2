/*
    File: FcstType.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstType_h
#define FcstType_h


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



typedef struct _FcstType
{
    Node		node;
    char		fcsttype[21];
    List		list;
} FcstType;
/*
    Function Prototypes
*/
    FcstType* GetFcstType(const char * where);
    FcstType* SelectFcstType(const char * where);
    int SelectFcstTypeCount(const char * where);
    int PutFcstType(const FcstType * structPtr);
    int InsertFcstType(const FcstType * structPtr);
    int UpdateFcstType(const FcstType* structPtr, const char *where);
    int DeleteFcstType(const char *where);
    int UpdateFcstTypeByRecord (const FcstType * newStructPtr, const FcstType * oldStructPtr);
    int InsertOrUpdateFcstType(const FcstType * structPtr);
    int InsertIfUniqueFcstType(const FcstType * structPtr, bool *isUnique);
    bool FcstTypeExists(const FcstType * structPtr);
    int DeleteFcstTypeByRecord(const FcstType * structPtr);
    void GetFcstTypePrimaryKeyWhereString (const FcstType * structPtr, char returnWhereString[] );
    void FreeFcstType(FcstType * structPtr);
    DbStatus * GetFcstTypeDbStatus();
    void SetFcstTypeErrorLogging(int value);
#endif

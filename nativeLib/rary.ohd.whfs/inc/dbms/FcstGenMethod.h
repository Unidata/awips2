/*
    File: FcstGenMethod.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstGenMethod_h
#define FcstGenMethod_h


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



typedef struct _FcstGenMethod
{
    Node		node;
    char		fcst_gen_method[31];
    List		list;
} FcstGenMethod;
/*
    Function Prototypes
*/
    FcstGenMethod* GetFcstGenMethod(const char * where);
    FcstGenMethod* SelectFcstGenMethod(const char * where);
    int SelectFcstGenMethodCount(const char * where);
    int PutFcstGenMethod(const FcstGenMethod * structPtr);
    int InsertFcstGenMethod(const FcstGenMethod * structPtr);
    int UpdateFcstGenMethod(const FcstGenMethod* structPtr, const char *where);
    int DeleteFcstGenMethod(const char *where);
    int UpdateFcstGenMethodByRecord (const FcstGenMethod * newStructPtr, const FcstGenMethod * oldStructPtr);
    int InsertOrUpdateFcstGenMethod(const FcstGenMethod * structPtr);
    int InsertIfUniqueFcstGenMethod(const FcstGenMethod * structPtr, bool *isUnique);
    bool FcstGenMethodExists(const FcstGenMethod * structPtr);
    int DeleteFcstGenMethodByRecord(const FcstGenMethod * structPtr);
    void GetFcstGenMethodPrimaryKeyWhereString (const FcstGenMethod * structPtr, char returnWhereString[] );
    void FreeFcstGenMethod(FcstGenMethod * structPtr);
    DbStatus * GetFcstGenMethodDbStatus();
    void SetFcstGenMethodErrorLogging(int value);
#endif

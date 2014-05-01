/*
    File: WatSupMethod.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef WatSupMethod_h
#define WatSupMethod_h


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



typedef struct _WatSupMethod
{
    Node		node;
    char		watsup_method[51];
    List		list;
} WatSupMethod;
/*
    Function Prototypes
*/
    WatSupMethod* GetWatSupMethod(const char * where);
    WatSupMethod* SelectWatSupMethod(const char * where);
    int SelectWatSupMethodCount(const char * where);
    int PutWatSupMethod(const WatSupMethod * structPtr);
    int InsertWatSupMethod(const WatSupMethod * structPtr);
    int UpdateWatSupMethod(const WatSupMethod* structPtr, const char *where);
    int DeleteWatSupMethod(const char *where);
    int UpdateWatSupMethodByRecord (const WatSupMethod * newStructPtr, const WatSupMethod * oldStructPtr);
    int InsertOrUpdateWatSupMethod(const WatSupMethod * structPtr);
    int InsertIfUniqueWatSupMethod(const WatSupMethod * structPtr, bool *isUnique);
    bool WatSupMethodExists(const WatSupMethod * structPtr);
    int DeleteWatSupMethodByRecord(const WatSupMethod * structPtr);
    void GetWatSupMethodPrimaryKeyWhereString (const WatSupMethod * structPtr, char returnWhereString[] );
    void FreeWatSupMethod(WatSupMethod * structPtr);
    DbStatus * GetWatSupMethodDbStatus();
    void SetWatSupMethodErrorLogging(int value);
#endif

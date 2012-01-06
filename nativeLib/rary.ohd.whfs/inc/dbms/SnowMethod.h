/*
    File: SnowMethod.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef SnowMethod_h
#define SnowMethod_h


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



typedef struct _SnowMethod
{
    Node		node;
    char		snow_method[31];
    List		list;
} SnowMethod;
/*
    Function Prototypes
*/
    SnowMethod* GetSnowMethod(const char * where);
    SnowMethod* SelectSnowMethod(const char * where);
    int SelectSnowMethodCount(const char * where);
    int PutSnowMethod(const SnowMethod * structPtr);
    int InsertSnowMethod(const SnowMethod * structPtr);
    int UpdateSnowMethod(const SnowMethod* structPtr, const char *where);
    int DeleteSnowMethod(const char *where);
    int UpdateSnowMethodByRecord (const SnowMethod * newStructPtr, const SnowMethod * oldStructPtr);
    int InsertOrUpdateSnowMethod(const SnowMethod * structPtr);
    int InsertIfUniqueSnowMethod(const SnowMethod * structPtr, bool *isUnique);
    bool SnowMethodExists(const SnowMethod * structPtr);
    int DeleteSnowMethodByRecord(const SnowMethod * structPtr);
    void GetSnowMethodPrimaryKeyWhereString (const SnowMethod * structPtr, char returnWhereString[] );
    void FreeSnowMethod(SnowMethod * structPtr);
    DbStatus * GetSnowMethodDbStatus();
    void SetSnowMethodErrorLogging(int value);
#endif

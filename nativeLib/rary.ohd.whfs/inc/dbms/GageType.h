/*
    File: GageType.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef GageType_h
#define GageType_h


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



typedef struct _GageType
{
    Node		node;
    char		type[11];
    List		list;
} GageType;
/*
    Function Prototypes
*/
    GageType* GetGageType(const char * where);
    GageType* SelectGageType(const char * where);
    int SelectGageTypeCount(const char * where);
    int PutGageType(const GageType * structPtr);
    int InsertGageType(const GageType * structPtr);
    int UpdateGageType(const GageType* structPtr, const char *where);
    int DeleteGageType(const char *where);
    int UpdateGageTypeByRecord (const GageType * newStructPtr, const GageType * oldStructPtr);
    int InsertOrUpdateGageType(const GageType * structPtr);
    int InsertIfUniqueGageType(const GageType * structPtr, bool *isUnique);
    bool GageTypeExists(const GageType * structPtr);
    int DeleteGageTypeByRecord(const GageType * structPtr);
    void GetGageTypePrimaryKeyWhereString (const GageType * structPtr, char returnWhereString[] );
    void FreeGageType(GageType * structPtr);
    DbStatus * GetGageTypeDbStatus();
    void SetGageTypeErrorLogging(int value);
#endif

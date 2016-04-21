/*
    File: GageOwner.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef GageOwner_h
#define GageOwner_h


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



typedef struct _GageOwner
{
    Node		node;
    char		owner[11];
    List		list;
} GageOwner;
/*
    Function Prototypes
*/
    GageOwner* GetGageOwner(const char * where);
    GageOwner* SelectGageOwner(const char * where);
    int SelectGageOwnerCount(const char * where);
    int PutGageOwner(const GageOwner * structPtr);
    int InsertGageOwner(const GageOwner * structPtr);
    int UpdateGageOwner(const GageOwner* structPtr, const char *where);
    int DeleteGageOwner(const char *where);
    int UpdateGageOwnerByRecord (const GageOwner * newStructPtr, const GageOwner * oldStructPtr);
    int InsertOrUpdateGageOwner(const GageOwner * structPtr);
    int InsertIfUniqueGageOwner(const GageOwner * structPtr, bool *isUnique);
    bool GageOwnerExists(const GageOwner * structPtr);
    int DeleteGageOwnerByRecord(const GageOwner * structPtr);
    void GetGageOwnerPrimaryKeyWhereString (const GageOwner * structPtr, char returnWhereString[] );
    void FreeGageOwner(GageOwner * structPtr);
    DbStatus * GetGageOwnerDbStatus();
    void SetGageOwnerErrorLogging(int value);
#endif

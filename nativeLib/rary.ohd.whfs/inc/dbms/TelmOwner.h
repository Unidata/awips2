/*
    File: TelmOwner.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef TelmOwner_h
#define TelmOwner_h


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



typedef struct _TelmOwner
{
    Node		node;
    char		owner[11];
    List		list;
} TelmOwner;
/*
    Function Prototypes
*/
    TelmOwner* GetTelmOwner(const char * where);
    TelmOwner* SelectTelmOwner(const char * where);
    int SelectTelmOwnerCount(const char * where);
    int PutTelmOwner(const TelmOwner * structPtr);
    int InsertTelmOwner(const TelmOwner * structPtr);
    int UpdateTelmOwner(const TelmOwner* structPtr, const char *where);
    int DeleteTelmOwner(const char *where);
    int UpdateTelmOwnerByRecord (const TelmOwner * newStructPtr, const TelmOwner * oldStructPtr);
    int InsertOrUpdateTelmOwner(const TelmOwner * structPtr);
    int InsertIfUniqueTelmOwner(const TelmOwner * structPtr, bool *isUnique);
    bool TelmOwnerExists(const TelmOwner * structPtr);
    int DeleteTelmOwnerByRecord(const TelmOwner * structPtr);
    void GetTelmOwnerPrimaryKeyWhereString (const TelmOwner * structPtr, char returnWhereString[] );
    void FreeTelmOwner(TelmOwner * structPtr);
    DbStatus * GetTelmOwnerDbStatus();
    void SetTelmOwnerErrorLogging(int value);
#endif

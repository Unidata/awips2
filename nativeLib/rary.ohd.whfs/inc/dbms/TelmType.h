/*
    File: TelmType.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef TelmType_h
#define TelmType_h


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



typedef struct _TelmType
{
    Node		node;
    char		type[11];
    List		list;
} TelmType;
/*
    Function Prototypes
*/
    TelmType* GetTelmType(const char * where);
    TelmType* SelectTelmType(const char * where);
    int SelectTelmTypeCount(const char * where);
    int PutTelmType(const TelmType * structPtr);
    int InsertTelmType(const TelmType * structPtr);
    int UpdateTelmType(const TelmType* structPtr, const char *where);
    int DeleteTelmType(const char *where);
    int UpdateTelmTypeByRecord (const TelmType * newStructPtr, const TelmType * oldStructPtr);
    int InsertOrUpdateTelmType(const TelmType * structPtr);
    int InsertIfUniqueTelmType(const TelmType * structPtr, bool *isUnique);
    bool TelmTypeExists(const TelmType * structPtr);
    int DeleteTelmTypeByRecord(const TelmType * structPtr);
    void GetTelmTypePrimaryKeyWhereString (const TelmType * structPtr, char returnWhereString[] );
    void FreeTelmType(TelmType * structPtr);
    DbStatus * GetTelmTypeDbStatus();
    void SetTelmTypeErrorLogging(int value);
#endif

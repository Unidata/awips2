/*
    File: DamTypes.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DamTypes_h
#define DamTypes_h


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



typedef struct _DamTypes
{
    Node		node;
    char		type[11];
    List		list;
} DamTypes;
/*
    Function Prototypes
*/
    DamTypes* GetDamTypes(const char * where);
    DamTypes* SelectDamTypes(const char * where);
    int SelectDamTypesCount(const char * where);
    int PutDamTypes(const DamTypes * structPtr);
    int InsertDamTypes(const DamTypes * structPtr);
    int UpdateDamTypes(const DamTypes* structPtr, const char *where);
    int DeleteDamTypes(const char *where);
    int UpdateDamTypesByRecord (const DamTypes * newStructPtr, const DamTypes * oldStructPtr);
    int InsertOrUpdateDamTypes(const DamTypes * structPtr);
    int InsertIfUniqueDamTypes(const DamTypes * structPtr, bool *isUnique);
    bool DamTypesExists(const DamTypes * structPtr);
    int DeleteDamTypesByRecord(const DamTypes * structPtr);
    void GetDamTypesPrimaryKeyWhereString (const DamTypes * structPtr, char returnWhereString[] );
    void FreeDamTypes(DamTypes * structPtr);
    DbStatus * GetDamTypesDbStatus();
    void SetDamTypesErrorLogging(int value);
#endif

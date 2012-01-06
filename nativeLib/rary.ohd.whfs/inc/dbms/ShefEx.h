/*
    File: ShefEx.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ShefEx_h
#define ShefEx_h


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



typedef struct _ShefEx
{
    Node		node;
    char		extremum[2];
    char		name[21];
    List		list;
} ShefEx;
/*
    Function Prototypes
*/
    ShefEx* GetShefEx(const char * where);
    ShefEx* SelectShefEx(const char * where);
    int SelectShefExCount(const char * where);
    int PutShefEx(const ShefEx * structPtr);
    int InsertShefEx(const ShefEx * structPtr);
    int UpdateShefEx(const ShefEx* structPtr, const char *where);
    int DeleteShefEx(const char *where);
    int UpdateShefExByRecord (const ShefEx * newStructPtr, const ShefEx * oldStructPtr);
    int InsertOrUpdateShefEx(const ShefEx * structPtr);
    int InsertIfUniqueShefEx(const ShefEx * structPtr, bool *isUnique);
    bool ShefExExists(const ShefEx * structPtr);
    int DeleteShefExByRecord(const ShefEx * structPtr);
    void GetShefExPrimaryKeyWhereString (const ShefEx * structPtr, char returnWhereString[] );
    void FreeShefEx(ShefEx * structPtr);
    DbStatus * GetShefExDbStatus();
    void SetShefExErrorLogging(int value);
#endif

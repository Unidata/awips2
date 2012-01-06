/*
    File: Wfo.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Wfo_h
#define Wfo_h


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



typedef struct _Wfo
{
    Node		node;
    char		wfo[4];
    List		list;
} Wfo;
/*
    Function Prototypes
*/
    Wfo* GetWfo(const char * where);
    Wfo* SelectWfo(const char * where);
    int SelectWfoCount(const char * where);
    int PutWfo(const Wfo * structPtr);
    int InsertWfo(const Wfo * structPtr);
    int UpdateWfo(const Wfo* structPtr, const char *where);
    int DeleteWfo(const char *where);
    int UpdateWfoByRecord (const Wfo * newStructPtr, const Wfo * oldStructPtr);
    int InsertOrUpdateWfo(const Wfo * structPtr);
    int InsertIfUniqueWfo(const Wfo * structPtr, bool *isUnique);
    bool WfoExists(const Wfo * structPtr);
    int DeleteWfoByRecord(const Wfo * structPtr);
    void GetWfoPrimaryKeyWhereString (const Wfo * structPtr, char returnWhereString[] );
    void FreeWfo(Wfo * structPtr);
    DbStatus * GetWfoDbStatus();
    void SetWfoErrorLogging(int value);
#endif

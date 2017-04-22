/*
    File: LocArea.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LocArea_h
#define LocArea_h


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



typedef struct _LocArea
{
    Node		node;
    char		lid[9];
    char		area[81];
    List		list;
} LocArea;
/*
    Function Prototypes
*/
    LocArea* GetLocArea(const char * where);
    LocArea* SelectLocArea(const char * where);
    int SelectLocAreaCount(const char * where);
    int PutLocArea(const LocArea * structPtr);
    int InsertLocArea(const LocArea * structPtr);
    int UpdateLocArea(const LocArea* structPtr, const char *where);
    int DeleteLocArea(const char *where);
    int UpdateLocAreaByRecord (const LocArea * newStructPtr, const LocArea * oldStructPtr);
    int InsertOrUpdateLocArea(const LocArea * structPtr);
    int InsertIfUniqueLocArea(const LocArea * structPtr, bool *isUnique);
    bool LocAreaExists(const LocArea * structPtr);
    int DeleteLocAreaByRecord(const LocArea * structPtr);
    void GetLocAreaPrimaryKeyWhereString (const LocArea * structPtr, char returnWhereString[] );
    void FreeLocArea(LocArea * structPtr);
    DbStatus * GetLocAreaDbStatus();
    void SetLocAreaErrorLogging(int value);
#endif

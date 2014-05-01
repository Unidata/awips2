/*
    File: Rescap.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Rescap_h
#define Rescap_h


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



typedef struct _Rescap
{
    Node		node;
    char		lid[9];
    double		elev;
    double		storage;
    List		list;
} Rescap;
/*
    Function Prototypes
*/
    Rescap* GetRescap(const char * where);
    Rescap* SelectRescap(const char * where);
    int SelectRescapCount(const char * where);
    int PutRescap(const Rescap * structPtr);
    int InsertRescap(const Rescap * structPtr);
    int UpdateRescap(const Rescap* structPtr, const char *where);
    int DeleteRescap(const char *where);
    int UpdateRescapByRecord (const Rescap * newStructPtr, const Rescap * oldStructPtr);
    int InsertOrUpdateRescap(const Rescap * structPtr);
    int InsertIfUniqueRescap(const Rescap * structPtr, bool *isUnique);
    bool RescapExists(const Rescap * structPtr);
    int DeleteRescapByRecord(const Rescap * structPtr);
    void GetRescapPrimaryKeyWhereString (const Rescap * structPtr, char returnWhereString[] );
    void FreeRescap(Rescap * structPtr);
    DbStatus * GetRescapDbStatus();
    void SetRescapErrorLogging(int value);
#endif

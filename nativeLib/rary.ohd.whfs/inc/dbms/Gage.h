/*
    File: Gage.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Gage_h
#define Gage_h


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



typedef struct _Gage
{
    Node		node;
    char		lid[9];
    date_t		gbegin;
    char		type[11];
    date_t		gend;
    char		remark[256];
    char		maint[11];
    char		owner[11];
    List		list;
} Gage;
/*
    Function Prototypes
*/
    Gage* GetGage(const char * where);
    Gage* SelectGage(const char * where);
    int SelectGageCount(const char * where);
    int PutGage(const Gage * structPtr);
    int InsertGage(const Gage * structPtr);
    int UpdateGage(const Gage* structPtr, const char *where);
    int DeleteGage(const char *where);
    int UpdateGageByRecord (const Gage * newStructPtr, const Gage * oldStructPtr);
    int InsertOrUpdateGage(const Gage * structPtr);
    int InsertIfUniqueGage(const Gage * structPtr, bool *isUnique);
    bool GageExists(const Gage * structPtr);
    int DeleteGageByRecord(const Gage * structPtr);
    void GetGagePrimaryKeyWhereString (const Gage * structPtr, char returnWhereString[] );
    void FreeGage(Gage * structPtr);
    DbStatus * GetGageDbStatus();
    void SetGageErrorLogging(int value);
#endif

/*
    File: ResOwner.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ResOwner_h
#define ResOwner_h


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



typedef struct _ResOwner
{
    Node		node;
    char		owner[11];
    List		list;
} ResOwner;
/*
    Function Prototypes
*/
    ResOwner* GetResOwner(const char * where);
    ResOwner* SelectResOwner(const char * where);
    int SelectResOwnerCount(const char * where);
    int PutResOwner(const ResOwner * structPtr);
    int InsertResOwner(const ResOwner * structPtr);
    int UpdateResOwner(const ResOwner* structPtr, const char *where);
    int DeleteResOwner(const char *where);
    int UpdateResOwnerByRecord (const ResOwner * newStructPtr, const ResOwner * oldStructPtr);
    int InsertOrUpdateResOwner(const ResOwner * structPtr);
    int InsertIfUniqueResOwner(const ResOwner * structPtr, bool *isUnique);
    bool ResOwnerExists(const ResOwner * structPtr);
    int DeleteResOwnerByRecord(const ResOwner * structPtr);
    void GetResOwnerPrimaryKeyWhereString (const ResOwner * structPtr, char returnWhereString[] );
    void FreeResOwner(ResOwner * structPtr);
    DbStatus * GetResOwnerDbStatus();
    void SetResOwnerErrorLogging(int value);
#endif

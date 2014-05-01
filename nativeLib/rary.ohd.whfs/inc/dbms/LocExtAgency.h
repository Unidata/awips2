/*
    File: LocExtAgency.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LocExtAgency_h
#define LocExtAgency_h


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



typedef struct _LocExtAgency
{
    Node		node;
    char		lid[9];
    char		agency_code[9];
    char		office[21];
    List		list;
} LocExtAgency;
/*
    Function Prototypes
*/
    LocExtAgency* GetLocExtAgency(const char * where);
    LocExtAgency* SelectLocExtAgency(const char * where);
    int SelectLocExtAgencyCount(const char * where);
    int PutLocExtAgency(const LocExtAgency * structPtr);
    int InsertLocExtAgency(const LocExtAgency * structPtr);
    int UpdateLocExtAgency(const LocExtAgency* structPtr, const char *where);
    int DeleteLocExtAgency(const char *where);
    int UpdateLocExtAgencyByRecord (const LocExtAgency * newStructPtr, const LocExtAgency * oldStructPtr);
    int InsertOrUpdateLocExtAgency(const LocExtAgency * structPtr);
    int InsertIfUniqueLocExtAgency(const LocExtAgency * structPtr, bool *isUnique);
    bool LocExtAgencyExists(const LocExtAgency * structPtr);
    int DeleteLocExtAgencyByRecord(const LocExtAgency * structPtr);
    void GetLocExtAgencyPrimaryKeyWhereString (const LocExtAgency * structPtr, char returnWhereString[] );
    void FreeLocExtAgency(LocExtAgency * structPtr);
    DbStatus * GetLocExtAgencyDbStatus();
    void SetLocExtAgencyErrorLogging(int value);
#endif

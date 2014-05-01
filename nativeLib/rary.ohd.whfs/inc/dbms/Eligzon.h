/*
    File: Eligzon.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Eligzon_h
#define Eligzon_h


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



typedef struct _Eligzon
{
    Node		node;
    char		state[3];
    char		zonenum[4];
    char		descr[21];
    List		list;
} Eligzon;
/*
    Function Prototypes
*/
    Eligzon* GetEligzon(const char * where);
    Eligzon* SelectEligzon(const char * where);
    int SelectEligzonCount(const char * where);
    int PutEligzon(const Eligzon * structPtr);
    int InsertEligzon(const Eligzon * structPtr);
    int UpdateEligzon(const Eligzon* structPtr, const char *where);
    int DeleteEligzon(const char *where);
    int UpdateEligzonByRecord (const Eligzon * newStructPtr, const Eligzon * oldStructPtr);
    int InsertOrUpdateEligzon(const Eligzon * structPtr);
    int InsertIfUniqueEligzon(const Eligzon * structPtr, bool *isUnique);
    bool EligzonExists(const Eligzon * structPtr);
    int DeleteEligzonByRecord(const Eligzon * structPtr);
    void GetEligzonPrimaryKeyWhereString (const Eligzon * structPtr, char returnWhereString[] );
    void FreeEligzon(Eligzon * structPtr);
    DbStatus * GetEligzonDbStatus();
    void SetEligzonErrorLogging(int value);
#endif

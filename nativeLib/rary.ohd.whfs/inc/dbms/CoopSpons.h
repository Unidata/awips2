/*
    File: CoopSpons.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef CoopSpons_h
#define CoopSpons_h


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



typedef struct _CoopSpons
{
    Node		node;
    char		spons[8];
    List		list;
} CoopSpons;
/*
    Function Prototypes
*/
    CoopSpons* GetCoopSpons(const char * where);
    CoopSpons* SelectCoopSpons(const char * where);
    int SelectCoopSponsCount(const char * where);
    int PutCoopSpons(const CoopSpons * structPtr);
    int InsertCoopSpons(const CoopSpons * structPtr);
    int UpdateCoopSpons(const CoopSpons* structPtr, const char *where);
    int DeleteCoopSpons(const char *where);
    int UpdateCoopSponsByRecord (const CoopSpons * newStructPtr, const CoopSpons * oldStructPtr);
    int InsertOrUpdateCoopSpons(const CoopSpons * structPtr);
    int InsertIfUniqueCoopSpons(const CoopSpons * structPtr, bool *isUnique);
    bool CoopSponsExists(const CoopSpons * structPtr);
    int DeleteCoopSponsByRecord(const CoopSpons * structPtr);
    void GetCoopSponsPrimaryKeyWhereString (const CoopSpons * structPtr, char returnWhereString[] );
    void FreeCoopSpons(CoopSpons * structPtr);
    DbStatus * GetCoopSponsDbStatus();
    void SetCoopSponsErrorLogging(int value);
#endif

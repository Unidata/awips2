/*
    File: WatSupCoordAgency.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef WatSupCoordAgency_h
#define WatSupCoordAgency_h


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



typedef struct _WatSupCoordAgency
{
    Node		node;
    char		watsup_coord_agency[65];
    List		list;
} WatSupCoordAgency;
/*
    Function Prototypes
*/
    WatSupCoordAgency* GetWatSupCoordAgency(const char * where);
    WatSupCoordAgency* SelectWatSupCoordAgency(const char * where);
    int SelectWatSupCoordAgencyCount(const char * where);
    int PutWatSupCoordAgency(const WatSupCoordAgency * structPtr);
    int InsertWatSupCoordAgency(const WatSupCoordAgency * structPtr);
    int UpdateWatSupCoordAgency(const WatSupCoordAgency* structPtr, const char *where);
    int DeleteWatSupCoordAgency(const char *where);
    int UpdateWatSupCoordAgencyByRecord (const WatSupCoordAgency * newStructPtr, const WatSupCoordAgency * oldStructPtr);
    int InsertOrUpdateWatSupCoordAgency(const WatSupCoordAgency * structPtr);
    int InsertIfUniqueWatSupCoordAgency(const WatSupCoordAgency * structPtr, bool *isUnique);
    bool WatSupCoordAgencyExists(const WatSupCoordAgency * structPtr);
    int DeleteWatSupCoordAgencyByRecord(const WatSupCoordAgency * structPtr);
    void GetWatSupCoordAgencyPrimaryKeyWhereString (const WatSupCoordAgency * structPtr, char returnWhereString[] );
    void FreeWatSupCoordAgency(WatSupCoordAgency * structPtr);
    DbStatus * GetWatSupCoordAgencyDbStatus();
    void SetWatSupCoordAgencyErrorLogging(int value);
#endif

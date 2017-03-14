/*
    File: WatSupRespAgency.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef WatSupRespAgency_h
#define WatSupRespAgency_h


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



typedef struct _WatSupRespAgency
{
    Node		node;
    char		watsup_resp_agency[65];
    List		list;
} WatSupRespAgency;
/*
    Function Prototypes
*/
    WatSupRespAgency* GetWatSupRespAgency(const char * where);
    WatSupRespAgency* SelectWatSupRespAgency(const char * where);
    int SelectWatSupRespAgencyCount(const char * where);
    int PutWatSupRespAgency(const WatSupRespAgency * structPtr);
    int InsertWatSupRespAgency(const WatSupRespAgency * structPtr);
    int UpdateWatSupRespAgency(const WatSupRespAgency* structPtr, const char *where);
    int DeleteWatSupRespAgency(const char *where);
    int UpdateWatSupRespAgencyByRecord (const WatSupRespAgency * newStructPtr, const WatSupRespAgency * oldStructPtr);
    int InsertOrUpdateWatSupRespAgency(const WatSupRespAgency * structPtr);
    int InsertIfUniqueWatSupRespAgency(const WatSupRespAgency * structPtr, bool *isUnique);
    bool WatSupRespAgencyExists(const WatSupRespAgency * structPtr);
    int DeleteWatSupRespAgencyByRecord(const WatSupRespAgency * structPtr);
    void GetWatSupRespAgencyPrimaryKeyWhereString (const WatSupRespAgency * structPtr, char returnWhereString[] );
    void FreeWatSupRespAgency(WatSupRespAgency * structPtr);
    DbStatus * GetWatSupRespAgencyDbStatus();
    void SetWatSupRespAgencyErrorLogging(int value);
#endif

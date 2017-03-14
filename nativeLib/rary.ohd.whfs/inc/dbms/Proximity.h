/*
    File: Proximity.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Proximity_h
#define Proximity_h


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



typedef struct _Proximity
{
    Node		node;
    char		proximity[7];
    List		list;
} Proximity;
/*
    Function Prototypes
*/
    Proximity* GetProximity(const char * where);
    Proximity* SelectProximity(const char * where);
    int SelectProximityCount(const char * where);
    int PutProximity(const Proximity * structPtr);
    int InsertProximity(const Proximity * structPtr);
    int UpdateProximity(const Proximity* structPtr, const char *where);
    int DeleteProximity(const char *where);
    int UpdateProximityByRecord (const Proximity * newStructPtr, const Proximity * oldStructPtr);
    int InsertOrUpdateProximity(const Proximity * structPtr);
    int InsertIfUniqueProximity(const Proximity * structPtr, bool *isUnique);
    bool ProximityExists(const Proximity * structPtr);
    int DeleteProximityByRecord(const Proximity * structPtr);
    void GetProximityPrimaryKeyWhereString (const Proximity * structPtr, char returnWhereString[] );
    void FreeProximity(Proximity * structPtr);
    DbStatus * GetProximityDbStatus();
    void SetProximityErrorLogging(int value);
#endif

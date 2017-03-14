/*
    File: HgStation.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef HgStation_h
#define HgStation_h


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



typedef struct _HgStation
{
    Node		node;
    char		lid[9];
    char		pe[3];
    char		ts[3];
    char		fcstts[3];
    List		list;
} HgStation;
/*
    Function Prototypes
*/
    HgStation* GetHgStation(const char * where);
    HgStation* SelectHgStation(const char * where);
    int SelectHgStationCount(const char * where);
    int PutHgStation(const HgStation * structPtr);
    int InsertHgStation(const HgStation * structPtr);
    int UpdateHgStation(const HgStation* structPtr, const char *where);
    int DeleteHgStation(const char *where);
    int UpdateHgStationByRecord (const HgStation * newStructPtr, const HgStation * oldStructPtr);
    int InsertOrUpdateHgStation(const HgStation * structPtr);
    int InsertIfUniqueHgStation(const HgStation * structPtr, bool *isUnique);
    bool HgStationExists(const HgStation * structPtr);
    int DeleteHgStationByRecord(const HgStation * structPtr);
    void GetHgStationPrimaryKeyWhereString (const HgStation * structPtr, char returnWhereString[] );
    void FreeHgStation(HgStation * structPtr);
    DbStatus * GetHgStationDbStatus();
    void SetHgStationErrorLogging(int value);
#endif

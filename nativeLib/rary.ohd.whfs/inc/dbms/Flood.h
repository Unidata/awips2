/*
    File: Flood.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Flood_h
#define Flood_h


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



typedef struct _Flood
{
    Node		node;
    char		lid[9];
    double		stage;
    char		damage[513];
    char		dispstmt[61];
    List		list;
} Flood;
/*
    Function Prototypes
*/
    Flood* GetFlood(const char * where);
    Flood* SelectFlood(const char * where);
    int SelectFloodCount(const char * where);
    int PutFlood(const Flood * structPtr);
    int InsertFlood(const Flood * structPtr);
    int UpdateFlood(const Flood* structPtr, const char *where);
    int DeleteFlood(const char *where);
    int UpdateFloodByRecord (const Flood * newStructPtr, const Flood * oldStructPtr);
    int InsertOrUpdateFlood(const Flood * structPtr);
    int InsertIfUniqueFlood(const Flood * structPtr, bool *isUnique);
    bool FloodExists(const Flood * structPtr);
    int DeleteFloodByRecord(const Flood * structPtr);
    void GetFloodPrimaryKeyWhereString (const Flood * structPtr, char returnWhereString[] );
    void FreeFlood(Flood * structPtr);
    DbStatus * GetFloodDbStatus();
    void SetFloodErrorLogging(int value);
#endif

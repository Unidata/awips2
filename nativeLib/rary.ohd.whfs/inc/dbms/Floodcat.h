/*
    File: Floodcat.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Floodcat_h
#define Floodcat_h


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



typedef struct _Floodcat
{
    Node		node;
    char		lid[9];
    double		minor_stage;
    double		moderate_stage;
    double		major_stage;
    double		minor_flow;
    double		moderate_flow;
    double		major_flow;
    List		list;
} Floodcat;
/*
    Function Prototypes
*/
    Floodcat* GetFloodcat(const char * where);
    Floodcat* SelectFloodcat(const char * where);
    int SelectFloodcatCount(const char * where);
    int PutFloodcat(const Floodcat * structPtr);
    int InsertFloodcat(const Floodcat * structPtr);
    int UpdateFloodcat(const Floodcat* structPtr, const char *where);
    int DeleteFloodcat(const char *where);
    int UpdateFloodcatByRecord (const Floodcat * newStructPtr, const Floodcat * oldStructPtr);
    int InsertOrUpdateFloodcat(const Floodcat * structPtr);
    int InsertIfUniqueFloodcat(const Floodcat * structPtr, bool *isUnique);
    bool FloodcatExists(const Floodcat * structPtr);
    int DeleteFloodcatByRecord(const Floodcat * structPtr);
    void GetFloodcatPrimaryKeyWhereString (const Floodcat * structPtr, char returnWhereString[] );
    void FreeFloodcat(Floodcat * structPtr);
    DbStatus * GetFloodcatDbStatus();
    void SetFloodcatErrorLogging(int value);
#endif

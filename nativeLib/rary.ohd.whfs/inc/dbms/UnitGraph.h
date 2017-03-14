/*
    File: UnitGraph.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef UnitGraph_h
#define UnitGraph_h


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



typedef struct _UnitGraph
{
    Node		node;
    char		lid[9];
    char		area_id[9];
    char		model[11];
    long		dur;
    long		ordinal;
    double		discharge;
    List		list;
} UnitGraph;
/*
    Function Prototypes
*/
    UnitGraph* GetUnitGraph(const char * where);
    UnitGraph* SelectUnitGraph(const char * where);
    int SelectUnitGraphCount(const char * where);
    int PutUnitGraph(const UnitGraph * structPtr);
    int InsertUnitGraph(const UnitGraph * structPtr);
    int UpdateUnitGraph(const UnitGraph* structPtr, const char *where);
    int DeleteUnitGraph(const char *where);
    int UpdateUnitGraphByRecord (const UnitGraph * newStructPtr, const UnitGraph * oldStructPtr);
    int InsertOrUpdateUnitGraph(const UnitGraph * structPtr);
    int InsertIfUniqueUnitGraph(const UnitGraph * structPtr, bool *isUnique);
    bool UnitGraphExists(const UnitGraph * structPtr);
    int DeleteUnitGraphByRecord(const UnitGraph * structPtr);
    void GetUnitGraphPrimaryKeyWhereString (const UnitGraph * structPtr, char returnWhereString[] );
    void FreeUnitGraph(UnitGraph * structPtr);
    DbStatus * GetUnitGraphDbStatus();
    void SetUnitGraphErrorLogging(int value);
#endif

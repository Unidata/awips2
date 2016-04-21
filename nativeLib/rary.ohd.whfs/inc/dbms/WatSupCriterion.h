/*
    File: WatSupCriterion.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef WatSupCriterion_h
#define WatSupCriterion_h


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



typedef struct _WatSupCriterion
{
    Node		node;
    char		watsup_criterion[31];
    List		list;
} WatSupCriterion;
/*
    Function Prototypes
*/
    WatSupCriterion* GetWatSupCriterion(const char * where);
    WatSupCriterion* SelectWatSupCriterion(const char * where);
    int SelectWatSupCriterionCount(const char * where);
    int PutWatSupCriterion(const WatSupCriterion * structPtr);
    int InsertWatSupCriterion(const WatSupCriterion * structPtr);
    int UpdateWatSupCriterion(const WatSupCriterion* structPtr, const char *where);
    int DeleteWatSupCriterion(const char *where);
    int UpdateWatSupCriterionByRecord (const WatSupCriterion * newStructPtr, const WatSupCriterion * oldStructPtr);
    int InsertOrUpdateWatSupCriterion(const WatSupCriterion * structPtr);
    int InsertIfUniqueWatSupCriterion(const WatSupCriterion * structPtr, bool *isUnique);
    bool WatSupCriterionExists(const WatSupCriterion * structPtr);
    int DeleteWatSupCriterionByRecord(const WatSupCriterion * structPtr);
    void GetWatSupCriterionPrimaryKeyWhereString (const WatSupCriterion * structPtr, char returnWhereString[] );
    void FreeWatSupCriterion(WatSupCriterion * structPtr);
    DbStatus * GetWatSupCriterionDbStatus();
    void SetWatSupCriterionErrorLogging(int value);
#endif

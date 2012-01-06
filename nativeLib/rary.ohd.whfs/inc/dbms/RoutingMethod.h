/*
    File: RoutingMethod.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RoutingMethod_h
#define RoutingMethod_h


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



typedef struct _RoutingMethod
{
    Node		node;
    char		hydraul_method[31];
    List		list;
} RoutingMethod;
/*
    Function Prototypes
*/
    RoutingMethod* GetRoutingMethod(const char * where);
    RoutingMethod* SelectRoutingMethod(const char * where);
    int SelectRoutingMethodCount(const char * where);
    int PutRoutingMethod(const RoutingMethod * structPtr);
    int InsertRoutingMethod(const RoutingMethod * structPtr);
    int UpdateRoutingMethod(const RoutingMethod* structPtr, const char *where);
    int DeleteRoutingMethod(const char *where);
    int UpdateRoutingMethodByRecord (const RoutingMethod * newStructPtr, const RoutingMethod * oldStructPtr);
    int InsertOrUpdateRoutingMethod(const RoutingMethod * structPtr);
    int InsertIfUniqueRoutingMethod(const RoutingMethod * structPtr, bool *isUnique);
    bool RoutingMethodExists(const RoutingMethod * structPtr);
    int DeleteRoutingMethodByRecord(const RoutingMethod * structPtr);
    void GetRoutingMethodPrimaryKeyWhereString (const RoutingMethod * structPtr, char returnWhereString[] );
    void FreeRoutingMethod(RoutingMethod * structPtr);
    DbStatus * GetRoutingMethodDbStatus();
    void SetRoutingMethodErrorLogging(int value);
#endif

/*
    File: HydrologicMethod.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef HydrologicMethod_h
#define HydrologicMethod_h


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



typedef struct _HydrologicMethod
{
    Node		node;
    char		hydrol_method[31];
    List		list;
} HydrologicMethod;
/*
    Function Prototypes
*/
    HydrologicMethod* GetHydrologicMethod(const char * where);
    HydrologicMethod* SelectHydrologicMethod(const char * where);
    int SelectHydrologicMethodCount(const char * where);
    int PutHydrologicMethod(const HydrologicMethod * structPtr);
    int InsertHydrologicMethod(const HydrologicMethod * structPtr);
    int UpdateHydrologicMethod(const HydrologicMethod* structPtr, const char *where);
    int DeleteHydrologicMethod(const char *where);
    int UpdateHydrologicMethodByRecord (const HydrologicMethod * newStructPtr, const HydrologicMethod * oldStructPtr);
    int InsertOrUpdateHydrologicMethod(const HydrologicMethod * structPtr);
    int InsertIfUniqueHydrologicMethod(const HydrologicMethod * structPtr, bool *isUnique);
    bool HydrologicMethodExists(const HydrologicMethod * structPtr);
    int DeleteHydrologicMethodByRecord(const HydrologicMethod * structPtr);
    void GetHydrologicMethodPrimaryKeyWhereString (const HydrologicMethod * structPtr, char returnWhereString[] );
    void FreeHydrologicMethod(HydrologicMethod * structPtr);
    DbStatus * GetHydrologicMethodDbStatus();
    void SetHydrologicMethodErrorLogging(int value);
#endif

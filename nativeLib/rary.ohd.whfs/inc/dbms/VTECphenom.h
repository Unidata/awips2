/*
    File: VTECphenom.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef VTECphenom_h
#define VTECphenom_h


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



typedef struct _VTECphenom
{
    Node		node;
    char		phenom[3];
    char		name[26];
    List		list;
} VTECphenom;
/*
    Function Prototypes
*/
    VTECphenom* GetVTECphenom(const char * where);
    VTECphenom* SelectVTECphenom(const char * where);
    int SelectVTECphenomCount(const char * where);
    int PutVTECphenom(const VTECphenom * structPtr);
    int InsertVTECphenom(const VTECphenom * structPtr);
    int UpdateVTECphenom(const VTECphenom* structPtr, const char *where);
    int DeleteVTECphenom(const char *where);
    int UpdateVTECphenomByRecord (const VTECphenom * newStructPtr, const VTECphenom * oldStructPtr);
    int InsertOrUpdateVTECphenom(const VTECphenom * structPtr);
    int InsertIfUniqueVTECphenom(const VTECphenom * structPtr, bool *isUnique);
    bool VTECphenomExists(const VTECphenom * structPtr);
    int DeleteVTECphenomByRecord(const VTECphenom * structPtr);
    void GetVTECphenomPrimaryKeyWhereString (const VTECphenom * structPtr, char returnWhereString[] );
    void FreeVTECphenom(VTECphenom * structPtr);
    DbStatus * GetVTECphenomDbStatus();
    void SetVTECphenomErrorLogging(int value);
#endif

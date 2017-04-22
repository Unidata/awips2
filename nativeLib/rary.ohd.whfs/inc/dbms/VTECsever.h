/*
    File: VTECsever.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef VTECsever_h
#define VTECsever_h


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



typedef struct _VTECsever
{
    Node		node;
    char		severity[2];
    char		name[26];
    List		list;
} VTECsever;
/*
    Function Prototypes
*/
    VTECsever* GetVTECsever(const char * where);
    VTECsever* SelectVTECsever(const char * where);
    int SelectVTECseverCount(const char * where);
    int PutVTECsever(const VTECsever * structPtr);
    int InsertVTECsever(const VTECsever * structPtr);
    int UpdateVTECsever(const VTECsever* structPtr, const char *where);
    int DeleteVTECsever(const char *where);
    int UpdateVTECseverByRecord (const VTECsever * newStructPtr, const VTECsever * oldStructPtr);
    int InsertOrUpdateVTECsever(const VTECsever * structPtr);
    int InsertIfUniqueVTECsever(const VTECsever * structPtr, bool *isUnique);
    bool VTECseverExists(const VTECsever * structPtr);
    int DeleteVTECseverByRecord(const VTECsever * structPtr);
    void GetVTECseverPrimaryKeyWhereString (const VTECsever * structPtr, char returnWhereString[] );
    void FreeVTECsever(VTECsever * structPtr);
    DbStatus * GetVTECseverDbStatus();
    void SetVTECseverErrorLogging(int value);
#endif

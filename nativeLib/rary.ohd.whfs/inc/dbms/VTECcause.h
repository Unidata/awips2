/*
    File: VTECcause.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef VTECcause_h
#define VTECcause_h


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



typedef struct _VTECcause
{
    Node		node;
    char		immed_cause[3];
    char		name[26];
    List		list;
} VTECcause;
/*
    Function Prototypes
*/
    VTECcause* GetVTECcause(const char * where);
    VTECcause* SelectVTECcause(const char * where);
    int SelectVTECcauseCount(const char * where);
    int PutVTECcause(const VTECcause * structPtr);
    int InsertVTECcause(const VTECcause * structPtr);
    int UpdateVTECcause(const VTECcause* structPtr, const char *where);
    int DeleteVTECcause(const char *where);
    int UpdateVTECcauseByRecord (const VTECcause * newStructPtr, const VTECcause * oldStructPtr);
    int InsertOrUpdateVTECcause(const VTECcause * structPtr);
    int InsertIfUniqueVTECcause(const VTECcause * structPtr, bool *isUnique);
    bool VTECcauseExists(const VTECcause * structPtr);
    int DeleteVTECcauseByRecord(const VTECcause * structPtr);
    void GetVTECcausePrimaryKeyWhereString (const VTECcause * structPtr, char returnWhereString[] );
    void FreeVTECcause(VTECcause * structPtr);
    DbStatus * GetVTECcauseDbStatus();
    void SetVTECcauseErrorLogging(int value);
#endif

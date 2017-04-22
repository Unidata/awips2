/*
    File: GageMaint.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef GageMaint_h
#define GageMaint_h


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



typedef struct _GageMaint
{
    Node		node;
    char		maint[11];
    List		list;
} GageMaint;
/*
    Function Prototypes
*/
    GageMaint* GetGageMaint(const char * where);
    GageMaint* SelectGageMaint(const char * where);
    int SelectGageMaintCount(const char * where);
    int PutGageMaint(const GageMaint * structPtr);
    int InsertGageMaint(const GageMaint * structPtr);
    int UpdateGageMaint(const GageMaint* structPtr, const char *where);
    int DeleteGageMaint(const char *where);
    int UpdateGageMaintByRecord (const GageMaint * newStructPtr, const GageMaint * oldStructPtr);
    int InsertOrUpdateGageMaint(const GageMaint * structPtr);
    int InsertIfUniqueGageMaint(const GageMaint * structPtr, bool *isUnique);
    bool GageMaintExists(const GageMaint * structPtr);
    int DeleteGageMaintByRecord(const GageMaint * structPtr);
    void GetGageMaintPrimaryKeyWhereString (const GageMaint * structPtr, char returnWhereString[] );
    void FreeGageMaint(GageMaint * structPtr);
    DbStatus * GetGageMaintDbStatus();
    void SetGageMaintErrorLogging(int value);
#endif

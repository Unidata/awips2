/*
    File: RpfFcstGroup.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RpfFcstGroup_h
#define RpfFcstGroup_h


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



typedef struct _RpfFcstGroup
{
    Node		node;
    char		group_id[9];
    char		group_name[33];
    long		ordinal;
    char		rec_all_included[2];
    List		list;
} RpfFcstGroup;
/*
    Function Prototypes
*/
    RpfFcstGroup* GetRpfFcstGroup(const char * where);
    RpfFcstGroup* SelectRpfFcstGroup(const char * where);
    int SelectRpfFcstGroupCount(const char * where);
    int PutRpfFcstGroup(const RpfFcstGroup * structPtr);
    int InsertRpfFcstGroup(const RpfFcstGroup * structPtr);
    int UpdateRpfFcstGroup(const RpfFcstGroup* structPtr, const char *where);
    int DeleteRpfFcstGroup(const char *where);
    int UpdateRpfFcstGroupByRecord (const RpfFcstGroup * newStructPtr, const RpfFcstGroup * oldStructPtr);
    int InsertOrUpdateRpfFcstGroup(const RpfFcstGroup * structPtr);
    int InsertIfUniqueRpfFcstGroup(const RpfFcstGroup * structPtr, bool *isUnique);
    bool RpfFcstGroupExists(const RpfFcstGroup * structPtr);
    int DeleteRpfFcstGroupByRecord(const RpfFcstGroup * structPtr);
    void GetRpfFcstGroupPrimaryKeyWhereString (const RpfFcstGroup * structPtr, char returnWhereString[] );
    void FreeRpfFcstGroup(RpfFcstGroup * structPtr);
    DbStatus * GetRpfFcstGroupDbStatus();
    void SetRpfFcstGroupErrorLogging(int value);
#endif

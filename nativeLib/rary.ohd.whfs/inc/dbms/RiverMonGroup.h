/*
    File: RiverMonGroup.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RiverMonGroup_h
#define RiverMonGroup_h


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



typedef struct _RiverMonGroup
{
    Node		node;
    char		group_id[9];
    char		group_name[33];
    long		ordinal;
    char		hsa[4];
    List		list;
} RiverMonGroup;
/*
    Function Prototypes
*/
    RiverMonGroup* GetRiverMonGroup(const char * where);
    RiverMonGroup* SelectRiverMonGroup(const char * where);
    int SelectRiverMonGroupCount(const char * where);
    int PutRiverMonGroup(const RiverMonGroup * structPtr);
    int InsertRiverMonGroup(const RiverMonGroup * structPtr);
    int UpdateRiverMonGroup(const RiverMonGroup* structPtr, const char *where);
    int DeleteRiverMonGroup(const char *where);
    int UpdateRiverMonGroupByRecord (const RiverMonGroup * newStructPtr, const RiverMonGroup * oldStructPtr);
    int InsertOrUpdateRiverMonGroup(const RiverMonGroup * structPtr);
    int InsertIfUniqueRiverMonGroup(const RiverMonGroup * structPtr, bool *isUnique);
    bool RiverMonGroupExists(const RiverMonGroup * structPtr);
    int DeleteRiverMonGroupByRecord(const RiverMonGroup * structPtr);
    void GetRiverMonGroupPrimaryKeyWhereString (const RiverMonGroup * structPtr, char returnWhereString[] );
    void FreeRiverMonGroup(RiverMonGroup * structPtr);
    DbStatus * GetRiverMonGroupDbStatus();
    void SetRiverMonGroupErrorLogging(int value);
#endif

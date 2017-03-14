/*
    File: PurgeDynData.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef PurgeDynData_h
#define PurgeDynData_h


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



typedef struct _PurgeDynData
{
    Node		node;
    char		table_name[19];
    char		time_column_name[19];
    long		num_hours_host;
    long		num_hours_backup;
    List		list;
} PurgeDynData;
/*
    Function Prototypes
*/
    PurgeDynData* GetPurgeDynData(const char * where);
    PurgeDynData* SelectPurgeDynData(const char * where);
    int SelectPurgeDynDataCount(const char * where);
    int PutPurgeDynData(const PurgeDynData * structPtr);
    int InsertPurgeDynData(const PurgeDynData * structPtr);
    int UpdatePurgeDynData(const PurgeDynData* structPtr, const char *where);
    int DeletePurgeDynData(const char *where);
    int UpdatePurgeDynDataByRecord (const PurgeDynData * newStructPtr, const PurgeDynData * oldStructPtr);
    int InsertOrUpdatePurgeDynData(const PurgeDynData * structPtr);
    int InsertIfUniquePurgeDynData(const PurgeDynData * structPtr, bool *isUnique);
    bool PurgeDynDataExists(const PurgeDynData * structPtr);
    int DeletePurgeDynDataByRecord(const PurgeDynData * structPtr);
    void GetPurgeDynDataPrimaryKeyWhereString (const PurgeDynData * structPtr, char returnWhereString[] );
    void FreePurgeDynData(PurgeDynData * structPtr);
    DbStatus * GetPurgeDynDataDbStatus();
    void SetPurgeDynDataErrorLogging(int value);
#endif

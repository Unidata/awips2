/*
    File: FrequencyUpdate.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FrequencyUpdate_h
#define FrequencyUpdate_h


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



typedef struct _FrequencyUpdate
{
    Node		node;
    char		frequency_update[31];
    List		list;
} FrequencyUpdate;
/*
    Function Prototypes
*/
    FrequencyUpdate* GetFrequencyUpdate(const char * where);
    FrequencyUpdate* SelectFrequencyUpdate(const char * where);
    int SelectFrequencyUpdateCount(const char * where);
    int PutFrequencyUpdate(const FrequencyUpdate * structPtr);
    int InsertFrequencyUpdate(const FrequencyUpdate * structPtr);
    int UpdateFrequencyUpdate(const FrequencyUpdate* structPtr, const char *where);
    int DeleteFrequencyUpdate(const char *where);
    int UpdateFrequencyUpdateByRecord (const FrequencyUpdate * newStructPtr, const FrequencyUpdate * oldStructPtr);
    int InsertOrUpdateFrequencyUpdate(const FrequencyUpdate * structPtr);
    int InsertIfUniqueFrequencyUpdate(const FrequencyUpdate * structPtr, bool *isUnique);
    bool FrequencyUpdateExists(const FrequencyUpdate * structPtr);
    int DeleteFrequencyUpdateByRecord(const FrequencyUpdate * structPtr);
    void GetFrequencyUpdatePrimaryKeyWhereString (const FrequencyUpdate * structPtr, char returnWhereString[] );
    void FreeFrequencyUpdate(FrequencyUpdate * structPtr);
    DbStatus * GetFrequencyUpdateDbStatus();
    void SetFrequencyUpdateErrorLogging(int value);
#endif

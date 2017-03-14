/*
    File: CoopComms.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef CoopComms_h
#define CoopComms_h


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



typedef struct _CoopComms
{
    Node		node;
    char		comm[11];
    List		list;
} CoopComms;
/*
    Function Prototypes
*/
    CoopComms* GetCoopComms(const char * where);
    CoopComms* SelectCoopComms(const char * where);
    int SelectCoopCommsCount(const char * where);
    int PutCoopComms(const CoopComms * structPtr);
    int InsertCoopComms(const CoopComms * structPtr);
    int UpdateCoopComms(const CoopComms* structPtr, const char *where);
    int DeleteCoopComms(const char *where);
    int UpdateCoopCommsByRecord (const CoopComms * newStructPtr, const CoopComms * oldStructPtr);
    int InsertOrUpdateCoopComms(const CoopComms * structPtr);
    int InsertIfUniqueCoopComms(const CoopComms * structPtr, bool *isUnique);
    bool CoopCommsExists(const CoopComms * structPtr);
    int DeleteCoopCommsByRecord(const CoopComms * structPtr);
    void GetCoopCommsPrimaryKeyWhereString (const CoopComms * structPtr, char returnWhereString[] );
    void FreeCoopComms(CoopComms * structPtr);
    DbStatus * GetCoopCommsDbStatus();
    void SetCoopCommsErrorLogging(int value);
#endif

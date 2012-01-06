/*
    File: CoopRecip.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef CoopRecip_h
#define CoopRecip_h


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



typedef struct _CoopRecip
{
    Node		node;
    char		recip[16];
    List		list;
} CoopRecip;
/*
    Function Prototypes
*/
    CoopRecip* GetCoopRecip(const char * where);
    CoopRecip* SelectCoopRecip(const char * where);
    int SelectCoopRecipCount(const char * where);
    int PutCoopRecip(const CoopRecip * structPtr);
    int InsertCoopRecip(const CoopRecip * structPtr);
    int UpdateCoopRecip(const CoopRecip* structPtr, const char *where);
    int DeleteCoopRecip(const char *where);
    int UpdateCoopRecipByRecord (const CoopRecip * newStructPtr, const CoopRecip * oldStructPtr);
    int InsertOrUpdateCoopRecip(const CoopRecip * structPtr);
    int InsertIfUniqueCoopRecip(const CoopRecip * structPtr, bool *isUnique);
    bool CoopRecipExists(const CoopRecip * structPtr);
    int DeleteCoopRecipByRecord(const CoopRecip * structPtr);
    void GetCoopRecipPrimaryKeyWhereString (const CoopRecip * structPtr, char returnWhereString[] );
    void FreeCoopRecip(CoopRecip * structPtr);
    DbStatus * GetCoopRecipDbStatus();
    void SetCoopRecipErrorLogging(int value);
#endif

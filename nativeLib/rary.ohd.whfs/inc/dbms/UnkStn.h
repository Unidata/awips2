/*
    File: UnkStn.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef UnkStn_h
#define UnkStn_h


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



typedef struct _UnkStn
{
    Node		node;
    char		lid[9];
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} UnkStn;
/*
    Function Prototypes
*/
    UnkStn* GetUnkStn(const char * where);
    UnkStn* SelectUnkStn(const char * where);
    int SelectUnkStnCount(const char * where);
    int PutUnkStn(const UnkStn * structPtr);
    int InsertUnkStn(const UnkStn * structPtr);
    int UpdateUnkStn(const UnkStn* structPtr, const char *where);
    int DeleteUnkStn(const char *where);
    int UpdateUnkStnByRecord (const UnkStn * newStructPtr, const UnkStn * oldStructPtr);
    int InsertOrUpdateUnkStn(const UnkStn * structPtr);
    int InsertIfUniqueUnkStn(const UnkStn * structPtr, bool *isUnique);
    bool UnkStnExists(const UnkStn * structPtr);
    int DeleteUnkStnByRecord(const UnkStn * structPtr);
    void GetUnkStnPrimaryKeyWhereString (const UnkStn * structPtr, char returnWhereString[] );
    void FreeUnkStn(UnkStn * structPtr);
    DbStatus * GetUnkStnDbStatus();
    void SetUnkStnErrorLogging(int value);
#endif

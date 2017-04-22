/*
    File: TelmPayor.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef TelmPayor_h
#define TelmPayor_h


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



typedef struct _TelmPayor
{
    Node		node;
    char		payor[11];
    List		list;
} TelmPayor;
/*
    Function Prototypes
*/
    TelmPayor* GetTelmPayor(const char * where);
    TelmPayor* SelectTelmPayor(const char * where);
    int SelectTelmPayorCount(const char * where);
    int PutTelmPayor(const TelmPayor * structPtr);
    int InsertTelmPayor(const TelmPayor * structPtr);
    int UpdateTelmPayor(const TelmPayor* structPtr, const char *where);
    int DeleteTelmPayor(const char *where);
    int UpdateTelmPayorByRecord (const TelmPayor * newStructPtr, const TelmPayor * oldStructPtr);
    int InsertOrUpdateTelmPayor(const TelmPayor * structPtr);
    int InsertIfUniqueTelmPayor(const TelmPayor * structPtr, bool *isUnique);
    bool TelmPayorExists(const TelmPayor * structPtr);
    int DeleteTelmPayorByRecord(const TelmPayor * structPtr);
    void GetTelmPayorPrimaryKeyWhereString (const TelmPayor * structPtr, char returnWhereString[] );
    void FreeTelmPayor(TelmPayor * structPtr);
    DbStatus * GetTelmPayorDbStatus();
    void SetTelmPayorErrorLogging(int value);
#endif

/*
    File: DcpOwner.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DcpOwner_h
#define DcpOwner_h


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



typedef struct _DcpOwner
{
    Node		node;
    char		owner[11];
    List		list;
} DcpOwner;
/*
    Function Prototypes
*/
    DcpOwner* GetDcpOwner(const char * where);
    DcpOwner* SelectDcpOwner(const char * where);
    int SelectDcpOwnerCount(const char * where);
    int PutDcpOwner(const DcpOwner * structPtr);
    int InsertDcpOwner(const DcpOwner * structPtr);
    int UpdateDcpOwner(const DcpOwner* structPtr, const char *where);
    int DeleteDcpOwner(const char *where);
    int UpdateDcpOwnerByRecord (const DcpOwner * newStructPtr, const DcpOwner * oldStructPtr);
    int InsertOrUpdateDcpOwner(const DcpOwner * structPtr);
    int InsertIfUniqueDcpOwner(const DcpOwner * structPtr, bool *isUnique);
    bool DcpOwnerExists(const DcpOwner * structPtr);
    int DeleteDcpOwnerByRecord(const DcpOwner * structPtr);
    void GetDcpOwnerPrimaryKeyWhereString (const DcpOwner * structPtr, char returnWhereString[] );
    void FreeDcpOwner(DcpOwner * structPtr);
    DbStatus * GetDcpOwnerDbStatus();
    void SetDcpOwnerErrorLogging(int value);
#endif

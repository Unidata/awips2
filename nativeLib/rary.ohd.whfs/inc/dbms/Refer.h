/*
    File: Refer.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Refer_h
#define Refer_h


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



typedef struct _Refer
{
    Node		node;
    char		lid[9];
    char		reference[71];
    List		list;
} Refer;
/*
    Function Prototypes
*/
    Refer* GetRefer(const char * where);
    Refer* SelectRefer(const char * where);
    int SelectReferCount(const char * where);
    int PutRefer(const Refer * structPtr);
    int InsertRefer(const Refer * structPtr);
    int UpdateRefer(const Refer* structPtr, const char *where);
    int DeleteRefer(const char *where);
    int UpdateReferByRecord (const Refer * newStructPtr, const Refer * oldStructPtr);
    int InsertOrUpdateRefer(const Refer * structPtr);
    int InsertIfUniqueRefer(const Refer * structPtr, bool *isUnique);
    bool ReferExists(const Refer * structPtr);
    int DeleteReferByRecord(const Refer * structPtr);
    void GetReferPrimaryKeyWhereString (const Refer * structPtr, char returnWhereString[] );
    void FreeRefer(Refer * structPtr);
    DbStatus * GetReferDbStatus();
    void SetReferErrorLogging(int value);
#endif

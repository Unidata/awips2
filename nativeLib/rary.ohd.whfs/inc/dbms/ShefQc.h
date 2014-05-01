/*
    File: ShefQc.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ShefQc_h
#define ShefQc_h


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



typedef struct _ShefQc
{
    Node		node;
    char		shef_qual_code[2];
    char		name[21];
    List		list;
} ShefQc;
/*
    Function Prototypes
*/
    ShefQc* GetShefQc(const char * where);
    ShefQc* SelectShefQc(const char * where);
    int SelectShefQcCount(const char * where);
    int PutShefQc(const ShefQc * structPtr);
    int InsertShefQc(const ShefQc * structPtr);
    int UpdateShefQc(const ShefQc* structPtr, const char *where);
    int DeleteShefQc(const char *where);
    int UpdateShefQcByRecord (const ShefQc * newStructPtr, const ShefQc * oldStructPtr);
    int InsertOrUpdateShefQc(const ShefQc * structPtr);
    int InsertIfUniqueShefQc(const ShefQc * structPtr, bool *isUnique);
    bool ShefQcExists(const ShefQc * structPtr);
    int DeleteShefQcByRecord(const ShefQc * structPtr);
    void GetShefQcPrimaryKeyWhereString (const ShefQc * structPtr, char returnWhereString[] );
    void FreeShefQc(ShefQc * structPtr);
    DbStatus * GetShefQcDbStatus();
    void SetShefQcErrorLogging(int value);
#endif

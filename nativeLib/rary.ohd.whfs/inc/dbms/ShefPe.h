/*
    File: ShefPe.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ShefPe_h
#define ShefPe_h


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



typedef struct _ShefPe
{
    Node		node;
    char		pe[3];
    char		name[21];
    char		eng_unit[9];
    char		met_unit[9];
    List		list;
} ShefPe;
/*
    Function Prototypes
*/
    ShefPe* GetShefPe(const char * where);
    ShefPe* SelectShefPe(const char * where);
    int SelectShefPeCount(const char * where);
    int PutShefPe(const ShefPe * structPtr);
    int InsertShefPe(const ShefPe * structPtr);
    int UpdateShefPe(const ShefPe* structPtr, const char *where);
    int DeleteShefPe(const char *where);
    int UpdateShefPeByRecord (const ShefPe * newStructPtr, const ShefPe * oldStructPtr);
    int InsertOrUpdateShefPe(const ShefPe * structPtr);
    int InsertIfUniqueShefPe(const ShefPe * structPtr, bool *isUnique);
    bool ShefPeExists(const ShefPe * structPtr);
    int DeleteShefPeByRecord(const ShefPe * structPtr);
    void GetShefPePrimaryKeyWhereString (const ShefPe * structPtr, char returnWhereString[] );
    void FreeShefPe(ShefPe * structPtr);
    DbStatus * GetShefPeDbStatus();
    void SetShefPeErrorLogging(int value);
#endif

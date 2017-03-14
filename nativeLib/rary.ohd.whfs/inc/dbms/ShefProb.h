/*
    File: ShefProb.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ShefProb_h
#define ShefProb_h


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



typedef struct _ShefProb
{
    Node		node;
    char		probcode[2];
    float		probability;
    char		name[21];
    List		list;
} ShefProb;
/*
    Function Prototypes
*/
    ShefProb* GetShefProb(const char * where);
    ShefProb* SelectShefProb(const char * where);
    int SelectShefProbCount(const char * where);
    int PutShefProb(const ShefProb * structPtr);
    int InsertShefProb(const ShefProb * structPtr);
    int UpdateShefProb(const ShefProb* structPtr, const char *where);
    int DeleteShefProb(const char *where);
    int UpdateShefProbByRecord (const ShefProb * newStructPtr, const ShefProb * oldStructPtr);
    int InsertOrUpdateShefProb(const ShefProb * structPtr);
    int InsertIfUniqueShefProb(const ShefProb * structPtr, bool *isUnique);
    bool ShefProbExists(const ShefProb * structPtr);
    int DeleteShefProbByRecord(const ShefProb * structPtr);
    void GetShefProbPrimaryKeyWhereString (const ShefProb * structPtr, char returnWhereString[] );
    void FreeShefProb(ShefProb * structPtr);
    DbStatus * GetShefProbDbStatus();
    void SetShefProbErrorLogging(int value);
#endif

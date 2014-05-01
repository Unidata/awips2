/*
    File: ShefDur.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ShefDur_h
#define ShefDur_h


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



typedef struct _ShefDur
{
    Node		node;
    short		dur;
    char		durcode[2];
    char		name[21];
    List		list;
} ShefDur;
/*
    Function Prototypes
*/
    ShefDur* GetShefDur(const char * where);
    ShefDur* SelectShefDur(const char * where);
    int SelectShefDurCount(const char * where);
    int PutShefDur(const ShefDur * structPtr);
    int InsertShefDur(const ShefDur * structPtr);
    int UpdateShefDur(const ShefDur* structPtr, const char *where);
    int DeleteShefDur(const char *where);
    int UpdateShefDurByRecord (const ShefDur * newStructPtr, const ShefDur * oldStructPtr);
    int InsertOrUpdateShefDur(const ShefDur * structPtr);
    int InsertIfUniqueShefDur(const ShefDur * structPtr, bool *isUnique);
    bool ShefDurExists(const ShefDur * structPtr);
    int DeleteShefDurByRecord(const ShefDur * structPtr);
    void GetShefDurPrimaryKeyWhereString (const ShefDur * structPtr, char returnWhereString[] );
    void FreeShefDur(ShefDur * structPtr);
    DbStatus * GetShefDurDbStatus();
    void SetShefDurErrorLogging(int value);
#endif

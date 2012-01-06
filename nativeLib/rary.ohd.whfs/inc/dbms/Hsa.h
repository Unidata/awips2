/*
    File: Hsa.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Hsa_h
#define Hsa_h


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



typedef struct _Hsa
{
    Node		node;
    char		hsa[4];
    List		list;
} Hsa;
/*
    Function Prototypes
*/
    Hsa* GetHsa(const char * where);
    Hsa* SelectHsa(const char * where);
    int SelectHsaCount(const char * where);
    int PutHsa(const Hsa * structPtr);
    int InsertHsa(const Hsa * structPtr);
    int UpdateHsa(const Hsa* structPtr, const char *where);
    int DeleteHsa(const char *where);
    int UpdateHsaByRecord (const Hsa * newStructPtr, const Hsa * oldStructPtr);
    int InsertOrUpdateHsa(const Hsa * structPtr);
    int InsertIfUniqueHsa(const Hsa * structPtr, bool *isUnique);
    bool HsaExists(const Hsa * structPtr);
    int DeleteHsaByRecord(const Hsa * structPtr);
    void GetHsaPrimaryKeyWhereString (const Hsa * structPtr, char returnWhereString[] );
    void FreeHsa(Hsa * structPtr);
    DbStatus * GetHsaDbStatus();
    void SetHsaErrorLogging(int value);
#endif

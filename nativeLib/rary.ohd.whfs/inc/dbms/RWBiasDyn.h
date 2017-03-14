/*
    File: RWBiasDyn.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RWBiasDyn_h
#define RWBiasDyn_h


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



typedef struct _RWBiasDyn
{
    Node		node;
    char		radid[4];
    char		office_id[6];
    dtime_t		obstime;
    short		memspan_ind;
    double		numpairs;
    float		sumgag;
    float		sumrad;
    float		bias;
    List		list;
} RWBiasDyn;
/*
    Function Prototypes
*/
    RWBiasDyn* GetRWBiasDyn(const char * where);
    RWBiasDyn* SelectRWBiasDyn(const char * where);
    int SelectRWBiasDynCount(const char * where);
    int PutRWBiasDyn(const RWBiasDyn * structPtr);
    int InsertRWBiasDyn(const RWBiasDyn * structPtr);
    int UpdateRWBiasDyn(const RWBiasDyn* structPtr, const char *where);
    int DeleteRWBiasDyn(const char *where);
    int UpdateRWBiasDynByRecord (const RWBiasDyn * newStructPtr, const RWBiasDyn * oldStructPtr);
    int InsertOrUpdateRWBiasDyn(const RWBiasDyn * structPtr);
    int InsertIfUniqueRWBiasDyn(const RWBiasDyn * structPtr, bool *isUnique);
    bool RWBiasDynExists(const RWBiasDyn * structPtr);
    int DeleteRWBiasDynByRecord(const RWBiasDyn * structPtr);
    void GetRWBiasDynPrimaryKeyWhereString (const RWBiasDyn * structPtr, char returnWhereString[] );
    void FreeRWBiasDyn(RWBiasDyn * structPtr);
    DbStatus * GetRWBiasDynDbStatus();
    void SetRWBiasDynErrorLogging(int value);
#endif

/*
    File: DAABiasDyn.h
    Author  : CDBGEN
    Created : Wed Dec 04 19:07:04 EST 2013 using database hd_ob9eempty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DAABiasDyn_h
#define DAABiasDyn_h


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



typedef struct _DAABiasDyn
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
} DAABiasDyn;
/*
    Function Prototypes
*/
    DAABiasDyn* GetDAABiasDyn(const char * where);
    DAABiasDyn* SelectDAABiasDyn(const char * where);
    int SelectDAABiasDynCount(const char * where);
    int PutDAABiasDyn(const DAABiasDyn * structPtr);
    int InsertDAABiasDyn(const DAABiasDyn * structPtr);
    int UpdateDAABiasDyn(const DAABiasDyn* structPtr, const char *where);
    int DeleteDAABiasDyn(const char *where);
    int UpdateDAABiasDynByRecord (const DAABiasDyn * newStructPtr, const DAABiasDyn * oldStructPtr);
    int InsertOrUpdateDAABiasDyn(const DAABiasDyn * structPtr);
    int InsertIfUniqueDAABiasDyn(const DAABiasDyn * structPtr, bool *isUnique);
    bool DAABiasDynExists(const DAABiasDyn * structPtr);
    int DeleteDAABiasDynByRecord(const DAABiasDyn * structPtr);
    void GetDAABiasDynPrimaryKeyWhereString (const DAABiasDyn * structPtr, char returnWhereString[] );
    void FreeDAABiasDyn(DAABiasDyn * structPtr);
    DbStatus * GetDAABiasDynDbStatus();
    void SetDAABiasDynErrorLogging(int value);
#endif

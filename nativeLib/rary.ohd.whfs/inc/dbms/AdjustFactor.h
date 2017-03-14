/*
    File: AdjustFactor.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef AdjustFactor_h
#define AdjustFactor_h


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



typedef struct _AdjustFactor
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    double		divisor;
    double		base;
    double		multiplier;
    double		adder;
    List		list;
} AdjustFactor;
/*
    Function Prototypes
*/
    AdjustFactor* GetAdjustFactor(const char * where);
    AdjustFactor* SelectAdjustFactor(const char * where);
    int SelectAdjustFactorCount(const char * where);
    int PutAdjustFactor(const AdjustFactor * structPtr);
    int InsertAdjustFactor(const AdjustFactor * structPtr);
    int UpdateAdjustFactor(const AdjustFactor* structPtr, const char *where);
    int DeleteAdjustFactor(const char *where);
    int UpdateAdjustFactorByRecord (const AdjustFactor * newStructPtr, const AdjustFactor * oldStructPtr);
    int InsertOrUpdateAdjustFactor(const AdjustFactor * structPtr);
    int InsertIfUniqueAdjustFactor(const AdjustFactor * structPtr, bool *isUnique);
    bool AdjustFactorExists(const AdjustFactor * structPtr);
    int DeleteAdjustFactorByRecord(const AdjustFactor * structPtr);
    void GetAdjustFactorPrimaryKeyWhereString (const AdjustFactor * structPtr, char returnWhereString[] );
    void FreeAdjustFactor(AdjustFactor * structPtr);
    DbStatus * GetAdjustFactorDbStatus();
    void SetAdjustFactorErrorLogging(int value);
#endif

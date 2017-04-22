/*
    File: SacSmaState.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef SacSmaState_h
#define SacSmaState_h


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



typedef struct _SacSmaState
{
    Node		node;
    char		basin_id[9];
    char		source[21];
    dtime_t		validtime;
    dtime_t		basistime;
    dtime_t		postingtime;
    double		uztwc;
    double		uzfwc;
    double		lztwc;
    double		lzfsc;
    double		lzfpc;
    double		adimc;
    List		list;
} SacSmaState;
/*
    Function Prototypes
*/
    SacSmaState* GetSacSmaState(const char * where);
    SacSmaState* SelectSacSmaState(const char * where);
    int SelectSacSmaStateCount(const char * where);
    int PutSacSmaState(const SacSmaState * structPtr);
    int InsertSacSmaState(const SacSmaState * structPtr);
    int UpdateSacSmaState(const SacSmaState* structPtr, const char *where);
    int DeleteSacSmaState(const char *where);
    int UpdateSacSmaStateByRecord (const SacSmaState * newStructPtr, const SacSmaState * oldStructPtr);
    int InsertOrUpdateSacSmaState(const SacSmaState * structPtr);
    int InsertIfUniqueSacSmaState(const SacSmaState * structPtr, bool *isUnique);
    bool SacSmaStateExists(const SacSmaState * structPtr);
    int DeleteSacSmaStateByRecord(const SacSmaState * structPtr);
    void GetSacSmaStatePrimaryKeyWhereString (const SacSmaState * structPtr, char returnWhereString[] );
    void FreeSacSmaState(SacSmaState * structPtr);
    DbStatus * GetSacSmaStateDbStatus();
    void SetSacSmaStateErrorLogging(int value);
#endif

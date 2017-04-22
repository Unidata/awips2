/*
    File: SacSmaParams.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef SacSmaParams_h
#define SacSmaParams_h


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



typedef struct _SacSmaParams
{
    Node		node;
    char		basin_id[9];
    char		source[21];
    dtime_t		validtime;
    dtime_t		postingtime;
    double		uztwm;
    double		uzfwm;
    double		uzk;
    double		pctim;
    double		adimp;
    double		riva;
    double		zperc;
    double		rexp;
    double		lztwm;
    double		lzfsm;
    double		lzfpm;
    double		lzsk;
    double		lzpk;
    double		pfree;
    double		rserv;
    double		side;
    double		peadj;
    double		pxadj;
    double		efc;
    List		list;
} SacSmaParams;
/*
    Function Prototypes
*/
    SacSmaParams* GetSacSmaParams(const char * where);
    SacSmaParams* SelectSacSmaParams(const char * where);
    int SelectSacSmaParamsCount(const char * where);
    int PutSacSmaParams(const SacSmaParams * structPtr);
    int InsertSacSmaParams(const SacSmaParams * structPtr);
    int UpdateSacSmaParams(const SacSmaParams* structPtr, const char *where);
    int DeleteSacSmaParams(const char *where);
    int UpdateSacSmaParamsByRecord (const SacSmaParams * newStructPtr, const SacSmaParams * oldStructPtr);
    int InsertOrUpdateSacSmaParams(const SacSmaParams * structPtr);
    int InsertIfUniqueSacSmaParams(const SacSmaParams * structPtr, bool *isUnique);
    bool SacSmaParamsExists(const SacSmaParams * structPtr);
    int DeleteSacSmaParamsByRecord(const SacSmaParams * structPtr);
    void GetSacSmaParamsPrimaryKeyWhereString (const SacSmaParams * structPtr, char returnWhereString[] );
    void FreeSacSmaParams(SacSmaParams * structPtr);
    DbStatus * GetSacSmaParamsDbStatus();
    void SetSacSmaParamsErrorLogging(int value);
#endif

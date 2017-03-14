/*
    File: Riverstat.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Riverstat_h
#define Riverstat_h


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



typedef struct _Riverstat
{
    Node		node;
    char		lid[9];
    char		primary_pe[3];
    double		bf;
    double		cb;
    double		da;
    double		response_time;
    double		threshold_runoff;
    double		fq;
    double		fs;
    char		gsno[11];
    char		level[21];
    double		mile;
    double		pool;
    char		por[31];
    char		rated[21];
    double		lat;
    double		lon;
    char		remark[256];
    date_t		rrevise;
    char		rsource[21];
    char		stream[33];
    char		tide[9];
    char		backwater[9];
    char		vdatum[21];
    double		action_flow;
    double		wstg;
    double		zd;
    date_t		ratedat;
    char		usgs_ratenum[6];
    long		uhgdur;
    char		use_latest_fcst[2];
    List		list;
} Riverstat;
/*
    Function Prototypes
*/
    Riverstat* GetRiverstat(const char * where);
    Riverstat* SelectRiverstat(const char * where);
    int SelectRiverstatCount(const char * where);
    int PutRiverstat(const Riverstat * structPtr);
    int InsertRiverstat(const Riverstat * structPtr);
    int UpdateRiverstat(const Riverstat* structPtr, const char *where);
    int DeleteRiverstat(const char *where);
    int UpdateRiverstatByRecord (const Riverstat * newStructPtr, const Riverstat * oldStructPtr);
    int InsertOrUpdateRiverstat(const Riverstat * structPtr);
    int InsertIfUniqueRiverstat(const Riverstat * structPtr, bool *isUnique);
    bool RiverstatExists(const Riverstat * structPtr);
    int DeleteRiverstatByRecord(const Riverstat * structPtr);
    void GetRiverstatPrimaryKeyWhereString (const Riverstat * structPtr, char returnWhereString[] );
    void FreeRiverstat(Riverstat * structPtr);
    DbStatus * GetRiverstatDbStatus();
    void SetRiverstatErrorLogging(int value);
#endif

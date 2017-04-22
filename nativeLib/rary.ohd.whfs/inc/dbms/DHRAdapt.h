/*
    File: DHRAdapt.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DHRAdapt_h
#define DHRAdapt_h


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



typedef struct _DHRAdapt
{
    Node		node;
    char		radid[4];
    dtime_t		obstime;
    float		min_reflth;
    float		max_reflth;
    float		ref_tltest;
    float		rng_tltin;
    float		rng_tltout;
    float		max_birng;
    float		min_birng;
    float		min_echoar;
    float		min_awrefl;
    float		max_pctred;
    float		mlt_zrcoef;
    float		pwr_zrcoef;
    float		min_zrefl;
    float		max_zrefl;
    float		max_stmspd;
    float		max_timdif;
    float		min_artcon;
    float		tim_p1cont;
    float		tim_p2cont;
    float		max_ecarch;
    float		rng_cutoff;
    float		rng_e1coef;
    float		rng_e2coef;
    float		rng_e3coef;
    float		min_prate;
    float		max_prate;
    float		tim_restrt;
    float		max_timint;
    float		min_timprd;
    float		thr_hlyout;
    float		end_timgag;
    float		max_prdval;
    float		max_hlyval;
    float		tim_biest;
    float		thr_nosets;
    float		res_bias;
    float		longest_lag;
    char		bias_applied[2];
    List		list;
} DHRAdapt;
/*
    Function Prototypes
*/
    DHRAdapt* GetDHRAdapt(const char * where);
    DHRAdapt* SelectDHRAdapt(const char * where);
    int SelectDHRAdaptCount(const char * where);
    int PutDHRAdapt(const DHRAdapt * structPtr);
    int InsertDHRAdapt(const DHRAdapt * structPtr);
    int UpdateDHRAdapt(const DHRAdapt* structPtr, const char *where);
    int DeleteDHRAdapt(const char *where);
    int UpdateDHRAdaptByRecord (const DHRAdapt * newStructPtr, const DHRAdapt * oldStructPtr);
    int InsertOrUpdateDHRAdapt(const DHRAdapt * structPtr);
    int InsertIfUniqueDHRAdapt(const DHRAdapt * structPtr, bool *isUnique);
    bool DHRAdaptExists(const DHRAdapt * structPtr);
    int DeleteDHRAdaptByRecord(const DHRAdapt * structPtr);
    void GetDHRAdaptPrimaryKeyWhereString (const DHRAdapt * structPtr, char returnWhereString[] );
    void FreeDHRAdapt(DHRAdapt * structPtr);
    DbStatus * GetDHRAdaptDbStatus();
    void SetDHRAdaptErrorLogging(int value);
#endif

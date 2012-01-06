/*
    File: DSPAdapt.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DSPAdapt_h
#define DSPAdapt_h


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



typedef struct _DSPAdapt
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
} DSPAdapt;
/*
    Function Prototypes
*/
    DSPAdapt* GetDSPAdapt(const char * where);
    DSPAdapt* SelectDSPAdapt(const char * where);
    int SelectDSPAdaptCount(const char * where);
    int PutDSPAdapt(const DSPAdapt * structPtr);
    int InsertDSPAdapt(const DSPAdapt * structPtr);
    int UpdateDSPAdapt(const DSPAdapt* structPtr, const char *where);
    int DeleteDSPAdapt(const char *where);
    int UpdateDSPAdaptByRecord (const DSPAdapt * newStructPtr, const DSPAdapt * oldStructPtr);
    int InsertOrUpdateDSPAdapt(const DSPAdapt * structPtr);
    int InsertIfUniqueDSPAdapt(const DSPAdapt * structPtr, bool *isUnique);
    bool DSPAdaptExists(const DSPAdapt * structPtr);
    int DeleteDSPAdaptByRecord(const DSPAdapt * structPtr);
    void GetDSPAdaptPrimaryKeyWhereString (const DSPAdapt * structPtr, char returnWhereString[] );
    void FreeDSPAdapt(DSPAdapt * structPtr);
    DbStatus * GetDSPAdaptDbStatus();
    void SetDSPAdaptErrorLogging(int value);
#endif

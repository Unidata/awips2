/*
    File: DpaAdapt.h
    Author  : CDBGEN
    Created : Thu Nov 08 10:55:29 EST 2007 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DpaAdapt_h
#define DpaAdapt_h


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



typedef struct _DpaAdapt
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
    float		beam_width;
    float		blockage_thresh;
    float		clutter_thresh;
    float		weight_thresh;
    float		hybrid_scan_thresh;
    float		low_reflect_thresh;
    float		detect_reflect_thr;
    float		detect_area_thresh;
    float		detect_time_thresh;
    float		exclusion_zones;
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
} DpaAdapt;
/*
    Function Prototypes
*/
    DpaAdapt* GetDpaAdapt(const char * where);
    DpaAdapt* SelectDpaAdapt(const char * where);
    int SelectDpaAdaptCount(const char * where);
    int PutDpaAdapt(const DpaAdapt * structPtr);
    int InsertDpaAdapt(const DpaAdapt * structPtr);
    int UpdateDpaAdapt(const DpaAdapt* structPtr, const char *where);
    int DeleteDpaAdapt(const char *where);
    int UpdateDpaAdaptByRecord (const DpaAdapt * newStructPtr, const DpaAdapt * oldStructPtr);
    int InsertOrUpdateDpaAdapt(const DpaAdapt * structPtr);
    int InsertIfUniqueDpaAdapt(const DpaAdapt * structPtr, bool *isUnique);
    bool DpaAdaptExists(const DpaAdapt * structPtr);
    int DeleteDpaAdaptByRecord(const DpaAdapt * structPtr);
    void GetDpaAdaptPrimaryKeyWhereString (const DpaAdapt * structPtr, char returnWhereString[] );
    void FreeDpaAdapt(DpaAdapt * structPtr);
    DbStatus * GetDpaAdaptDbStatus();
    void SetDpaAdaptErrorLogging(int value);
#endif

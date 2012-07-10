/*
    File: DPAAdapt.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DPAAdapt_h
#define DPAAdapt_h


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



typedef struct _DPAAdapt
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
} DPAAdapt;
/*
    Function Prototypes
*/
    DPAAdapt* GetDPAAdapt(const char * where);
    DPAAdapt* SelectDPAAdapt(const char * where);
    int SelectDPAAdaptCount(const char * where);
    int PutDPAAdapt(const DPAAdapt * structPtr);
    int InsertDPAAdapt(const DPAAdapt * structPtr);
    int UpdateDPAAdapt(const DPAAdapt* structPtr, const char *where);
    int DeleteDPAAdapt(const char *where);
    int UpdateDPAAdaptByRecord (const DPAAdapt * newStructPtr, const DPAAdapt * oldStructPtr);
    int InsertOrUpdateDPAAdapt(const DPAAdapt * structPtr);
    int InsertIfUniqueDPAAdapt(const DPAAdapt * structPtr, bool *isUnique);
    bool DPAAdaptExists(const DPAAdapt * structPtr);
    int DeleteDPAAdaptByRecord(const DPAAdapt * structPtr);
    void GetDPAAdaptPrimaryKeyWhereString (const DPAAdapt * structPtr, char returnWhereString[] );
    void FreeDPAAdapt(DPAAdapt * structPtr);
    DbStatus * GetDPAAdaptDbStatus();
    void SetDPAAdaptErrorLogging(int value);
#endif

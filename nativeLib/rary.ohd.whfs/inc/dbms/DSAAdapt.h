/*
    File: DSAAdapt.h
    Author  : CDBGEN
    Created : Mon Jul 29 08:10:26 EDT 2013 using database hd_ob9eempty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DSAAdapt_h
#define DSAAdapt_h


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



typedef struct _DSAAdapt
{
    Node		node;
    char		radid[4];
    dtime_t		obstime;
    short		num_of_adap;
    float		default_ml_depth;
    char		ml_overide_flag[9];
    float		kdp_mult;
    float		kdp_power;
    float		z_r_mult;
    float		z_r_power;
    float		zdr_z_mult;
    float		zdr_z_power;
    float		zdr_zdr_power;
    float		min_corr_precip;
    float		min_corr_kdp;
    float		refl_max;
    float		kdp_max_beam_blk;
    float		max_usability_blk;
    float		kdp_min_usage_rate;
    float		ws_mult;
    float		gr_mult;
    float		rh_mult;
    float		ds_mult;
    float		ic_mult;
    float		grid_is_full;
    float		paif_rate;
    float		paif_area;
    float		rain_time_thresh;
    float		num_zones;
    float		max_precip_rate;
    float		restart_time;
    float		max_interp_time;
    float		max_hourly_acc;
    float		time_bias;
    float		num_grpairs;
    float		reset_bias;
    float		longst_lag;
    List		list;
} DSAAdapt;
/*
    Function Prototypes
*/
    DSAAdapt* GetDSAAdapt(const char * where);
    DSAAdapt* SelectDSAAdapt(const char * where);
    int SelectDSAAdaptCount(const char * where);
    int PutDSAAdapt(const DSAAdapt * structPtr);
    int InsertDSAAdapt(const DSAAdapt * structPtr);
    int UpdateDSAAdapt(const DSAAdapt* structPtr, const char *where);
    int DeleteDSAAdapt(const char *where);
    int UpdateDSAAdaptByRecord (const DSAAdapt * newStructPtr, const DSAAdapt * oldStructPtr);
    int InsertOrUpdateDSAAdapt(const DSAAdapt * structPtr);
    int InsertIfUniqueDSAAdapt(const DSAAdapt * structPtr, bool *isUnique);
    bool DSAAdaptExists(const DSAAdapt * structPtr);
    int DeleteDSAAdaptByRecord(const DSAAdapt * structPtr);
    void GetDSAAdaptPrimaryKeyWhereString (const DSAAdapt * structPtr, char returnWhereString[] );
    void FreeDSAAdapt(DSAAdapt * structPtr);
    DbStatus * GetDSAAdaptDbStatus();
    void SetDSAAdaptErrorLogging(int value);
#endif

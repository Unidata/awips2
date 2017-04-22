/*
    File: RWParams.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RWParams_h
#define RWParams_h


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



typedef struct _RWParams
{
    Node		node;
    float		rw_min_rain;
    float		rw_sep_dist;
    float		rw_lag0_ind_corr;
    float		rw_lag0_cond_corr;
    short		num_near_gages;
    short		num_near_rad_bins;
    float		def_cond_var_rad;
    float		def_ind_corr_scl;
    float		def_cond_corr_scl;
    float		min_ind_corr_scl;
    float		min_cond_corr_scl;
    float		max_ind_corr_scl;
    float		max_cond_corr_scl;
    short		nn_srch_method;
    List		list;
} RWParams;
/*
    Function Prototypes
*/
    RWParams* GetRWParams(const char * where);
    RWParams* SelectRWParams(const char * where);
    int SelectRWParamsCount(const char * where);
    int PutRWParams(const RWParams * structPtr);
    int InsertRWParams(const RWParams * structPtr);
    int UpdateRWParams(const RWParams* structPtr, const char *where);
    int DeleteRWParams(const char *where);
    void FreeRWParams(RWParams * structPtr);
    DbStatus * GetRWParamsDbStatus();
    void SetRWParamsErrorLogging(int value);
#endif

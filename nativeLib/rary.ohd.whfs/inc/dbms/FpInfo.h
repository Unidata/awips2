// This is a view record !
/*
    File: FpInfo.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FpInfo_h
#define FpInfo_h


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



typedef struct _FpInfo
{
    Node		node;
    char		lid[9];
    char		name[51];
    char		county[21];
    char		state[3];
    char		hsa[4];
    char		primary_back[4];
    char		secondary_back[4];
    char		stream[33];
    double		bf;
    double		wstg;
    double		fs;
    double		fq;
    double		action_flow;
    char		pe[3];
    char		use_latest_fcst[2];
    char		proximity[7];
    char		reach[81];
    char		group_id[9];
    long		ordinal;
    double		chg_threshold;
    char		rec_type[4];
    long		backhrs;
    long		forwardhrs;
    double		adjustendhrs;
    double		minor_stage;
    double		moderate_stage;
    double		major_stage;
    double		minor_flow;
    double		moderate_flow;
    double		major_flow;
    List		list;
} FpInfo;
/*
    Function Prototypes
*/
    FpInfo* GetFpInfo(const char * where);
    FpInfo* SelectFpInfo(const char * where);
    int SelectFpInfoCount(const char * where);
    void FreeFpInfo(FpInfo * structPtr);
    DbStatus * GetFpInfoDbStatus();
    void SetFpInfoErrorLogging(int value);
#endif

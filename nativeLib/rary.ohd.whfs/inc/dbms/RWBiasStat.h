/*
    File: RWBiasStat.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RWBiasStat_h
#define RWBiasStat_h


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



typedef struct _RWBiasStat
{
    Node		node;
    char		office_id[6];
    float		min_gr_value_bias;
    long		npair_bias_select;
    long		npair_svar_update;
    long		std_cut;
    long		lag_cut;
    long		init_span;
    long		bias_qc_opt;
    long		num_span;
    float		mem_span1;
    float		mem_span2;
    float		mem_span3;
    float		mem_span4;
    float		mem_span5;
    float		mem_span6;
    float		mem_span7;
    float		mem_span8;
    float		mem_span9;
    float		mem_span10;
    List		list;
} RWBiasStat;
/*
    Function Prototypes
*/
    RWBiasStat* GetRWBiasStat(const char * where);
    RWBiasStat* SelectRWBiasStat(const char * where);
    int SelectRWBiasStatCount(const char * where);
    int PutRWBiasStat(const RWBiasStat * structPtr);
    int InsertRWBiasStat(const RWBiasStat * structPtr);
    int UpdateRWBiasStat(const RWBiasStat* structPtr, const char *where);
    int DeleteRWBiasStat(const char *where);
    int UpdateRWBiasStatByRecord (const RWBiasStat * newStructPtr, const RWBiasStat * oldStructPtr);
    int InsertOrUpdateRWBiasStat(const RWBiasStat * structPtr);
    int InsertIfUniqueRWBiasStat(const RWBiasStat * structPtr, bool *isUnique);
    bool RWBiasStatExists(const RWBiasStat * structPtr);
    int DeleteRWBiasStatByRecord(const RWBiasStat * structPtr);
    void GetRWBiasStatPrimaryKeyWhereString (const RWBiasStat * structPtr, char returnWhereString[] );
    void FreeRWBiasStat(RWBiasStat * structPtr);
    DbStatus * GetRWBiasStatDbStatus();
    void SetRWBiasStatErrorLogging(int value);
#endif

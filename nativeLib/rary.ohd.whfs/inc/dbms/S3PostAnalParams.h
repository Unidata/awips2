/*
    File: S3PostAnalParams.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef S3PostAnalParams_h
#define S3PostAnalParams_h


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



typedef struct _S3PostAnalParams
{
    Node		node;
    short		gg_weighting;
    float		gg_min_gage_val;
    short		gg_min_dist;
    float		kernel_est_scale;
    float		rhat;
    List		list;
} S3PostAnalParams;
/*
    Function Prototypes
*/
    S3PostAnalParams* GetS3PostAnalParams(const char * where);
    S3PostAnalParams* SelectS3PostAnalParams(const char * where);
    int SelectS3PostAnalParamsCount(const char * where);
    int PutS3PostAnalParams(const S3PostAnalParams * structPtr);
    int InsertS3PostAnalParams(const S3PostAnalParams * structPtr);
    int UpdateS3PostAnalParams(const S3PostAnalParams* structPtr, const char *where);
    int DeleteS3PostAnalParams(const char *where);
    void FreeS3PostAnalParams(S3PostAnalParams * structPtr);
    DbStatus * GetS3PostAnalParamsDbStatus();
    void SetS3PostAnalParamsErrorLogging(int value);
#endif

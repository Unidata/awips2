/*
    File: RpfParams.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RpfParams_h
#define RpfParams_h


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



typedef struct _RpfParams
{
    Node		node;
    long		obshrs;
    long		fcsthrs;
    char		missval[13];
    char		misscat[13];
    char		misstim[13];
    long		rvsexphrs;
    long		flsexphrs;
    long		flwexphrs;
    List		list;
} RpfParams;
/*
    Function Prototypes
*/
    RpfParams* GetRpfParams(const char * where);
    RpfParams* SelectRpfParams(const char * where);
    int SelectRpfParamsCount(const char * where);
    int PutRpfParams(const RpfParams * structPtr);
    int InsertRpfParams(const RpfParams * structPtr);
    int UpdateRpfParams(const RpfParams* structPtr, const char *where);
    int DeleteRpfParams(const char *where);
    void FreeRpfParams(RpfParams * structPtr);
    DbStatus * GetRpfParamsDbStatus();
    void SetRpfParamsErrorLogging(int value);
#endif

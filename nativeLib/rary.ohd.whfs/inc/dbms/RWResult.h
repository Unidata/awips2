/*
    File: RWResult.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RWResult_h
#define RWResult_h


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



typedef struct _RWResult
{
    Node		node;
    char		rfc[9];
    dtime_t		obstime;
    short		num_gag_avail;
    long		num_rad_avail;
    long		num_pseudo_gages;
    char		sat_avail[2];
    char		mapx_field_type[11];
    char		draw_precip[2];
    char		auto_save[2];
    dtime_t		last_exec_time;
    dtime_t		last_save_time;
    List		list;
} RWResult;
/*
    Function Prototypes
*/
    RWResult* GetRWResult(const char * where);
    RWResult* SelectRWResult(const char * where);
    int SelectRWResultCount(const char * where);
    int PutRWResult(const RWResult * structPtr);
    int InsertRWResult(const RWResult * structPtr);
    int UpdateRWResult(const RWResult* structPtr, const char *where);
    int DeleteRWResult(const char *where);
    int UpdateRWResultByRecord (const RWResult * newStructPtr, const RWResult * oldStructPtr);
    int InsertOrUpdateRWResult(const RWResult * structPtr);
    int InsertIfUniqueRWResult(const RWResult * structPtr, bool *isUnique);
    bool RWResultExists(const RWResult * structPtr);
    int DeleteRWResultByRecord(const RWResult * structPtr);
    void GetRWResultPrimaryKeyWhereString (const RWResult * structPtr, char returnWhereString[] );
    void FreeRWResult(RWResult * structPtr);
    DbStatus * GetRWResultDbStatus();
    void SetRWResultErrorLogging(int value);
#endif

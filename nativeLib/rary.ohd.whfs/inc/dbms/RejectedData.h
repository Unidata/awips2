/*
    File: RejectedData.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RejectedData_h
#define RejectedData_h


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



typedef struct _RejectedData
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    float		probability;
    dtime_t		validtime;
    dtime_t		basistime;
    dtime_t		postingtime;
    double		value;
    short		revision;
    char		shef_qual_code[2];
    char		product_id[11];
    dtime_t		producttime;
    long		quality_code;
    char		reject_type[2];
    char		userid[33];
    List		list;
} RejectedData;
/*
    Function Prototypes
*/
    RejectedData* GetRejectedData(const char * where);
    RejectedData* SelectRejectedData(const char * where);
    int SelectRejectedDataCount(const char * where);
    int PutRejectedData(const RejectedData * structPtr);
    int InsertRejectedData(const RejectedData * structPtr);
    int UpdateRejectedData(const RejectedData* structPtr, const char *where);
    int DeleteRejectedData(const char *where);
    int UpdateRejectedDataByRecord (const RejectedData * newStructPtr, const RejectedData * oldStructPtr);
    int InsertOrUpdateRejectedData(const RejectedData * structPtr);
    int InsertIfUniqueRejectedData(const RejectedData * structPtr, bool *isUnique);
    bool RejectedDataExists(const RejectedData * structPtr);
    int DeleteRejectedDataByRecord(const RejectedData * structPtr);
    void GetRejectedDataPrimaryKeyWhereString (const RejectedData * structPtr, char returnWhereString[] );
    void FreeRejectedData(RejectedData * structPtr);
    DbStatus * GetRejectedDataDbStatus();
    void SetRejectedDataErrorLogging(int value);
#endif

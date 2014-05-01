/*
    File: FcstPrecip.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstPrecip_h
#define FcstPrecip_h


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



typedef struct _FcstPrecip
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
    double		value;
    char		shef_qual_code[2];
    long		quality_code;
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} FcstPrecip;
/*
    Function Prototypes
*/
    FcstPrecip* GetFcstPrecip(const char * where);
    FcstPrecip* SelectFcstPrecip(const char * where);
    int SelectFcstPrecipCount(const char * where);
    int PutFcstPrecip(const FcstPrecip * structPtr);
    int InsertFcstPrecip(const FcstPrecip * structPtr);
    int UpdateFcstPrecip(const FcstPrecip* structPtr, const char *where);
    int DeleteFcstPrecip(const char *where);
    int UpdateFcstPrecipByRecord (const FcstPrecip * newStructPtr, const FcstPrecip * oldStructPtr);
    int InsertOrUpdateFcstPrecip(const FcstPrecip * structPtr);
    int InsertIfUniqueFcstPrecip(const FcstPrecip * structPtr, bool *isUnique);
    bool FcstPrecipExists(const FcstPrecip * structPtr);
    int DeleteFcstPrecipByRecord(const FcstPrecip * structPtr);
    void GetFcstPrecipPrimaryKeyWhereString (const FcstPrecip * structPtr, char returnWhereString[] );
    void FreeFcstPrecip(FcstPrecip * structPtr);
    DbStatus * GetFcstPrecipDbStatus();
    void SetFcstPrecipErrorLogging(int value);
#endif

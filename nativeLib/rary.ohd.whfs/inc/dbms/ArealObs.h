/*
    File: ArealObs.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ArealObs_h
#define ArealObs_h


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



typedef struct _ArealObs
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    dtime_t		obstime;
    double		value;
    char		shef_qual_code[2];
    long		quality_code;
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} ArealObs;
/*
    Function Prototypes
*/
    ArealObs* GetArealObs(const char * where);
    ArealObs* SelectArealObs(const char * where);
    int SelectArealObsCount(const char * where);
    int PutArealObs(const ArealObs * structPtr);
    int InsertArealObs(const ArealObs * structPtr);
    int UpdateArealObs(const ArealObs* structPtr, const char *where);
    int DeleteArealObs(const char *where);
    int UpdateArealObsByRecord (const ArealObs * newStructPtr, const ArealObs * oldStructPtr);
    int InsertOrUpdateArealObs(const ArealObs * structPtr);
    int InsertIfUniqueArealObs(const ArealObs * structPtr, bool *isUnique);
    bool ArealObsExists(const ArealObs * structPtr);
    int DeleteArealObsByRecord(const ArealObs * structPtr);
    void GetArealObsPrimaryKeyWhereString (const ArealObs * structPtr, char returnWhereString[] );
    void FreeArealObs(ArealObs * structPtr);
    DbStatus * GetArealObsDbStatus();
    void SetArealObsErrorLogging(int value);
#endif

/*
    File: YUnique.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef YUnique_h
#define YUnique_h


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



typedef struct _YUnique
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
} YUnique;
/*
    Function Prototypes
*/
    YUnique* GetYUnique(const char * where);
    YUnique* SelectYUnique(const char * where);
    int SelectYUniqueCount(const char * where);
    int PutYUnique(const YUnique * structPtr);
    int InsertYUnique(const YUnique * structPtr);
    int UpdateYUnique(const YUnique* structPtr, const char *where);
    int DeleteYUnique(const char *where);
    int UpdateYUniqueByRecord (const YUnique * newStructPtr, const YUnique * oldStructPtr);
    int InsertOrUpdateYUnique(const YUnique * structPtr);
    int InsertIfUniqueYUnique(const YUnique * structPtr, bool *isUnique);
    bool YUniqueExists(const YUnique * structPtr);
    int DeleteYUniqueByRecord(const YUnique * structPtr);
    void GetYUniquePrimaryKeyWhereString (const YUnique * structPtr, char returnWhereString[] );
    void FreeYUnique(YUnique * structPtr);
    DbStatus * GetYUniqueDbStatus();
    void SetYUniqueErrorLogging(int value);
#endif

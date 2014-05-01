/*
    File: Height.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Height_h
#define Height_h


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



typedef struct _Height
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
} Height;
/*
    Function Prototypes
*/
    Height* GetHeight(const char * where);
    Height* SelectHeight(const char * where);
    int SelectHeightCount(const char * where);
    int PutHeight(const Height * structPtr);
    int InsertHeight(const Height * structPtr);
    int UpdateHeight(const Height* structPtr, const char *where);
    int DeleteHeight(const char *where);
    int UpdateHeightByRecord (const Height * newStructPtr, const Height * oldStructPtr);
    int InsertOrUpdateHeight(const Height * structPtr);
    int InsertIfUniqueHeight(const Height * structPtr, bool *isUnique);
    bool HeightExists(const Height * structPtr);
    int DeleteHeightByRecord(const Height * structPtr);
    void GetHeightPrimaryKeyWhereString (const Height * structPtr, char returnWhereString[] );
    void FreeHeight(Height * structPtr);
    DbStatus * GetHeightDbStatus();
    void SetHeightErrorLogging(int value);
#endif

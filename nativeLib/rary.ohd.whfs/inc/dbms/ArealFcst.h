/*
    File: ArealFcst.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ArealFcst_h
#define ArealFcst_h


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



typedef struct _ArealFcst
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
} ArealFcst;
/*
    Function Prototypes
*/
    ArealFcst* GetArealFcst(const char * where);
    ArealFcst* SelectArealFcst(const char * where);
    int SelectArealFcstCount(const char * where);
    int PutArealFcst(const ArealFcst * structPtr);
    int InsertArealFcst(const ArealFcst * structPtr);
    int UpdateArealFcst(const ArealFcst* structPtr, const char *where);
    int DeleteArealFcst(const char *where);
    int UpdateArealFcstByRecord (const ArealFcst * newStructPtr, const ArealFcst * oldStructPtr);
    int InsertOrUpdateArealFcst(const ArealFcst * structPtr);
    int InsertIfUniqueArealFcst(const ArealFcst * structPtr, bool *isUnique);
    bool ArealFcstExists(const ArealFcst * structPtr);
    int DeleteArealFcstByRecord(const ArealFcst * structPtr);
    void GetArealFcstPrimaryKeyWhereString (const ArealFcst * structPtr, char returnWhereString[] );
    void FreeArealFcst(ArealFcst * structPtr);
    DbStatus * GetArealFcstDbStatus();
    void SetArealFcstErrorLogging(int value);
#endif

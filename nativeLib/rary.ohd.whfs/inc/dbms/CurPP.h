/*
    File: CurPP.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef CurPP_h
#define CurPP_h


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



typedef struct _CurPP
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
} CurPP;
/*
    Function Prototypes
*/
    CurPP* GetCurPP(const char * where);
    CurPP* SelectCurPP(const char * where);
    int SelectCurPPCount(const char * where);
    int PutCurPP(const CurPP * structPtr);
    int InsertCurPP(const CurPP * structPtr);
    int UpdateCurPP(const CurPP* structPtr, const char *where);
    int DeleteCurPP(const char *where);
    int UpdateCurPPByRecord (const CurPP * newStructPtr, const CurPP * oldStructPtr);
    int InsertOrUpdateCurPP(const CurPP * structPtr);
    int InsertIfUniqueCurPP(const CurPP * structPtr, bool *isUnique);
    bool CurPPExists(const CurPP * structPtr);
    int DeleteCurPPByRecord(const CurPP * structPtr);
    void GetCurPPPrimaryKeyWhereString (const CurPP * structPtr, char returnWhereString[] );
    void FreeCurPP(CurPP * structPtr);
    DbStatus * GetCurPPDbStatus();
    void SetCurPPErrorLogging(int value);
#endif

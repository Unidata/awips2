/*
    File: CurPC.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef CurPC_h
#define CurPC_h


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



typedef struct _CurPC
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
} CurPC;
/*
    Function Prototypes
*/
    CurPC* GetCurPC(const char * where);
    CurPC* SelectCurPC(const char * where);
    int SelectCurPCCount(const char * where);
    int PutCurPC(const CurPC * structPtr);
    int InsertCurPC(const CurPC * structPtr);
    int UpdateCurPC(const CurPC* structPtr, const char *where);
    int DeleteCurPC(const char *where);
    int UpdateCurPCByRecord (const CurPC * newStructPtr, const CurPC * oldStructPtr);
    int InsertOrUpdateCurPC(const CurPC * structPtr);
    int InsertIfUniqueCurPC(const CurPC * structPtr, bool *isUnique);
    bool CurPCExists(const CurPC * structPtr);
    int DeleteCurPCByRecord(const CurPC * structPtr);
    void GetCurPCPrimaryKeyWhereString (const CurPC * structPtr, char returnWhereString[] );
    void FreeCurPC(CurPC * structPtr);
    DbStatus * GetCurPCDbStatus();
    void SetCurPCErrorLogging(int value);
#endif

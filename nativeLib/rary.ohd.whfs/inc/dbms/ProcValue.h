/*
    File: ProcValue.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ProcValue_h
#define ProcValue_h


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



typedef struct _ProcValue
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
} ProcValue;
/*
    Function Prototypes
*/
    ProcValue* GetProcValue(const char * where);
    ProcValue* SelectProcValue(const char * where);
    int SelectProcValueCount(const char * where);
    int PutProcValue(const ProcValue * structPtr);
    int InsertProcValue(const ProcValue * structPtr);
    int UpdateProcValue(const ProcValue* structPtr, const char *where);
    int DeleteProcValue(const char *where);
    int UpdateProcValueByRecord (const ProcValue * newStructPtr, const ProcValue * oldStructPtr);
    int InsertOrUpdateProcValue(const ProcValue * structPtr);
    int InsertIfUniqueProcValue(const ProcValue * structPtr, bool *isUnique);
    bool ProcValueExists(const ProcValue * structPtr);
    int DeleteProcValueByRecord(const ProcValue * structPtr);
    void GetProcValuePrimaryKeyWhereString (const ProcValue * structPtr, char returnWhereString[] );
    void FreeProcValue(ProcValue * structPtr);
    DbStatus * GetProcValueDbStatus();
    void SetProcValueErrorLogging(int value);
#endif

/*
    File: PairedValue.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef PairedValue_h
#define PairedValue_h


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



typedef struct _PairedValue
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
    long		ref_value;
    double		value;
    char		shef_qual_code[2];
    long		quality_code;
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} PairedValue;
/*
    Function Prototypes
*/
    PairedValue* GetPairedValue(const char * where);
    PairedValue* SelectPairedValue(const char * where);
    int SelectPairedValueCount(const char * where);
    int PutPairedValue(const PairedValue * structPtr);
    int InsertPairedValue(const PairedValue * structPtr);
    int UpdatePairedValue(const PairedValue* structPtr, const char *where);
    int DeletePairedValue(const char *where);
    int UpdatePairedValueByRecord (const PairedValue * newStructPtr, const PairedValue * oldStructPtr);
    int InsertOrUpdatePairedValue(const PairedValue * structPtr);
    int InsertIfUniquePairedValue(const PairedValue * structPtr, bool *isUnique);
    bool PairedValueExists(const PairedValue * structPtr);
    int DeletePairedValueByRecord(const PairedValue * structPtr);
    void GetPairedValuePrimaryKeyWhereString (const PairedValue * structPtr, char returnWhereString[] );
    void FreePairedValue(PairedValue * structPtr);
    DbStatus * GetPairedValueDbStatus();
    void SetPairedValueErrorLogging(int value);
#endif

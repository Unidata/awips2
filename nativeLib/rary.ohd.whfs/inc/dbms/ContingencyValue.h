/*
    File: ContingencyValue.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ContingencyValue_h
#define ContingencyValue_h


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



typedef struct _ContingencyValue
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
} ContingencyValue;
/*
    Function Prototypes
*/
    ContingencyValue* GetContingencyValue(const char * where);
    ContingencyValue* SelectContingencyValue(const char * where);
    int SelectContingencyValueCount(const char * where);
    int PutContingencyValue(const ContingencyValue * structPtr);
    int InsertContingencyValue(const ContingencyValue * structPtr);
    int UpdateContingencyValue(const ContingencyValue* structPtr, const char *where);
    int DeleteContingencyValue(const char *where);
    int UpdateContingencyValueByRecord (const ContingencyValue * newStructPtr, const ContingencyValue * oldStructPtr);
    int InsertOrUpdateContingencyValue(const ContingencyValue * structPtr);
    int InsertIfUniqueContingencyValue(const ContingencyValue * structPtr, bool *isUnique);
    bool ContingencyValueExists(const ContingencyValue * structPtr);
    int DeleteContingencyValueByRecord(const ContingencyValue * structPtr);
    void GetContingencyValuePrimaryKeyWhereString (const ContingencyValue * structPtr, char returnWhereString[] );
    void FreeContingencyValue(ContingencyValue * structPtr);
    DbStatus * GetContingencyValueDbStatus();
    void SetContingencyValueErrorLogging(int value);
#endif

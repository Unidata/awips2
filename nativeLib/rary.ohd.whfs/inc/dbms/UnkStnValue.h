/*
    File: UnkStnValue.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef UnkStnValue_h
#define UnkStnValue_h


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



typedef struct _UnkStnValue
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    dtime_t		obstime;
    double		value;
    short		revision;
    char		shef_qual_code[2];
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} UnkStnValue;
/*
    Function Prototypes
*/
    UnkStnValue* GetUnkStnValue(const char * where);
    UnkStnValue* SelectUnkStnValue(const char * where);
    int SelectUnkStnValueCount(const char * where);
    int PutUnkStnValue(const UnkStnValue * structPtr);
    int InsertUnkStnValue(const UnkStnValue * structPtr);
    int UpdateUnkStnValue(const UnkStnValue* structPtr, const char *where);
    int DeleteUnkStnValue(const char *where);
    int UpdateUnkStnValueByRecord (const UnkStnValue * newStructPtr, const UnkStnValue * oldStructPtr);
    int InsertOrUpdateUnkStnValue(const UnkStnValue * structPtr);
    int InsertIfUniqueUnkStnValue(const UnkStnValue * structPtr, bool *isUnique);
    bool UnkStnValueExists(const UnkStnValue * structPtr);
    int DeleteUnkStnValueByRecord(const UnkStnValue * structPtr);
    void GetUnkStnValuePrimaryKeyWhereString (const UnkStnValue * structPtr, char returnWhereString[] );
    void FreeUnkStnValue(UnkStnValue * structPtr);
    DbStatus * GetUnkStnValueDbStatus();
    void SetUnkStnValueErrorLogging(int value);
#endif

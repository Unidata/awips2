/*
    File: Crest.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Crest_h
#define Crest_h


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



typedef struct _Crest
{
    Node		node;
    char		lid[9];
    date_t		datcrst;
    char		cremark[81];
    char		hw[2];
    char		jam[2];
    char		olddatum[2];
    long		q;
    double		stage;
    char		suppress[2];
    char		timcrst[6];
    char		prelim[2];
    List		list;
} Crest;
/*
    Function Prototypes
*/
    Crest* GetCrest(const char * where);
    Crest* SelectCrest(const char * where);
    int SelectCrestCount(const char * where);
    int PutCrest(const Crest * structPtr);
    int InsertCrest(const Crest * structPtr);
    int UpdateCrest(const Crest* structPtr, const char *where);
    int DeleteCrest(const char *where);
    int UpdateCrestByRecord (const Crest * newStructPtr, const Crest * oldStructPtr);
    int InsertOrUpdateCrest(const Crest * structPtr);
    int InsertIfUniqueCrest(const Crest * structPtr, bool *isUnique);
    bool CrestExists(const Crest * structPtr);
    int DeleteCrestByRecord(const Crest * structPtr);
    void GetCrestPrimaryKeyWhereString (const Crest * structPtr, char returnWhereString[] );
    void FreeCrest(Crest * structPtr);
    DbStatus * GetCrestDbStatus();
    void SetCrestErrorLogging(int value);
#endif

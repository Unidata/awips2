/*
    File: SDBOut.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:47 EDT 2008 using database dc_ob7empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef SDBOut_h
#define SDBOut_h


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



typedef struct _SDBOut
{
    Node		node;
    char		nidid[11];
    char		src[4];
    char		scenario[3];
    char		down_name[26];
    double		slope;
    double		max_flow;
    double		max_depth;
    double		time_max_depth;
    double		time_flood;
    double		time_deflood;
    char		comments[31];
    dtime_t		updated;
    List		list;
} SDBOut;
/*
    Function Prototypes
*/
    SDBOut* GetSDBOut(const char * where);
    SDBOut* SelectSDBOut(const char * where);
    int SelectSDBOutCount(const char * where);
    int PutSDBOut(const SDBOut * structPtr);
    int InsertSDBOut(const SDBOut * structPtr);
    int UpdateSDBOut(const SDBOut* structPtr, const char *where);
    int DeleteSDBOut(const char *where);
    int UpdateSDBOutByRecord (const SDBOut * newStructPtr, const SDBOut * oldStructPtr);
    int InsertOrUpdateSDBOut(const SDBOut * structPtr);
    int InsertIfUniqueSDBOut(const SDBOut * structPtr, bool *isUnique);
    bool SDBOutExists(const SDBOut * structPtr);
    int DeleteSDBOutByRecord(const SDBOut * structPtr);
    void GetSDBOutPrimaryKeyWhereString (const SDBOut * structPtr, char returnWhereString[] );
    void FreeSDBOut(SDBOut * structPtr);
    DbStatus * GetSDBOutDbStatus();
    void SetSDBOutErrorLogging(int value);
#endif

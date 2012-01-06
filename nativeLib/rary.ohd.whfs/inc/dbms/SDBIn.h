/*
    File: SDBIn.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:47 EDT 2008 using database dc_ob7empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef SDBIn_h
#define SDBIn_h


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



typedef struct _SDBIn
{
    Node		node;
    char		nidid[11];
    char		src[4];
    char		scenario[3];
    double		hde;
    double		bme;
    double		vol;
    double		sa;
    double		tfm;
    double		qo;
    double		bw;
    long		idam;
    char		comments[31];
    dtime_t		updated;
    List		list;
} SDBIn;
/*
    Function Prototypes
*/
    SDBIn* GetSDBIn(const char * where);
    SDBIn* SelectSDBIn(const char * where);
    int SelectSDBInCount(const char * where);
    int PutSDBIn(const SDBIn * structPtr);
    int InsertSDBIn(const SDBIn * structPtr);
    int UpdateSDBIn(const SDBIn* structPtr, const char *where);
    int DeleteSDBIn(const char *where);
    int UpdateSDBInByRecord (const SDBIn * newStructPtr, const SDBIn * oldStructPtr);
    int InsertOrUpdateSDBIn(const SDBIn * structPtr);
    int InsertIfUniqueSDBIn(const SDBIn * structPtr, bool *isUnique);
    bool SDBInExists(const SDBIn * structPtr);
    int DeleteSDBInByRecord(const SDBIn * structPtr);
    void GetSDBInPrimaryKeyWhereString (const SDBIn * structPtr, char returnWhereString[] );
    void FreeSDBIn(SDBIn * structPtr);
    DbStatus * GetSDBInDbStatus();
    void SetSDBInErrorLogging(int value);
#endif

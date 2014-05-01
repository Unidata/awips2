/*
    File: LWstmt.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LWstmt_h
#define LWstmt_h


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



typedef struct _LWstmt
{
    Node		node;
    char		lid[9];
    char		pe[3];
    double		lower_value;
    double		upper_value;
    long		criteria_rank;
    char		statement[513];
    char		lw_criteria[513];
    char		lw_source[513];
    List		list;
} LWstmt;
/*
    Function Prototypes
*/
    LWstmt* GetLWstmt(const char * where);
    LWstmt* SelectLWstmt(const char * where);
    int SelectLWstmtCount(const char * where);
    int PutLWstmt(const LWstmt * structPtr);
    int InsertLWstmt(const LWstmt * structPtr);
    int UpdateLWstmt(const LWstmt* structPtr, const char *where);
    int DeleteLWstmt(const char *where);
    int UpdateLWstmtByRecord (const LWstmt * newStructPtr, const LWstmt * oldStructPtr);
    int InsertOrUpdateLWstmt(const LWstmt * structPtr);
    int InsertIfUniqueLWstmt(const LWstmt * structPtr, bool *isUnique);
    bool LWstmtExists(const LWstmt * structPtr);
    int DeleteLWstmtByRecord(const LWstmt * structPtr);
    void GetLWstmtPrimaryKeyWhereString (const LWstmt * structPtr, char returnWhereString[] );
    void FreeLWstmt(LWstmt * structPtr);
    DbStatus * GetLWstmtDbStatus();
    void SetLWstmtErrorLogging(int value);
#endif

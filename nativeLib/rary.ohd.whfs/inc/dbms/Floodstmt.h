/*
    File: Floodstmt.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Floodstmt_h
#define Floodstmt_h


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



typedef struct _Floodstmt
{
    Node		node;
    char		lid[9];
    double		impact_value;
    char		statement[513];
    char		rf[2];
    char		datestart[6];
    char		dateend[6];
    char		impact_pe[3];
    List		list;
} Floodstmt;
/*
    Function Prototypes
*/
    Floodstmt* GetFloodstmt(const char * where);
    Floodstmt* SelectFloodstmt(const char * where);
    int SelectFloodstmtCount(const char * where);
    int PutFloodstmt(const Floodstmt * structPtr);
    int InsertFloodstmt(const Floodstmt * structPtr);
    int UpdateFloodstmt(const Floodstmt* structPtr, const char *where);
    int DeleteFloodstmt(const char *where);
    int UpdateFloodstmtByRecord (const Floodstmt * newStructPtr, const Floodstmt * oldStructPtr);
    int InsertOrUpdateFloodstmt(const Floodstmt * structPtr);
    int InsertIfUniqueFloodstmt(const Floodstmt * structPtr, bool *isUnique);
    bool FloodstmtExists(const Floodstmt * structPtr);
    int DeleteFloodstmtByRecord(const Floodstmt * structPtr);
    void GetFloodstmtPrimaryKeyWhereString (const Floodstmt * structPtr, char returnWhereString[] );
    void FreeFloodstmt(Floodstmt * structPtr);
    DbStatus * GetFloodstmtDbStatus();
    void SetFloodstmtErrorLogging(int value);
#endif

/*
    File: Dcp.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Dcp_h
#define Dcp_h


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



typedef struct _Dcp
{
    Node		node;
    char		lid[9];
    char		criteria[51];
    char		owner[11];
    char		goes[9];
    char		rptfreq[5];
    char		rptime[9];
    char		notify[2];
    List		list;
} Dcp;
/*
    Function Prototypes
*/
    Dcp* GetDcp(const char * where);
    Dcp* SelectDcp(const char * where);
    int SelectDcpCount(const char * where);
    int PutDcp(const Dcp * structPtr);
    int InsertDcp(const Dcp * structPtr);
    int UpdateDcp(const Dcp* structPtr, const char *where);
    int DeleteDcp(const char *where);
    int UpdateDcpByRecord (const Dcp * newStructPtr, const Dcp * oldStructPtr);
    int InsertOrUpdateDcp(const Dcp * structPtr);
    int InsertIfUniqueDcp(const Dcp * structPtr, bool *isUnique);
    bool DcpExists(const Dcp * structPtr);
    int DeleteDcpByRecord(const Dcp * structPtr);
    void GetDcpPrimaryKeyWhereString (const Dcp * structPtr, char returnWhereString[] );
    void FreeDcp(Dcp * structPtr);
    DbStatus * GetDcpDbStatus();
    void SetDcpErrorLogging(int value);
#endif

/*
    File: Rfc.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Rfc_h
#define Rfc_h


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



typedef struct _Rfc
{
    Node		node;
    char		rfc[6];
    List		list;
} Rfc;
/*
    Function Prototypes
*/
    Rfc* GetRfc(const char * where);
    Rfc* SelectRfc(const char * where);
    int SelectRfcCount(const char * where);
    int PutRfc(const Rfc * structPtr);
    int InsertRfc(const Rfc * structPtr);
    int UpdateRfc(const Rfc* structPtr, const char *where);
    int DeleteRfc(const char *where);
    int UpdateRfcByRecord (const Rfc * newStructPtr, const Rfc * oldStructPtr);
    int InsertOrUpdateRfc(const Rfc * structPtr);
    int InsertIfUniqueRfc(const Rfc * structPtr, bool *isUnique);
    bool RfcExists(const Rfc * structPtr);
    int DeleteRfcByRecord(const Rfc * structPtr);
    void GetRfcPrimaryKeyWhereString (const Rfc * structPtr, char returnWhereString[] );
    void FreeRfc(Rfc * structPtr);
    DbStatus * GetRfcDbStatus();
    void SetRfcErrorLogging(int value);
#endif

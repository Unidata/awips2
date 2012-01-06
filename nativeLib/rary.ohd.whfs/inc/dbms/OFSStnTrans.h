/*
    File: OFSStnTrans.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef OFSStnTrans_h
#define OFSStnTrans_h


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



typedef struct _OFSStnTrans
{
    Node		node;
    char		lid[9];
    char		ofs_data_type[5];
    char		shef_source_code[2];
    char		ofs_lid[9];
    List		list;
} OFSStnTrans;
/*
    Function Prototypes
*/
    OFSStnTrans* GetOFSStnTrans(const char * where);
    OFSStnTrans* SelectOFSStnTrans(const char * where);
    int SelectOFSStnTransCount(const char * where);
    int PutOFSStnTrans(const OFSStnTrans * structPtr);
    int InsertOFSStnTrans(const OFSStnTrans * structPtr);
    int UpdateOFSStnTrans(const OFSStnTrans* structPtr, const char *where);
    int DeleteOFSStnTrans(const char *where);
    int UpdateOFSStnTransByRecord (const OFSStnTrans * newStructPtr, const OFSStnTrans * oldStructPtr);
    int InsertOrUpdateOFSStnTrans(const OFSStnTrans * structPtr);
    int InsertIfUniqueOFSStnTrans(const OFSStnTrans * structPtr, bool *isUnique);
    bool OFSStnTransExists(const OFSStnTrans * structPtr);
    int DeleteOFSStnTransByRecord(const OFSStnTrans * structPtr);
    void GetOFSStnTransPrimaryKeyWhereString (const OFSStnTrans * structPtr, char returnWhereString[] );
    void FreeOFSStnTrans(OFSStnTrans * structPtr);
    DbStatus * GetOFSStnTransDbStatus();
    void SetOFSStnTransErrorLogging(int value);
#endif

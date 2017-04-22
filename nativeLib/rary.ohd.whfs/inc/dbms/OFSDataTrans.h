/*
    File: OFSDataTrans.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef OFSDataTrans_h
#define OFSDataTrans_h


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



typedef struct _OFSDataTrans
{
    Node		node;
    char		pe[3];
    short		dur;
    char		extremum[2];
    char		ofs_data_type[5];
    float		fwd_time_window;
    float		bkw_time_window;
    List		list;
} OFSDataTrans;
/*
    Function Prototypes
*/
    OFSDataTrans* GetOFSDataTrans(const char * where);
    OFSDataTrans* SelectOFSDataTrans(const char * where);
    int SelectOFSDataTransCount(const char * where);
    int PutOFSDataTrans(const OFSDataTrans * structPtr);
    int InsertOFSDataTrans(const OFSDataTrans * structPtr);
    int UpdateOFSDataTrans(const OFSDataTrans* structPtr, const char *where);
    int DeleteOFSDataTrans(const char *where);
    int UpdateOFSDataTransByRecord (const OFSDataTrans * newStructPtr, const OFSDataTrans * oldStructPtr);
    int InsertOrUpdateOFSDataTrans(const OFSDataTrans * structPtr);
    int InsertIfUniqueOFSDataTrans(const OFSDataTrans * structPtr, bool *isUnique);
    bool OFSDataTransExists(const OFSDataTrans * structPtr);
    int DeleteOFSDataTransByRecord(const OFSDataTrans * structPtr);
    void GetOFSDataTransPrimaryKeyWhereString (const OFSDataTrans * structPtr, char returnWhereString[] );
    void FreeOFSDataTrans(OFSDataTrans * structPtr);
    DbStatus * GetOFSDataTransDbStatus();
    void SetOFSDataTransErrorLogging(int value);
#endif

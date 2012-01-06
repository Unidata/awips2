/*
    File: ShefPETrans.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ShefPETrans_h
#define ShefPETrans_h


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



typedef struct _ShefPETrans
{
    Node		node;
    char		pe[4];
    long		coded_value;
    char		value_trans[81];
    List		list;
} ShefPETrans;
/*
    Function Prototypes
*/
    ShefPETrans* GetShefPETrans(const char * where);
    ShefPETrans* SelectShefPETrans(const char * where);
    int SelectShefPETransCount(const char * where);
    int PutShefPETrans(const ShefPETrans * structPtr);
    int InsertShefPETrans(const ShefPETrans * structPtr);
    int UpdateShefPETrans(const ShefPETrans* structPtr, const char *where);
    int DeleteShefPETrans(const char *where);
    int UpdateShefPETransByRecord (const ShefPETrans * newStructPtr, const ShefPETrans * oldStructPtr);
    int InsertOrUpdateShefPETrans(const ShefPETrans * structPtr);
    int InsertIfUniqueShefPETrans(const ShefPETrans * structPtr, bool *isUnique);
    bool ShefPETransExists(const ShefPETrans * structPtr);
    int DeleteShefPETransByRecord(const ShefPETrans * structPtr);
    void GetShefPETransPrimaryKeyWhereString (const ShefPETrans * structPtr, char returnWhereString[] );
    void FreeShefPETrans(ShefPETrans * structPtr);
    DbStatus * GetShefPETransDbStatus();
    void SetShefPETransErrorLogging(int value);
#endif

/*
    File: StnClass.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef StnClass_h
#define StnClass_h


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



typedef struct _StnClass
{
    Node		node;
    char		lid[9];
    char		disp_class[11];
    char		dcp[2];
    char		observer[2];
    char		telem_type[11];
    List		list;
} StnClass;
/*
    Function Prototypes
*/
    StnClass* GetStnClass(const char * where);
    StnClass* SelectStnClass(const char * where);
    int SelectStnClassCount(const char * where);
    int PutStnClass(const StnClass * structPtr);
    int InsertStnClass(const StnClass * structPtr);
    int UpdateStnClass(const StnClass* structPtr, const char *where);
    int DeleteStnClass(const char *where);
    int UpdateStnClassByRecord (const StnClass * newStructPtr, const StnClass * oldStructPtr);
    int InsertOrUpdateStnClass(const StnClass * structPtr);
    int InsertIfUniqueStnClass(const StnClass * structPtr, bool *isUnique);
    bool StnClassExists(const StnClass * structPtr);
    int DeleteStnClassByRecord(const StnClass * structPtr);
    void GetStnClassPrimaryKeyWhereString (const StnClass * structPtr, char returnWhereString[] );
    void FreeStnClass(StnClass * structPtr);
    DbStatus * GetStnClassDbStatus();
    void SetStnClassErrorLogging(int value);
#endif

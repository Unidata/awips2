/*
    File: Lowwater.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Lowwater_h
#define Lowwater_h


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



typedef struct _Lowwater
{
    Node		node;
    char		lid[9];
    date_t		lwdat;
    long		q;
    char		lwrem[81];
    double		stage;
    List		list;
} Lowwater;
/*
    Function Prototypes
*/
    Lowwater* GetLowwater(const char * where);
    Lowwater* SelectLowwater(const char * where);
    int SelectLowwaterCount(const char * where);
    int PutLowwater(const Lowwater * structPtr);
    int InsertLowwater(const Lowwater * structPtr);
    int UpdateLowwater(const Lowwater* structPtr, const char *where);
    int DeleteLowwater(const char *where);
    int UpdateLowwaterByRecord (const Lowwater * newStructPtr, const Lowwater * oldStructPtr);
    int InsertOrUpdateLowwater(const Lowwater * structPtr);
    int InsertIfUniqueLowwater(const Lowwater * structPtr, bool *isUnique);
    bool LowwaterExists(const Lowwater * structPtr);
    int DeleteLowwaterByRecord(const Lowwater * structPtr);
    void GetLowwaterPrimaryKeyWhereString (const Lowwater * structPtr, char returnWhereString[] );
    void FreeLowwater(Lowwater * structPtr);
    DbStatus * GetLowwaterDbStatus();
    void SetLowwaterErrorLogging(int value);
#endif

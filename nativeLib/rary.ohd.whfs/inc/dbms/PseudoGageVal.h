/*
    File: PseudoGageVal.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef PseudoGageVal_h
#define PseudoGageVal_h


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



typedef struct _PseudoGageVal
{
    Node		node;
    char		pseudo_gage_id[9];
    dtime_t		obstime;
    double		lat;
    double		lon;
    float		gage_value;
    char		man_edited[2];
    float		prev_gage_value;
    List		list;
} PseudoGageVal;
/*
    Function Prototypes
*/
    PseudoGageVal* GetPseudoGageVal(const char * where);
    PseudoGageVal* SelectPseudoGageVal(const char * where);
    int SelectPseudoGageValCount(const char * where);
    int PutPseudoGageVal(const PseudoGageVal * structPtr);
    int InsertPseudoGageVal(const PseudoGageVal * structPtr);
    int UpdatePseudoGageVal(const PseudoGageVal* structPtr, const char *where);
    int DeletePseudoGageVal(const char *where);
    int UpdatePseudoGageValByRecord (const PseudoGageVal * newStructPtr, const PseudoGageVal * oldStructPtr);
    int InsertOrUpdatePseudoGageVal(const PseudoGageVal * structPtr);
    int InsertIfUniquePseudoGageVal(const PseudoGageVal * structPtr, bool *isUnique);
    bool PseudoGageValExists(const PseudoGageVal * structPtr);
    int DeletePseudoGageValByRecord(const PseudoGageVal * structPtr);
    void GetPseudoGageValPrimaryKeyWhereString (const PseudoGageVal * structPtr, char returnWhereString[] );
    void FreePseudoGageVal(PseudoGageVal * structPtr);
    DbStatus * GetPseudoGageValDbStatus();
    void SetPseudoGageValErrorLogging(int value);
#endif

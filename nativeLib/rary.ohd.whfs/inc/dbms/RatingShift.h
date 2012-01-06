/*
    File: RatingShift.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RatingShift_h
#define RatingShift_h


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



typedef struct _RatingShift
{
    Node		node;
    char		lid[9];
    date_t		date;
    double		shift_amount;
    char		active[2];
    List		list;
} RatingShift;
/*
    Function Prototypes
*/
    RatingShift* GetRatingShift(const char * where);
    RatingShift* SelectRatingShift(const char * where);
    int SelectRatingShiftCount(const char * where);
    int PutRatingShift(const RatingShift * structPtr);
    int InsertRatingShift(const RatingShift * structPtr);
    int UpdateRatingShift(const RatingShift* structPtr, const char *where);
    int DeleteRatingShift(const char *where);
    int UpdateRatingShiftByRecord (const RatingShift * newStructPtr, const RatingShift * oldStructPtr);
    int InsertOrUpdateRatingShift(const RatingShift * structPtr);
    int InsertIfUniqueRatingShift(const RatingShift * structPtr, bool *isUnique);
    bool RatingShiftExists(const RatingShift * structPtr);
    int DeleteRatingShiftByRecord(const RatingShift * structPtr);
    void GetRatingShiftPrimaryKeyWhereString (const RatingShift * structPtr, char returnWhereString[] );
    void FreeRatingShift(RatingShift * structPtr);
    DbStatus * GetRatingShiftDbStatus();
    void SetRatingShiftErrorLogging(int value);
#endif

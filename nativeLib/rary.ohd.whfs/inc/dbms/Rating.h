/*
    File: Rating.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Rating_h
#define Rating_h


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



typedef struct _Rating
{
    Node		node;
    char		lid[9];
    double		stage;
    double		discharge;
    List		list;
} Rating;
/*
    Function Prototypes
*/
    Rating* GetRating(const char * where);
    Rating* SelectRating(const char * where);
    int SelectRatingCount(const char * where);
    int PutRating(const Rating * structPtr);
    int InsertRating(const Rating * structPtr);
    int UpdateRating(const Rating* structPtr, const char *where);
    int DeleteRating(const char *where);
    int UpdateRatingByRecord (const Rating * newStructPtr, const Rating * oldStructPtr);
    int InsertOrUpdateRating(const Rating * structPtr);
    int InsertIfUniqueRating(const Rating * structPtr, bool *isUnique);
    bool RatingExists(const Rating * structPtr);
    int DeleteRatingByRecord(const Rating * structPtr);
    void GetRatingPrimaryKeyWhereString (const Rating * structPtr, char returnWhereString[] );
    void FreeRating(Rating * structPtr);
    DbStatus * GetRatingDbStatus();
    void SetRatingErrorLogging(int value);
#endif

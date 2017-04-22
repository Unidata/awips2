/*
    File: CommentValue.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef CommentValue_h
#define CommentValue_h


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



typedef struct _CommentValue
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    float		probability;
    dtime_t		validtime;
    dtime_t		basistime;
    double		value;
    char		shef_qual_code[2];
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    char		shef_comment[81];
    List		list;
} CommentValue;
/*
    Function Prototypes
*/
    CommentValue* GetCommentValue(const char * where);
    CommentValue* SelectCommentValue(const char * where);
    int SelectCommentValueCount(const char * where);
    int PutCommentValue(const CommentValue * structPtr);
    int InsertCommentValue(const CommentValue * structPtr);
    int UpdateCommentValue(const CommentValue* structPtr, const char *where);
    int DeleteCommentValue(const char *where);
    int UpdateCommentValueByRecord (const CommentValue * newStructPtr, const CommentValue * oldStructPtr);
    int InsertOrUpdateCommentValue(const CommentValue * structPtr);
    int InsertIfUniqueCommentValue(const CommentValue * structPtr, bool *isUnique);
    bool CommentValueExists(const CommentValue * structPtr);
    int DeleteCommentValueByRecord(const CommentValue * structPtr);
    void GetCommentValuePrimaryKeyWhereString (const CommentValue * structPtr, char returnWhereString[] );
    void FreeCommentValue(CommentValue * structPtr);
    DbStatus * GetCommentValueDbStatus();
    void SetCommentValueErrorLogging(int value);
#endif

/*
    File: PostProcessor.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef PostProcessor_h
#define PostProcessor_h


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



typedef struct _PostProcessor
{
    Node		node;
    char		post_processor[31];
    List		list;
} PostProcessor;
/*
    Function Prototypes
*/
    PostProcessor* GetPostProcessor(const char * where);
    PostProcessor* SelectPostProcessor(const char * where);
    int SelectPostProcessorCount(const char * where);
    int PutPostProcessor(const PostProcessor * structPtr);
    int InsertPostProcessor(const PostProcessor * structPtr);
    int UpdatePostProcessor(const PostProcessor* structPtr, const char *where);
    int DeletePostProcessor(const char *where);
    int UpdatePostProcessorByRecord (const PostProcessor * newStructPtr, const PostProcessor * oldStructPtr);
    int InsertOrUpdatePostProcessor(const PostProcessor * structPtr);
    int InsertIfUniquePostProcessor(const PostProcessor * structPtr, bool *isUnique);
    bool PostProcessorExists(const PostProcessor * structPtr);
    int DeletePostProcessorByRecord(const PostProcessor * structPtr);
    void GetPostProcessorPrimaryKeyWhereString (const PostProcessor * structPtr, char returnWhereString[] );
    void FreePostProcessor(PostProcessor * structPtr);
    DbStatus * GetPostProcessorDbStatus();
    void SetPostProcessorErrorLogging(int value);
#endif

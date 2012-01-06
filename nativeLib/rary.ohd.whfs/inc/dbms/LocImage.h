/*
    File: LocImage.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LocImage_h
#define LocImage_h


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



typedef struct _LocImage
{
    Node		node;
    char		lid[9];
    char		imageid[11];
    char		title[31];
    char		descr[81];
    char		format[11];
    char		url_internal[121];
    char		url_external[121];
    List		list;
} LocImage;
/*
    Function Prototypes
*/
    LocImage* GetLocImage(const char * where);
    LocImage* SelectLocImage(const char * where);
    int SelectLocImageCount(const char * where);
    int PutLocImage(const LocImage * structPtr);
    int InsertLocImage(const LocImage * structPtr);
    int UpdateLocImage(const LocImage* structPtr, const char *where);
    int DeleteLocImage(const char *where);
    int UpdateLocImageByRecord (const LocImage * newStructPtr, const LocImage * oldStructPtr);
    int InsertOrUpdateLocImage(const LocImage * structPtr);
    int InsertIfUniqueLocImage(const LocImage * structPtr, bool *isUnique);
    bool LocImageExists(const LocImage * structPtr);
    int DeleteLocImageByRecord(const LocImage * structPtr);
    void GetLocImagePrimaryKeyWhereString (const LocImage * structPtr, char returnWhereString[] );
    void FreeLocImage(LocImage * structPtr);
    DbStatus * GetLocImageDbStatus();
    void SetLocImageErrorLogging(int value);
#endif

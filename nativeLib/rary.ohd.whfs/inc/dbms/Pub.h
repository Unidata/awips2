/*
    File: Pub.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Pub_h
#define Pub_h


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



typedef struct _Pub
{
    Node		node;
    char		lid[9];
    date_t		pbegin;
    char		ppub[26];
    date_t		pend;
    List		list;
} Pub;
/*
    Function Prototypes
*/
    Pub* GetPub(const char * where);
    Pub* SelectPub(const char * where);
    int SelectPubCount(const char * where);
    int PutPub(const Pub * structPtr);
    int InsertPub(const Pub * structPtr);
    int UpdatePub(const Pub* structPtr, const char *where);
    int DeletePub(const char *where);
    int UpdatePubByRecord (const Pub * newStructPtr, const Pub * oldStructPtr);
    int InsertOrUpdatePub(const Pub * structPtr);
    int InsertIfUniquePub(const Pub * structPtr, bool *isUnique);
    bool PubExists(const Pub * structPtr);
    int DeletePubByRecord(const Pub * structPtr);
    void GetPubPrimaryKeyWhereString (const Pub * structPtr, char returnWhereString[] );
    void FreePub(Pub * structPtr);
    DbStatus * GetPubDbStatus();
    void SetPubErrorLogging(int value);
#endif

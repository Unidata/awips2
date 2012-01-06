/*
    File: OfficeNotes.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef OfficeNotes_h
#define OfficeNotes_h


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



typedef struct _OfficeNotes
{
    Node		node;
    char		topic[9];
    char		id[9];
    dtime_t		datatime;
    dtime_t		postingtime;
    dtime_t		updatetime;
    dtime_t		expiretime;
    char		note[513];
    List		list;
} OfficeNotes;
/*
    Function Prototypes
*/
    OfficeNotes* GetOfficeNotes(const char * where);
    OfficeNotes* SelectOfficeNotes(const char * where);
    int SelectOfficeNotesCount(const char * where);
    int PutOfficeNotes(const OfficeNotes * structPtr);
    int InsertOfficeNotes(const OfficeNotes * structPtr);
    int UpdateOfficeNotes(const OfficeNotes* structPtr, const char *where);
    int DeleteOfficeNotes(const char *where);
    int UpdateOfficeNotesByRecord (const OfficeNotes * newStructPtr, const OfficeNotes * oldStructPtr);
    int InsertOrUpdateOfficeNotes(const OfficeNotes * structPtr);
    int InsertIfUniqueOfficeNotes(const OfficeNotes * structPtr, bool *isUnique);
    bool OfficeNotesExists(const OfficeNotes * structPtr);
    int DeleteOfficeNotesByRecord(const OfficeNotes * structPtr);
    void GetOfficeNotesPrimaryKeyWhereString (const OfficeNotes * structPtr, char returnWhereString[] );
    void FreeOfficeNotes(OfficeNotes * structPtr);
    DbStatus * GetOfficeNotesDbStatus();
    void SetOfficeNotesErrorLogging(int value);
#endif

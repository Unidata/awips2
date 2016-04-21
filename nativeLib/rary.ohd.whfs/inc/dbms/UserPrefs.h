/*
    File: UserPrefs.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef UserPrefs_h
#define UserPrefs_h


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



typedef struct _UserPrefs
{
    Node		node;
    char		userid[33];
    long		title;
    long		statlist;
    long		sortlist;
    long		fieldlist;
    List		list;
} UserPrefs;
/*
    Function Prototypes
*/
    UserPrefs* GetUserPrefs(const char * where);
    UserPrefs* SelectUserPrefs(const char * where);
    int SelectUserPrefsCount(const char * where);
    int PutUserPrefs(const UserPrefs * structPtr);
    int InsertUserPrefs(const UserPrefs * structPtr);
    int UpdateUserPrefs(const UserPrefs* structPtr, const char *where);
    int DeleteUserPrefs(const char *where);
    int UpdateUserPrefsByRecord (const UserPrefs * newStructPtr, const UserPrefs * oldStructPtr);
    int InsertOrUpdateUserPrefs(const UserPrefs * structPtr);
    int InsertIfUniqueUserPrefs(const UserPrefs * structPtr, bool *isUnique);
    bool UserPrefsExists(const UserPrefs * structPtr);
    int DeleteUserPrefsByRecord(const UserPrefs * structPtr);
    void GetUserPrefsPrimaryKeyWhereString (const UserPrefs * structPtr, char returnWhereString[] );
    void FreeUserPrefs(UserPrefs * structPtr);
    DbStatus * GetUserPrefsDbStatus();
    void SetUserPrefsErrorLogging(int value);
#endif

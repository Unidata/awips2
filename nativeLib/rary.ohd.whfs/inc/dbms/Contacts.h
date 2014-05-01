/*
    File: Contacts.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Contacts_h
#define Contacts_h


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



typedef struct _Contacts
{
    Node		node;
    char		lid[9];
    char		contact[29];
    char		phone[19];
    char		email[61];
    char		remark[256];
    long		priority;
    List		list;
} Contacts;
/*
    Function Prototypes
*/
    Contacts* GetContacts(const char * where);
    Contacts* SelectContacts(const char * where);
    int SelectContactsCount(const char * where);
    int PutContacts(const Contacts * structPtr);
    int InsertContacts(const Contacts * structPtr);
    int UpdateContacts(const Contacts* structPtr, const char *where);
    int DeleteContacts(const char *where);
    int UpdateContactsByRecord (const Contacts * newStructPtr, const Contacts * oldStructPtr);
    int InsertOrUpdateContacts(const Contacts * structPtr);
    int InsertIfUniqueContacts(const Contacts * structPtr, bool *isUnique);
    bool ContactsExists(const Contacts * structPtr);
    int DeleteContactsByRecord(const Contacts * structPtr);
    void GetContactsPrimaryKeyWhereString (const Contacts * structPtr, char returnWhereString[] );
    void FreeContacts(Contacts * structPtr);
    DbStatus * GetContactsDbStatus();
    void SetContactsErrorLogging(int value);
#endif

/*
    File: Admin.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Admin_h
#define Admin_h


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



typedef struct _Admin
{
    Node		node;
    char		focalpoint[25];
    char		ofc[21];
    char		phone[13];
    char		region[21];
    char		regno[2];
    char		cd404[9];
    date_t		tenyr;
    date_t		oneyr;
    char		hsa[6];
    short		hsa_num;
    char		hb_password[9];
    List		list;
} Admin;
/*
    Function Prototypes
*/
    Admin* GetAdmin(const char * where);
    Admin* SelectAdmin(const char * where);
    int SelectAdminCount(const char * where);
    int PutAdmin(const Admin * structPtr);
    int InsertAdmin(const Admin * structPtr);
    int UpdateAdmin(const Admin* structPtr, const char *where);
    int DeleteAdmin(const char *where);
    int UpdateAdminByRecord (const Admin * newStructPtr, const Admin * oldStructPtr);
    int InsertOrUpdateAdmin(const Admin * structPtr);
    int InsertIfUniqueAdmin(const Admin * structPtr, bool *isUnique);
    bool AdminExists(const Admin * structPtr);
    int DeleteAdminByRecord(const Admin * structPtr);
    void GetAdminPrimaryKeyWhereString (const Admin * structPtr, char returnWhereString[] );
    void FreeAdmin(Admin * structPtr);
    DbStatus * GetAdminDbStatus();
    void SetAdminErrorLogging(int value);
#endif

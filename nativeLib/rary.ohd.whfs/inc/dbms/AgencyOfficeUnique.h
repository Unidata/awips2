// This is a view record !
/*
    File: AgencyOfficeUnique.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef AgencyOfficeUnique_h
#define AgencyOfficeUnique_h


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



typedef struct _AgencyOfficeUnique
{
    Node		node;
    char		agency_code[9];
    char		office[21];
    List		list;
} AgencyOfficeUnique;
/*
    Function Prototypes
*/
    AgencyOfficeUnique* GetAgencyOfficeUnique(const char * where);
    AgencyOfficeUnique* SelectAgencyOfficeUnique(const char * where);
    int SelectAgencyOfficeUniqueCount(const char * where);
    void FreeAgencyOfficeUnique(AgencyOfficeUnique * structPtr);
    DbStatus * GetAgencyOfficeUniqueDbStatus();
    void SetAgencyOfficeUniqueErrorLogging(int value);
#endif

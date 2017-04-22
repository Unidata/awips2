/*
    File: DefiningIssueCriteria.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DefiningIssueCriteria_h
#define DefiningIssueCriteria_h


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



typedef struct _DefiningIssueCriteria
{
    Node		node;
    char		def_issue_crit[51];
    List		list;
} DefiningIssueCriteria;
/*
    Function Prototypes
*/
    DefiningIssueCriteria* GetDefiningIssueCriteria(const char * where);
    DefiningIssueCriteria* SelectDefiningIssueCriteria(const char * where);
    int SelectDefiningIssueCriteriaCount(const char * where);
    int PutDefiningIssueCriteria(const DefiningIssueCriteria * structPtr);
    int InsertDefiningIssueCriteria(const DefiningIssueCriteria * structPtr);
    int UpdateDefiningIssueCriteria(const DefiningIssueCriteria* structPtr, const char *where);
    int DeleteDefiningIssueCriteria(const char *where);
    int UpdateDefiningIssueCriteriaByRecord (const DefiningIssueCriteria * newStructPtr, const DefiningIssueCriteria * oldStructPtr);
    int InsertOrUpdateDefiningIssueCriteria(const DefiningIssueCriteria * structPtr);
    int InsertIfUniqueDefiningIssueCriteria(const DefiningIssueCriteria * structPtr, bool *isUnique);
    bool DefiningIssueCriteriaExists(const DefiningIssueCriteria * structPtr);
    int DeleteDefiningIssueCriteriaByRecord(const DefiningIssueCriteria * structPtr);
    void GetDefiningIssueCriteriaPrimaryKeyWhereString (const DefiningIssueCriteria * structPtr, char returnWhereString[] );
    void FreeDefiningIssueCriteria(DefiningIssueCriteria * structPtr);
    DbStatus * GetDefiningIssueCriteriaDbStatus();
    void SetDefiningIssueCriteriaErrorLogging(int value);
#endif

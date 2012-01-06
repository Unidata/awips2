/*
    File: Descrip.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Descrip_h
#define Descrip_h


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



typedef struct _Descrip
{
    Node		node;
    char		lid[9];
    char		bed[61];
    char		divert[61];
    char		remark[256];
    char		ice[161];
    char		proximity[7];
    char		reach[81];
    char		res[256];
    char		topo[256];
    List		list;
} Descrip;
/*
    Function Prototypes
*/
    Descrip* GetDescrip(const char * where);
    Descrip* SelectDescrip(const char * where);
    int SelectDescripCount(const char * where);
    int PutDescrip(const Descrip * structPtr);
    int InsertDescrip(const Descrip * structPtr);
    int UpdateDescrip(const Descrip* structPtr, const char *where);
    int DeleteDescrip(const char *where);
    int UpdateDescripByRecord (const Descrip * newStructPtr, const Descrip * oldStructPtr);
    int InsertOrUpdateDescrip(const Descrip * structPtr);
    int InsertIfUniqueDescrip(const Descrip * structPtr, bool *isUnique);
    bool DescripExists(const Descrip * structPtr);
    int DeleteDescripByRecord(const Descrip * structPtr);
    void GetDescripPrimaryKeyWhereString (const Descrip * structPtr, char returnWhereString[] );
    void FreeDescrip(Descrip * structPtr);
    DbStatus * GetDescripDbStatus();
    void SetDescripErrorLogging(int value);
#endif

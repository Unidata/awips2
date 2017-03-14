/*
    File: VTECsignif.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef VTECsignif_h
#define VTECsignif_h


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



typedef struct _VTECsignif
{
    Node		node;
    char		signif[2];
    char		name[26];
    List		list;
} VTECsignif;
/*
    Function Prototypes
*/
    VTECsignif* GetVTECsignif(const char * where);
    VTECsignif* SelectVTECsignif(const char * where);
    int SelectVTECsignifCount(const char * where);
    int PutVTECsignif(const VTECsignif * structPtr);
    int InsertVTECsignif(const VTECsignif * structPtr);
    int UpdateVTECsignif(const VTECsignif* structPtr, const char *where);
    int DeleteVTECsignif(const char *where);
    int UpdateVTECsignifByRecord (const VTECsignif * newStructPtr, const VTECsignif * oldStructPtr);
    int InsertOrUpdateVTECsignif(const VTECsignif * structPtr);
    int InsertIfUniqueVTECsignif(const VTECsignif * structPtr, bool *isUnique);
    bool VTECsignifExists(const VTECsignif * structPtr);
    int DeleteVTECsignifByRecord(const VTECsignif * structPtr);
    void GetVTECsignifPrimaryKeyWhereString (const VTECsignif * structPtr, char returnWhereString[] );
    void FreeVTECsignif(VTECsignif * structPtr);
    DbStatus * GetVTECsignifDbStatus();
    void SetVTECsignifErrorLogging(int value);
#endif

/*
    File: VTECaction.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef VTECaction_h
#define VTECaction_h


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



typedef struct _VTECaction
{
    Node		node;
    char		action[4];
    char		name[26];
    List		list;
} VTECaction;
/*
    Function Prototypes
*/
    VTECaction* GetVTECaction(const char * where);
    VTECaction* SelectVTECaction(const char * where);
    int SelectVTECactionCount(const char * where);
    int PutVTECaction(const VTECaction * structPtr);
    int InsertVTECaction(const VTECaction * structPtr);
    int UpdateVTECaction(const VTECaction* structPtr, const char *where);
    int DeleteVTECaction(const char *where);
    int UpdateVTECactionByRecord (const VTECaction * newStructPtr, const VTECaction * oldStructPtr);
    int InsertOrUpdateVTECaction(const VTECaction * structPtr);
    int InsertIfUniqueVTECaction(const VTECaction * structPtr, bool *isUnique);
    bool VTECactionExists(const VTECaction * structPtr);
    int DeleteVTECactionByRecord(const VTECaction * structPtr);
    void GetVTECactionPrimaryKeyWhereString (const VTECaction * structPtr, char returnWhereString[] );
    void FreeVTECaction(VTECaction * structPtr);
    DbStatus * GetVTECactionDbStatus();
    void SetVTECactionErrorLogging(int value);
#endif

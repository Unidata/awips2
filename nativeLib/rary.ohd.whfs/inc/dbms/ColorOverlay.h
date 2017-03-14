/*
    File: ColorOverlay.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ColorOverlay_h
#define ColorOverlay_h


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



typedef struct _ColorOverlay
{
    Node		node;
    char		userid[33];
    char		application_name[21];
    char		overlay_type[21];
    char		color_name[26];
    List		list;
} ColorOverlay;
/*
    Function Prototypes
*/
    ColorOverlay* GetColorOverlay(const char * where);
    ColorOverlay* SelectColorOverlay(const char * where);
    int SelectColorOverlayCount(const char * where);
    int PutColorOverlay(const ColorOverlay * structPtr);
    int InsertColorOverlay(const ColorOverlay * structPtr);
    int UpdateColorOverlay(const ColorOverlay* structPtr, const char *where);
    int DeleteColorOverlay(const char *where);
    int UpdateColorOverlayByRecord (const ColorOverlay * newStructPtr, const ColorOverlay * oldStructPtr);
    int InsertOrUpdateColorOverlay(const ColorOverlay * structPtr);
    int InsertIfUniqueColorOverlay(const ColorOverlay * structPtr, bool *isUnique);
    bool ColorOverlayExists(const ColorOverlay * structPtr);
    int DeleteColorOverlayByRecord(const ColorOverlay * structPtr);
    void GetColorOverlayPrimaryKeyWhereString (const ColorOverlay * structPtr, char returnWhereString[] );
    void FreeColorOverlay(ColorOverlay * structPtr);
    DbStatus * GetColorOverlayDbStatus();
    void SetColorOverlayErrorLogging(int value);
#endif

/*=========================================================================*/
/*                          FILE NAME:  set_coloroverlays.c                */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   set_coloroverlays                  */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdlib.h>
#include "post_stage3.h"
#include "post_drawa.h"
#include "ColorOverlay.h"

/****************************************************************/
/*  FUNCTION NAME:   set_coloroverlays                          */
/*       FUNCTION:   set colors for overlays                    */
/****************************************************************

Function type:
   void

Called by function:
   ReadParameters

******************************************** BEGIN set_coloroverlays *********/

void set_coloroverlays()
{

 char where[100],where1[100];
 int numcol;

 ColorOverlay *coHead,*coPtr;

 /*----------------------------------------------------------------*/
 /*  color_list_overlay[0]  -- state boundary                      */
 /*                    [1]  -- river                               */
 /*                    [2]  -- map basin boundary                  */
 /*                    [3]  -- forecast group basin boundary       */
 /*                    [4]  -- city location                       */
 /*                    [5]  -- precip gage location                */
 /*                    [6]  -- rfc boundary                        */
 /*                    [7]  -- county boundary                     */
 /*                    [8]  -- radar ring - radar available        */
 /*                    [9]  -- radar ring - not available      	   */	
 /*----------------------------------------------------------------*/

 color_list_overlays[0] = "white";
 color_list_overlays[1] = "SkyBlue";
 color_list_overlays[2] = "SpringGreen";
 color_list_overlays[3] = "SpringGreen";
 color_list_overlays[4] = "wheat";
 color_list_overlays[5] = "sandybrown";
 color_list_overlays[6] = "yellow";
 color_list_overlays[7] = "PaleTurquoise1";
 color_list_overlays[8] = "green";
 color_list_overlays[9] = "red";


 /*-----------------------------------------------*/
 /*  read overlay colors from ColorOverlay table  */
 /*-----------------------------------------------*/

 sprintf(where1,"WHERE userid = '%s' AND application_name= '%s'",LOGNAME,app_name);
 numcol = 0;

 sprintf(where,"%s AND overlay_type= 'STATE'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[0] = (char *) malloc(26*sizeof(char));
       strcpy(color_list_overlays[0],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

 sprintf(where,"%s AND overlay_type= 'RIVER'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[1] = (char *) malloc(26*sizeof(char));
       strcpy(color_list_overlays[1],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

 sprintf(where,"%s AND overlay_type= 'MAP_BASIN'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[2] = (char *) malloc(26*sizeof(char));
       strcpy(color_list_overlays[2],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

 sprintf(where,"%s AND overlay_type= 'FCSTGROUP_BASIN'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[3] = (char *) malloc(26*sizeof(char));
       strcpy(color_list_overlays[3],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

 sprintf(where,"%s AND overlay_type= 'CITY'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[4] = (char *) malloc(26*sizeof(char));
       strcpy(color_list_overlays[4],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

 sprintf(where,"%s AND overlay_type= 'PRECIP_STN'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[5] = (char *) malloc(26*sizeof(char));
       strcpy(color_list_overlays[5],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

 sprintf(where,"%s AND overlay_type= 'RFC_BOUNDARY'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[6] = (char *) malloc(26*sizeof(char));
       strcpy(color_list_overlays[6],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

 sprintf(where,"%s AND overlay_type= 'COUNTY'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[7] = (char *) malloc(26*sizeof(char));
       strcpy(color_list_overlays[7],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

/* -------------------------------------------------------------- */
/*
sprintf(where,"%s AND overlay_type= 'RADAR_RING_RADGAGE'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[8] = (char *) malloc(26*sizeof(char));
       strcpy(color_list_overlays[8],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }
 sprintf(where,"%s AND overlay_type= 'RADAR_RING_GAGEONLY'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[10] = (char *) malloc(26*sizeof(char));
       strcpy(color_list_overlays[10],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

 -------------------------------------------------------------- */

 sprintf(where,"%s AND overlay_type= 'RADAR_RING_AVAIL'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[8] = (char *) malloc(26*sizeof(char)); /* 9 */
       strcpy(color_list_overlays[8],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

 sprintf(where,"%s AND overlay_type= 'RADAR_RING_NOTAVAIL'",where1);

 if((coHead = GetColorOverlay(where)))
 {
    coPtr = (ColorOverlay*) ListFirst(&coHead->list);
    if(coPtr)
    {

       color_list_overlays[9] = (char *) malloc(26*sizeof(char));/*11 */
       strcpy(color_list_overlays[9],coPtr->color_name);
       numcol++;
    }

    FreeColorOverlay(coHead);

 }

 if(numcol == 0)
   printf("default overlay colors used\n");
 else
   printf("overlay colors read from database\n");


}

/********************************************* END set_coloroverlays *********/

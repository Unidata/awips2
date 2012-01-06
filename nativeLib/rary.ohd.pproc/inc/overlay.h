/*=========================================================================*/
/*                    FILE PATH/NAME:   st3_includes/overlay.h             */
/*                                                                         */
/*                              HEADER FILE                                */
/*                                                                         */
/*=========================================================================*/
#ifndef OVERLAY_H
#define OVERLAY_H

#include "stage3.h"

typedef struct         {
			char id[9];
                        char name[21];
			int order,npts;
			HRAP *hrap;
		       } overlay_struct;

overlay_struct  **fgbasin;
overlay_struct  **mapbasin;
overlay_struct  **river;
overlay_struct  **bound;
overlay_struct  **state;
overlay_struct  **county;

struct         {
   short state;
   short county;
   short river;
   short mapbasin;
   short fgbasin;
   short gridtocounty;
   short gridtobasin;
   short site_boundary;
	       } overlay_avail;

char         ***loc_basin;
char         ***loc_cty;
char         ***loc_area;
int             nummap;
int             numfg;
int             numrivers;
int             numrfc;
int             numstates;
int             numcounty;

#endif /* #ifndef OVERLAY_H */

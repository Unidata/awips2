/*=========================================================================*/
/*                              NAME:   overlay.h                          */
/*                                                                         */
/*                              HEADER FILE                                */
/*                                                                         */
/*=========================================================================*/
#include "hrap.h"

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
   short site_boundary;
   short state;
   short county;
   short river;
   short mapbasin;
   short fgbasin;
   short gridtocounty;
   short gridtobasin;
	       } overlay_avail;

overlay_struct   **read_overlay_data();

char         ***loc_basin;
char         ***loc_cty;
int             nummap;
int             numfg;
int             numrivers;
int             numrfc;
int             numstates;
int             numcounty;

/*  function prototypes  */

HRAP                    LatLongToHrap();
void                    show_states();
void                    show_county();
void                    show_cities_and_towns();
void                    show_rivers();
void                    show_basin_boundaries();
void                    show_radar_rings();

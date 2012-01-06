/*=========================================================================*/
/*                    FILE NAME:   save_gif.h                     */
/*                                                                         */
/*                              HEADER FILE                                */
/*                                                                         */
/*=========================================================================*/

typedef struct HRAP    {
			float x,y;
		       } HRAP;


/*-----------------------------------------*/
/*  structures used by mpefield_save_gif   */
/*-----------------------------------------*/

typedef struct _town
{
  char          name[25];
  HRAP          location;
} town_struct;

typedef struct _state
{
  char         name[25];
  HRAP         *hrap;
  int          npts;
} state_struct;

typedef struct _rfc
{
  char         id[9];
  HRAP         *hrap;
  int          npts;
} rfc_struct;  

typedef struct _overlay
{
  town_struct    town[50];
  state_struct   state[180];
  rfc_struct     rfc;
  int            ntowns,nstates;
      
} overlay_struct;

overlay_struct gdata;
overlay_struct overlays;

typedef struct _gui
{
 float    orig_x;
 float    orig_y;
 float    scalex, scaley;
 int      offset;
 int      zoom_factor;
 int      zoom_on;
 int      XOR;
 int      YOR;
 int      HRAPX;
 int      HRAPY;
 int      WIDTH;
 int      HEIGHT;
 
} gui_struct;


gui_struct gui;

/*-----------------------------------------------*/

int             get_apps_defaults();

/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "sharp95.h"

/* 
 * Global vars 
 * 
 * Let's go ahead and define all of our global vars here. The ones from
 * globals.h
 */

#define compiledate "------------- Last Modified: " __DATE__ " ------------"
short nobanner = 0;
float st_spd = -999.0, st_dir = -999.0;
float bd_spd = -999.0, bd_dir = -999.0;
float mucape = -999.0, mucin = -999.0;

/* 24 Mar 2008 */
float p_bot = -999.0, p_top = -999.0;

float user_level = 850.0;   /* default user-defined level */
float mml_layer  = 100.0;   /* mean-mixed layer */
float mu_layer   = 400.0;   /* most-unstable layer */

/* Titling junk */
char raobtitle[80], stn_abbrev[4], raob_type[80], mdl_type[80];
char raobtitle2[80], stn_abbrev2[4], raob_type2[80], mdl_type2[80];
char raobsavefilename[256], sars_filename[256], sup_filename[256]; //CHIN, change from 80 to 256

History hist;

struct _configure config;

#ifndef _WIN32
void main(int argc, char *argv[])
{
	int i;
	char st[80];

	printf("\n\n\n-----------------------------------------------------\n");
	printf(      "---------------------- SHARP v4.10 ------------------\n");
	printf(      "----John Hart----Rich Thompson----Gregg Grosshans----\n");
        printf(      "-------------Hail algorithms by Ryan Jewell----------\n");
	printf(      "--- National Centers for Environmental Prediction ---\n");
	printf(      "-------- Storm Prediction Center, Norman, OK --------\n");
	strcpy(st, compiledate);
	printf(      "%s\n", st);
	printf(      "-----------------------------------------------------\n");

	/* Default drawing mode */
	drawing_mode = DRAW_SKEWT;
        /* added 25OCT06 by RLT */
	display_mode_right = DISPLAY_STP_RIGHT;
        display_mode_left = DISPLAY_SARS_LEFT;	
	hodo_mode = HODO_NORMAL;
	auto_mode = 0;

	/* Read the data descriptions */
	i = readconfigfile();
	if (i) {
	  fprintf(stderr, 
	    "Could not read any nsharpconf configuration files. Stopping.\n");
	  exit(1);
	}

	read_nsharp_config();

	history_init(&hist, 2, NULL);

	nobanner=1;  /* Turn this crap off for now */

	/* Deal with command line */
	for (i=1;i<argc;i++) {	
          if(!strcmp(argv[i], "-nb")) {
             printf( "[NSHARP] - No Banner Option Detected.\n" );
             nobanner = 1;
          }
        
	  if (!strcmp(argv[i], "-winter")) {
            printf( "[NSHARP] - Winter Mode Selected.\n" ); 
            display_mode_left = DISPLAY_WINTER_LEFT;
          }
        
	  if (strstr(argv[i], "-file:")) {
	    strcpy(st, argv[i]+6);
            printf( "[NSHARP] - Input File found: %s\n", st); 
	    strcpy(autostart.filename, st);
          }
        
	  if (strstr(argv[i], "-stn:")) {
	    strcpy(st, argv[i]+5);
            printf( "[NSHARP] - Input Station found: %s\n", st); 
	    strcpy(autostart.station, st);
          }

	  if (strstr(argv[i], "-gif:")) {
	    strcpy(st, argv[i]+5);
            printf( "[NSHARP] - Output GIF filename found: %s\n", st); 
	    strcpy(autostart.giffile, st);
          }
        
	  if (strstr(argv[i], "-hour")) {
	    strcpy(st, argv[i]+6);
            printf( "[NSHARP] - Input Hour found: %s\n", st); 
	    strcpy(autostart.dattim, st);
          }
        
	  if (!strcmp(argv[i], "-auto")) {
            printf( "[NSHARP] - Automatic mode selected.\n" ); 
	    auto_mode = 1;
          }
	}

	/* Pass args so that X gets command line args */
	make_screen(argc, argv);
}
#endif

/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  DOS Video Graphics Routines (Part #4)                      */
/*  These routines handle sub-screen windows and option boxes. */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  GRAB_WINDOW                                                */
/*  PUT_WINDOW                                                 */
/*  PARCEL_OPTIONS                                             */
/*  GENERAL_OPTIONS                                            */
/*  RESET_OPTIONS                                              */
/*  PRINT_OPTIONS                                              */
/*                                                             */
/***************************************************************/

#define VIDEO
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <malloc.h>
#include <sharp95.h>
#include <xwcmn.h>


	/*NP*/
	void grab_window( short x1, short y1, short x2, short y2, char *buffer)
	/*************************************************************/
	/*  GRAB_WINDOW                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Saves a portion of graphic screen in memory for later    */
	/*  recall.  Used in the menuing and messaging system.       */
	/*************************************************************/
	{
	}


	/*NP*/
	void put_window( short x1, short y1, char *buffer)
	/*************************************************************/
	/*  PUT_WINDOW                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  places a previously saved portion of the screen back at  */
	/*  position (x1, y1).   Used for menuing and messaging.     */
	/*************************************************************/
	{
	/*_putimage( x1, y1, buffer, _GPSET );*/
	}


	/*NP*/
	void parcel_options( void )
	/*************************************************************/
	/*  PARCEL_OPTIONS                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Gives the user several options as to which parcel to     */
	/*  lift.                                                    */
	/*************************************************************/
	{
	}

	/*NP*/
	void general_options( void )
	/*************************************************************/
	/*  GENERAL_OPTIONS                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Gives the user total control of the SHARP environment.   */
	/*************************************************************/
	{
	}

	/*NP*/
	void reset_options( short mode, short pagenum )
	/*************************************************************/
	/*  RESET_OPTIONS                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Asks the user if they want to reload original sounding,  */
	/*  or just clear screen.                                    */
	/*************************************************************/
	{
	redraw_graph( mode );
        define_parcel( lplvals.flag, lplvals.pres );
        trace_parcel(lplvals.pres, lplvals.temp, lplvals.dwpt); 
	show_page( pagenum );
	return;
	}


	/*NP*/
	void print_options( void )
	/*************************************************************/
	/*  PRINT_OPTIONS                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Determines method of printout based on config file       */
	/*************************************************************/
	{
	}


	/*NP*/
	void Define_Print_Option( short hitnum)
	/*************************************************************/
	/*  DEFINE_PRINT_OPTION                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Initiate Printout based on user selection.               */
	/*************************************************************/
	{

	/* ----- Define New Default Option ----- */
	if(hitnum==1) strcpy(config.prntype, "DMP");
	if(hitnum==2) strcpy(config.prntype, "HP-PCL");
	if(hitnum==3) strcpy(config.prntype, "PCL5 HPGL");
	if(hitnum==4) strcpy(config.prntype, "GIF");
	if(hitnum==5) strcpy(config.prntype, "TEXT");
	if(hitnum>5) return;

	/* ----- Print using HPGL Language ----- */
	if (strstr(config.prntype, "PCL5 HPGL"))
	   {
	   /* print_sounding_hpgl(); */
	   print_sounding_ps(1);
	   return;
	   }

	/* ----- Print using PCL Raster Graphics ----- */
	if (strstr(config.prntype, "HP-PCL"))
	   {
	   print_sounding_pcl();
	   return;
	   }


	/* ----- Capture Screen to GIF Image ----- */
	if (strstr(config.prntype, "GIF"))
	   {
	   system( "VGA2GIF 1 1 418 418 SHARP.GIF" );
	   return;
	   }

	/* ----- Print using simple Dot Matrix ----- */
	if (strstr(config.prntype, "DMP"))
	   {
	   print_sounding_epson();
	   return;
	   }
	}

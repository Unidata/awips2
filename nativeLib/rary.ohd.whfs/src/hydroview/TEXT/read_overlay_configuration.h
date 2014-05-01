/*******************************************************************************
* FILENAME:            read_overlay_configuration.h
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*         DESCRIPTION: This file contains prototypes of routines defined
*                      within the read_overlay_configuration.c file.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       December 3, 2001
* ORGANIZATION:        HSEB
* MACHINE:             HP-UX 9000 / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        12/3/2001    Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef READ_OVERLAY_CONFIGURATION_H
#define READ_OVERLAY_CONFIGURATION_H

/* Type defintions. */
#define DEFAULT_COLOR_NAME "White"
#define DEFAULT_LINE_WIDTH 3
#define LEN_REPLY 1024
#define NUM_OVERLAYS 50

/* This enumeration will help to determine the parsing order of the
   the record read in from the configuration file. */
enum _Fields { FieldId , FieldState , FieldMemory , FieldFill , FieldColor ,
	       FieldLine , FieldNumber , FieldPath , FieldFile , FieldType ,
	       FieldRoutine , FieldAllDone } ;

/* This enumeration defines the routines that can be used to calculate
   the overlay . */
enum CalcRoutines { DRAW_RADAR_RINGS , DRAW_LAT_LON_LINES , 
                    DRAW_RADAR_LOCATIONS , DRAW_FSL_CITY_LOCATIONS , 
                    DRAW_MPE_CITY_LOCATIONS , DRAW_WHFS_CITY_LOCATIONS ,
                    DRAW_HRAP_GRID , DRAW_HRAP_BOUNDARY , DRAW_TOPOGRAPHY,
		    DRAW_TOPOGRAPHY_CONTOURS, END_OF_CALC_ROUTINES } ;

/* Function prototyps. */
int read_overlay_configuration ( ) ;

#endif /* #ifndef READ_OVERLAY_CONFIGURATION_H */

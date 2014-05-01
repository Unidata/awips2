/*******************************************************************************
* FILENAME:            plot_routines.h
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:  
*         DESCRIPTION: This file contains prototypes of routines defined
*                      within the plot_routines.c file.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 29, 2001
* ORGANIZATION:        HSEB
* MACHINE:             HP-UX 9000 / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/29/2001   Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef PLOT_ROUTINES_H
#define PLOT_ROUTINES_H

#include <Xm/Xm.h>

#define CITY_LABEL_X_OFFSET 3
#define CITY_LABEL_Y_OFFSET 3
#define CITY_CIRCLE_DIAMETER 4
#define MAX_CITY_NAME_LENGTH 100
#define MAX_CITY_NAMES 5
#define MAX_RADAR_ID_LENGTH 10
#define RADAR_CIRCLE_DIAMETER 4
#define RADAR_LABEL_X_OFFSET 3
#define RADAR_LABEL_Y_OFFSET 3
#define MAX_RECORD_LENGTH 1024

void _draw_radar_rings          ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _draw_lat_lon_lines        ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _draw_radar_locations      ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _draw_fsl_city_locations   ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _draw_mpe_city_locations   ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _draw_mpe_gage_ids         ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _draw_mpe_gage_values      ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _draw_whfs_city_locations  ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _draw_hrap_boundary        ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _draw_hrap_grid            ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _draw_hrap_area            ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _show_radar_rings          ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;

#endif /* #ifndef PLOT_ROUTINES_H */

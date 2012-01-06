/*******************************************************************************
* FILENAME:            external_plot_routines.h
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

#ifndef EXTERNAL_PLOT_ROUTINES_H
#define EXTERNAL_PLOT_ROUTINES_H

#include "map_menubar_cb.h" /* For the struct _Map_Segment definition. */

/* This enumeration defines the routines that can be used to retrieve
   data from the database. Note that this enumeration must match the
   "data_routines" array in the read_overlay_configuration.c file. */
enum ExternalRoutinesEnum {PLOT_BINFILES_BASINS , PLOT_BINFILE_LAKES , 
                           PLOT_BINFILE_RIVERS_STREAMS ,
			   PLOT_BINFILE_HIWAYS_ROADS ,
                           END_OF_BINFILE_ROUTINES } ;

/* The ExternalOverlayRoutine typedef is defined in map_menubar_cb.h. */

void _plot_binfile_basins ( void * pData ,
			    int overlay ,
			    struct _Map_Segment ** seg ,
                            enum ShapeFileType * type ,
                            Boolean * fillarea ) ;

void _plot_binfile_lakes ( void * pData ,
                            int overlay ,
                            struct _Map_Segment ** seg ,
                            enum ShapeFileType * type ,
                            Boolean * fillarea ) ;

void _plot_binfile_rivers_streams ( void * pData ,
                            int overlay ,
                            struct _Map_Segment ** seg ,
                            enum ShapeFileType * type ,
                            Boolean * fillarea ) ;

void _plot_binfile_hiways_roads ( void * pData ,
                            int overlay ,
                            struct _Map_Segment ** seg ,
                            enum ShapeFileType * type ,
                            Boolean * fillarea ) ;

void _plot_binfile_roads ( void * pData ,
                            enum MapOverlays overlay ,
                            struct _Map_Segment ** seg ,
                            enum ShapeFileType * type ,
                            Boolean * fillarea ) ;

#endif /* #ifndef EXTERNAL_PLOT_ROUTINES_H */

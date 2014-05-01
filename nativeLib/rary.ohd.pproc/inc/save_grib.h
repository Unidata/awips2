/*******************************************************************************
* FILENAME:            save_grib.h
* DESCRIPTION:         Contains the prototype for the save_grib
*                      routine.  This is shared by MPE Fieldgen
*                      and MPE Editor.
*                      
*
* ORIGINAL AUTHOR:    Bryon Lawrence
* CREATION DATE:      May 11, 2006
* ORGANIZATION:       OHD, HSEB
* MACHINE:            Linux
* MODIFICATION HISTORY:
*     DATE         PROGRAMMER        DESCRIPTION/REASON
*     5/11/2006    B. Lawrence       Made this header more robust.  Added
*                                    documentation.
********************************************************************************
*/

#ifndef SAVE_GRIB_H
#define SAVE_GRIB_H

#define XMRG_FILE_LENGTH 30

char * save_grib ( char xmrgfile [XMRG_FILE_LENGTH], 
                   int * leninf, 
                   char gribfile [XMRG_FILE_LENGTH], 
                   int * lenfn);

#endif /* #ifndef SAVE_GRIB_H */

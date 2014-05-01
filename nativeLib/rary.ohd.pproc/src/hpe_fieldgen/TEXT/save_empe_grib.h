/*******************************************************************************
* FILENAME:            save_empe_grib.h
* DESCRIPTION:         Contains the prototype for the save_hpe_grib
*                      routine.
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
*     8/20/2007    Guoxian Zhou      Made changes for empe_fieldgen.
*    10/15/2007    Guoxian Zhou      Made name change from empe to hpe.
********************************************************************************
*/

#ifndef SAVE_HPE_GRIB_H
#define SAVE_HPE_GRIB_H

/*

#define XMRG_FILE_LENGTH 30
char * save_grib ( char xmrgfile [XMRG_FILE_LENGTH], 
                   int * leninf, 
                   char gribfile [XMRG_FILE_LENGTH], 
                   int * lenfn);
*/

char * save_hpe_grib ( const char * xmrgfile,
                       const char * gribfile, 
                       const char * proc_flag);

#endif /* #ifndef SAVE_HPE_GRIB_H */

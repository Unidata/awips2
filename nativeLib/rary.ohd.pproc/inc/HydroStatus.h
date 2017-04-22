/*******************************************************************************
* FILENAME:             HydroStatus.h
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*   MODULE 1:           HydroStatus.h
* DESCRIPTION:          This header file contains the definitions
*                       of the various error codes used in the
*                       Hmap_mpe application. 
*                        
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        February 11, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell-Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        2/11/02      Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef HYDROSTATUS_H
#define HYDROSTATUS_H

typedef enum HydroStatus { 

            HydroStatus_OK                = 0 ,
            HydroStatus_MissingEnv        = 1 ,
            HydroStatus_StringOverflow    = 2 ,
            HydroStatus_NoData            = 3 ,
            HydroStatus_BadMalloc         = 4 ,
            HydroStatus_BadFilename       = 5 ,
            HydroStatus_netCDFerror       = 6 ,
            HydroStatus_NoCoordFile       = 7

            } HydroStatus ;

#endif /* #ifndef HYDROSTATUS_H */

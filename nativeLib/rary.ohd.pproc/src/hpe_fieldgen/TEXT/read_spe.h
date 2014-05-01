/*******************************************************************************
* FILENAME:             ReadSPE.h
* DESCRIPTION:          Contains the prototype for the ReadSPE routine.
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        May 20, 2003
* ORGANIZATION:         OHD / HSEB
* MACHINE O/S:          HP-UX / Redhat Linux 
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   5/20/2003    Bryon Lawrence    Original Coding
*   3/22/2004    Bryon Lawrence    Modified to be callable from Fortran.
*                                  User now responsible for passing in
*                                  array to hold SPE data.
*   3/23/2004    Bryon Lawrence    Added the prototype for the TADJ routine.
*   3/18/2005    Bryon Lawrence    Modified read_spe calling arguments as
*                                  part of porting MPE FieldGen to C.
********************************************************************************
*/

#ifndef READSPE_H
#define READSPE_H

#include "mpe_fieldgen.h"

#ifdef __WHFS_LINUX
#define TADJ tadj_
#else
#define TADJ tadj
#endif

void TADJ ( int * iyr , int * imo , int * ida , int * im , int * ihr ,
            int * is , int * tdiff , int * tunit ) ;

void read_spe ( const char * satpre_filename ,
                const geo_data_struct * pGeoData ,
                double ** pSatPre,
                int * spe_status );

#endif /* #ifndef READSPE_H */

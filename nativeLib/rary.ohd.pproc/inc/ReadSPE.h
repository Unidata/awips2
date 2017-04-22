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
********************************************************************************
*/

#ifndef READSPE_H
#define READSPE_H

#define TADJ tadj_

void TADJ ( int * iyr , int * imo , int * ida , int * im , int * ihr ,
            int * is , int * tdiff , int * tunit ) ;

void ReadSPE ( const char * filename ,
               const int * xor ,
               const int * yor ,
               const int * site_xsize ,
               const int * site_ysize ,
               short int * site_spe ,
               short int * x_factor ,
               short int * u_thres ,
               float * mm_factor ,
               int * spe_status ) ;

#endif /* #ifndef READSPE_H */

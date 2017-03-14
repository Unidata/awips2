/*******************************************************************************
* FILENAME:            read_numpseudo_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:         Contains the prototypes for the read_numpseudo_RFCW
*                      and update_numpseudo_RFCW routines.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       February 11, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE              PROGRAMMER        DESCRIPTION/REASON
* February 11, 2002  Bryon Lawrence    Original Coding
* November 4, 2004   Bryon Lawrence    Added the const qualifier to the
*                                      datetime parameter in the 
*                                      update_numpseudo_RFCW routine.
********************************************************************************
*/
#ifndef READ_NUMPSEUDO_RFCW_H
#define READ_NUMPSEUDO_RFCW_H

void read_numpseudo_RFCW ( const char * dt , int * num ); 
void update_numpseudo_RFCW ( const char * datetime );

#endif /* #ifndef READ_NUMPSEUDO_RFCW_H */

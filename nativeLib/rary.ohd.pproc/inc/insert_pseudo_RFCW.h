/*******************************************************************************
* FILENAME:             insert_pseudo_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:          This routine contains the prototype for the
*                       insert_pseudo_RFCW routine.
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        February 11, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE               PROGRAMMER        DESCRIPTION/REASON
* February 11, 2002  Bryon Lawrence    Original Coding
* November 4, 2004   Bryon Lawrence    Added const qualifier to gid 
*                                      parameter of insert_pseudo_RFCW.
********************************************************************************
*/

#ifndef INSERT_PSEUDO_RFCW_H
#define INSERT_PSEUDO_RFCW_H

void insert_pseudo_RFCW ( const char *gid , char *dt , float lat , float lon , 
                          float val ) ;

#endif /* #ifndef INSERT_PSEUDO_RFCW_H */

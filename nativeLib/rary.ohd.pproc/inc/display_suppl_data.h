/*******************************************************************************
* FILENAME:             display_suppl_data.h
* GENERAL INFORMATION:
* DESCRIPTION:          This header file contains prototypes for the
*                       display_suppl_data and read_suppl_data routines.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        February 12, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE               PROGRAMMER        DESCRIPTION/REASON
* February 12, 2002  Bryon Lawrence    Original Coding   
* December 15, 2004  Bryon Lawrence    Modified read_suppl_data prototype.
********************************************************************************
*/
#ifndef DISPLAY_SUPPL_DATA_H
#define DISPLAY_SUPPL_DATA_H

void display_suppl_data ( Widget w, XtPointer clientdata,
                          XtPointer calldata );
void read_suppl_data ( const char * rid, const char * datetime,
                       int * status );

#endif /* #ifndef DISPLAY_SUPPL_DATA_H */

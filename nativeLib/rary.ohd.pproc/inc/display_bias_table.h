/*******************************************************************************
* FILENAME:             display_bias_table.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototype for the 
*                       	display_bias_table,
* 				radarBiasTable,
* 				quitBiasTableCallback,
* 				quitRadarBiasTableCallback,
* 				applyThresholdChangeCallback,
* 				applyBiasChangeCallback,
* 				validateInputCallback,
* 				setModifyCallback,
*				toggleYesNo
*			routines in the display_bias_table.c source file.
*                       
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 11, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux 
* MODIFICATION HISTORY:
* DATE                   PROGRAMMER          DESCRIPTION/REASON
* February 11, 2002     Moria Shebsovich      Original Coding
* March 28, 2002        Bryon Lawrence        Modified the calling
*                                             arguments of "display_bias_table"
*                                             to meet the standards of Motif.
*                                             Cleaned up the appearance of this
*                                             file to promote readability.
*
********************************************************************************
*/

#ifndef DISPLAY_BIAS_TABLE_H
#define DISPLAY_BIAS_TABLE_H

#include <Xm/Xm.h>

void display_bias_table ( Widget w , XtPointer clientdata ,
                          XtPointer calldata ) ;
void radarBiasTable ( Widget w , XtPointer clientdata , XtPointer calldata ) ;
void quitBiasTableCallback ( Widget w , XtPointer clientdata ,
                             XtPointer calldata ) ;
void quitRadarBiasTableCallback ( Widget w , XtPointer clientdata ,
                                  XtPointer calldata ) ;
void applyThresholdChangeCallback ( Widget w , XtPointer clientdata ,
                                    XtPointer calldata ) ;
void applyBiasChangeCallback ( Widget w , XtPointer clientdata ,
                               XtPointer calldata ) ;
void validateInputCallback ( Widget w, XtPointer clientdata ,
                             XtPointer calldata ) ;
void setModifyCallback ( Widget w , XtPointer clientdata ,
                         XtPointer calldata ) ;
void toggleYesNo ( Widget w , XtPointer clientdata , XtPointer calldata ) ;
void read_bias_table_param ( const char * rid ) ;
int get_rfc_bias_value ( const char * rid, char * office_id, float * pBias );
const char * retrieve_fxa_local_site ( );

#endif /* #ifndef DISPLAY_BIAS_TABLE_H */

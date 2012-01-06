/*******************************************************************************
* FILENAME:              damDisplayControl_show.h
* GENERAL INFORMATION:
* DESCRIPTION:           This file contains the prototypes
*                        for damDisplayControl_show routines.
*
********************************************************************************
*/

#ifndef DAMDISPLAYCONTROL_SHOW_H
#define DAMDISPLAYCONTROL_SHOW_H

#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>

#include "ArealDisplayControl.h"
#include "List.h"
#include "drawDams.h"

#define IHFS_DATABASE 0
#define DAMCAT_DATABASE 1

void dam_display_show ( Widget w ) ;
void dam_dc_AddCallbacks ( ) ;
void setDisplayOptions ( Widget w , XtPointer clientdata , 
                         XtPointer calldata ) ;
void dam_close_display ( Widget w , XtPointer clientdata , 
                         XtPointer calldata ) ;
void dam_clear_display ( Widget w , XtPointer clientdata , 
                         XtPointer calldata ) ;
void dam_MapData ( Widget w , XtPointer clientdata , 
                   XtPointer calldata ) ;

char * buildWhereClause( );
char * buildLatLonFilter( );
int switchDatabases ( int databaseToOpen ) ;
DamReportList * dude ( DamReportList * inputPtr,
                       char *damId,
                       char *damName,
                       double damLat,
                       double damLong ) ;

int getDamDrawingState ( ) ;
Boolean showDamIdLabel ( ) ;
Boolean showDamNameLabel ( ) ;
Boolean showDamIconSymbol ( ) ;
void FreeDamReportList ( DamReportList * sp ) ;

#endif /* #ifndef  DAMDISPLAYCONTROL_SHOW_H */

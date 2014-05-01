/*******************************************************************************
* FILENAME:             display_precip_data.h
* GENERAL INFORMATION:
* DESCRIPTION:          This routine contains prototypes for the following
*                       display functions:
*                          display_gage_RFCW
*                          display_gageonly
*                          display_gageonly_RFCW
*                          display_height
*                          display_height_RFCW
*                          display_index
*                          display_index_RFCW
*                          display_lmosaic
*                          display_lmosaic_RFCW
*                          display_locbias
*                          display_locbias_RFCW
*                          display_locspan
*                          display_locspan_RFCW
*                          display_max_temp_prism
*                          display_max_temp_prism_RFCW
*                          display_min_temp_prism
*                          display_min_temp_prism_RFCW
*                          display_mmosaic
*                          display_mmosaic_RFCW
*                          display_mlmosaic
*                          display_mlmosaic_RFCW
*                          display_prism
*                          display_prism_RFCW
*                          display_rmosaic
*                          display_rmosaic_RFCW
*                          display_rfcmosaic
*                          display_rfcmosaic_RFCW
*                          display_satpre
*                          display_satpre_RFCW
*                          display_lsatpre
*                          display_lsatpre_RFCW
*                          display_xmrg
*                          display_xmrg_RFCW
*                          display_sgmosaic
*                          display_sgmosaic_RFCW
*                          display_srmosaic
*                          display_srmosaic_RFCW
*                          display_srgmosaic
*                          display_srgmosaic_RFCW
*                          get_month_name
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        February 7, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE             PROGRAMMER        DESCRIPTION/REASON
*
* February 7, 2002 Bryon Lawrence    Original Coding
* March 9, 2004    Bryon Lawrence    Added prototypes for display_mlmosaic,
*                                    display_mlmosaic_RFCW, display_lsatpre,
*                                    display_lsatpre_RFCW
*
********************************************************************************
*/

#ifndef DISPLAY_PRECIP_DATA_H
#define DISPLAY_PRECIP_DATA_H

#include <Xm/Xm.h>

#include "drawa.h"

void display_bmosaic ( int map_number ) ;
void display_bmosaic_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata ) ;
void display_gage_RFCW ( int num , draw_struct * data ) ;

void display_gageonly ( int map_number  ) ;
void display_gageonly_RFCW ( Widget w , XtPointer clientdata ,
                             XtPointer calldata ) ;
void display_height ( int map_number ) ;
void display_height_RFCW ( Widget w , XtPointer clientdata ,
                           XtPointer calldata  ) ;
void display_index ( int map_number ) ;
void display_index_RFCW ( Widget w , XtPointer clientdata ,
                          XtPointer calldata ) ;
void display_lmosaic ( int map_number ) ;
void display_lmosaic_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata ) ;
void display_locbias ( int map_number ) ;
void display_locbias_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata ) ;
void display_locspan ( int map_number ) ;
void display_locspan_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata ) ;
void display_mmosaic ( int map_number ) ;
void display_mmosaic_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata ) ;
void display_mlmosaic ( int map_number ) ;
void display_mlmosaic_RFCW ( Widget w , XtPointer clientdata ,
                             XtPointer calldata ) ;
void display_p3lmosaic ( int map_number ) ;
void display_p3lmosaic_RFCW ( Widget w , XtPointer clientdata ,
                              XtPointer calldata ) ;
void display_prism ( int map_number ) ;
void display_prism_RFCW ( Widget w , XtPointer clientdata ,
                          XtPointer calldata ) ;
void display_max_temp_prism ( int map_number ) ;
void display_max_temp_prism_RFCW ( Widget w , XtPointer clientdata ,
                                   XtPointer calldata ) ;
void display_min_temp_prism ( int map_number ) ;
void display_min_temp_prism_RFCW ( Widget w , XtPointer clientdata ,
                                   XtPointer calldata ) ;
void display_rmosaic ( int map_number ) ;
void display_rmosaic_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata ) ;
void display_rfcmosaic ( int map_number ) ;
void display_rfcmosaic_RFCW ( Widget w , XtPointer clientdata ,
                              XtPointer calldata ) ;
void display_avgrmosaic ( int map_number ) ;
void display_avgrmosaic_RFCW ( Widget w , XtPointer clientdata ,
                               XtPointer calldata ) ;
void display_maxrmosaic ( int map_number ) ;
void display_maxrmosaic_RFCW ( Widget w , XtPointer clientdata ,
                               XtPointer calldata ) ;
void display_satpre ( int map_number ) ;
void display_satpre_RFCW ( Widget w , XtPointer clientdata ,
                           XtPointer calldata ) ;
void display_lsatpre ( int map_number ) ;
void display_lsatpre_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata ) ;
void display_sgmosaic ( int map_number );
void display_sgmosaic_RFCW ( Widget w , XtPointer clientdata ,
                             XtPointer calldata ) ;
void display_srmosaic ( int map_number );
void display_srmosaic_RFCW ( Widget w , XtPointer clientdata ,
                             XtPointer calldata );
void display_srgmosaic ( int map_number );
void display_srgmosaic_RFCW ( Widget w , XtPointer clientdata ,
                              XtPointer calldata );
void display_rfcbmosaic ( int map_number );
void display_rfcbmosaic_RFCW ( Widget w, XtPointer clientdata,
                               XtPointer calldata );
void display_rfcmmosaic ( int map_number );
void display_rfcmmosaic_RFCW ( Widget w, XtPointer clientdata,
                               XtPointer calldata );

void display_xmrg ( int map_number ) ;
void display_xmrg_RFCW ( Widget w , XtPointer clientdata ,
                         XtPointer calldata ) ;

void display_rawq2mosaic ( int map_number ) ;
void display_rawq2mosaic_RFCW ( Widget w , XtPointer clientdata ,
                               XtPointer calldata ) ;
void display_localbiasq2mosaic ( int map_number ) ;
void display_localbiasq2mosaic_RFCW ( Widget w , XtPointer clientdata ,
                                      XtPointer calldata ) ;
void display_multiq2mosaic ( int map_number ) ;
void display_multiq2mosaic_RFCW ( Widget w , XtPointer clientdata ,
                                  XtPointer calldata ) ;

const char * get_month_name ( int month_number );

enum climo_menu_items { MonthlyPrecipItem, MonthlyMaxTempItem,
                        MonthlyMinTempItem, NumClimoItems };
Widget mpeClimo [ NumClimoItems ];

#endif /* #ifndef DISPLAY_PRECIP_DATA_H */

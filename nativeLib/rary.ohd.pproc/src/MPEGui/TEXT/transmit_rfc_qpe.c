
/*******************************************************************************
* FILENAME:            transmit_rfc_qpe.c
* DESCRIPTION:
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       June 1, 2006
* ORGANIZATION:        OHD/HSEB
* MACHINE:             Redhat Linux
* MODIFICATION HISTORY:
*    DATE         PROGRAMMER        DESCRIPTION/REASON
*    June 1, 2006 Bryon Lawrence    Original Coding
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>

#include "GeneralUtil.h"
#include "mpe_log_utils.h"
#include "rfcwide_callbacks.h"

/*******************************************************************************
* MODULE NAME:  transmit_rfc_qpe
* PURPOSE:      Calls the transmit_rfc_qpe script.  This script in turn
*               sends all of the most recently edited best estimate QPE fields
*               to the SBN and ultimately the WFOs. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void transmit_rfc_qpe ( Widget w, XtPointer client_data, XtPointer call_data )
{
   static char * precip_proc_bin_token = "pproc_local_bin";
   static char precip_proc_bin_dir [ 120 ];
   char command_string [ 120 ];
   static int first = 1;
   int length;
   static int reply_length = 0;

   if ( first == 1 )
   {
      /* This is the first time this routine has been called.  Buffer the
         the value of the precip_proc bin token. */
      length = strlen ( precip_proc_bin_token );
      get_apps_defaults ( precip_proc_bin_token, &length, precip_proc_bin_dir,
                          & reply_length );
      first = 0;
   }

   if ( reply_length > 0 )
   {
      sprintf ( command_string, "%s/transmit_rfc_qpe", 
                                precip_proc_bin_dir );

      system ( command_string );
   }
   else
   {
      flogMessage ( stdout, "%s token not defined ... cannot transmit QPE "
                        "files.\n", precip_proc_bin_token );
   }

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

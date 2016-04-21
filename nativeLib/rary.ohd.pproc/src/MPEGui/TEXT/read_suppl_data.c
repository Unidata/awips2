#include "create_ss_interface_rfcwide.h"
#include "display_suppl_data.h"
#include "post_functions.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "drawa.h"
#include "DPARadar.h"

void read_suppl_data ( const char * rid, const char * datetime,
	               int * status )

{

   /* This function reads the supplemental data from the DPARadar table 
      for display by the supplemental data viewer

      calling subroutine: create_ss_interface_rfcwide
   */

    char where[80];

    DPARadar * formHead = NULL;
    DPARadar * formPtr = NULL;

   /*-----------------------------------------*/
   /*  get record from DPARadar table         */
   /*-----------------------------------------*/

   *status = 0;

   sprintf(where,"WHERE radid = '%s' AND obstime = '%s'",rid,datetime);   
   formHead = GetDPARadar ( where );

   if ( formHead != NULL )
   {
     formPtr = (DPARadar*) ListFirst(&formHead->list);

     *status = ListCount(&formHead->list);

     suppldata.nisolbin  = formPtr->nisolbin;
     suppldata.noutint   = formPtr->noutint;
     suppldata.noutrep   = formPtr->noutrep;
     suppldata.nbadscan  = formPtr->nbadscan;
     suppldata.nhourout  = formPtr->nhourout;
     suppldata.volcovpat = formPtr->volcovpat;
     suppldata.opermode  = formPtr->opermode;
     suppldata.areared   = formPtr->areared;
     suppldata.biscanr   = formPtr->biscanr;
     suppldata.supplmess = formPtr->supplmess;

     suppldata.minoff    = formPtr->minoff;
     suppldata.maxvald   = formPtr->maxvald;
     suppldata.maxvalh   = formPtr->maxvalh;
     suppldata.bias      = formPtr->s1_bias_value;

     /* Modified to use yearsec_dt_to_ansi. */
     memset ( suppldata.gentime, '\0', ANSI_YEARSEC_TIME_LEN );
     yearsec_dt_to_ansi ( formPtr->producttime, suppldata.gentime );      

     FreeDPARadar(formHead);

   }

}

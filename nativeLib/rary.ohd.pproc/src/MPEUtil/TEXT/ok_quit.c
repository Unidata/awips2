#include <stdlib.h>

#include "mpe_log_utils.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "drawa.h"


/***************************************************************************/
/*  FUNCTION NAME:   ok_quit                                               */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   none

******************************************** BEGIN ok_quit ************/

void ok_quit(w, data, call_data)
   Widget w;
   caddr_t *data, *call_data;
{
  long int irc;

  /*-------------------------------------------*/
  /*   close database                          */
  /*-------------------------------------------*/
  
  closedb(&irc);
  if(irc !=0)
  {
     logMessage("PostgreSQL error# %ld ",irc);
     logMessage(" occurred attempting to close database \n");
   }

  exit(0);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
/******************************** END ok_quit ************/


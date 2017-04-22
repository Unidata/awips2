#include "post_stage3_interface.h"
#include "post_stage3.h"
#include "post_drawa.h"
#include "post_stage3_globals.h"


/***************************************************************************/
/*  FUNCTION NAME:   close_gage                                            */
/*       FUNCTION:   destroy shell containing single gage display          */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) display_gage

Functions called:
   none

Local variables:
   shell - Widget structure;  shell to destroy

******************************************** BEGIN close_gage **************/

void close_gage(w, shell, call_data)
   Widget       w;
   Widget       shell;
   caddr_t     *call_data;
{
 XtDestroyWidget(shell);
}

/********************************************* END close_gage **************/


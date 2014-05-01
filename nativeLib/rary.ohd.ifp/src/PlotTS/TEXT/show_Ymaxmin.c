/* *******************************************************************/
/*      File:           show_Ymaxmin.c                               */
/*      Date:           Apr 05 2002                                  */
/*      Author:         Ai Vo                                        */
/*      Purpose:        routine to handle callbacks when new Yman/min*/
/*                      is entered by user                           */
/*                      Note: Rerun: will keep user input Yscales    */
/*                      Continue/Next will use Y scales determined   */
/*                      dynamically by the timeseries data           */
/* *******************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>


#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/ScrollBar.h> 
#include <Xm/Text.h>
#include <Xm/TextF.h>


/* ***************************************** */
/* ************* Header files ************** */
/* ***************************************** */

#include "YscalesInp.h"
#include "show_Ymaxmin.h"
#include "TSPlots.h"
#include "TSCallbacks.h"



extern PLOTMGR  *PlotMgr;
/* ************************************************************** */
/* Dialog can be called by a standalone application    */
/* or by a callbacks function from any type of button             */
/* ************************************************************** */
void    show_YmaxminDS(Widget w, XtPointer data, XtPointer cbs )

{
        int n ;
        
        n = (int)data;
        
        PlotMgr->active_graph = n;
        
	/* **********************************************************   */
	/*   Create create_Ymax_minDS for the first time only and       */
	/*   just manage it when called  and unmanage it when doesn't   */
	/*  need. 						        */
	/* **********************************************************   */
        
	if ( !Ymax_minDS  )
    	{
       		 create_Ymax_minDS(XtParent(w));		
       		 Ymaxmin_callbacks();                  
	}

 	if (! XtIsManaged(Ymax_minDS))
   	{
                XtManageChild(Ymax_minFO);
      		XtManageChild(Ymax_minDS);                
   	}
   
   return;

}

/* ******************************************* */
/* callbacks(): To add all callbacks */
/* for the Ymax/Ymin Dialog          */
/* ******************************************* */

void Ymaxmin_callbacks() 
{

	/* ********************************************/
	/* Adding Callbacks for Ymax/Ymin User inputs */
	/* ********************************************/
	 
	 XtAddCallback(YmaxminCancelBttn, XmNactivateCallback, Ymax_minClose_CB, (XtPointer) 0);
         XtAddCallback(YmaxminOKBttn, XmNactivateCallback, Ymax_minOK_CB, (XtPointer) 0);

}

void Ymax_minClose_CB (Widget w, XtPointer data, XtPointer cbs)

{
       XtUnmanageChild(Ymax_minDS); 
        
     
}

void Ymax_minOK_CB (Widget w, XtPointer data, XtPointer cbs)

{
   char      *smaxin;
   char      *sminin;
   int       maxval, minval;
   float     orgmin, orgmax;
   GRAPH     *graph;
   
   int       n, valid_data = 0;
  
   n = PlotMgr->active_graph;
 
   graph = (GRAPH *)&PlotMgr->Graph[n];

   orgmin = graph->ymin;
   orgmax = graph->ymax;
   
   smaxin = XmTextGetString(YmaxText);
   sminin = XmTextGetString(YminText);
   
   valid_data = 0;
   if( strlen(smaxin) != 0 && smaxin != NULL) {
      /*printf("smaxin = %s\n",smaxin);*/
      /* check for valid input value */
      if( (maxval = atoi(smaxin) ) != 0){                  
         graph->ymax = (float) maxval;
         valid_data = 1;                 
      }
 
   }
   
   if( strlen(sminin) != 0 && sminin != NULL){
      /*printf("sminin = %s\n",sminin);*/
      /* check for valid input value */
      if( (minval = atoi(sminin) ) != 0){           
         graph->ymin = (float) minval; 
         valid_data = 1;      
      }
   }
   
   if( valid_data == 1)  { 
     if( graph->ymin > graph->ymax) {
        graph->ymin = orgmin;
        graph->ymax = orgmax;
        
     }
     else{
        reDrawGraph(graph);
        write_ymaxminFile();
     }
   }

   XtUnmanageChild(Ymax_minDS);
   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTS/RCS/show_Ymaxmin.c,v $";
 static char rcs_id2[] = "$Id: show_Ymaxmin.c,v 1.2 2002/05/16 12:40:23 dws Exp $";}
/*  ===================================================  */

}

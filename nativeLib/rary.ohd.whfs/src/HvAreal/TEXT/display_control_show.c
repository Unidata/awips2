/*
File:		display_control_show.c
Purpose:	To control the areal display of HydroView.
*/


#include "desiredProduct_show.h"
#include "display_control_show.h"


/*********************************************************************/

void	displayControlShow(Widget w)
{
   HvDisplayControl  *hdc = getHvDisplayControl();
   int	dummy;


   if ( ( displayControlDS == NULL ) || ( ! displayControlDS ) )
   {
      /* create all the widgets */

      create_displayControlDS(GetTopShell(w));
      XtUnmanageChild(ad_optionValue2TB);


      /* set up the callbacks */

      dc_AddCallbacks();


      /* manage dialog shell and form */

      XtManageChild(displayControlFO);
      XtManageChild(displayControlDS);


      /* set a static variable indicating window is displayed */

      dummy = dc_window_status(1);


      /* take the default data and set the GUI to match */

      dc_setGuiFromData(hdc);

      //redrawMap();
   }

   else
   {

      XtManageChild(displayControlFO);
      XtManageChild(displayControlDS);
   }

   return;
}


/*********************************************************************/

void	dc_AddCallbacks(void)
{
   Atom		atom;


   /* window manager callbacks. */

   atom = XmInternAtom(XtDisplay(displayControlDS),
		       "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(displayControlDS, atom,
			   dc_CloseCallback, NULL);


   /* stvo added 01/14/2000  Disable manager close function
      disableMgrClose ( displayControlDS );
      */

   /* close button callbacks */

   XtAddCallback(dc_closePB, XmNactivateCallback,
		 dc_CloseCallback, NULL);


   /* add callbacks for areal form */

   dc_ArealAddCallbacks();


   return;
}


/*********************************************************************/

void 	dc_ArealAddCallbacks()
{
   ColorBar 	*colorBar = dc_GetColorBar();
   WidgetList 	modeButtonList;
   WidgetList 	precipButtonList;
   WidgetList 	resButtonList;
   int 		numModeButtons = 0;
   int 		numPrecipButtons = 0;
   int		numResButtons = 0;
   int 		allowItemDeselect = True;
   int 		i;


   /* set up option TB callbacks */

   XtAddCallback(ad_optionIdTB, XmNvalueChangedCallback,
		 arealOptionChangedCallback, NULL);
   XtAddCallback(ad_optionNameTB, XmNvalueChangedCallback,
		 arealOptionChangedCallback, NULL);
   XtAddCallback(ad_optionFillTB, XmNvalueChangedCallback,
		 arealOptionChangedCallback, NULL);
   XtAddCallback(ad_optionValue1TB, XmNvalueChangedCallback,
		 arealOptionChangedCallback, NULL);
   XtAddCallback(ad_optionValue2TB, XmNvalueChangedCallback,
		 arealOptionChangedCallback, NULL);

   XtAddCallback(aw_ratioPB, XmNactivateCallback,
		 comparisonTypeChangedCallback, NULL);
   XtAddCallback(aw_diffPB, XmNactivateCallback,
		 comparisonTypeChangedCallback, NULL);


   /* set up Option menu pushbutton callbacks */

   GetWidgetChildren(ad_modePDM, &modeButtonList, &numModeButtons);
   for (i = 0; i < numModeButtons; i++)
   {
      XtAddCallback(modeButtonList[i], XmNactivateCallback,
		    productTypeChangedCallback, NULL);
   }


   /* set up Option menu pushbutton callbacks */

   GetWidgetChildren(ad_precipPDM, &precipButtonList, &numPrecipButtons);
   for (i = 0; i < numPrecipButtons; i++)
   {
      XtAddCallback(precipButtonList[i], XmNactivateCallback,
		    productTypeChangedCallback, NULL);
   }


   /* set up Option menu pushbutton callbacks */

   GetWidgetChildren(ad_resPDM, &resButtonList, &numResButtons);
   for (i = 0; i < numResButtons; i++)
   {
      XtAddCallback(resButtonList[i], XmNactivateCallback,
		    productTypeChangedCallback, NULL);
   }


   /* add Callback for XmList for product times */

   XtAddCallback(ad_productLI, XmNbrowseSelectionCallback,
		 arealProductSelectedCallback, &allowItemDeselect);


   /* add Callbacks for displaying the Color Threshold and
      the ffm_summary, and the Desired Products dialogs */

   XtAddCallback(ad_desiredProductsPB, XmNactivateCallback,
		 showDesiredProductsDialogCallback, NULL);

   XtAddCallback(aw_optionSummaryPB, XmNactivateCallback,
		 showFfmSummaryDialogCallback, NULL);

   XtAddCallback(aw_optionThresholdPB, XmNactivateCallback,
		 showThresholdDialogCallback, NULL);


   /* add Callbacks for setting */
   /*
   XtAddCallback(aw_ratioPB, XmNactivateCallback,
   XtAddCallback(aw_diffPB, XmNactivateCallback,
   */


   /* add Callback for exposing colorBar legend
      Free the colorBar before hand to make sure it doesn't try to
      draw before the colorBar is ready */

   freeColorBar(colorBar);
   XtAddCallback(ad_legendDA, XmNexposeCallback,
		 redrawColorBarCallback, colorBar);

   return;
}


/***********************************************************************/

void showDesiredProductsDialogCallback(Widget w, XtPointer ptr, XtPointer cbs)
{

   desiredProductShow(w);

   return;
}


/***********************************************************************/

void showFfmSummaryDialogCallback(Widget w, XtPointer ptr, XtPointer cbs)
{

   //show_ffm_summary(w);

   return;
}


/***********************************************************************/

void	showThresholdDialogCallback(Widget w, XtPointer ptr, XtPointer cbs)
{

   color_threshold_show(w);

   return;
}


/***********************************************************************/

void	dc_CloseCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   /* stvo commented out 01/14/2000
      int dummy;

      XtDestroyWidget(displayControlDS);
      displayControlDS = NULL;
      */

   /* set a static variable indicating window is NOT displayed.
      this is used by other functions.  */

   /* stvo commented out 01/14/2000
      dummy = dc_window_status(0);
      */

   /* stvo added 01/14/2000 */

   XtUnmanageChild(displayControlDS);
   return;
}


/***********************************************************************/

void dc_ClearCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   /*
   clearForm(obselemFM);
   */
   return;
}


/***********************************************************************/


void setDataFromGuiCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   HvDisplayControl *hdc = getHvDisplayControl();

   dc_setDataFromGui(hdc);

   return;
}


/***********************************************************************/

void dc_setDataFromGui(HvDisplayControl *hdc)
{

   /* set areal data */

   setArealOptionDataFromGui(hdc);
   setArealFilterDataFromGui(hdc);


   /* redraw the main map */

   //redrawMap();

   return;
}


/***********************************************************************/

void iconFilterStationCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   HvDisplayControl *hdc = getHvDisplayControl();

   dc_setDataFromGui(hdc);


   return;
}


/***********************************************************************/

void comparisonTypeChangedCallback(Widget w, XtPointer ptr, XtPointer cbs)
{

   arealOptionChangedCallback(w, NULL, NULL);

   drawArealProduct(getHvDisplayControl());

   return;
}


/***********************************************************************/

void arealOptionChangedCallback(Widget w, XtPointer ptr, XtPointer cbs)
{

   HvDisplayControl *hdc = getHvDisplayControl();


   /* set data from the GUI */

   setArealOptionDataFromGui(hdc);


   /* redraw the Map */

   //redrawMap();


   return;
}

/***********************************************************************/
/***********************************************************************/

void getArealTimePeriodDataFromString(char 	*origString,
				      time_t 	*timet,
				      long 	*duration)
{
   int 	i;
   char	ansiTime[ANSI_TIME_LEN];


   for (i = 0; i < ANSI_YEARSEC_TIME_LEN; i++)
   {
      ansiTime[i] = origString[i];
   }
   ansiTime[ANSI_YEARSEC_TIME_LEN+1] = '\0';
   yearsec_ansi_to_timet(ansiTime, timet);

   sscanf(&origString[ANSI_YEARSEC_TIME_LEN+1], "%ld", duration);


   return;
}


/***********************************************************************/

void dc_setGuiFromData(HvDisplayControl *hdc)
{

   setArealOptionGuiFromData(hdc);

   return;
}


/***********************************************************************/

void setArealOptionGuiFromData(HvDisplayControl *hdc)
{

   XmToggleButtonSetState(ad_optionIdTB,
			  hdc->displaySettings.areal.showId, False );
   XmToggleButtonSetState(ad_optionNameTB,
			  hdc->displaySettings.areal.showName, False);


   XmToggleButtonSetState(ad_optionValue1TB,
			  hdc->displaySettings.areal.showValue1, False );

   XmToggleButtonSetState(ad_optionValue2TB,
			  hdc->displaySettings.areal.showValue2, False);


   XmToggleButtonSetState(ad_optionFillTB,
			  hdc->displaySettings.areal.fillArea, False);


   /* set the ArealProductDescriptor buttons */

   SetMenuPos(ad_modeOM, hdc->displaySettings.areal.descriptor.mode);

   SetMenuPos(ad_precipOM,
	      hdc->displaySettings.areal.descriptor.precipType);

   SetMenuPos(ad_resOM,
	      hdc->displaySettings.areal.descriptor.resolutionLevel);


   /* set the Comparison Type */

   SetMenuPos(aw_compareOM,
	      hdc->displaySettings.areal.comparisonType);


   loadArealProductList(hdc);
   arealProductSelectedCallback(ad_productLI, NULL, NULL);

   return;
}


/***********************************************************************/

Boolean isSelectedPos(Widget xmList, int pos)
{
   Boolean 	result = False;
   int   	*posList = NULL;
   int		posCount = 0, i;

   XmListGetSelectedPos(xmList, &posList, &posCount);


   for ( i = 0; i < posCount; i++)
   {
      if (pos == posList[i])
      {
	 result = True;
	 break;
      }
   }

   if (posList)
      free(posList);

   posCount = 0;

   return result;
}


/*************************************************************************/

void dc_drawArealDataCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   HvDisplayControl *hdc = getHvDisplayControl();


   drawBoundedArealData(hdc);

   drawGriddedArealData(hdc);

   return;
}


/*************************************************************************/

ColorBar *dc_GetColorBar(void)
{
   static ColorBar colorBar;

   colorBar.accessCount++;

   return &colorBar;
}


/*************************************************************************/

/*************************************************************************
   Sets and returns a state variable indicating whether the
   display control window is displayed.

   =-1, return state value
   =0, set to false and return state value
   =1, set to true and return state value
   ***********************************************************************/

int dc_window_status(int new_state)
{
   static int display_control_window_state;

   if (new_state == 0 || new_state == 1)
   {
      display_control_window_state = new_state;

      if (new_state == 0)
	 printf("areal display window no longer exists...\n");
      else
	 printf("areal display window now exists...\n");

   }


   return display_control_window_state;
}


/*************************************************************************/

void	disableMgrClose(Widget w)
{
   Arg    args[2];

   XtSetArg(args[0], XmNdeleteResponse, XmDO_NOTHING);
   XtSetValues(w ,args, 1);

   return;
}


/*************************************************************************/

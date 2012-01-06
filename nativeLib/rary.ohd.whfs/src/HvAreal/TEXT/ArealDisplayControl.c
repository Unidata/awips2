#include "ArealDisplayControl.h"
#include "display_control.h"
#include "display_control_show.h"
#include "mapBackgrounds.h"
#include "map_library.h"
#include "hv_color_threshold.h"
/**********************************************************************/

void drawArealProduct(HvDisplayControl *hdc)
{
   ArealProductTypeDescriptor 	descriptor;
   ArealProductSpecifier 	specifier;
   ArealProduct 		product;
   time_t duration;
   ColorBar *colorBar = dc_GetColorBar();
   char	thresholdUseName[BUFSIZ];
   char description1Text[BUFSIZ];
   char description2Text[BUFSIZ];
   
   
   /* free the areal products */
   
   freeHvArealProducts(hdc);
   
   
   if (hdc->displaySettings.areal.selectedItemPos != -1)
   {
      /* set data from the GUI */ 
      
      descriptor = hdc->displaySettings.areal.descriptor;
      
      specifier = hdc->displaySettings.areal.selectedSpecifier;
      
      
      /* initialize ColorBar Legend */
      
      freeColorThresholdArray(&hdc->displaySettings.areal.ctArray);
      
      determineThresholdSetKey(thresholdUseName, &duration, hdc);
      
    //  loadColorThresholdArray(&hdc->displaySettings.areal.ctArray,
	//		      thresholdUseName, duration,
	//		      ad_legendDA);
      
      /*
      printf("Original printColorThresholdArray\n");
      printColorThresholdArray(&hdc->displaySettings.areal.ctArray);
      */
      
      createProductDescriptionText(descriptor, specifier,		    
				   description1Text,
				   description2Text);
      
      initColorBar(colorBar, ad_legendDA,
		   &hdc->displaySettings.areal.ctArray,
		   description1Text, description2Text);
      
      drawColorBar(colorBar);
      
      
      /* get the data for the product to be displayed
	 convert it to HydroView format. free the returned product */
      
      bld_areal_product(descriptor, specifier, &product);
      
      printArealProduct(&product);
      
      convertProduct(&product, hdc);
      printf("product data converted.\n");
      
      freeArealProduct(&product);
   }
   
   /* redraw the Map */
   
   mUpdateMap ( 0 );
   mUpdateLegend( 0 );
   return;     
}


/**********************************************************************/

void arealProductSelectedCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   HvDisplayControl *hdc = getHvDisplayControl();
   int allowDeselect = 0 ;
   int curSelectedPos = -1 ;
   
   if ( ptr != ( XtPointer ) NULL )
   {
      allowDeselect = * ( ( int * ) ptr ) ;
   }
   
   /* set watch cursor */
   
   SetCursor(displayControlFO, XC_watch);
   
   /*
   printf("allowDeselect = %d\n", allowDeselect);
   */
   
   
   /* check if item just selected was previously selected */
   
   curSelectedPos = ListRsrcGetFirstSelectedPos(ad_productLI);
   /*
   printf("curSelectedPos = %d\n",curSelectedPos);
   */
   
   
   /* check to see if position just selected is same as previously
      selected.  If so, deselect it.  Otherwise, permit the selection. */
   
   if ((allowDeselect) && (hdc->displaySettings.areal.selectedItemPos == curSelectedPos))
   {
      XmListDeselectPos(w, curSelectedPos);
      
      hdc->displaySettings.areal.selectedItemPos = -1;
      curSelectedPos = -1;
   }
   
   else
   {
      hdc->displaySettings.areal.selectedItemPos = curSelectedPos;
   }
   
   
   if (curSelectedPos == -1)
   {
      
      freeHvArealProducts(hdc);
      
      
      /* redraw the Map */
      
      mUpdateMap ( 0 ) ;
   }
   
   else
   {
      hdc->displaySettings.areal.selectedSpecifier = 
	 hdc->displaySettings.areal.specifiers[curSelectedPos-1];
      
      drawArealProduct(hdc); 
   }
   
   
   /* set watch cursor */
   
   UnsetCursor(displayControlFO);
   
   
   return;
}


/***********************************************************************/

void setArealOptionDataFromGui(HvDisplayControl *hdc)
{
   
   /* set areal option data from the GUI's ToggleButtons  */
   
   hdc->displaySettings.areal.showId   = 
      XmToggleButtonGetState(ad_optionIdTB);
   
   hdc->displaySettings.areal.showName =
      XmToggleButtonGetState(ad_optionNameTB);
   
   hdc->displaySettings.areal.fillArea =
      XmToggleButtonGetState(ad_optionFillTB);
   
   hdc->displaySettings.areal.showValue1 =
      XmToggleButtonGetState(ad_optionValue1TB);
   
   hdc->displaySettings.areal.comparisonType =
      GetMenuPos(aw_compareOM);
   
   return; 
}


/***********************************************************************/

void setArealFilterDataFromGui(HvDisplayControl *hdc)
{   
   int	curSelectedPos = -1;   
   int	posCount = 0;
   int	*posList = NULL;
   
   
   /* get currently selected list position */
   
   XmListGetSelectedPos(ad_productLI, &posList, &posCount);
   if (posCount > 0)
   {	
      curSelectedPos = posList[0] ;	
      
      hdc->displaySettings.areal.selectedItemPos = curSelectedPos;
   }
   
   
   if (posList)
      free(posList);
   
   
   /* get productTypeDescriptor from the GUI */
   
   hdc->displaySettings.areal.descriptor = getProductDescriptorFromGUI();
   
   
   return;
}  


/***********************************************************************/

void setBoundedArealFilterDataFromGui(HvDisplayControl *hdc)
{
      
   /* read in the appropriate data */
   
   if (hdc->displaySettings.areal.selectedItemPos != -1)
   {
      
   }
   
   else
   {
      /* free old areas */
      
      if (hdc->areas) free(hdc->areas);
      
      hdc->numAreas = 0;
   }
   
   
   return;
}


/****************************************************************************/

void  loadArealProductList(HvDisplayControl *hdc)
{
   int pos;
   
   /* this else block is mainly for when the dialog has been
      destroyed and recreated */
   
   XmListDeleteAllItems(ad_productLI); 
   
   
   /* load the XmList from the in memory data, do NOT go to the database */
   
   loadProductList(hdc, False);
   
   
   /* select the appropriate item in the XmList, but don't activate
      the callback */
   
   pos = hdc->displaySettings.areal.selectedItemPos;
   
   if (pos != -1 )
   {
      XmListSelectPos(ad_productLI, pos, False);
   }
   
   return;   
}


/****************************************************************************/

void  productTypeChangedCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   /* this Callback is called when the user chooses an option in
      the prodTypeOM, and at other times.    
      The first item in ad_productLI is selected, causing
      setDataFromGuiCallback to be called.  */
   
   HvDisplayControl *hdc = getHvDisplayControl();
   ArealProductTypeDescriptor  curProductDescriptor;
   static ArealProductTypeDescriptor  previousDescriptor;
   int allowItemDeselect = False;
   
   
   /* set watch cursor */
   
   SetCursor(displayControlFO, XC_watch);
   
   
   /* get the current product descriptor */
   
   curProductDescriptor = getProductDescriptorFromGUI();
   printProductDescriptor(curProductDescriptor);
   
   hdc->displaySettings.areal.descriptor = curProductDescriptor;
   
   
   /* set the product type based on the GUI.
      sensitize/desensitize according to the selection */
   
   sensitizeArealProductSettingsGui();
   
   
   /*  if the productType changed, load the product list */
   
   if (! isEqualProductDescriptor(curProductDescriptor,previousDescriptor))
   {
      
      freeHvArealProducts(hdc);
      
      /* if just the resolution changed and the mode is PRECIP_MODE and
	 the precipType is not QPF_PRECIP, then leave the list as-is,
	 including the selected product.      
	 The purpose of this is to allow the user to
	 change the resolution without changing the selected product. */
      
      if  ((curProductDescriptor.mode == PRECIP_MODE) &&
	   (curProductDescriptor.precipType != QPF_PRECIP) &&
	   (isDescriptorEqualExceptResolution(curProductDescriptor,
					      previousDescriptor) ))
      {
	 
	 allowItemDeselect = False;
	 arealProductSelectedCallback(ad_productLI, &allowItemDeselect, NULL);
      }
      
      else
      {
	 XmListDeleteAllItems(ad_productLI);
	 
	 
	 /* load data for product list  */
	 
	 loadProductList(hdc, True); 
      }
      
      previousDescriptor = curProductDescriptor;
      
      mUpdateMap ( 0 ) ;
   }
   
   else  /* curProductType == previousProductType  */
   {
      printf("productType is same\n");
      loadArealProductList(hdc);
   }
   
   
   /* change cursor back */
   
   UnsetCursor(displayControlFO);
   
   return;		
}


/***********************************************************************/

int isEqualProductDescriptor(ArealProductTypeDescriptor d1,
			     ArealProductTypeDescriptor d2)
{
   int isEqual = 0;
   
   
   if ((d1.mode == d2.mode) &&
       (d1.precipType == d2.precipType) &&
       (d1.resolutionLevel == d2.resolutionLevel))
   {
      isEqual = 1;	
   }
   
   return isEqual;   
}


/***********************************************************************/

int isDescriptorEqualExceptResolution(ArealProductTypeDescriptor d1,
				      ArealProductTypeDescriptor d2)
{
   int isEqual = 0;
   
   
   if ((d1.mode == d2.mode) &&
       (d1.precipType == d2.precipType))
   {
      isEqual = 1;	
   }
   
   return isEqual;   
}

/***********************************************************************/

void sensitizeArealProductSettingsGui()
{
   
   ArealDisplayMode displayMode;
   PrecipType precipType;
   ResolutionLevel resLevel;
   
   
   /* read the product type from the Option Menu */
   
   displayMode = GetMenuPos(ad_modeOM);
   precipType = GetMenuPos(ad_precipOM);
   resLevel = GetMenuPos(ad_resOM);
   
   
   if (displayMode == FFG_MODE)
   {
      DeSensitize(ad_precipOM);	  	
   }
   
   else
   {
      Sensitize(ad_precipOM);	  
   }
   
   
   return;     
   
}

/***********************************************************************/

ArealProductTypeDescriptor getProductDescriptorFromGUI(void)
{
   ArealProductTypeDescriptor descriptor;
   
   
   /* read the mode type from the Option Menu */
   
   descriptor.mode = GetMenuPos(ad_modeOM);
   
   
   /* read the precip type from the option menu */
   
   descriptor.precipType = GetMenuPos(ad_precipOM);
   
   
   /* get the resolution level from the option menu */
   
   descriptor.resolutionLevel = GetMenuPos(ad_resOM);
   
   
   return descriptor;
}


/***********************************************************************/

void loadProductList(HvDisplayControl *hdc, int reloadData)
{   
   RussText bigText[MAX_LISTED_PRODUCTS];
   char timeString[30];
   char statusString[10];
   struct tm *tmPtr;
   ArealProductTypeDescriptor descriptor;
   int i;
   ArealProductSpecifier *specifiers = NULL;
   int numSpecifiers = 0;
   int hours = 0;
   
   ArealProductSpecifier	*desiredSpecifiers;
   int				numdesiredSpecifiers;
   
   
   /* translate the raw Desired product information into actual
      Desired Product, using the system time */
   
   desiredSpecifiers =
      loadDesiredSpecifiers(hdc->displaySettings.areal.DesiredControls,
			    hdc->displaySettings.areal.numDesiredControls,
			    &numdesiredSpecifiers);
   
   
   /* set up the descriptor */
   
   descriptor = hdc->displaySettings.areal.descriptor;
   
   printf("loadProductList(): descriptor = ");
   printProductDescriptor(descriptor);
   
   
   /* finally make the big call */
   
   bld_prodlist(descriptor,
		0,
		desiredSpecifiers,
		numdesiredSpecifiers,
		MAX_LISTED_PRODUCTS,
		hdc->displaySettings.areal.specifiers,
		&hdc->displaySettings.areal.numSpecifiers);
   
   
   numSpecifiers = hdc->displaySettings.areal.numSpecifiers;
   specifiers = &hdc->displaySettings.areal.specifiers[0];
   
   printf("after bld_prodlist: num desiredSpecs, TotalSpecs = %d %d\n", 
	  numdesiredSpecifiers,
	  hdc->displaySettings.areal.numSpecifiers);
   
   
   /* sort the array of specifiers. use different sorts
      depending upon whether sorting FFG data or QPF data */ 
   
   if (descriptor.mode == FFG_MODE || descriptor.precipType == QPF_PRECIP)
      qsort(specifiers, numSpecifiers,
	    sizeof(ArealProductSpecifier),
	    compareSpecifiers2);
   else
      qsort(specifiers, numSpecifiers,
	    sizeof(ArealProductSpecifier),
	    compareSpecifiers);
   
   
   for (i = 0; i < numSpecifiers; i++)
   {
      tmPtr = gmtime(&specifiers[i].endTime);
      hours =  specifiers[i].duration/SECONDS_PER_HOUR;
      
      strftime(timeString, 20, "%a %m/%d %H:%M",tmPtr);
      
      
      /* show indication if the returned product data is all zeroes */
      
      if (specifiers[i].dataStatus == ALL_ZERO)
	 strcpy(statusString,"NoPrcp");   
      else 
	 strcpy(statusString,"");   
      
      
      /* fill bigText with the string to put in list */
      
      sprintf(bigText[i], "%s %-3d %s  %s",
	      specifiers[i].sourceId, hours, timeString,  statusString);
      
   }
   
   
   /* load the XmList */
 
   if (numSpecifiers > 0)
   {
      loadXmList100(ad_productLI, bigText, numSpecifiers);
   }
   else
   {
      sprintf(bigText[0], "No products available");
      loadXmList100(ad_productLI, bigText, 1);
   }
   
   return;   
}


/***********************************************************************/

void getDurationText(long duration, char *durationText)	
{
   
   /* gives text representation of SHEF duration code */
   
   if ((duration > 1000) && (duration < 2000))
   {
      duration -= 1000; 
   }
   
   else if ((duration > 2000) && (duration < 3000))
   {
      duration -= 2000;
      duration *= HOURS_PER_DAY;
   }
   
   else
   {
      duration = -1;
   }
   
   
   if (duration == 1)
      sprintf(durationText,"%ld hour", duration);
   else if (duration > 1) 
      sprintf(durationText,"%ld hours", duration);
   else
      sprintf(durationText,"Unknown duration = %ld", duration);
   
   return;   
}


/********************************************************************/

void freeHvArealProducts(HvDisplayControl *hdc)
{
   
   /* free the stored boundedArealProduct */
   
   if (hdc->areas)
   {
      free(hdc->areas);
      hdc->areas = NULL;
      hdc->numAreas = 0;
   }
   
   
   /* free the stored gridded product */
   
   freeGrid(&hdc->grid);
   
   return;   
}


/***********************************************************************/

void freeArealProduct(ArealProduct *product)
{
   if (product->precip_grid)
   {
      free(product->precip_grid);
      product->precip_grid = NULL;
      product->precip_cnt  = 0;
   }
   
   if (product->precip_data)
   {
      free(product->precip_data);
      product->precip_data = NULL;
      product->precip_cnt  = 0;
   }
   
   if (product->ffg_grid)
   {
      free(product->ffg_grid);
      product->ffg_grid = NULL;
      product->ffg_cnt  = 0;
   }
   
   if (product->ffg_data)
   {
      free(product->ffg_data);
      product->ffg_data = NULL;
      product->ffg_cnt  = 0;
   } 
   
   return;     
}


/***********************************************************************/

void convertProduct(const ArealProduct *product, HvDisplayControl *hdc)
{
   
   ArealProductTypeDescriptor descriptor;
   
   descriptor = product->descr;   
   
   if (descriptor.resolutionLevel == GRID_RES)
   {  
      /*
      printFloatArray(product->precip_grid, 131, 131, "chip1.out");
      */
      
      fillGrid(product, hdc); 	
   }	
   
   else 
   {
      fillAreas(product, hdc);	
   }
   
   return;
}


/***********************************************************************/

void fillGrid(const ArealProduct *product, HvDisplayControl *hdc)
{
   Grid *grid = &hdc->grid;
   
   
   initGrid(grid, product, hdc->displaySettings.areal.comparisonType);
   
   /*
   printFloatArray(product->precip_grid, grid->numRows, grid->numColumns,
   "chip2.out");
   */	  
   
   
   return;   
}


/***********************************************************************/

void fillAreas(const ArealProduct *product, HvDisplayControl *hdc)
{
   char boundaryType[BUFSIZ];  
   long i;
   long index;
   
   strcpy(boundaryType,"");
   
   if ( product->descr.resolutionLevel == COUNTY_RES )
      strcpy(boundaryType, "COUNTY");
   
   else if ( product->descr.resolutionLevel == ZONE_RES )
      strcpy(boundaryType, "ZONE"); 	
   
   else if ( product->descr.resolutionLevel == BASIN_RES )
      strcpy(boundaryType, "BASIN"); 	
   
   printf("fill_areas(): boundaryType = :%s:\n",boundaryType);
   
   readMapAreasFromDb(&hdc->areas, &hdc->numAreas, boundaryType);
   
   
   if (product->descr.mode == PRECIP_MODE)
   {
      for (i= 0; i < product->precip_cnt; i++ )
      {	 
	 index = getMatchingAreaIndex(product->precip_data[i].lid,
				      hdc->areas, hdc->numAreas);
	 
	 if (index != -1)
	 {
	    if (product->precip_data[i].isOk)
	    {
	       hdc->areas[index].value1 = product->precip_data[i].value;
	    }
	    
	    else
	    {
	       hdc->areas[index].value1 = MISSING;	 
	    }
	 }	 
      }
   }
   
   
   else if  (product->descr.mode == FFG_MODE)
   {
      for (i= 0; i < product->ffg_cnt; i++ )
      {
	 index = getMatchingAreaIndex(product->ffg_data[i].lid,
				      hdc->areas, hdc->numAreas);
	 
	 if (index != -1)
	 {
	    if (product->ffg_data[i].isOk)
	    {
	       hdc->areas[index].value1 = product->ffg_data[i].value;
	    }
	    else
	    {
	       hdc->areas[index].value1 = MISSING;	 
	    } 	  
	 }
      }
   }
   
   
   else if (product->descr.mode == COMPARISON_MODE)
   {
      for (i= 0; i < product->precip_cnt; i++ )
      {
	 index = getMatchingAreaIndex(product->precip_data[i].lid,
				      hdc->areas, hdc->numAreas);
	 
	 
	 if (index != -1)
	 {
	    if ((product->precip_data[i].isOk) && 
		(product->ffg_data[i].isOk))
	    {
	       
	       printf("precip = %s:%5.2f: ffg = %s:%5.2f:\n",
		      product->precip_data[i].lid, product->precip_data[i].value,
		      product->ffg_data[i].lid, product->ffg_data[i].value);
	       
	       
	       if (hdc->displaySettings.areal.comparisonType == COMPARE_DIFFERENCE)
	       {			    
		  /*	compute precip - ffg */
		  
		  hdc->areas[index].value1 = 
		     product->precip_data[i].value -
		     product->ffg_data[i].value;
		  
	       }
	       else if (hdc->displaySettings.areal.comparisonType == COMPARE_RATIO)
	       {
		  hdc->areas[index].value1 = 
		     (product->precip_data[i].value / product->ffg_data[i].value);
		  
	       }
	       
	    }
	    else
	    {
	       hdc->areas[index].value1 = MISSING;	 
	    }
	 }
      }
      
      
   }
   
   return;   
}


/***********************************************************************/


void determineThresholdSetKey(char 		*thresholdUseName, 
			      time_t 		*duration,
			      HvDisplayControl 	*hdc)
{
   ArealProductTypeDescriptor descriptor = hdc->displaySettings.areal.descriptor;
   ArealProductSpecifier specifier = hdc->displaySettings.areal.selectedSpecifier;
   FfmComparisonType comparisonType = hdc->displaySettings.areal.comparisonType;
   
   char buffer[BUFSIZ];
   
   /* determine mode */
   
   if (descriptor.mode == PRECIP_MODE)
   {
      strcpy(buffer, "PRECIP");       	
   }
   
   else if (descriptor.mode == FFG_MODE)
   {
      strcpy(buffer, "FFG");	
   }
   
   else if (descriptor.mode == COMPARISON_MODE)
   {
      strcpy(buffer, "COMPARE");
      
      if (comparisonType == COMPARE_DIFFERENCE)
      {
	 strcat(buffer, "-DIFF");
      }
      
      else
      {
	 strcat(buffer, "-RATIO");  
      }
      
   }
   
   else
   {
      strcpy(buffer, "Default");	
   }
   
   
   /* determine duration */
   
   *duration = specifier.duration;
   strcpy(thresholdUseName, buffer);
   
   return;
   
}


/***********************************************************************/

void createProductDescriptionText(ArealProductTypeDescriptor descriptor,
				  ArealProductSpecifier specifier,
				  char *description1,
				  char *description2)
{   
   char modeText[BUFSIZ];
   char precipText[BUFSIZ];
   char resText[BUFSIZ];
   char timeString[25+1];
   char specifierText[BUFSIZ];
   
   struct tm *tmPtr = NULL;
   long hours = 0;
   
   
   /* get text from display mode */
   
   switch (descriptor.mode)
   {
      case PRECIP_MODE:
	 strcpy(modeText,"PRECIP");
	 break;
	 
      case FFG_MODE:
	 strcpy(modeText,"FFG");
	 break;
	 
      case COMPARISON_MODE:
	 strcpy(modeText,"Comparison");
	 break;
	 
      default:
	 strcpy(modeText,"ERROR_MODE");
	 break;
   }
   
   
   /* get text from precipType */
   
   switch (descriptor.precipType)
   {
      case STAGE1_PRECIP:
	 strcpy(precipText,"Stage 1 Precip");
	 break;
	 
      case STAGE2_GAGE_ONLY_PRECIP:
	 strcpy(precipText,"Stage 2 Gage Only");
	 break;
	 
      case STAGE2_GAGE_RADAR_PRECIP:
	 strcpy(precipText,"Stage 2 Gage + Radar");
	 break;
#ifdef ELVIS    	
      case POINT_GAGE_PRECIP:
	 strcpy(precipText,"Point Gage");
	 break;   
#endif
      case QPF_PRECIP:
	 strcpy(precipText,"QPF");
	 break;     
	 
      default:
	 strcpy(precipText,"ERROR_PRECIP");
	 break;
   }	
   
   
   /* get text from resolutionLevel */
   
   switch (descriptor.resolutionLevel)
   {
      case GRID_RES:
	 strcpy(resText,"Grid");
	 break;
	 
      case COUNTY_RES:
	 strcpy(resText,"County");
	 break;
	 
      case ZONE_RES:
	 strcpy(resText,"Zone");
	 break;
	 
      case BASIN_RES:
	 strcpy(resText,"Basin");
	 break;   
	 
      default:
	 strcpy(resText,"ERROR_RESOLUTION");
	 break;
   }
   
   
   /* get text from specifier */
   
   tmPtr = gmtime(&specifier.endTime);
   hours =  specifier.duration/SECONDS_PER_HOUR;
   
   
   strftime(timeString, 25, "%a %m/%d %H:%M ",tmPtr);
   
   if (hours != 1)
   {
      sprintf(specifierText, "%s %ld hours %s",
	      specifier.sourceId,
	      hours,
	      timeString);
   }
   
   else
   {
      sprintf(specifierText, "%s %ld hour %s",
	      specifier.sourceId,
	      hours,
	      timeString);
   }
   
   
   
   /* don't show precip type when FFG is being displayed */
   
   if (descriptor.mode != FFG_MODE)
   {
      sprintf(description1, "%s %s %s", modeText, precipText, resText);
   }
   
   else
   {
      sprintf(description1, "%s %s", modeText, resText); 
   }
   
   sprintf(description2, "%s", specifierText);
   
   return;
}

/***********************************************************************/

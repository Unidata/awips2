#include "ArealProductControl.h"
#include "GeneralUtil.h"


/*********************************************************************/

ArealProductSpecifier * 
loadDesiredSpecifiers(ArealProductControl *desiredControls,
		      long		   numDesiredControls,
		      int		   *numDesiredSpecifiers)
   
{
   ArealProductSpecifier *desiredSpecifiers = NULL;
   time_t 		curTime;
   ArealProductSpecifier *newSpecs = NULL;
   long 		numNewSpecs;
   int			*is_duplicate;
   int			i, j, k;
   int			num_dups;
   long 		newSize = 0;
   
   
   printf("loading desired product specifiers...\n");
   
   /* get current time */
   
   time(&curTime);
   
   
   /* initialize */
   
   *numDesiredSpecifiers = 0;
      
   
   for (i = 0; i < numDesiredControls; i++)
   {
      
      /* generate the product specifier from the control information,
	 for this particular entry */
      
      newSpecs = generateSpecifiers(curTime, desiredControls[i], &numNewSpecs);
      
      
      /* allocate an array to indicate whether the new specifier is a
	 duplicate of an existing specifier. */
      
      is_duplicate = (int *) malloc(sizeof(int) * (numNewSpecs));
      if (is_duplicate == NULL)
      {
	 fprintf(stderr, "Error in malloc of is_duplicate()\n");
	 return NULL;
      }
            
      
      /* now check each of the potential new entries against the 
	 existing ones to make sure that there are no duplicates. 
	 this will happen when "relative" windows coincide with "absolute"
	 time windows.  note that the source is not set yet, so there
	 is no need to compare that */
      
      for (j = 0; j < numNewSpecs; j++)
      {
	 is_duplicate[j] = 0;
	 
	 /* now check against the already loaded specifiers */
	 
	 for (k = 0; k < *numDesiredSpecifiers; k++)
	 {
	    if ((newSpecs[j].endTime  == desiredSpecifiers[k].endTime) &&
		(newSpecs[j].duration == desiredSpecifiers[k].duration) )
	    {
	       is_duplicate[j] = 1;
	       printf("duplicate removed; entry already defined %d.\n", i);
	    }
	 }
      }
      
      
      /* get the count of the number of duplicates */
      
      num_dups = 0;
      for (j = 0; j < numNewSpecs; j++)
      {
	 if (is_duplicate[j]) num_dups++;
      }      
                  
      
      /* change the size of productSpecifiers  */
      
      newSize = sizeof(ArealProductSpecifier) * 
	 (*numDesiredSpecifiers + (numNewSpecs - num_dups));
      
      desiredSpecifiers = (ArealProductSpecifier *)
	 realloc(desiredSpecifiers, newSize);
      
      
      /* append the returned specifiers to the big list.
	 only add the entry if it is not a duplicate */
      
      for (j = 0; j < numNewSpecs; j++)
      {
	 if (!is_duplicate[j])
	 {
	    desiredSpecifiers[*numDesiredSpecifiers] = newSpecs[j];
	    (*numDesiredSpecifiers)++;
	 }
      }
      
      
      /* free memory and initialize foro the next pass */
      
      free(is_duplicate);
      free(newSpecs);
      
      newSpecs = NULL;
      numNewSpecs = 0;      
   }
   
      
   
   /* print for debugging purposes */
   
   for (i = 0 ; i < *numDesiredSpecifiers; i++)
   {
      printProductSpecifier(&desiredSpecifiers[i]);	
   }
   printf("numDesiredSpecs = %d \n",*numDesiredSpecifiers);
   
   
   return desiredSpecifiers;
}  
 

/*********************************************************************
      
   This function is extremely similar to the one above. It 
   is used for the GUI management of the desired products.
   
    ******************************************************************/

ArealProductSpecifier * readArealProductControls(int *numSpecifiers)
{
   ArealProductControl		*controls = NULL;
   ArealProductSpecifier 	*productSpecifiers = NULL;
   ArealProductSpecifier 	*newSpecs = NULL;
   long numArealProductControls = 0;
   long numNewSpecs;
   long newSize = 0;
   
   long i, j, k, num_dups;
   time_t curTime;
   int	*is_duplicate;
   
   
   /* change time() to hvTime() ? */
   
   time(&curTime);
   
   
   /* init the number of specifiers */
   
   *numSpecifiers = 0;
   
   controls = readArealProductControlFromDb(&numArealProductControls);
   printf("numArealProductControls in db = %ld\n",numArealProductControls);
   
   
   for (i = 0; i < numArealProductControls; i++)
   {
      
      /* generate the product specifiers from the control information
	 for this particular entry */
      
      newSpecs = generateSpecifiers(curTime, controls[i], &numNewSpecs);
      
      
      /* allocate an array to indicate whether the new specifier is a
	 duplicate of another new specifier. */
      
      is_duplicate = (int *) malloc (sizeof(int) * (numNewSpecs));
      if (is_duplicate == NULL)
      {
	 fprintf(stderr, "Error in malloc of is_duplicate()\n");
	 return NULL ;
      }
            
      
      /* now check each of the potential new entries against the 
	 existing ones to make sure that there are no duplicates. 
	 this will happen when "relative" windows coincide with "absolute"
	 time windows. note that the source is not set yet, so there
	 is no need to compare that */
      
      for (j = 0; j < numNewSpecs; j++)
      {
	 is_duplicate[j] = 0;
	 
	 /* now check against the already loaded specifiers */
	 
	 for (k = 0; k < *numSpecifiers; k++)
	 {
	    if ((newSpecs[j].endTime  == productSpecifiers[k].endTime) &&
		(newSpecs[j].duration == productSpecifiers[k].duration) )
	    {
	       is_duplicate[j] = 1;
	       printf("duplicate removed due to predefined entry %ld.\n", 
		      i);
	    }
	 }
      }
      
      
      /* get the count of the number of duplicates */
      
      num_dups = 0;
      for (j = 0; j < numNewSpecs; j++)
      {
	 if (is_duplicate[j]) num_dups++;
      }            
      
      
      /* change the size of productSpecifiers  */
      
      newSize = sizeof(ArealProductSpecifier) * 
	 (*numSpecifiers + (numNewSpecs - num_dups));
      
      productSpecifiers = (ArealProductSpecifier *)
	 realloc(productSpecifiers, newSize);
      
      
      /* append the returned specifiers to the big list.
	 only add the entry if it is not a duplicate */
      
      for (j = 0; j < numNewSpecs; j++)
      {
	 if (!is_duplicate[j])
	 {
	    productSpecifiers[*numSpecifiers] = newSpecs[j];
	    (*numSpecifiers)++;
	 }
      }
      
      
      /* free up the space and initialize the count */
      
      free(is_duplicate);
      free(newSpecs);
      newSpecs = NULL;
      numNewSpecs = 0;
      
   }
   
   
   /* clean up the data for the controls */
   
   free(controls);
   
   
   /* print for debugging purposes */
   
   for (i = 0 ; i < *numSpecifiers; i++)
   {
      printProductSpecifier(&productSpecifiers[i]);	
   }
   printf("numSpecs = %d \n",*numSpecifiers);
   
   
   return productSpecifiers;
}


/***********************************************************************/

ArealProductControl * readArealProductControlFromDb(long *numArealProductControls)
{
   HvAbsDesiredProd *absHead = NULL;
   HvAbsDesiredProd *absPtr = NULL;
   
   HvRelDesiredProd *relHead = NULL;
   HvRelDesiredProd *relPtr = NULL;
   
   ArealProductControl *controls = NULL;
   ArealProductControl c;
   
   long absCount = 0;
   long relCount = 0;
   long controlCount = 0;
   char where[BUFSIZ];
   
   int i = 0;
   
   
   printf("readArealProductControlFromDb \n");
   
   /* set the where clauses for both selects */
   
   sprintf(where, " WHERE active = 'T' ");
   
   
  /* if ( ( absHead = GetHvAbsDesiredProd(where) ) != NULL ) 
   {
      absCount = ListCount(&absHead->list); 	
   }
   
   if ( ( relHead = GetHvRelDesiredProd(where) ) != NULL ) 
   {
      relCount = ListCount(&relHead->list); 	
   } */
   
   controlCount = absCount + relCount;
   
   
   /* allocate space for the ArealProductControls */
   
   controls = (ArealProductControl *)
      malloc (sizeof(ArealProductControl) * (controlCount));
   
   if (controls == NULL)
   {
      return NULL;	
   }
   
   
   i = 0;
   
   if (absHead)
   {
      absPtr = (HvAbsDesiredProd *) ListFirst(&absHead->list);
      
      
      while (absPtr)
      {
	 c.hourMode='A';
	 c.hour = absPtr->end_hour;
	 c.duration = absPtr->dur_hours;
	 c.maxHoursBack = absPtr->lookback_hours;
	 
	 controls[i] = c;
	 
	 i++;
	 absPtr = (HvAbsDesiredProd *) ListNext(&absPtr->node);    
      }
      
      /* FreeHvAbsDesiredProd(absHead); */
      absHead = NULL;
      
   }
   
   
   if (relHead)
   {
      relPtr = (HvRelDesiredProd *) ListFirst(&relHead->list);
      
      
      while (relPtr)
      {
	 c.hourMode='R';
	 c.hour = relPtr->end_hour_offset;
	 c.duration = relPtr->dur_hours;
	 c.maxHoursBack = -9999;
	 
	 controls[i] = c;
	 
	 i++;
	 relPtr = (HvRelDesiredProd *) ListNext(&relPtr->node);    
      }
      
      /* FreeHvRelDesiredProd(relHead); */
      relHead = NULL;
      
   }
   
   *numArealProductControls = controlCount;
   
   return controls;  
}


/**********************************************************************/

ArealProductSpecifier * generateSpecifiers(time_t curTime,
					   ArealProductControl control,
					   long *numSpecs)
{
   
   ArealProductSpecifier *specs = NULL;
   
   int i;
   
   time_t latestTime = 0;    
   time_t earliestTime;   
   time_t iterationTime;
   
   time_t earliestDay;
   
   long size;
   
   latestTime = curTime;
   latestTime /= SECONDS_PER_HOUR;
   latestTime *= SECONDS_PER_HOUR;
   
   
   if (control.hourMode == 'A')
   {
      earliestTime = latestTime - (control.maxHoursBack * SECONDS_PER_HOUR);
      
      earliestDay = earliestTime;
      earliestDay /= SECONDS_PER_DAY;
      earliestDay *= SECONDS_PER_DAY;
      
      
      /* determine the number of Specifiers to allocate */
      
      iterationTime = earliestDay + (control.hour * SECONDS_PER_HOUR);
      *numSpecs = 0;
      while (iterationTime <= latestTime)
      {
	 if (iterationTime >= earliestTime)
	 {
	    (*numSpecs)++;
	 }
	 
	 iterationTime += SECONDS_PER_DAY;
      }
      
      
      /* allocate memory for the number of Specifiers */
      
      size = sizeof(ArealProductSpecifier) * (*numSpecs);
      specs = (ArealProductSpecifier *) malloc(size);
      if (specs == NULL)
      {
	 return NULL;    
      }
      
      
      /* load up specs with data */
      
      iterationTime = earliestDay + (control.hour * SECONDS_PER_HOUR);
      i = 0;
      while ( (iterationTime <= latestTime) && (i < *numSpecs) )
      {
	 if (iterationTime >= earliestTime)
	 {
	    specs[i].endTime  = iterationTime;
	    specs[i].duration = (control.duration * SECONDS_PER_HOUR);
	    strcpy(specs[i].sourceId, "N/A");
	    i++;
	 }
	 
	 iterationTime += SECONDS_PER_DAY;
      }
      
      
   }
   
   else /*  (control.hourMode == 'R') */
   {
      *numSpecs = 1;
      size = sizeof(ArealProductSpecifier) * (*numSpecs);
      specs = (ArealProductSpecifier *) malloc(size);
      
      if (specs)
      {
	 specs[0].endTime = latestTime -
	    (control.hour * SECONDS_PER_HOUR);
	 
	 specs[0].duration = (control.duration * SECONDS_PER_HOUR);
	 
	 strcpy(specs[0].sourceId, "N/A");
      }
   }
   
   
   return (specs);
}


/***********************************************************************/

int compareSpecifiers3(const void *elem1, const void *elem2)
{
   
   /* returns
      -1 if elem1 < elem2
      0  if elem1 == elem2
      1  if elem1 > elem2
      
      comparison is based on:
      
      sourceId ascending
      duration ascending
      endTime descending
            
      */
   
   ArealProductSpecifier *spec1 = (ArealProductSpecifier *) elem1;   
   ArealProductSpecifier *spec2 = (ArealProductSpecifier *) elem2;   
   int rv = -1;
   int sourceIdCompare = 0;
   
   
   sourceIdCompare = strcmp(spec1->sourceId, spec2->sourceId);
   
   if (sourceIdCompare < 0)
      rv = -1;
   else if (sourceIdCompare > 0)
      rv = 1;
   
   
   /* sourceId is same */
   
   else 
   {
      if (spec1->duration > spec2->duration)
	 rv = 1;	
      
      else if (spec1->duration < spec2->duration)
	 rv = -1;
      
      
      /* (spec1->duration == spec2->duration) */
      
      else      
      {
	 
	 if (spec1->endTime > spec2->endTime)
	    rv = -1;   
	 else if (spec1->endTime < spec2->endTime)
	    rv = 1;   
	 else  /* (spec1->endTime == spec2->endTime) */
	    rv = 0;   
      }
   }
      
   
   return rv;
}

/***********************************************************************/

int compareSpecifiers2(const void *elem1, const void *elem2)
{
   
   /* returns
      -1 if elem1 < elem2
      0  if elem1 == elem2
      1  if elem1 > elem2
      
      comparison is based on:
      
      sourceId ascending
      endTime descending
      duration ascending
            
      */
   
   ArealProductSpecifier *spec1 = (ArealProductSpecifier *) elem1;   
   ArealProductSpecifier *spec2 = (ArealProductSpecifier *) elem2;   
   int rv = -1;
   int sourceIdCompare = 0;
   
   
   sourceIdCompare = strcmp(spec1->sourceId, spec2->sourceId);
   
   if (sourceIdCompare < 0)
      rv = -1;
   else if (sourceIdCompare > 0)
      rv = 1;
   
   
   /* sourceId is same */
   
   else 
   {
      if (spec1->endTime > spec2->endTime)
	 rv = -1;   
      else if (spec1->endTime < spec2->endTime)
	 rv = 1;
      
      /* (spec1->endTime == spec2->endTime) */
      
      else  
      {
	 if (spec1->duration > spec2->duration)
	    rv = 1;		 
	 else if (spec1->duration < spec2->duration)
	    rv = -1;
	 else if (spec1->duration == spec2->duration)
	    rv = 0;   
	 	 
      }
   }
   
   
   return rv;
}

/***********************************************************************/

int compareSpecifiers(const void *elem1, const void *elem2)
{
   
   /* returns
      -1 if elem1 < elem2
      0  if elem1 == elem2
      1  if elem1 > elem2
      
      comparison is based on:
      
      sourceId ascending
      endTime descending
      duration ascending
      
      but with sourceId of primary radar always being first!
            
      */
   
   ArealProductSpecifier *spec1 = (ArealProductSpecifier *) elem1;   
   ArealProductSpecifier *spec2 = (ArealProductSpecifier *) elem2;   
   int 			rv = -1;
   int 			sourceIdCompare = 0;
   
   static int 		first_time = 1;
   static char		primary_radar[RADAR_ID_LEN + 1];
   int			is_elem1_primary, is_elem2_primary;
   int			gad_token_len=0, gad_value_len=0;   
   
   /* get the primary radar info the first time through here */
   
   if (first_time)
   {
      gad_token_len = strlen("whfs_primary_radar");
      get_apps_defaults("whfs_primary_radar", &gad_token_len, primary_radar, &gad_value_len);
      if (strlen(primary_radar) <= 0)
	 fprintf(stderr, "whfs_primary_radar undefined");
      
      first_time = 0;
   }
   
   
   /* check if either element is for the primary radar */
   
   if (strcmp(spec1->sourceId, primary_radar) == 0)
      is_elem1_primary = 1;
   else
      is_elem1_primary = 0;
   
   if (strcmp(spec2->sourceId, primary_radar) == 0)
      is_elem2_primary = 1;
   else
      is_elem2_primary = 0;
   
   
   /* first compare the source ids, giving special consideration
      if matching the primary radar id */
   
   sourceIdCompare = strcmp(spec1->sourceId, spec2->sourceId);
   
   if (sourceIdCompare < 0)
   {
      if (is_elem1_primary)
	 rv = -1;
      else if (is_elem2_primary)
	 rv = 1;
      else
	 rv = -1;
   }
   else if (sourceIdCompare > 0)
   {
      if (is_elem1_primary)
	 rv = -1;
      else if (is_elem2_primary)
	 rv = 1;
      else
	 rv = 1;
   }
   
   
   /* sourceId is same */
   
   else 
   {
      if (spec1->endTime > spec2->endTime)
	 rv = -1;   
      else if (spec1->endTime < spec2->endTime)
	 rv = 1;
      
      /* (spec1->endTime == spec2->endTime) */
      
      else  
      {
	 if (spec1->duration > spec2->duration)
	    rv = 1;		 
	 else if (spec1->duration < spec2->duration)
	    rv = -1;
	 else if (spec1->duration == spec2->duration)
	    rv = 0;   
	 	 
      }
   }
   
   
   return rv;
}


/***********************************************************************/

void printArealProduct(const ArealProduct *product)
{
   int i;
   
   printf("precip_status, cnt = %d %ld\n",
	  product->precip_status, product->precip_cnt);
   printf("precip_data, grid = %p %p\n",
	  product->precip_data, product->precip_grid);     
   
   printf("ffg_status, cnt = %d %ld\n",
	  product->ffg_status, product->ffg_cnt);   
   printf("ffg_data, grid = %p %p\n",
	  product->ffg_data,   product->ffg_grid);  
   
   
   
   printf("PRECIP product part\n");
   if (product->precip_data)
   {
      for (i = 0; i < product->precip_cnt ; i++)
      { 
	 printArealData(product->precip_data[i]);
      }
   }
   
   if (product->precip_grid)
   {
      printf("precip grid info not printed out...\n");
      /*
      printFloatArray(product->precip_grid, 131, 131,
      "precip_grid_print.out"); 
      */
   }
   
   
   printf("FFG product part\n");
   if (product->ffg_data)
   {
      for (i = 0; i < product->ffg_cnt; i++)
      {
	 printArealData(product->ffg_data[i]);
      }
   }
   
   
   if (product->ffg_grid)
   {
      printf("ffg grid info not printed out...\n");
      /*
      printFloatArray(product->ffg_grid, 131, 131,
      "ffg_grid_print.out");
      */
   }
   
   return;   
}


/***********************************************************************/

void printArealData(ArealData data)
{
   if (data.isOk)
   {
      printf("lid = %s, value = %f  validtime = %ld  isOk = %d\n",
	     data.lid, data.value, data.validtime, data.isOk);
   }
}


/***********************************************************************/

void printProductDescriptor(ArealProductTypeDescriptor descriptor)
{
   
   printf("mode = %d  \n",descriptor.mode);  
   printf("precipType = %d \n", descriptor.precipType);
   printf("resolutionLevel = %d \n", descriptor.resolutionLevel);
   
   return;   
}


/***********************************************************************/

void printProductSpecifier(ArealProductSpecifier *specifier)
{
   
   printf("Id =%s: ", specifier->sourceId);
   printf("dur sec/hrs = %ld %ld ",
	  specifier->duration, specifier->duration/SECONDS_PER_HOUR);  
   printf("endTime = %ld %s", specifier->endTime, ctime(&specifier->endTime));
   
   return;   
}

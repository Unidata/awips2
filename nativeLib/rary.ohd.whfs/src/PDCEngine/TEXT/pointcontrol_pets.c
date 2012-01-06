
/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "LoadUnique.h"
#include "pointcontrol_options.h"
#include "pointcontrol_pets.h"
#include "ShefPe.h"
#include "ShefTs.h"

/*****************************************************************************
   load and save physical element and typesource from Ingestfilter table.
   this is called once only when window is created.
   **************************************************************************/
/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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
static pc_pets_struct pc_petsdata ;




int save_PeTsIngestfilter()
{
   char    ** values = NULL;
   char    where[255];
   int	   maxTS_for_PE = 150;
   int     ulcnt = 0, cnt;
   UniqueList	*ulPtr = NULL ;
   UniqueList	*ulHead = NULL ;
   
   /* get a list of the unique pe,ts combinations */
   cnt = 0;
   sprintf(where, " WHERE ts like 'R%%' OR ts LIKE 'P%%'");
   ulHead = LoadUnique("pe||ts", "IngestFilter", where, &ulcnt);

   if ( ulHead == NULL )
   {
      fprintf ( stderr , "In routine \"save_PeTsIngestfilter\":\n"
                         "Could not load a list of unique pe, ts\n"
                         "combinations.\n" ) ;
      return 1 ;
   }
   else
   {
      /* allocate for the original list */
      pc_petsdata.orgbuf   =  malloc( sizeof(PhysTypeSrcText) * ulcnt);
   
      /* allocate for the data type separated lists */
      pc_petsdata.riverbuf =  malloc( sizeof(PhysTypeSrcText) * maxTS_for_PE);
      pc_petsdata.rainbuf  =  malloc( sizeof(PhysTypeSrcText) * maxTS_for_PE);
      pc_petsdata.snowbuf  =  malloc( sizeof(PhysTypeSrcText) * maxTS_for_PE);
      pc_petsdata.tempbuf  =  malloc( sizeof(PhysTypeSrcText) * maxTS_for_PE);
      pc_petsdata.otherbuf =  malloc( sizeof(PhysTypeSrcText) * maxTS_for_PE);
      pc_petsdata.element_buffer     = malloc( sizeof(PhysTypeSrcText) * 100);   
      pc_petsdata.adhoc_typeSourceBuffer  = malloc( sizeof(PhysTypeSrcText) * 100);
   
      /* loop thru the unique list and load into the buffer
         for the original, unseparated combinations */
      ulPtr = (UniqueList *) ListFirst(&ulHead->list);
   
      while (ulPtr)
      {
         values = ParseUnique ( ulPtr, &ulcnt );

         if ( ( values == NULL ) || ( ulcnt < 2 ) )
         {
            fprintf ( stderr, "\nIn routine 'SavePeTsIngestFilter':\n"
                              "Could not parse the unique string '%s'.\n",
                              ulPtr->uchar );
            break;
         }

         memset(pc_petsdata.orgbuf[cnt], 0, SHEF_PE_LEN + SHEF_TS_LEN + 1);
         strncpy ( pc_petsdata.orgbuf[cnt], values [0], SHEF_PE_LEN );
         strncat ( pc_petsdata.orgbuf[cnt], values [1], SHEF_TS_LEN );
         FreeParseUnique ( values );

         values = NULL;

         ulPtr = (UniqueList *) ListNext(&ulPtr->node);
         cnt++;
      }
   
      pc_petsdata.nitems = cnt;
   
      if (ulHead) 
         FreeUnique(ulHead);
   
      return 0;
   }
}



void pc_engine_LoadAdHocElementTypes()
{
	int itemCount = 20;
	ElementTypeText text[20] = { "River", "Rain", "Snow", "Temperature",
							 "Agriculture", "Evaporation", "FishCount", "Ground", 
							 "Ice", "Lake", "Moisture", "GateDam", 
							 "Pressure", "Radiation", "Weather", "Wind",
							 "Power", "WaterQuality", "YUnique", "Processed"
   							};
   							
   	int i = 0;
   	
   	
   	/*  free old memory, if needed */
   	if (pc_petsdata.element_type_count > 0)
   	{
   	    free(pc_petsdata.elementTypeTextArray);
   	    pc_petsdata.elementTypeTextArray = NULL;
   	}
   	
   	/* allocate new memory */
   	pc_petsdata.elementTypeTextArray = malloc(itemCount * sizeof(ElementTypeText));
   	pc_petsdata.element_type_count = itemCount;	
   	
   	
   	for (i = 0; i < itemCount; i++)
   	{
   	    strcpy(pc_petsdata.elementTypeTextArray[i], text[i]);
 //  		printf("pc_engineLoadElementtypes(): elementTypeText %s = \n", 
//   				 pc_petsdata.elementTypeTextArray[i]);
   		
   	} 
   						
   	return;						
	
	
}

/******************************************************************************
   count_PhysElemTypeList()
   save count of physical elements for River/Rain/Snow/Temp in memory 
   ***************************************************************************/
/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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
void count_PhysElemTypes ( )
{
   char	ctmp ;
   int	n ;
   int	pets_len = SHEF_PE_LEN + SHEF_TS_LEN ;
   pc_options_struct * pc_options = get_pc_options();
   
   /* initialize */
   pc_petsdata.nriver = 0;
   pc_petsdata.nrain  = 0;
   pc_petsdata.nsnow  = 0;
   pc_petsdata.ntemp  = 0;
   
   /* add pe = Primary and pe = "PC and PP" to physical element list */
   strcpy(pc_petsdata.riverbuf[pc_petsdata.nriver++], "Primary  ");
   strcpy(pc_petsdata.rainbuf[pc_petsdata.nrain++],   "PC and PP");

   /* count how many items are in the pe-ts lists for each data type
      and load the items into the permanent buffers.  Check whether
      processed observations are being comingled with regular observations
      or if they are being treated separately. */
   if ( pc_options->process_mode == PROC_AS_OBS )
   {
      for ( n = 0 ; n < pc_petsdata.nitems ; n ++ )
      {
         if ( pc_petsdata.orgbuf [ n ] [ 0 ] == 'H' ||
              pc_petsdata.orgbuf [ n ] [ 0 ] == 'Q' )
         {
            memset(pc_petsdata.riverbuf[pc_petsdata.nriver], 0, pets_len + 1);
	    strncpy(pc_petsdata.riverbuf[pc_petsdata.nriver],
	            pc_petsdata.orgbuf[n], pets_len);	 
            pc_petsdata.nriver++; 
         }

         else if ( pc_petsdata.orgbuf [ n ] [ 0 ] == 'P' )
         {
            ctmp = pc_petsdata.orgbuf[n][1];
	    if (ctmp != 'A' && ctmp != 'E' && ctmp != 'D' && ctmp != 'L')
	    {
	       memset(pc_petsdata.rainbuf[pc_petsdata.nrain], 0, pets_len + 1);
	       strncpy(pc_petsdata.rainbuf[pc_petsdata.nrain],
	   	       pc_petsdata.orgbuf[n], pets_len);
	       pc_petsdata.nrain++;
	    }
         }
      
         else if ( pc_petsdata.orgbuf [ n ] [ 0 ] == 'S' )
         {
		    memset(pc_petsdata.snowbuf[pc_petsdata.nsnow], 0, pets_len + 1);
		    strncpy(pc_petsdata.snowbuf[pc_petsdata.nsnow],
		    pc_petsdata.orgbuf[n], pets_len);
		    pc_petsdata.nsnow++;
         }
      
         else if ( pc_petsdata.orgbuf [ n] [ 0 ] == 'T' )
         {
		    memset(pc_petsdata.tempbuf[pc_petsdata.ntemp], 0, pets_len + 1);
		    strncpy(pc_petsdata.tempbuf[pc_petsdata.ntemp],
			pc_petsdata.orgbuf[n], pets_len);
		    pc_petsdata.ntemp++;
         }
      }
   }

   else
   {
      for ( n = 0 ; n < pc_petsdata.nitems ; n ++ )
      {
         if ( ( pc_petsdata.orgbuf [ n ] [ 0 ] == 'H' || 
                pc_petsdata.orgbuf [ n ] [ 0 ] == 'Q' ) &&
              ( pc_petsdata.orgbuf [ n ] [ 2 ] != 'P' ) )
         {
            memset(pc_petsdata.riverbuf[pc_petsdata.nriver], 0, pets_len + 1);
	        strncpy(pc_petsdata.riverbuf[pc_petsdata.nriver],
	        pc_petsdata.orgbuf[n], pets_len);	 
            pc_petsdata.nriver++; 
         }

         else if ( ( pc_petsdata.orgbuf [ n ] [ 0 ] == 'P' ) && 
                   ( pc_petsdata.orgbuf [ n ] [ 2 ] != 'P' ) )
         {
            ctmp = pc_petsdata.orgbuf[n][1];
	    if (ctmp != 'A' && ctmp != 'E' && ctmp != 'D' && ctmp != 'L')
	    {
	       memset(pc_petsdata.rainbuf[pc_petsdata.nrain], 0, pets_len + 1);
	       strncpy(pc_petsdata.rainbuf[pc_petsdata.nrain],
	   	   pc_petsdata.orgbuf[n], pets_len);
	       pc_petsdata.nrain++;
	    }
         }
      
         else if ( ( pc_petsdata.orgbuf [ n ] [ 0 ] == 'S' ) && 
                   ( pc_petsdata.orgbuf [ n ] [ 2 ] != 'P' ) )
         {
		    memset(pc_petsdata.snowbuf[pc_petsdata.nsnow], 0, pets_len + 1);
		    strncpy(pc_petsdata.snowbuf[pc_petsdata.nsnow],
		   	pc_petsdata.orgbuf[n], pets_len);
		    pc_petsdata.nsnow++;
         }
      
         else if ( ( pc_petsdata.orgbuf [ n ] [ 0 ] == 'T' ) &&
                   ( pc_petsdata.orgbuf [ n ] [ 2 ] != 'P' ) )
         {
		    memset(pc_petsdata.tempbuf[pc_petsdata.ntemp], 0, pets_len + 1);
		    strncpy(pc_petsdata.tempbuf[pc_petsdata.ntemp],
			pc_petsdata.orgbuf[n], pets_len);
		    pc_petsdata.ntemp++;
         }
      }
   }
   
   return;   
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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
int count_OtherTypes ( int pos )
{
   char		cnow, ctmp;
   static	char  *s="$AEFGILMNPRXUVWYZ";
   int		n, count;
   int		pets_len = SHEF_PE_LEN + SHEF_TS_LEN;
   pc_options_struct * pc_options = get_pc_options();
  
   /* If shef_procobs token is set to "OFF" and "Processed" selected
      from the "other" list then the physical element list contains only
      those pe's from Ingestfilter table where ts like 'P%'.
      NOTE: A-Z positioned from [1-13] corresponding to
      [Agriculture-Processed] in other list widget. */
  
   pc_options->process_selected = 0;
  
   if (s[pos] == 'Z' && pc_options->process_mode == PROC_AS_PROC)
   {
      /* "Processed" is selected from the list of other phys elems */

      pc_options->process_selected = 1;
   }

   /* loop on the pe-ts combinations and see which ones are of
      type "other" */
   count = 0;

   for (n = 0; n < pc_petsdata.nitems; n++)
   {
      /* cnow is a first letter from physical element */

      cnow = pc_petsdata.orgbuf[n][0];

      if (pc_options->process_selected)
      {
      	
      	 /* cnow is now the first letter from type source */
         cnow = pc_petsdata.orgbuf[n][2];

	     if (cnow == 'P') /*user selected Processed for the physical element type */
       	 {
	         memset(pc_petsdata.otherbuf[count], 0, pets_len + 1);
	         strncpy(pc_petsdata.otherbuf[count++], pc_petsdata.orgbuf[n],
	         pets_len);
       	 }

      }
      else /* user has selected something other than a processed type source */
      {
         if (cnow == s[pos])
       	 {
    	    ctmp = pc_petsdata.orgbuf[n][1];
    
    		/*
    		 * Checking for PEs that for Precip
    		 * Note PA, PD, PE an PL are Pressure physical elements
    		 * Continue if it IS a precip p.e.
    		 */
    	    if (cnow == 'P' && ctmp != 'A' && ctmp != 'D' &&
    		ctmp != 'E' && ctmp != 'L')
            {
                   continue;
            }

                /* Check the shef_procobs token to determine if processed
       		observations should be included. */
    	    memset(pc_petsdata.otherbuf[count], 0, pets_len + 1);

            if ( ( pc_options->process_mode == PROC_AS_OBS ) ||
   		        ( ( pc_options->process_mode == PROC_AS_PROC ) &&
                   ( pc_petsdata.orgbuf [ n ] [ 2 ] != 'P' ) ) )
            {
        	       strncpy ( pc_petsdata.otherbuf[count++] ,
                                 pc_petsdata.orgbuf[n] ,
        	                 pets_len ) ;
            }
	    }

    	 if (count != 0 && cnow != s[pos])
             break;

      } //end else

   } //end for

   return count ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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
/******************************************************************************
   check_ShefProcObs()
   Checks and stores the value of the shef_procobs token.
   ***************************************************************************/
int check_ShefProcObs ( )
{
   char         shefprocobs [ 128 ] ;
   int          len = 0, rlen = 0, istatus = 0;
   static int   shefprocOn = 0, first = 1;
   pc_options_struct * pc_options = get_pc_options();

   if ( first )
   {
      len = strlen ( "shef_procobs" ) ;
      istatus = get_apps_defaults ( "shef_procobs" , & len , shefprocobs , 
                                    & rlen ) ;

      if (istatus == 0 && strncmp(shefprocobs, "ON", 2) == 0)
      {
         shefprocOn = 1;
      }

      first = 0;
   }

   if ( shefprocOn == 1 )
   {
      pc_options->process_mode = PROC_AS_OBS ;
   }
   else
   {
      pc_options->process_mode = PROC_AS_PROC ;
   }

   return shefprocOn ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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
/******************************************************************************
   check_ShefPostLatest()
   Checks and stores the value of the shef_post_latest token.
   ***************************************************************************/
int check_ShefPostLatest()
{
   char         shefpost[128];
   int          len = 0, rlen = 0, istatus = 0;
   static int   shefpostOn = 0, first = 1;

   if ( first )
   {
      len = strlen ( "shef_post_latest" ) ;
      istatus = get_apps_defaults ( "shef_post_latest" , & len , shefpost , 
                                    & rlen ) ;

      if (istatus == 0 &&
          strncmp(shefpost, "OFF", 2) != 0 && strncmp(shefpost, "off", 2) != 0)
      {
         shefpostOn = 1;
      }

      first = 0;
   }

   return shefpostOn ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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
void addNameFromShefPe(char *buf)
{
   static ShefPe        *sHead = NULL ;
   static int           first = 1;
   ShefPe               *sPtr = NULL ;
   char                 pe[SHEF_PE_LEN + 1], where[80];


   /* load the data the first time this function is called */

   if (first)
   {
      sprintf(where, " ");
      sHead = GetShefPe(where);
      first = 0;
   }


   /* for the given code, append the descriptive name, if one exists. */

   strncpy(pe, buf, SHEF_PE_LEN);
   pe [ SHEF_PE_LEN + 1 ] = '\0' ;

   sPtr = (ShefPe *) ListFirst(&sHead->list);
   while (sPtr)
   {
      if (strncmp(pe, sPtr->pe, SHEF_PE_LEN) == 0)
      {
         strcat(buf, "  ");
         strcat(buf, sPtr->name);
         break;
      }
      sPtr = (ShefPe *) ListNext(&sPtr->node);
   }

   return;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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
void addNameFromShefTs(char *buf)
{
   static ShefTs        *tsHead = NULL ;
   static int           first = 1;
   ShefTs               *tsPtr = NULL ;
   char                 ts[SHEF_TS_LEN + 1], where[80];

   /* load the data the first time this function is called */

   if (first)
   {
      sprintf(where, " ");
      tsHead = GetShefTs(where);

      if ( tsHead == NULL )
      {
         fprintf ( stderr , "In routine \"addNameFromShefTs\":\n"
                            "Could not load ShefTs information.\n" ) ;
         return ;
      }

      first = 0;
   }

   /* for the given code, append the descriptive name, if one exists. */

   strcpy(ts, buf);

   tsPtr = (ShefTs *) ListFirst(&tsHead->list);
   while (tsPtr)
   {
      if (strncmp(ts, tsPtr->ts, SHEF_TS_LEN) == 0)
      {
         strcat(buf, "  ");
         strcat(buf, tsPtr->name);
         break;
      }

      tsPtr = (ShefTs *) ListNext(&tsPtr->node);
   }

   return;
}

void load_physicalelement_list ( PhysTypeSrcText * textbuf ,
                                 int nitems )
{
   char                 buf[200] , pbuf[200] ;
   int                  i , kount ;

   kount = 0 ;

   memset ( pbuf , '\0' , 10 ) ;

   for (i = 0; i < nitems; i++)
   {
      if (strncmp(textbuf[i], "Primary  ", 9) == 0 ||
          strncmp(textbuf[i], "PC and PP", 9) == 0)
      {
         strncpy(buf, textbuf[i], 9);
         buf[9] = '\0';
         strcpy(pc_petsdata.element_buffer[ kount ++ ], buf);
         printf("load_physicalelement_list():  pc_petsdata.element_buffer[%d] = :%s:\n",
         		kount - 1,  pc_petsdata.element_buffer[kount -1]);
      }
      else
      {
         strncpy(buf, textbuf[i], 2);
         buf[2] = '\0';

         /* the list passed in is for unique pe||ts combinations;
            this function only lists the pe codes, so duplicate
            pe codes in successive entries in the list must
            be filtered out. this if check does the filtering. */

         if (strncmp(buf, pbuf, 2) != 0)
         {
            addNameFromShefPe(buf);
            strcpy(pc_petsdata.element_buffer [ kount ++ ], buf);
         }

         strcpy(pbuf, buf);
      }
   }

   /* Save number of current active items in
      physical element list widget */
   pc_petsdata.element_count = kount;

   return ;
}

void load_adhoc_typesource_list ( int pos )
{
   char header[] = "load_adhoc_typesource_list";
   char                 buf [ SHEF_PE_LEN + 2 + SHEF_PE_NAME_LEN + 1 ] ;
   int                  n ;
   int                  nitems = 0;
   int					count = 0;
   int                  include_in_ts_list ;
   char *selectedPhysicalElement = NULL;
   int peIsPrimary = 0;
   pc_options_struct * pc_options = get_pc_options();

    printf("%s  -----pc_petsdata.nitems = %d \n", header, pc_petsdata.nitems);


   count  = 0;
   nitems = pc_petsdata.nitems;

   /* loop on the number of known pe-ts combinations.
      if process table selected then only shows ts = (P* ) */
   printf("%s  before selectedPhysicalElement line, pos = %d \n", header, pos);
   selectedPhysicalElement = pc_petsdata.element_buffer[pos - 1];
   printf("%s   after selectedPhysicalElement line, pos = %d \n", header, pos);
   
   /* determine if the  */
   if  (strncmp(selectedPhysicalElement, "Primary", strlen("Primary")) == 0) 
   {
   	    peIsPrimary = 1;
   }

   for (n = 0; n < nitems; n++)
   {
      /* check if the PE part matches */
      
     /* 
      printf("%s selectedPhysicalElement = %s \n", header, selectedPhysicalElement);
      printf("%s pc_petsdata.orgbuf[ %d ] = %s \n", header, n, pc_petsdata.orgbuf[n]);
     */
     
      if (
            (strncmp(selectedPhysicalElement, pc_petsdata.orgbuf[n], 2) == 0)
         )
      {
         strncpy(buf, (char *)&pc_petsdata.orgbuf[n][2], 2);
         buf[2]='\0';

         include_in_ts_list = 0;


         /* if treating processed data with regular obs, then any ts
            in a pets combination that matches the pe is considered.
            the processed table should not even be allowed to be selected
            in this case, so we don't need to check for that. */

         if (pc_options->process_mode == PROC_AS_OBS)
         {
            include_in_ts_list = 1;

            /* if the processed data are not comingled with obs, i.e. it
               has its own place as one of the 'other' tables. */

         }
         else
         {
            /* if the processed table is the selected table and the
               type source is a P* typesource, then consider it. */

            if ( pc_options->element_type >= OTHER_AD_HOC_TYPE && 
                 pc_options->process_selected == 1 )
            {
               if (buf[0] == 'P')
                  include_in_ts_list  = 1;
            }
            /* if the processed table is not the selected table, then only
               consider it if it is not a P* typesource */
            else
            {
               if (buf[0] != 'P')
                  include_in_ts_list  = 1;
            }
         }



         if (include_in_ts_list)
         {
            addNameFromShefTs(buf);
            strcpy(pc_petsdata.adhoc_typeSourceBuffer [ count ++ ] , buf);
    
         }
      }

      /* if have already loaded one at least, and the pe no longer
         matches, then we know there are no more to be loaded,
         so bail out of loop. */

      if ( count > 0 &&
          strncmp ( selectedPhysicalElement , 
                    pc_petsdata.orgbuf[n], 2) != 0 )
      {
         break ;
      }
 

   }
   
   if (peIsPrimary)
   {
       count = 0;	
   }

   /* save number of current type source in
      type source list widget */

   pc_petsdata.adhoc_type_source_count = count;

   return ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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

pc_pets_struct * get_pc_petsdata ( )
{
   return & pc_petsdata ;
}

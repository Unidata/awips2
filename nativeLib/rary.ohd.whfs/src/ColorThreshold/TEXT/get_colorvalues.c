/*******************************************************************************
 * FILENAME:             get_colorvalues.c
 *
 * ORIGINAL AUTHOR:      Bryon Lawrence
 * CREATION DATE:        July 2002
 * ORGANIZATION:         OHD11 / HSEB
 * MACHINE:              LINUX
 * MODIFICATION HISTORY:
 *     DATE         PROGRAMMER        DESCRIPTION/REASON
 *  March 13, 2008  Bryon Lawrence    Refactored       
 ********************************************************************************
 */
#include <string.h> 
#include <stdio.h>

#include "color_threshold_show.h"
#include "ColorValue.h"
#include "get_colorvalues.h"
#include "limits.h"
#include "LoadUnique.h"

/* Member variables only visible to this class. */
static const int NO_DURATION_FOUND = -1;
static const char * OFFICE_COLOR_SET_ID = "default";

static char user_id [USERID_LEN + 1] = {'\0'};
static char application_name [APP_NAME_LEN + 1 ] = {'\0'};
static char coloruse_name [ COLOR_USE_NAME_LEN + 1] = {'\0'};
static int duration = 0;
static char threshold_unit = '\0';
static const NamedColorSetGroup * pColorSetGroup = NULL;

/*******************************************************************************
 * MODULE NAME:   get_closest_multihour_duration
 * PURPOSE:       For a given user id, application name, color use name,
 *                and unit, search the ColorValue table to find all of the 
 *                available durations.  Determine the duration which is the
 *                closest to the user-specified duration and return it to the
 *                caller.
 *
 * ARGUMENTS:
 *   TYPE   DATA TYPE NAME              DESCRIPTION/UNITS
 *   Input  char *    userid            The id of the user.
 *
 * RETURNS:
 *   DATA TYPE  DESCRIPTION
 *   int        The closest duration found in the ColorValue table
 *              for the specified user id, application,
 *              color use, and threshold unit.
 *
 * APIs UTILIZED:
 *   NAME          HEADER FILE     DESCRIPTION
 *   FreeUnique    LoadUnique.h    Frees the linked list returned by
 *                                 the LoadUnique function.
 *   ListFirst     List.h          Lists the first node in a linked list.
 *   ListNext      List.h          Lists subsequent nodes in a linked list.
 *   LoadUnique    LoadUnique.h    Loads a unique list of user-specified data
 *                                 from the database.
 *
 * LOCAL DATA ELEMENTS:
 *   DATA TYPE    NAME              DESCRIPTION
 *   char *       where_clause      Used to build the where clause to get the
 *                                  unique list of durations from the
 *                                  ColorValue table.
 *   int          closest_duration  Keeps track of which of the MultiHour
 *                                  color schemes has a duration which is
 *                                  closest to the requested accumulation
 *                                  interval.  
 *   int          duration_diff     The difference between the requested 
 *                                  duration and the duration of the color
 *                                  scheme being processed.
 *   int          duration_value    The value of duration of the color scheme
 *                                  currently being processed.  
 *   int          min_dur_diff      The smallest duration difference
 *                                  found so far.
 *   int          status_count      Indicates success or failure and the 
 *                                  number of rows returned from LoadUnique.
 *   UniqueList * pDurationHead     Points the beginning of the linked list
 *                                  of unique durations for the MultiHour QPE
 *                                  product.
 *   UniqueList * pDurationNode     Points to the current node being processed
 *                                  in the linked list of unique durations.
 *
 * DATA FILES AND/OR DATABASE:
 *   Requires the ColorValue table in the IHFS database. 
 *
 * ERROR HANDLING:
 *    ERROR CODE             DESCRIPTION
 *    -1                     A close duration could not be found.      
 ********************************************************************************
 */
static int get_closest_multihour_duration(const char * id )
{
	char ** values = NULL;
	char * where_clause = NULL;
	int closest_duration = NO_DURATION_FOUND;
	int duration_diff;
	int duration_value;
	int min_dur_diff = INT_MAX;
	int status_count;
	UniqueList * pDurationHead = NULL;
	UniqueList * pDurationNode = NULL;

	where_clause = ( char * ) malloc ( BUFSIZ  * sizeof (char));

	if ( where_clause != NULL )
	{
	   /* Build the where clause. */
	   sprintf(where_clause, " WHERE userid = '%s'"
		                     " AND application_name = '%s'"
		                     " AND color_use_name = '%s'"
		                     " AND threshold_unit = '%c'"
		                     " ORDER BY duration ASC" , id, 
		                     application_name,
			                 coloruse_name, threshold_unit);
	
	    printf("Load Unique query: %s\n", where_clause);

	    /* Load the unique durations for the specified userid, application_name,
	    color_use_name, and threshold_unit. */
	    pDurationHead = LoadUnique("duration", "ColorValue", where_clause,
			                       &status_count);

	   if ( (status_count > 0 ) && (pDurationHead != NULL ))
	   {
	      pDurationNode = ( UniqueList * ) ListFirst( &pDurationHead->list);

		  while (pDurationNode != NULL)
		  {
			 duration_value = atoi(pDurationNode->uchar);
		     duration_diff = abs(duration - duration_value);

			 /* Is this duration the closest to the original duration
			    found so far? */
			 if (duration_diff < min_dur_diff)
			 {
			 	 closest_duration = duration_value;
				 min_dur_diff = duration_diff;
			 }
			 else
			 {
			    /* The closest duration match has been found. */
				break;
			 }

			 pDurationNode = ( UniqueList * ) ListNext( &pDurationNode->node) ;
		  }
	   }

	   /* Free the memory used for the linked list of distinct durations. */
	   if (pDurationHead != NULL) 
           {
		   FreeUnique(pDurationHead) ;
		   pDurationHead = NULL;
	   }
	
	   free(where_clause);
	   where_clause = NULL;
	}

	return closest_duration;
}

/*******************************************************************************
 * MODULE NAME:   getColorValueTableEntries
 * PURPOSE:       Query the ColorValue table, retrieving all records containing 
 *                the given user id, application name, color use name,
 *                duration, and unit.
 *
 * ARGUMENTS:
 *   TYPE   DATA TYPE NAME              DESCRIPTION/UNITS
 *   Input  int       dur               The duration of the color set to retrieve.
 *   Input  char *    userid            The owner of the color set to retrieve.
 *
 * RETURNS:
 *   DATA TYPE     DESCRIPTION
 *   ColorValue *  A collection of ColorValue structures representing a specific
 *                 color set.  Returns NULL if no data are found.
 *
 * APIs UTILIZED:
 *   NAME          HEADER FILE     DESCRIPTION
 *   GetColorValue ColorValue.h    Retrieves ColorValue records from the ColorValue
 *                                 IHFS table based on a user-supplied query.
 *
 * LOCAL DATA ELEMENTS:
 *   DATA TYPE    NAME              DESCRIPTION
 *   char *       where_clause      Dynamically allocated character array in
 *                                  which the ColorValue table query is built.
 *   ColorValue * cvHead            A pointer to the head node in the ColorValue
 *                                  linked list.
 *   ColorValue * cvNode            A pointer to a node in the ColorValue
 *                                  linked list.
 * 
 * DATA FILES AND/OR DATABASE:
 *   Requires the ColorValue table in the IHFS database. 
 *
 ********************************************************************************
 */

static ColorValue * getColorValueTableEntries ( int dur, const char * id)
		                            
{
	char * where_clause = NULL;
	ColorValue * cvHead = NULL;
	ColorValue * cvNode = NULL;

	where_clause = ( char * ) malloc ( BUFSIZ * sizeof ( char ));
	
	if ( where_clause != NULL )
	{
   	   sprintf(where_clause, " WHERE userid = '%s'"
	           " AND application_name = '%s'"
		       " AND color_use_name = '%s'"
		       " AND duration = %d"
		       " AND threshold_unit = '%c' "
		       " ORDER BY color_use_name , duration , "
		       " threshold_value ", id, application_name, coloruse_name,
			   dur, threshold_unit) ;

	   printf("GetColorValue where clause = %s\n", where_clause);

	   cvHead = GetColorValue(where_clause) ;
 
	   free ( where_clause );
	   where_clause = NULL;
	}
	
	return cvHead;
}

/*******************************************************************************
 * MODULE NAME:   getUserColorSet
 * PURPOSE:       Retrieves a color set for the given user id, application name, color use name,
 *                duration, and unit.
 *
 * ARGUMENTS:
 *    None
 * 
 * RETURNS:
 *   DATA TYPE     DESCRIPTION
 *   ColorValue *  A collection of ColorValue structures representing a specific
 *                 color set.  Returns NULL if no data are found.
 *
 * APIs UTILIZED:
 *   NAME                               HEADER FILE     DESCRIPTION
 *   get_closest_multihour_duration		N/A             Finds the duration
 *                                                      closest to that 
 *                                                      specified by the user
 *                                                      for the given user id,
 *                                                      application, color use,
 *                                                      and unit.  
 * 
 * LOCAL DATA ELEMENTS:
 *   DATA TYPE    NAME              DESCRIPTION
 *   ColorValue * cvHead            A pointer to the head node in the ColorValue
 *                                  linked list.
 *   int          closest_duration  The closest duration in the ColorValue table
 *                                  for the specified user id, application,
 *                                  color use, and unit. 
 * 
 * DATA FILES AND/OR DATABASE:
 *   Requires the ColorValue table in the IHFS database. 
 *
 ********************************************************************************
 */

static ColorValue * getUserColorSet ()
{
	ColorValue * cvHead = NULL;
	int closest_duration;
	
	/* Attempt to find the closest color duration match for the 
	 given user id. */
    printf("Trying to find the best user-defined colorset.\n");
	closest_duration = get_closest_multihour_duration(user_id);
			
	if (closest_duration != NO_DURATION_FOUND)
	{
		cvHead = getColorValueTableEntries ( closest_duration, user_id);
	}
	
    return cvHead;	
}

/*******************************************************************************
 * MODULE NAME:   getOfficeColorSet
 * PURPOSE:       Retrieves an office color set for the specified application name, 
 *                color use name, duration, and unit.
 *
 * ARGUMENTS:
 *    None
 * 
 * RETURNS:
 *   DATA TYPE     DESCRIPTION
 *   ColorValue *  A collection of ColorValue structures representing a specific
 *                 color set.  Returns NULL if no data are found.
 *
 * APIs UTILIZED:
 *   NAME                               HEADER FILE     DESCRIPTION
 *   get_closest_multihour_duration		N/A             Finds the duration
 *                                                      closest to that 
 *                                                      specified by the user
 *                                                      for the given user id,
 *                                                      application, color use,
 *                                                      and unit.  
 * 
 * LOCAL DATA ELEMENTS:
 *   DATA TYPE    NAME              DESCRIPTION
 *   ColorValue * cvHead            A pointer to the head node in the ColorValue
 *                                  linked list.
 *   int          closest_duration  The closest duration in the ColorValue table
 *                                  for the specified user id, application,
 *                                  color use, and unit. 
 * 
 * DATA FILES AND/OR DATABASE:
 *   Requires the ColorValue table in the IHFS database. 
 *
 ********************************************************************************
 */
static ColorValue * getOfficeColorSet ()
{
	ColorValue * cvHead = NULL;
	
	int closest_duration;
		
	/* Attempt to find the closest color duration match for the 
   	   given user id. */
	printf("Trying to find the best office-defined colorset.\n");
	closest_duration = get_closest_multihour_duration(OFFICE_COLOR_SET_ID);

	if (closest_duration != NO_DURATION_FOUND)
	{
		cvHead = getColorValueTableEntries ( closest_duration, OFFICE_COLOR_SET_ID );
	}
		
	return cvHead;
}
/*******************************************************************************
 * MODULE NAME:   getDefaultColorSet
 * PURPOSE:       Retrieves the default color set and duration for the given color use name.
 *
 * ARGUMENTS:
 *    None
 * 
 * RETURNS:
 *   DATA TYPE     DESCRIPTION
 *   ColorValue *  A collection of ColorValue structures representing a specific
 *                 color set.  Returns NULL if no data are found.
 *
 * APIs UTILIZED:
 *   NAME           HEADER FILE     DESCRIPTION
 *   ListAdd        List.h          Adds a node to a linked list. 
 *   ListInit       List.h          Initializes a linked list.
 * 
 * LOCAL DATA ELEMENTS:
 *   DATA TYPE    NAME              DESCRIPTION
 * 	ColorValue * cvHead             Pointer to the head of the ColorValue linked list.
 *  ColorValue * cvNode             Pointer to a node in the ColorValue linked list.
 *                                  This is sort of like an iterator.
 *	int color_count                 The number of color name, value pairs to process.
 *	int dur                         The duration of the default color set.
 *	int i                           Loop variable.
 *	int j                           Loop variable.
 *	int status                      Test the return value of string comparisons.
 * 
 * DATA FILES AND/OR DATABASE:
 * None.
 *
 ********************************************************************************
 */
static ColorValue * getDefaultColorSet ()
{
	ColorValue * cvHead = NULL;
	ColorValue * cvNode = NULL;
	int color_count;
	int dur;
	int i;
	int j;
	int status;

	printf("Trying to find the default colorset.\n");
	
    /* Loop over the default colors provided by the user.
	      Look for the specified color use name. */
	if ( pColorSetGroup != NULL )
	{
       for ( i = 0; i < pColorSetGroup->set_count; ++i )
	   {
		   status = strcmp ( coloruse_name,
			   	      pColorSetGroup->color_group_array[i].color_use_db_name );

		   if ( status == 0 )
		   {
			   printf ( "default levels, colors for %s uses.\n",
				  	    pColorSetGroup->color_group_array[i].color_use_db_name );
			   color_count =
			   pColorSetGroup->color_group_array[i].threshold_array.length;
			   

			   /* Set the duration. */
			   dur = pColorSetGroup->color_group_array[i].default_duration;
			   
			   /* Build a linked list containing the default color value 
			 	      information. */
			   for ( j = 0; j < color_count; ++j )
			   {
			      cvNode = ( ColorValue * ) malloc ( sizeof ( ColorValue ) );

			 	  if ( cvNode == NULL )
			 	  {
			 	     fprintf ( stderr , "\nIn routine 'get_colorvalues':\n"
			 				   	     "Could not allocate %d bytes of memory\n"
			 					     "for a new node in the linked list of\n"
			 					     "ColorValue structures.\n" ,
			 					     sizeof ( ColorValue ) );
			 	     break;
			 	  }

			 	  memset ( cvNode->userid, '\0', USERID_LEN + 1 );
			 	  strncpy ( cvNode->userid , "default", USERID_LEN );
			 	  memset ( cvNode->application_name, '\0', APP_NAME_LEN + 1 );
			 	  strncpy ( cvNode->application_name , application_name, APP_NAME_LEN);

 		 	      memset (cvNode->color_use_name, '\0', COLOR_USE_NAME_LEN + 1 );
			 	  strncpy ( cvNode->color_use_name ,
			 				pColorSetGroup->color_group_array[i].color_use_db_name,
			 				COLOR_USE_NAME_LEN );

			 	  cvNode->duration = dur;
			 	  cvNode->threshold_value =
			 	  pColorSetGroup->color_group_array[i].threshold_array.thresholds[j].value;
			 	  cvNode->threshold_unit [ 0 ] = threshold_unit;
			 	  cvNode->threshold_unit [ 1 ] = '\0';
			 	  memset ( cvNode->color_name, '\0', COLOR_NAME_LEN + 1 );
			 	  strncpy ( cvNode->color_name ,
			 	  	        pColorSetGroup->color_group_array[i].threshold_array.thresholds[j].colorName,
			 				COLOR_NAME_LEN );

			 	  if ( cvHead == NULL )
			 	  {
			 	     cvHead = cvNode;
			 	     ListInit ( & cvNode->list );
			 	  }

			 	  ListAdd ( & cvHead->list , & cvNode->node );
		       }
		   }
	   }
	}	 

	return cvHead;
}

/*******************************************************************************
 * MODULE NAME:   get_colorvalues
 * PURPOSE:       Systematically tries to find the color scheme for a 
 *                requested user id, application name, product, duration
 *                and threshold unit.
 *
 *                First, the ColorValue table is queried for the given user id,
 *                application name, product, duration, and threshold unit.
 *
 *                If this fails then an attempt is made to find the user color scheme 
 *                with the closest duration. 
 *
 *                If this fails, then the ColorValue table is queried to find a
 *                an office color set.
 * 
 *                If this fails, then an attempt is made to find the office color
 *                scheme with the closest duration.
 *
 *                If this fails, then the hard-coded color scheme for this 
 *                product is used.
 * 
 *                If no color set can be found for this product, then this application
 *                returns a NULL value.
 *
 * ARGUMENTS:
 *   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
 *   Input  char *      id                   The userid. Cannot be NULL.
 *   Input  char *      app_name             The name of the application.
 *                                           Cannot be NULL.
 *   Input  char *      color_use            The name of the product
 *                                           Cannot be NULL.
 *   Input  int         dur                  The duration in seconds.
 *   Input  char        unit                 The threshold unit, English or
 *                                           Metric.  Must be 'E' or 'M'
 *   Output int *       numcol               The number of colors retrieved.
 *   Output int *       numlev               The number of colors retrieved
 *                                           minus the Missing and Minimum
 *                                           threshold values.
 *   Input NamedColorSetGroup * pColorSet    Contains the default color sets
 *                                           and durations.
 *
 * RETURNS:
 *   DATA TYPE       DESCRIPTION
 *   ColorValue *    A linked list containing the color scheme retrieved for 
 *                   the specified product.  Returns NULL if no color set 
 *                   could be found.
 * APIs UTILIZED:
 *   NAME                           HEADER FILE   DESCRIPTION
 *
 * LOCAL DATA ELEMENTS:
 *   DATA TYPE    NAME              DESCRIPTION
 *   ColorValue * cvHead            Points to the head node of the linked
 *                                  list of ColorValue data.
 *
 * DATA FILES AND/OR DATABASE:
 * Requires the ColorValue table in the IHFS database.
 *
 * ERROR HANDLING:
 * Returns NULL if a color list could not be found.
 ********************************************************************************
 */

ColorValue * get_colorvalues(const char * id,
		                     const char * app_name, 
		                     const char * color_use,
		                     int dur, 
		                     char unit, 
		                     int * numcol, 
		                     int * numlev,
		                     const NamedColorSetGroup * pColorSet) 
{
	ColorValue * cvHead = NULL;
	
	* numcol = 0;
	* numlev = 0;
    
	printf("GET_COLORVALUES called with: user_id %s\n"
		"application_name: %s color_use_name: %s duration %d\n"
		"threshold unit: %c\n", id, app_name, color_use,
			dur, unit);

	/* Check to make sure that all input parameters were supplied. */
	if ( (id == NULL ) || (id [ 0 ] == '\0' )) {
		fprintf(stderr, "\nIn routine 'get_colorvalues':\n"
			"An empty user id was passed into this routine.\n") ;
	} else if ( (app_name == NULL ) || (app_name [ 0 ] == '\0' )) {
		fprintf(stderr, "\nIn routine 'get_colorvalues':\n"
			"An empty application name was passed into this\n"
			"routine.\n") ;
	} else if ( (color_use == NULL ) || (color_use [ 0 ] == '\0' )) {
		fprintf(stderr, "\nIn routine 'get_colorvalues':\n"
			"An empty color use name was passed into this\n"
			"routine.\n") ;
	} else if ( (unit != 'E' ) && (unit != 'M' )) {
		fprintf(stderr, "\nIn routine 'get_colorvalues':\n"
			"An invalid threshold unit was passed into this\n"
			"routine. Must be 'E' for English or 'M' for\n"
			"Metric\n") ;
	}

	/* Initialize static variables visible only to this file. */
	memset ( user_id, '\0', USERID_LEN + 1);
	strncpy ( user_id, id, USERID_LEN );
	memset ( application_name, '\0', APP_NAME_LEN + 1);
	strncpy ( application_name, app_name, APP_NAME_LEN );
	memset ( coloruse_name, '\0', COLOR_USE_NAME_LEN + 1 );
	strncpy ( coloruse_name, color_use, COLOR_USE_NAME_LEN + 1 );
	duration = dur;
	threshold_unit = unit;
	pColorSetGroup = pColorSet;
	
        /* Try to find a user defined color set. */
	cvHead = getUserColorSet();
	
	if ( cvHead == NULL )
	{
		printf ("Could not find user-defined colorset.\n");

                /* Try to find an office-defined color set. */
		cvHead = getOfficeColorSet();
		
		if ( cvHead == NULL )
		{
			printf ("Could not find office-defined colorset.\n");

                        /* Try to find a default color set. */
			cvHead = getDefaultColorSet();
			
			if ( cvHead == NULL )
			{
				fprintf ( stderr , "\nIn routine 'get_colorvalues':\n"
							   	   "Colors/levels not defined for application = %s"
								   "use_name = %s  logname = %s\n" ,
								   application_name , coloruse_name , user_id );
			}
		}
	}

	if ( cvHead != NULL )
	{
		* numcol = ListCount ( & cvHead->list );
		* numlev = * numcol - 2;
	}
	
	return cvHead;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

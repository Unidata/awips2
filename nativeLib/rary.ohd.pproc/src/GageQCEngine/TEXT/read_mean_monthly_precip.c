
/*******************************************************************************
* FILENAME:             read_mean_monthly_precip
* GENERAL INFORMATION:  Contains the routine to read and buffer
*                       the monthly mean precipitation data from
*                       the precipitation PRISM files.
*   
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        March 16, 2005
* ORGANIZATION:         OHD-11 HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:
*    DATE         PROGRAMMER        DESCRIPTION/REASON
*    3/16/2006    Bryon Lawrence    Original Coding
********************************************************************************
*/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"
#include "read_xmrg.h"
#include "Swap2Bytes.h"
#include "TestByteOrder.h"
#include "TestXmrg.h"

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
int read_mean_monthly_precip (const char * prism_path,
                              const char * mpe_site_id, 
                              int smonth, 
                              int emonth)
{
   /*---------------------------------------*/
   /*  routine to read isohyet data         */
   /* called from routine read_maps         */
   /*---------------------------------------*/
   
   const char * mon = NULL;
   extern int isohyets_used;
   char message[GAGEQC_MESSAGE_LEN];
   int i, j, ier, max_value, k;

   int status;
   int length;
   int num_bytes = 16;
   int word_position = 1;
   extern struct isoh *isoh;
   char fbuf[150];
   short temp [ MAXX ];
   enum TestByteResult result = DontFlipBytes;
   max_value = 0;
	
   /* Allocate memory for the structure to contain the mean
    * monthly precipitation climatology (isohyets). */
   isoh = (struct isoh *) calloc (1, sizeof (struct isoh));
	
   if (isoh == NULL)
   {
logMessage ("could not allocate isoh space\n");
	exit (1);
   }

   /* This information is no longer pertinent.  It has been
    * Initialized to dummy values. */    
   isoh->maxi = -1;
   isoh->maxj = -1;
   isoh->max_lat = -1;
   isoh->max_lon = -1;
   isoh->total_lat = -1;
   isoh->total_lon = -1;
	isoh->delta_lat = -1;
	isoh->delta_lon = -1;

    /* Do not use this structure.  */
    isoh->coord = NULL;
	isoh->value = (short int ***) calloc (12, sizeof (short int **));

	if (isoh->value == NULL)
	{
	logMessage ("no memory for isoh array\n");
		exit (1);
	}

    /* Loop over the months. Determine for which months PRISM
     * data are needed. */
	for (k = 0; k < 12; k++)
	{
		ier = is_good (k, smonth, emonth);

		if (ier == -1)
		{
			continue;
		}

		isoh->value[k] = (short int **) calloc (MAXY, sizeof (short int *));

		if (isoh->value[k] == NULL)
		{
		logMessage ("no memory for isoh array\n");
			exit (1);
		}
	}

	for (i = 0; i < MAXY; i++)
	{
		for (k = 0; k < 12; k++)
		{
			ier = is_good (k, smonth, emonth);

			if (ier == -1)
			{
				continue;
			}

			isoh->value[k][i] = (short int *) calloc ( MAXX, 
			                                          sizeof (short int));

			if (isoh->value[k][i] == NULL)
			{
			logMessage ("no memory for isoh array\n");
				exit (1);
			}
		}
	}

    /* Read in the PRISM files. */
    /* j increments latitude  i increments longitude */

	for (k = 0; k < 12; k++)
	{
		ier = is_good (k, smonth, emonth);

		if (ier == -1)
		{
			continue;
		}
		
		mon = get_mon_name ( k + 1 );

        /* Create the PRISM filename. */
        sprintf (fbuf, "%s/prism_mean_precip_%s_%s", prism_path, 
	               mpe_site_id, mon);
		               
        /* Test the byte ordering in the PRISM file. */ 
        TestByteOrder_ ( fbuf, &num_bytes, & word_position, & result );
        
        if ( result == FlipTestFailed )
        {
        	sprintf ( message, "In file %s, line %d:\n"
        	         "Could not determine byte order of file %s.\n"
        	         "Check if this file exists.\n", __FILE__,
        	         __LINE__, fbuf );
        logMessage ( message );
        	logMessage ( message );
                return DAILYQC_FAILED;
        }

        /* read in data file */
        /* i is longitude j is latitude */
        
		for ( i = 0; i < MAXY; ++ i )
		{
            /* read the PRISM file one record at a time */
            read_xmrg ( & MAXX, & MAXY, & i, fbuf,
                        & length, & status, temp );
            
            if ( status != 0 )
            {
            	sprintf ( message, "Error reading %s.\n", fbuf );
            logMessage ( message );
            	logMessage ( message );
                return DAILYQC_FAILED;
            }
		
		    if ( result == FlipBytes )
		    {
		    	Swap2Bytes_ ( temp, ( size_t *) & MAXX );
		    }
		    	
			for (j = 0; j < MAXX; j++)
			{
				isoh->value[k][i][j] = temp [ j ];
			}
		}

	}

	isohyets_used = 1;
	return DAILYQC_OK;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/read_mean_monthly_precip.c,v $";
 static char rcs_id2[] = "$Id: read_mean_monthly_precip.c,v 1.2 2007/10/18 17:18:18 lawrence Exp $";}
/*  ===================================================  */

}

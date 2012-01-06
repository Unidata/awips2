/*******************************************************************************
* FILENAME:             read_mean_monthly_temp
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
int read_mean_monthly_temp (const char * prism_path,
                            const char * mpe_site_id, 
                            int smonth, 
                            int emonth)
{
	const char *mon = NULL;
	extern int maxmin_used;
	extern int init_maxmin;
	int i, j, ier, k;
	int length;
        int num_bytes = 16;
	int status;
        int word_position = 1;
	extern struct maxmin *maxmin;
	char message[GAGEQC_MESSAGE_LEN];
	char fbuf[100];
        enum TestByteResult result = DontFlipBytes;
        short temp [ MAXX ];

	if (init_maxmin == -1)
	{
        /* Allocate memory for the structure which will contain the monthly
         * normal max and min temperatures. */
		maxmin = (struct maxmin *) calloc (1, sizeof (struct maxmin));
		
		if (maxmin == NULL)
		{
		logMessage ("could not allocate maxmin space\n");
			exit (1);
		}

		maxmin->maxi = -1;
		maxmin->maxj = -1;
		maxmin->max_lat = -1;
		maxmin->max_lon = -1;
		maxmin->total_lat = -1;
		maxmin->total_lon = -1;
		maxmin->delta_lat = -1;
		maxmin->delta_lon = -1;

		/* Dont use the coord portion of the maxmin structure. */
		maxmin->coord =	NULL;
		maxmin->maxvalue = (short int ***) calloc (12, sizeof (short int **));
		maxmin->minvalue = (short int ***) calloc (12, sizeof (short int **));

		if ( maxmin->maxvalue == NULL || maxmin->minvalue == NULL)
		{
		logMessage ("no memory for maxmin array\n");
			exit (1);
		}

		for (k = 0; k < 12; k++)
		{
			ier = is_good (k, smonth, emonth);

			if (ier == -1)
			{
				continue;
			}

			maxmin->maxvalue[k] = (short int **) calloc (MAXY, sizeof (short int *));
			maxmin->minvalue[k] = (short int **) calloc (MAXY, sizeof (short int *));

			if (maxmin->maxvalue[k] == NULL || maxmin->minvalue[k] == NULL )
			{
			logMessage ("no memory for maxmin array\n");
    			exit (1);
			}

		}

		for ( i = 0; i < MAXY; ++i )
		{
			for (k = 0; k < 12; k++)
			{

				ier = is_good (k, smonth, emonth);

				if (ier == -1)
				{
					continue;
				}

				maxmin->maxvalue[k][i] =
					(short int *) calloc (MAXX, sizeof (short int));
				maxmin->minvalue[k][i] =
					(short int *) calloc (MAXX, sizeof (short int));

				if (maxmin->maxvalue[k][i] == NULL
					 || maxmin->minvalue[k][i] == NULL)
				{
				logMessage ("no memory for maxmin array\n");
					exit (1);
				}
			}
		}
	}

	for (k = 0; k < 12; k++)
	{

		ier = is_good (k, smonth, emonth);

		if (ier == -1)
		{
			continue;
		}

        /* Read the temperature prism files containing the monthly mean max
         * temperature data. */
		mon = get_mon_name ( k + 1 );
		
	    /* Create the max temperature PRISM filename. */
		sprintf (fbuf, "%s/prism_max_temp_%s_%s", prism_path, 
		               mpe_site_id, mon);
		               
		/* Test the byte ordering of the PRISM file. */
        TestByteOrder_ ( fbuf, &num_bytes , & word_position, & result );
        
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
		    	
			for (j = 0; j < MAXX; ++ j)
			{
				maxmin->maxvalue[k][i][j] = temp [ j ];
			}
		}

        mon = get_mon_name ( k + 1 );
		
	    /* Create the min temperature PRISM filename. */
		sprintf (fbuf, "%s/prism_min_temp_%s_%s", prism_path, 
		               mpe_site_id, mon);
		               
		/* Test the byte ordering of the PRISM file. */
        TestXmrgByteOrder_ ( fbuf, &XOR, & result );
        
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
				maxmin->minvalue[k][i][j] = temp [ j ];
			}
		}

		init_maxmin = 1;

	}
	

	maxmin_used = 1;
	return DAILYQC_OK; 
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
const char * get_mon_name ( int month_number )
{
   const static char * months [ 12 ] = { "jan",
                                         "feb",
                                         "mar",
                                         "apr",
                                         "may",
                                         "jun",
                                         "jul",
                                         "aug",
                                         "sep",
                                         "oct",
                                         "nov",
                                         "dec" };
   return months [ month_number - 1 ];


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

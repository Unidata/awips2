/*******************************************************************************
* FILENAME:            map_temp_gages_to_grid
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* 
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

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
int map_temp_gages_to_grid (int smonth,
	  	            int emonth,
			    const char *hrap_tgage_file,
		            const char *area_name,
 			    const struct station *tstation, 
 			    int num_tstations)
{
        extern int mpe_dqc_max_temp_neighbors;

        char message [ GAGEQC_MESSAGE_LEN ];
	const char * station_list_file = NULL;
	int i, k, l, m, h;
	struct HRAP hrap;
	point irap;

	char dbuf[200];

	float conv = .0174, sorted[mpe_dqc_max_temp_neighbors];
	float dist, dist1, dist2;

	struct stat statbuf;
	int oxor, oyor, omaxx, omaxy;
	int ier, newflag, mm, mer;
	FILE *fz = NULL;
	time_t ost_mtime;
	char *p = NULL, kbuf[mpe_dqc_max_temp_neighbors * 5 + 50];
	struct tm *tm = NULL;

struct hrap_grid * hrap_grid; 

hrap_grid = get_hrap_grid ( );

	newflag = 0;
	
	/* Retrieve the path of the station list file. */
	station_list_file = get_station_list_path ( area_name );

    /* Check the creation times of the temperature hrap grid and station files. */
	ier = stat (hrap_tgage_file, &statbuf);

    /* get creation time of station_list file */
	mer = stat (station_list_file, &statbuf);

   /* grid file does not exist */
	if (ier == -1)
	{
		/* The temperature hrap grid file does not exist. */ 
		newflag = 2;
	}	
	else
	{
        /* The temperature grid file does exist. */
		fz = fopen (hrap_tgage_file, "r");
 
                if ( fz == NULL )
                {
                   sprintf ( message, "Error opening %s.\n",
                                       hrap_tgage_file );
                   logMessage(message);
                   return DAILYQC_FAILED;
                }

		p = fgets (dbuf, 100, fz);

		ier = sscanf (dbuf, "%ld %d %d %d %d", &ost_mtime, 
		                     &oxor,
						     &oyor, 
						     &omaxx,
						     &omaxy );

		tm = gmtime ( & statbuf.st_mtime );

		if (ost_mtime != statbuf.st_mtime ||
			oxor != XOR ||
			oyor != YOR ||
			omaxx != MAXX ||
			omaxy != MAXY )
		{
			/* The station list is newer than the temperature
			 * grid file or the coordinates of the MPE forecast
			 * area have changed. */
			newflag = 1;
		}

		fclose (fz);
    
		fz = NULL;
	}

	if (newflag == 0)
	{
        /* Use the temperature grid file as is... */
		fz = fopen (hrap_tgage_file, "r");

                if (fz == NULL )
                {
                   sprintf ( message, "Error opening %s.\n",
                                       hrap_tgage_file );
                   logMessage ( message ); 
                   return DAILYQC_FAILED;
                }

		p = fgets (dbuf, 100, fz);

	}
	else if (newflag == 1)
	{
        /* Open the grid file for update. */
		fz = fopen (hrap_tgage_file, "w+");

                if (fz == NULL )
                {
                   sprintf ( message, "Error opening %s.\n",
                                       hrap_tgage_file );
                   logMessage ( message );
                   return DAILYQC_FAILED;
                }
	}
	else
	{
		/* Create the grid file. */
		fz = fopen (hrap_tgage_file, "w");

                if (fz == NULL )
                {
                   sprintf ( message, "Error opening %s.\n",
                                       hrap_tgage_file );
                   return DAILYQC_FAILED;
                }
	}
	
	if (newflag != 0)
	{
        /* Write the header in the temperature grid file. */
		strcpy (dbuf, "TGage to grid recalculation\n");
		fprintf (fz, "%ld %d %d %d %d\n", statbuf.st_mtime, 
		           XOR, YOR, MAXX, MAXY );
	}

	for (i = 0; i < hrap_grid->maxi; i++)
	{
		for (k = 0; k < hrap_grid->maxj; k++)
		{
			irap.x = hrap_grid->hrap_minx + i;
			irap.y = hrap_grid->hrap_miny + k;

			hrap = HrapToLatLongMpe (irap);

			for (l = 0; l < mpe_dqc_max_temp_neighbors; l++)
				sorted[l] = 9999999;

			if (newflag == 0)
			{
                /* Read the nearest neighbor list from the
                 * temperature grid file. */
				m = 0;
				fread (kbuf, sizeof (char), 
                                       mpe_dqc_max_temp_neighbors * 5, fz);
				kbuf[mpe_dqc_max_temp_neighbors * 5] = 0;
				
				for (mm = 0; mm < mpe_dqc_max_temp_neighbors; 
                                     mm++)
				{
					hrap_grid->gage[i][k].tindex[mm] = atoi (&kbuf[m]);
					m = m + 5;
				}

			}
			else
			{
                /* Recompute the nearest neighbor list. */
				for (m = 0; m < num_tstations; m++)
				{
					dist1 = hrap.y - tstation[m].lat;
					dist2 =
						(hrap.x -
						 tstation[m].lon) * cos ((hrap.y +
														  tstation[m].lat) / 2 * conv);

					dist = pow (dist1, 2) + pow (dist2, 2);

					for (l = 0; 
                                             l < mpe_dqc_max_temp_neighbors; 
                                             l++)
					{

						if (dist < sorted[l])
						{

							for (h = 
                                         mpe_dqc_max_temp_neighbors - 1; 
                                         h > l; h--)
							{

								sorted[h] = sorted[h - 1];

								hrap_grid->gage[i][k].tindex[h] =
									hrap_grid->gage[i][k].tindex[h - 1];

							}

							sorted[l] = dist;

							hrap_grid->gage[i][k].tindex[l] = m;

							break;

						}


					}

				}

				for (l = 0; l < mpe_dqc_max_temp_neighbors; l++)
				{
					/* Write the recomputed neighborlist to the grid
					 * file. */
					fprintf (fz, "%04d ", hrap_grid->gage[i][k].tindex[l]);
				}
			}
		}
	}

	fclose (fz);
	fz = NULL;

	return DAILYQC_OK;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/map_temp_gages_to_grid.c,v $";
 static char rcs_id2[] = "$Id: map_temp_gages_to_grid.c,v 1.3 2007/05/23 20:55:48 whfs Exp lawrence $";}
/*  ===================================================  */

}

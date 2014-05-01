/*******************************************************************************
* FILENAME:            map_freeze_gages_to_grid
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:a         map_freeze_gages_to_grid
* DESCRIPTION:         
*
* ORIGINAL AUTHOR:     Craig Peterson
* CREATION DATE:       Unknown
* ORGANIZATION:        CBRFC
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        8/28/2006    B. Lawrence       Modified for MPE Editor.
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

int map_freeze_gages_to_grid (int smonth,
    			      int emonth,
			      const struct hrap_grid *hrap_grid,
			      const char *hrap_zgage_file,
			      const char *area_name,
			      const struct station *zstation,
			      const struct station *station,
			      int num_zstations, int num_stations)
{
        char message [ GAGEQC_MESSAGE_LEN ];
	const char *station_list_file = NULL;


	int i, k,l, m, h;
	struct HRAP hrap;
	point irap;
	char dbuf[200];
	float conv = .0174, sorted[30];
	float dist, dist1, dist2;
	struct stat statbuf;
    int oxor, oyor, omaxx, omaxy;
	int ier, newflag, mm, mer;
	FILE *fz = NULL;
	time_t ost_mtime;
	char *p = NULL, kbuf[200];
	struct tm *tm = NULL;

	newflag = 0;
	
	
    /* Retrieve the path of the station list file. */
    station_list_file = get_station_list_path ( area_name );

    /* Check the existence of the freezing level grid file. */
	ier = stat (hrap_zgage_file, &statbuf);
 
   /* get creation time of station_list file */
	mer = stat (station_list_file, &statbuf);

    /* grid file does not exist */
	if (ier == -1)
	{
		newflag = 2;
	}
	else
	{
        /* The grid file does exist. Open it for reading. */
		fz = fopen (hrap_zgage_file, "r");

                if ( fz == NULL )
                {
                   sprintf ( message, "Error opening %s.\n",
                                      hrap_zgage_file );
                   logMessage ( message );
                   return DAILYQC_FAILED;
                }

		p = fgets (dbuf, 100, fz);

		ier = sscanf (dbuf, "%ld %d %d %d %d", &ost_mtime, 
		                    &oxor,
						    &oyor, 
						    &omaxx,
						    &omaxy);

		tm = gmtime (&statbuf.st_mtime);

		if (ost_mtime != statbuf.st_mtime ||
			 oxor != XOR ||
			 oyor != YOR ||
			 omaxx != MAXX ||
			 omaxy != MAXY )
	    {
	       newflag = 1;
	    }

		fclose (fz);
        fz = NULL;
	}

	if (newflag == 0)
	{
        /* The grid file exists and is up-to-date.  Use it as it is. */
		fz = fopen (hrap_zgage_file, "r");

                if ( fz == NULL )
                {
                   sprintf ( message, "Error opening %s.\n",
                                      hrap_zgage_file );
                   logMessage ( message );
                   return DAILYQC_FAILED;
                }

		p = fgets (dbuf, 100, fz);
	}
	else if (newflag == 1)
	{
		/* The grid file exists but it needs to be updated. */
		fz = fopen (hrap_zgage_file, "w+");

                if ( fz == NULL )
                {
                   sprintf ( message, "Error opening %s.\n",
                                      hrap_zgage_file );
                   logMessage ( message );
                   return DAILYQC_FAILED;
                }
 
	}
	else
	{
		/* The grid file doesn't exist ... create it. */
		fz = fopen (hrap_zgage_file, "w");

                if ( fz == NULL )
                {
                   sprintf ( message, "Error opening %s.\n",
                                      hrap_zgage_file );
                   logMessage ( message );
                   return DAILYQC_FAILED;
                }
	}

	if (newflag != 0)
	{
		/* Write a updated header to the freezing level grid file. */
	logMessage ("ZGage to grid recalculation\n");
		fprintf (fz, "%ld %d %d %d %d\n", statbuf.st_mtime, 
		             XOR,
					 YOR,
					 MAXX,
					 MAXY );
	}

	for (i = 0; i < hrap_grid->maxi; i++)
	{
		for (k = 0; k < hrap_grid->maxj; k++)
		{
			irap.x = hrap_grid->hrap_minx + i;
			irap.y = hrap_grid->hrap_miny + k;

			hrap = HrapToLatLongMpe (irap);

			for (l = 0; l < 30; l++)
			{
				sorted[l] = 9999999;
			}

			if (newflag == 0)
			{
                /* Read the 30 closest neighbors from the grid file. */
				m = 0;
				fread (kbuf, sizeof (char), 25, fz);
				kbuf[25] = 0;
				for (mm = 0; mm < 5; mm++)
				{

					hrap_grid->gage[i][k].zindex[mm] = atoi (&kbuf[m]);

					m = m + 5;
				}
			}
			else
			{
				/* Recompute and store the 30 nearest neighbors. */
				for (m = 0; m < num_zstations; m++)
				{
					dist1 = hrap.y - zstation[m].lat;
					dist2 =	(hrap.x - zstation[m].lon) * cos ((hrap.y +
									  zstation[m].lat) / 2 * conv);

					dist = pow (dist1, 2) + pow (dist2, 2);

					for (l = 0; l < 5; l++)
					{

					    if (dist < sorted[l])
						{

							for (h = 4; h > l; h--)
							{
								sorted[h] = sorted[h - 1];
								hrap_grid->gage[i][k].zindex[h] =
								   hrap_grid->gage[i][k].zindex[h - 1];
							}

							sorted[l] = dist;

							hrap_grid->gage[i][k].zindex[l] = m;

							break;

						}


					}
				}

				for (l = 0; l < 5; l++)
				{
					fprintf (fz, "%04d ", hrap_grid->gage[i][k].zindex[l]);
				}
			}
		}
	}

	fclose (fz);
	fz = NULL;
	
	/* Update the precip station list to contain the nearest freezing 
	 * level stations. */

	for (i = 0; i < num_stations; i++)
	{

		for (l = 0; l < 5; l++)
		{
			sorted[l] = 9999999;
		}

		for (m = 0; m < num_zstations; m++)
		{

			if (i == m)
			{
				continue;
			}

			dist1 = station[i].lat - zstation[m].lat;
			dist2 = station[i].lon - zstation[m].lon;

			dist = pow (dist1, 2) + pow (dist2, 2);

			for (l = 0; l < 5; l++)
			{
				if (dist < sorted[l])
				{

					for (h = 4; h > l; h--)
					{
						sorted[h] = sorted[h - 1];
						station[i].zindex[h] = station[i].zindex[h - 1];
					}
					
					sorted[l] = dist;
					station[i].zindex[l] = m;

					break;

				}
			}
		}
	}

	return DAILYQC_OK;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

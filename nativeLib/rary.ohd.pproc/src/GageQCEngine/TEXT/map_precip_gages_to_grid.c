/*******************************************************************************
* FILENAME:            map_precip_gages_to_grid
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
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"
#include "mpe_topo.h"

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

struct hrap_grid *hrap_grid = NULL;

struct hrap_grid * map_precip_gages_to_grid (int smonth,
							                 int emonth,
							                 const char *hrap_gage_file,
							                 const char * area_name,
							                 struct station *station, 
							                 int num_stations)
{
        char message [ GAGEQC_MESSAGE_LEN ];
        extern int mpe_dqc_max_precip_neighbors;
	extern int maxmin_used;
	const struct topo *topo = NULL;
	extern int isohyets_used;
	int i, k, kk, maxi, maxj, l, m, h;
	struct HRAP hrap;
	point irap;
	extern struct dval dval;
	char dbuf[200];
	const char * station_list_file = NULL;
	extern struct isoh *isoh;
	extern struct maxmin *maxmin;
	extern struct pcp *pcp;
	extern struct pcp *spf;
	extern struct pcp *tpf;
	extern int pcp_in_use[500];
	long r, s;
	float conv = .0174, sorted[mpe_dqc_max_precip_neighbors];
	float dist, dist1, dist2, distance, value;


	int jj;
	int mini, minj;
	struct stat statbuf;
	long oxor, oyor, omaxx, omaxy;
	int ier, newflag, mm, mer;
	FILE *fz = NULL;
	time_t ost_mtime;
	char *p = NULL, kbuf[mpe_dqc_max_precip_neighbors * 5 + 50];
	struct tm *tm = NULL;
	int ix, iy;
	float lat, lon;
	float elevation;


        /* Retrieve the path of the station list file. */
        station_list_file = get_station_list_path ( area_name );
        
	/* Retrieve the topography data. */
	topo = get_topo_coord ();

        if ( topo == NULL )
        {
          logMessage ( "Could not retrieve topo information ... Could not create\n"
                    "map precipitation gages to HRAP grid. Check if topo file exists.\n" );
           return NULL;
        }

	/* Allocate memory for the hrap_grid structure. */
	hrap_grid = (struct hrap_grid *) calloc (1, sizeof (struct hrap_grid));

	/* Was the memory allocation successful. */
	if (hrap_grid == NULL)
	{
	logMessage ("could not allocate hrap_grid space\n");
		exit (1);
	}

	pcp = (struct pcp *) calloc (1, sizeof (struct pcp));

	if (pcp == NULL)
	{
	logMessage ("could not allocate pcp space\n");
		exit (1);
	}

	spf = (struct pcp *) calloc (1, sizeof (struct pcp));

	if (spf == NULL)
	{
	logMessage ("could not allocate pcp space\n");
		exit (1);
	}

	tpf = (struct pcp *) calloc (1, sizeof (struct pcp));

	if (tpf == NULL)
	{
	logMessage ("could not allocate pcp space\n");
		exit (1);

	}

	/* Initialize the array of precipitation flags here. */
	for (i = 0; i < 500; i++)
	{
		pcp_in_use[i] = -1;
	}

	/* Initialize the maximum and minimum values. */
	maxi = -1;
	maxj = -1;
	mini = 999999;
	minj = 99999;

	/* Log message. */
logMessage ("HRAP Coordinates\n");

	/* Use the coordinates of the MPE forecast area here. */
	/* need longitude of 4 corners of display */
	/* Get the coordinates of 4 corners of the MPE forecast area. */
	/* These coordinates are from the read_geo_data routine. */
	hrap_grid->hrap_minx = XOR;
	hrap_grid->hrap_miny = YOR;
	hrap_grid->maxi = MAXX;
	hrap_grid->maxj = MAXY;

        mini = XOR;
        minj = YOR;
	maxi = hrap_grid->maxi;
	maxj = hrap_grid->maxj;

	pcp->value = (short int **) calloc (maxi, sizeof (short int *));

	if (pcp->value == NULL)
	{
	logMessage ("could not allocate pcp space\n");
		exit (1);
	}

	spf->value = (short int **) calloc (maxi, sizeof (short int *));
	if (spf->value == NULL)
	{
	logMessage ("could not allocate pcp space\n");
		exit (1);
	}

	tpf->value = (short int **) calloc (maxi + 1000, sizeof (short int *));
	if (tpf->value == NULL)
	{

	logMessage ("could not allocate pcp space\n");
		exit (1);

	}

	for (i = 0; i < maxi; i++)
	{
		pcp->value[i] = (short int *) calloc (maxj, sizeof (short int));

		if (pcp->value[i] == NULL)
		{
		logMessage ("no memory for pcp array\n");
			exit (1);
		}

	}

	for (i = 0; i < maxi; i++)
	{

		spf->value[i] = (short int *) calloc (maxj, sizeof (short int));

		if (spf->value[i] == NULL)
		{
		logMessage ("no memory for pcp array\n");
			exit (1);
		}

	}

	for (i = 0; i < maxi + 1000; i++)
	{

		tpf->value[i] = (short int *) calloc (maxj, sizeof (short int));

		if (tpf->value[i] == NULL)
		{
		logMessage ("no memory for pcp array\n");
			exit (1);
		}
	}

	hrap_grid->coord =
		(struct coord **) calloc (maxi, sizeof (struct coord *));
	hrap_grid->isoh = (short int ***) calloc (maxi, sizeof (short int **));
	hrap_grid->gage = (struct gage **) calloc (maxi, sizeof (struct gage *));
	hrap_grid->owner = (short int **) calloc (maxi, sizeof (short int **));
	hrap_grid->max = (short int ***) calloc (maxi, sizeof (short int **));
	hrap_grid->min = (short int ***) calloc (maxi, sizeof (short int **));
	hrap_grid->elev = (short int **) calloc (maxi, sizeof (short int **));

	if (hrap_grid->coord == NULL ||
		 hrap_grid->isoh == NULL ||
		 hrap_grid->gage == NULL ||
		 hrap_grid->owner == NULL ||
		 hrap_grid->max == NULL ||
		 hrap_grid->min == NULL || hrap_grid->elev == NULL)
	{

	logMessage ("no memory for hrap array\n");
		exit (1);

	}

	for (k = 0; k < 12; k++)
	{
		ier = is_good (k, smonth, emonth);

		if (ier == -1)
			continue;

		hrap_grid->isoh[k] = (short int **) calloc (maxi, sizeof (short int *));
		hrap_grid->max[k] = (short int **) calloc (maxi, sizeof (short int *));
		hrap_grid->min[k] = (short int **) calloc (maxi, sizeof (short int *));

		if (hrap_grid->isoh[k] == NULL ||
			 hrap_grid->max[k] == NULL || hrap_grid->min[k] == NULL)
		{
		logMessage ("no memory for hrap array\n");
			exit (1);
		}
	}

	for (i = 0; i < maxi; i++)
	{
		hrap_grid->coord[i] = (struct coord *)
			calloc (maxj, sizeof (struct coord));
		hrap_grid->gage[i] = (struct gage *)
			calloc (maxj, sizeof (struct gage));
		hrap_grid->owner[i] = (short int *) calloc (maxj, sizeof (short int));
		hrap_grid->elev[i] = (short int *) calloc (maxj, sizeof (short int));

		if (hrap_grid->coord[i] == NULL ||
			 hrap_grid->gage[i] == NULL ||
			 hrap_grid->owner[i] == NULL || hrap_grid->elev[i] == NULL)
		{
		logMessage ("no memory for hrap array\n");
			exit (1);
		}


		for (k = 0; k < 12; k++)
		{
			ier = is_good (k, smonth, emonth);

			if (ier == -1)
			{
				continue;
			}

			hrap_grid->isoh[k][i] =
				(short int *) calloc (maxj, sizeof (short int));
			hrap_grid->max[k][i] =
				(short int *) calloc (maxj, sizeof (short int));
			hrap_grid->min[k][i] =
				(short int *) calloc (maxj, sizeof (short int));

			if (hrap_grid->isoh[k][i] == NULL ||
				 hrap_grid->max[k][i] == NULL || hrap_grid->min[k][i] == NULL)
			{
			logMessage ("no memory for hrap array\n");
				exit (1);
			}
		}
	}

	newflag = 0;

	/* Retrieve the creation time of the hrap_gage_file. */
	ier = stat (hrap_gage_file, &statbuf);

	/* get creation time of station_list file. 
	 * This is done to determine if the hrap grid file is older
	 * than the station list.  If it is, then the hrap grid file
	 * needs to be regenerated. */
	mer = stat (station_list_file, &statbuf);

	/* Does the grid file exist? */

	if (ier == -1)
	{
		newflag = 2;
	}
	else
	{
		fz = fopen (hrap_gage_file, "r");
		p = fgets (dbuf, 100, fz);

		ier = sscanf (dbuf, "%ld %ld %ld %ld %ld",
			  &ost_mtime, &oxor, &oyor, &omaxx, &omaxy);

		tm = gmtime (&statbuf.st_mtime);

		/* If the station file time and the time stamp in the
		 * hrap grid file are not equal of if the size of the HRAP
		 * forecast area has changed, then recreate the grid file. */
		if (ost_mtime != statbuf.st_mtime ||
			 oxor != XOR || oyor != YOR || omaxx != MAXX || omaxy != MAXY)
		{
			newflag = 1;
		}

		fclose (fz);
		fz = NULL;

	}

	if (newflag == 0)
	{
		/* The grid file is up-to-date.  Open it for reading. */
		fz = fopen (hrap_gage_file, "r");
		p = fgets (dbuf, 100, fz);

	}
	else if (newflag == 1)
	{
		/* The station file is newer than the grid file.  Open
		 * the grid file for update. */
		fz = fopen (hrap_gage_file, "w+");
               
                if ( fz == NULL )
                {
                  logMessage ( "Could not open %s for update.\n",
                            hrap_gage_file );
                   return NULL;
                }
	}
	else
	{
		/* The grid file does not exist.  Create it. */
		fz = fopen (hrap_gage_file, "w");

                if ( fz == NULL )
                {
                  logMessage ( "Could not open %s for writing.\n",
                            hrap_gage_file );
                   return NULL;
                }
	}

	if (newflag != 0)
	{
		/* Recompute the precip gages to HRAP grid. */
	logMessage ("Gage to grid recalculation\n");
		fprintf (fz, "%ld %d %d %d %d\n", statbuf.st_mtime, XOR,
					YOR, MAXX, MAXY);

	}

	for (i = 0; i < hrap_grid->maxi; i++)
	{

		for (k = 0; k < hrap_grid->maxj; k++)
		{
            /* Work in the absolute HRAP Coordinate System. */
            irap.x= ( short ) ( mini + i );
            irap.y= ( short ) ( minj + k );
        
            hrap=HrapToLatLongMpe ( irap );
	
            hrap_grid->coord[i][k].lat=hrap.y; 
	        hrap_grid->coord[i][k].lon=hrap.x; 
	
            hrap_grid->coord[i][k].x=i;
            
            hrap_grid->coord[i][k].y=k; 
            
            /* Initialize the elevation associated this HRAP grid
             * cell. */
			iy = (topo->max_lat - hrap.y) / topo->delta_lat;
			ix = (topo->max_lon - hrap.x) / topo->delta_lon;

			if (topo == NULL || ix <= 0 || iy <= 0 ||
				 ix >= topo->maxi - 1 || iy >= topo->maxj - 1)
			{
				hrap_grid->elev[i][k] = -1;
			}
			else
			{
				distance = 0.0;
				value = 0.0;

				for (kk = ix; kk <= ix + 1; kk++)
				{
					for (jj = iy; jj <= iy + 1; jj++)
					{

						lat = topo->max_lat - (float) jj *topo->delta_lat;
						lon = topo->max_lon - (float) kk *topo->delta_lon;
						dist2 = (hrap.x - lon) * cos ((lat + hrap.y) / 2 * conv);
						dist1 = hrap.y - lat;
						dist = pow (dist1, 2) + pow (dist2, 2);

						if (dist <= 0.000001)
						{
							dist = 0.000001;
						}

						dist = 1 / dist;

						value = value + dist * (float) topo->value[kk][jj];
						distance = distance + dist;

					}
				}

				elevation = (value * 32.1) / distance;
				hrap_grid->elev[i][k] = (int) elevation;

			}

			/* Retrieve easonal isohyet for this point. */

			for (l = 0; l < mpe_dqc_max_precip_neighbors; l++)
			{
				sorted[l] = 9999999;
			}

			if (newflag == 0)
			{
				/* The grid file exists. */
				m = 0;
				fread (kbuf, sizeof (char), 
                                       mpe_dqc_max_precip_neighbors * 5, fz);
				kbuf[mpe_dqc_max_precip_neighbors * 5] = 0;

				for (mm = 0; mm < mpe_dqc_max_precip_neighbors;
                                     mm++)
				{
					/* Initialize the list of closest stations index. */
					hrap_grid->gage[i][k].index[mm] = atoi (&kbuf[m]);
					m = m + 5;
				}

			}
			else
			{
				/* The hrap gage file either does not exist or it is old. */
				for (m = 0; m < num_stations; m++)
				{
					
					/* Loop over the stations. Find the closest neighbors. */
					dist1 = hrap.y - station[m].lat;
					dist2 = (hrap.x - station[m].lon) * cos ((hrap.y +
							station[m].lat) / 2 * conv);
					dist = pow (dist1, 2) + pow (dist2, 2);

					for (l = 0; 
                                             l < mpe_dqc_max_precip_neighbors; 
                                             l++)
					{
						if (dist < sorted[l])
						{
							for (h = mpe_dqc_max_precip_neighbors - 1; h > l; h--)
							{
								sorted[h] = sorted[h - 1];
								hrap_grid->gage[i][k].index[h] =
									hrap_grid->gage[i][k].index[h - 1];
							}

							sorted[l] = dist;
							hrap_grid->gage[i][k].index[l] = m;

							break;

						}

					}

				}

                /* For this station write the closest neighbors to the grid file. */
				for (l = 0; l < mpe_dqc_max_precip_neighbors; 
                                     l++)
				{
					fprintf (fz, "%04d ", hrap_grid->gage[i][k].index[l]);
				}

			}

            /* For this HRAP grid cell, load the monthly mean precipitation and max/min temperature information. */
			for (h = 0; h < 12; ++h)
			{
				ier = is_good (h, smonth, emonth);

				if (ier == -1)
				{
					continue;
				}

				hrap_grid->isoh[h][i][k] = -1;
				hrap_grid->max[h][i][k] = -9999;
				hrap_grid->min[h][i][k] = -9999;

				if ( isohyets_used != -1 )
				{
                    /* The precipitation PRISM data have been read in. */
                    /* Much of this code will go away.  The PRISM data
                     * are already in HRAP format. */
				   hrap_grid->isoh[h][i][k] = isoh->value[h][k][i];
				}

               
				if ( maxmin_used != -1 )
				{
					/* The max/min temperature PRISM data are available. */
					hrap_grid->max[h][i][k] = maxmin->maxvalue [h][k][i];
			   	    hrap_grid->min[h][i][k] = maxmin->minvalue [h][k][i];

				}


			}

		}

	}

	fclose (fz);
	fz = NULL;
	return hrap_grid;

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
struct hrap_grid * get_hrap_grid ( )
{
    return hrap_grid;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/map_precip_gages_to_grid.c,v $";
 static char rcs_id2[] = "$Id: map_precip_gages_to_grid.c,v 1.3 2007/10/18 17:18:15 lawrence Exp $";}
/*  ===================================================  */

}

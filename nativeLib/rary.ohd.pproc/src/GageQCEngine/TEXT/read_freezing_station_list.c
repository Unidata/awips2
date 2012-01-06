#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "GeneralUtil.h"
#include "mpe_log_utils.h"

int max_zstations = 0;

struct station *zstation = NULL;

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   read_freezing_station_list
* PURPOSE:       Reads the list of HZI stations from the station list file.
*                Dynamically allocates an array of station structures to
*                contain the read-in station data.  This array must be
*                deallocated by the caller of this routine.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Output int *       num_gages      The number of TAI stations read from the
*                                     station list file.
*   Input  char *      area_name      The name of the area to read the station
*                                     list for.  If NULL, then it defaults
*                                     to the master station list.
*   Input  int         master_flag    1 - read from the master file, skip PPH
*                                         section.
*                                     0 - not reading from master file ...
*                                         there is no PPH section.
*
* RETURNS:
*   DATA TYPE            DESCRIPTION
*   struct station *     Dynamically allocated array of freezing level station
*                        data.
*
* APIs UTILIZED:
*   NAME                       HEADER FILE     DESCRIPTION
*   get_station_list_path      gageqc_types.h  Retrieves the station path.
*   logMessage                 gageqc_types.h  Writes to the log file.
*
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE    NAME                         DESCRIPTION
*   char *       fname
*   char         message[]
*   char         zstation_custom_file[]
*   struct dval  dval
*   int          i
*   int          ier
*   int          sflag
*   int          field
*   int          j
*   int          xadd
*   int          yadd
*   int          index
*   FILE *       fp
*   char *       p
*   char         kbuf[]
*   char         hb5[]
*   char         parm[]
*   int          record_num
*   int          num_records
*   int          status
*   float        lat
*   float        lon
*   float        conv
*   long         t
*   long         u
*   float        elev
*
* DATA FILES AND/OR DATABASE:
* Relies on the existence of the station file.
* The path to this file is given by token mpe_station_list_dir.
* The name of the file is built as <area_name>_station_list.  If area_name
* is NULL, then the name of the file is <mpe_site_id>_station_list where
* mpe_site_id is a token.
*
* ERROR HANDLING:
*    ERROR CODE                        DESCRIPTION
*    NULL                              No station list information could be
*                                      read.
********************************************************************************
*/
struct station * read_freezing_station_list (int *num_gages,
			                     const char *area_name, 
                                             int master_flag)
{
   const char *fname = NULL;
   char message[LOG_MESSAGE_LEN];
   extern char zstation_list_custom_file[];
   extern struct dval dval;
   int i, ier, sflag, field, j, xadd, yadd;
   int index;
   FILE *fp;
   char *p, kbuf[MAX_STATION_RECORD_LEN], hb5[10], parm[10];
   HRAP hrap_point;
   int record_num = 1;
   int num_records;
   int space_test;
   int status;
   float lat, lon;
   float conv = .0174;
   long t, u;
   float elev;

   max_zstations = 0;
   *num_gages = 0;

   fname = get_station_list_path (area_name);

   if (fname == NULL)
   {
      sprintf (message, "No path or name defined for the station list.\n");
      logMessage (message);
      return NULL;
   }

   fp = fopen (fname, "r");

   memset (message, '\0', LOG_MESSAGE_LEN);

   if (fp == NULL)
   {
     logMessage ("could not open %s\n", fname);
      sprintf (message, "Could not open file: %s\n", fname);
      logMessage (message);
      return NULL;
   }
   else
   {
      sprintf (message, "Opened file: %s\n", fname);
      logMessage (message);
   }

   if (master_flag == 1)
   {
      /* Skip the PPH data section. */
      i = 0;

      /* Read the number of PPH records in the file. */
      p = fgets (kbuf, MAX_STATION_RECORD_LEN, fp);

      if (p == NULL)
      {
	 sprintf (message, "Reached EOF while reading first record of\n"
		  "file %s\n", fname);
	logMessage ("%s", message);
	 logMessage (message);
	 fclose (fp);
	 return NULL;
      }

      ier = sscanf (kbuf, "%d", &num_records);

      if (ier != 1)
      {
	 sprintf (message, "Could not read the number of 1 hour gages from\n"
		  "%s record %d\n", fname, record_num);
	logMessage ("%s", message);
	 logMessage (message);
	 fclose (fp);
	 return NULL;
      }

      ++record_num;

      /* Need to skip the PPH records. */
      /* Has this already been done?  If so, then use the fseek routine. */
      for (i = 0; i < num_records; ++i)
      {
	 p = fgets (kbuf, MAX_STATION_RECORD_LEN, fp);

	 if (p == NULL)
	 {
	    sprintf (message, "Reached EOF while attempting to read "
		     "record %d in file %s\n", record_num, fname);
	    logMessage (message);
	    fclose (fp);
	    return NULL;
	 }

	 ++record_num;
      }
   }

   /* Read the record containing the number of PPD stations. */
   p = fgets (kbuf, MAX_STATION_RECORD_LEN, fp);

   if (p == NULL)
   {
      sprintf (message, "Reached EOF while reading record %d of\n"
	       "file %s\n", record_num, fname);
     logMessage ("%s", message);
      logMessage (message);
      fclose (fp);
      return NULL;
   }

   ier = sscanf (kbuf, "%d", &num_records);

   if (ier != 1)
   {
      sprintf (message, "Could not read the number of 1 hour gages from\n"
	       "%s record %d\n", fname, record_num);
     logMessage ("%s", message);
      logMessage (message);
      fclose (fp);
      return NULL;
   }

   ++record_num;

   /* Skip the PPD records. */
   /* Has this already been done?  If so, then use the fseek routine. */
   for (i = 0; i < num_records; ++i)
   {
      p = fgets (kbuf, MAX_STATION_RECORD_LEN, fp);

      if (p == NULL)
      {
	 sprintf (message, "Reached EOF while attempting to read "
		  "record %d in file %s\n", record_num, fname);
	 logMessage (message);
	 fclose (fp);
	 return NULL;
      }

      ++record_num;
   }

   /* Read the number of TAI stations. */
   /* Read the record containing the number of PPD stations. */
   p = fgets (kbuf, MAX_STATION_RECORD_LEN, fp);

   if (p == NULL)
   {
      sprintf (message, "Reached EOF while reading record %d of\n"
	       "file %s\n", record_num, fname);
     logMessage ("%s", message);
      logMessage (message);
      fclose (fp);
      return NULL;
   }

   ier = sscanf (kbuf, "%d", &num_records);

   if (ier != 1)
   {
      sprintf (message, "Could not read the number of temperature gages\n"
	       "from %s record %d\n", fname, record_num);
     logMessage ("%s", message);
      logMessage (message);
      fclose (fp);
      return NULL;
   }

   ++record_num;

   /* Skip the temperature records. */
   /* Has this already been done?  If so, then use the fseek routine. */
   for (i = 0; i < num_records; ++i)
   {
      p = fgets (kbuf, MAX_STATION_RECORD_LEN, fp);

      if (p == NULL)
      {
	 sprintf (message, "Reached EOF while attempting to read "
		  "record %d in file %s\n", record_num, fname);
	 logMessage (message);
	 fclose (fp);
	 return NULL;
      }

      ++record_num;
   }

   /* Read the number of freezing level gages. */
   p = fgets (kbuf, MAX_STATION_RECORD_LEN, fp);

   if (p == NULL)
   {
      sprintf (message, "Reached EOF while reading record %d of\n"
	       "file %s\n", record_num, fname);
     logMessage ("%s", message);
      logMessage (message);
      fclose (fp);
      return NULL;
   }

   ier = sscanf (kbuf, "%d", &num_records);

   if (ier != 1)
   {
      sprintf (message, "Could not read the number of freezing level gages\n"
	       "from %s record %d\n", fname, record_num);
     logMessage ("%s", message);
      logMessage (message);
      fclose (fp);
      return NULL;
   }

   /* Allocate space for the array of freezing level station structures. */
   zstation = (struct station *) malloc (num_records *
					 sizeof (struct station));

   if (zstation == NULL)
   {
      sprintf (message, "Could not allocate memory for the array of "
	       "struct stations for freezing level stations.\n");
      logMessage (message);
   }

   for (i = 0; i < num_records; ++i)
   {
      p = fgets (kbuf, MAX_STATION_RECORD_LEN, fp);

      if (p == NULL)
      {
	 sprintf (message, "Reached EOF while reading record %d of\n"
		  "file %s\n", record_num, fname);
	 logMessage (message);
	 break;
      }


      zstation[i].hb5 = calloc (10, sizeof (char));
      zstation[i].name = calloc (50, sizeof (char));
      zstation[i].parm = calloc (10, sizeof (char));
      zstation[i].xadd = 0;
      zstation[i].yadd = 0;

      ier = sscanf (kbuf, "%s %s %f %f %f %d ", zstation[i].hb5,
		    zstation[i].parm, &zstation[i].lat,
		    &zstation[i].lon, &elev, &zstation[i].tip);

      if (ier != 6)
      {
	 sprintf (message, "Incomplete record. File %s record %d.\n",
		  fname, record_num);
	 continue;
      }

      ++record_num;

      lat = zstation[i].lat;
      lon = zstation[i].lon;

      /* Store the station's coordinates in HRAP. */
      hrap_point = LatLongToHrapMpe (lat, lon);

      zstation[i].hrap_x = hrap_point.x;
      zstation[i].hrap_y = hrap_point.y;

      t = dval.a * cos (lat * conv) / (1 + sin (lat * conv))
	 * cos ((lon - dval.lo - 90) * conv) + dval.xo + .5;

      zstation[i].x = t;

      u = dval.a * cos (lat * conv) / (1 + sin (lat * conv))
	 * sin ((lon - dval.lo - 90) * conv) + dval.yo + .5;

      zstation[i].y = u;

      sflag = 1;
      field = 0;
      for (j = 0; j < strlen (kbuf); j++)
      {

         space_test = isspace ( kbuf[j] );

	 if ( ( space_test == 0 )  && ( sflag == 1) )
	 {
	    sflag = 0;
	    field++;

	 }

	 else if ( space_test != 0 )
         {
	    sflag = 1;
         }

	 if (field == 7)
	 {

	    kbuf[j + 49] = 0;

	    /* Remove any trailing white space characters. */
	    strip_tblanks (&kbuf[j]);
	    strcpy (zstation[i].name, &kbuf[j]);
	    break;

	 }

      }

   }

   max_zstations = i;
   *num_gages = max_zstations;

   fclose (fp);
   fp = NULL;

   /* Read the freezing level custom file */
   /* Open the custom station position file. */
   sprintf (zstation_list_custom_file, "%s_label_position", fname);

   /* Read in the custom positioning information for the temperature
      stations. */
   record_num = 0;
   sprintf (zstation_list_custom_file, "%s.custom", fname);
   fp = fopen (zstation_list_custom_file, "r");

   if (fp != NULL)
   {
      memset (message, '\0', 150);
      sprintf (message, "Opened file: %s\n", zstation_list_custom_file);
      logMessage (message);

      index = 0;

      p = fgets (kbuf, 80, fp);
      while ((p != NULL) && (index < max_zstations))
      {
	 ++record_num;

	 ier = sscanf (kbuf, "%s %s %d %d", hb5, parm, &xadd, &yadd);
	 if (ier != 4)
	 {
	    sprintf (message, "In file %s, record number %d is incomplete. "
		     "This record is being skipped.\n",
		     zstation_list_custom_file, record_num);
	    p = fgets (kbuf, MAX_STATION_RECORD_LEN, fp);
	    continue;
	 }

	 status = strcmp (hb5, zstation[index].hb5);

	 if (status < 0)
	 {
	    /* Read the next record from the custom climo file. */
	    p = fgets (kbuf, 80, fp);
	 }
	 else if (status > 0)
	 {
	    /* increment the index in the precipitation station array. */
	    ++index;
	 }
	 else
	 {
	    if (strncmp (parm, zstation[index].parm, 3) == 0 &&
		parm[4] == zstation[index].parm[4])
	    {
	       zstation[index].xadd = xadd;
	       zstation[index].yadd = yadd;
	    }

	    ++index;
	 }
      }

      fclose (fp);
      fp = NULL;

   }
   else
   {
      memset (message, '\0', LOG_MESSAGE_LEN);
      sprintf (message, "Could not open custom freezing station file: %s\n",
	       zstation_list_custom_file);
      logMessage (message);
   }

   return zstation;
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
struct station *
get_freezing_station_list (int *num_gages)
{
   *num_gages = max_zstations;
   return zstation;
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
void free_freezing_station_list ()
{
   int i;

   if (zstation != NULL)
   {
      for ( i = 0; i < max_zstations; ++i )
      {
         free ( zstation[i].hb5 );
         free ( zstation[i].name );
         free ( zstation[i].parm );
      }

      free (zstation);
      zstation = NULL;
      max_zstations = 0;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

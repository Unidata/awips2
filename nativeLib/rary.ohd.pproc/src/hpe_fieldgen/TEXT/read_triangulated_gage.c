/*******************************************************************************
* FILENAME:            read_triangulated_gage.c
*
* Purpose:
*
* This file has routines from the p3_util library. it has routines that calculate the gage triangles    
* and stores as a structure.
*
*
* calling function: p3_lmosaic
* functions called: 
*
* input variables
* 
* output variables
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   August 2005   Ram Varma         ABRFC P3 mosaic algorithm 
**
********************************************************************************
*/

#include "empe_fieldgen.h"
#include "p3.h"

/* This file has routines from the p3_util library. it has routines that calculate the gage triangles    
*and stores as a structure.
*/

/* Global variables. */
p3_gage_struct gages[5000];
long int numofgages;
TRIANGLE *tg = NULL; 
TRIANGLE *tr = NULL;
long numpseudos;
long numofradtri;
long numofgagetri;
long int howmany = 0;

extern int HRAP_X_p3;
extern int HRAP_Y_p3;
extern int HRAP_XOR_p3;
extern int HRAP_YOR_p3;

static char rfcwide_utiltriangles_directory[150];
static int get_apps_defaults_utiltriangles = 0;

/*************************************************************************************/

//this function is called from the p3_lmosaic.c file. this calculates the gage triangles

/***********************************************************************
* Purpose:
* calculates the gage triangles
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm 
*
***********************************************************************/

void
triangulategage ()
{
   double reference_lat, reference_lon;

   reference_lon = gages[0].lon;
   reference_lat = gages[0].lat;


   ll_to_mi (gages, numofgages, reference_lon, reference_lat);
   get_contig (gages, numofgages);
   //printf("done getting contigs....\n");
   tg = calculate_triangles (gages, numofgages, &numofgagetri, tg);
   mi_to_ll (gages, numofgages, reference_lon, reference_lat);

   printouttrifile ("gage_triangles");
}

/***************************************************************************************/

/***********************************************************************
* Purpose:
* allocates memory
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm 
*
***********************************************************************/


int init_tr_mem ()
{
   int ret = 1;
   howmany = HRAP_X_p3 * HRAP_Y_p3 * 2;
   if (tr == NULL)
   {
      tr = (TRIANGLE *) malloc (howmany * (sizeof (TRIANGLE)));
   }
   if (tr == NULL)
   {
      sprintf (message, "memory allocation problem..\n");
      hpe_fieldgen_printMessage(message );
      ret = -1;
   }
   return ret;
}


//this routine reads in the triangles file created by the p3_util library.

/***********************************************************************
* Purpose: reads in the triangles file created by the p3_util library
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm 
*
***********************************************************************/


int readintriangles ()
{
   int counter, len;
   char pdir[150], fname[150];
   FILE *trianglefile;
   int ret = 1;
   //TRIANGLE   *temp = NULL;

   counter = 0;
   if (get_apps_defaults_utiltriangles == 0)
   {
      len = strlen ("rfcwide_utiltriangles_dir");
      get_apps_defaults ("rfcwide_utiltriangles_dir", &len, pdir, &len);
      bzero (rfcwide_utiltriangles_directory, 150);
      strcpy (rfcwide_utiltriangles_directory, pdir);
      get_apps_defaults_utiltriangles = 1;
   }
   else
   {
      bzero (pdir, 150);
      strcpy (pdir, rfcwide_utiltriangles_directory);
   }
   sprintf (fname, "%s/utiltriangles", pdir);
   trianglefile = fopen (fname, "rb");
   if (trianglefile == NULL)
   {
      sprintf (message, "Error: Can't open file: %s\n", fname);
      hpe_fieldgen_printMessage( message);
      ret = -1;
   }
   ret = init_tr_mem ();
   while (!feof (trianglefile))
   {
      fread (&tr[counter].a, sizeof (long), 1, trianglefile);
      fread (&tr[counter].b, sizeof (long), 1, trianglefile);
      fread (&tr[counter].c, sizeof (long), 1, trianglefile);
      fread (&tr[counter].a_to_b, sizeof (long), 1, trianglefile);
      fread (&tr[counter].b_to_c, sizeof (long), 1, trianglefile);
      fread (&tr[counter].c_to_a, sizeof (long), 1, trianglefile);

      counter++;
   }
   fclose (trianglefile);
   numofradtri = counter - 1;
   //tr = temp;
   if (tr == NULL)
   {
      sprintf (message, "fatal memory allocation problem..\n");
      hpe_fieldgen_printMessage( message);
      ret = -1;
   }
   return ret;
}

/****************************************************************************************/

//reads the radar contigs file created by the p3_util library.

/***********************************************************************
* Purpose: reads the radar contigs file created by the p3_util library
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm 
*
***********************************************************************/


int readradarcont ()
{
   FILE *radarconfile;
   int i, j, k, counter;
   int len;
   char pdir[150], fname[150];

   counter = 0;
   if (get_apps_defaults_utiltriangles == 0)
   {
      len = strlen ("rfcwide_utiltriangles_dir");
      get_apps_defaults ("rfcwide_utiltriangles_dir", &len, pdir, &len);
      bzero (rfcwide_utiltriangles_directory, 150);
      strcpy (rfcwide_utiltriangles_directory, pdir);
      get_apps_defaults_utiltriangles = 1;
   }
   else
   {
      bzero (pdir, 150);
      strcpy (pdir, rfcwide_utiltriangles_directory);
   }
   memset (fname, '\0', 150);
   sprintf (fname, "%s/radarconfile", pdir);
   radarconfile = fopen (fname, "rb");
   if (radarconfile == NULL)
   {
      sprintf (message, "Error: Can't open file: %s\n", fname);
      hpe_fieldgen_printMessage( message);
      return -1;
   }

   radarpts =
      (p3_gage_struct *) malloc (sizeof (p3_gage_struct) * HRAP_X_p3 *
				 HRAP_Y_p3);

   for (i = 0; i < HRAP_X_p3; i++)
   {
      for (j = 0; j < HRAP_Y_p3; j++)
      {
	 fread (&radarpts[counter].lat, sizeof (double), 1, radarconfile);
	 fread (&radarpts[counter].lon, sizeof (double), 1, radarconfile);
	 //printf("lat = %lf, lon = %lf\n",radarpts[counter].lat,radarpts[counter].lon);
	 fread (&radarpts[counter].n_contig, sizeof (long), 1, radarconfile);
	 radarpts[counter].contig =
	    (long *) malloc (radarpts[counter].n_contig * sizeof (long));
	 for (k = 0; k < radarpts[counter].n_contig; k++)
	 {
	    fread (&radarpts[counter].contig[k], sizeof (long), 1,
		   radarconfile);
	 }
	 counter++;
      }
   }
   fclose (radarconfile);
   numofradarpts = counter;
   return 1;
}

/***************************************************************************************/
//this function is called by p3_lmosaic routine to read in the triangles and contigs file
//created by the p3_util library.

/***********************************************************************
* Purpose: 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm 
*
***********************************************************************/


int readradartriangles ()
{
   double reference_lat, reference_lon;

   int ret = 1;
   ret = readradarcont ();
   reference_lon = radarpts[0].lon;
   reference_lat = radarpts[0].lat;
   ll_to_mi (radarpts, numofradarpts, reference_lon, reference_lat);
   ret = readintriangles ();
   mi_to_ll (radarpts, numofradarpts, reference_lon, reference_lat);
   return ret;
}


/***********************************************************************
* Purpose: free memory
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm 
*
***********************************************************************/


void
free_gages ()
{
   int i;

   for (i = 0; i < numofgages; ++i)
   {
      if (gages[i].contig != NULL)
      {
	 free (gages[i].contig);
	 gages[i].contig = NULL;
      }
   }
}


/***********************************************************************
* Purpose: free memory
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm 
*
***********************************************************************/


void
free_tr ()
{
   if (tr != NULL)
   {
      free (tr);
      tr = NULL;
   }
}

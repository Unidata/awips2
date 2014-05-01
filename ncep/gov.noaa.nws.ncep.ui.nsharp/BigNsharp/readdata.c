
/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  Sounding Data Assimilation Routine                         */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "sharp95.h"

	/*NP*/
	short read_sndg1( void )
	/*************************************************************/
	/*  READ_SNDG1                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine will read the input file and store          */
	/*  the necessary sounding information in the array SNDG.    */
	/*  NOTE:  Array starts at ZERO for surface data.            */
	/*                                                           */
	/*  Input filename is read from configuration structure.     */
	/*                                                           */
	/*  Routines have also been included that will extend a      */
	/*  complete sounding above 100mb, assuming an isothermal    */
	/*  layer and using a hydrostatic routine for the heights.   */
	/*                                                           */
	/*  Return Codes:  0 = No Errors, array stored               */
	/*                 1 = Input File Not Found                  */
	/*                 2 = Error Reading File                    */
	/*************************************************************/
	{
	FILE *fp;
	short ok, i, aok, nparms = 0;
	int stype, newlev, ind, j;
	float x1, x2, x3, x4, x5, x6, snd[150][6];
	char rlin[82], fs[25], parms[128], **parmlist = NULL;
	Sounding  *s = NULL;

	printf( "[READ_SNDG1] - Starting\n");

	/* ----- Open File if it exists ----- */
	fp = fopen( config.filename, "r" );
	if (fp)
	   {

	   /* ----- Read Title Line ----- */
	   while(fgets( rlin, 80, fp ) != 0)
	     {
	     if(strstr( rlin, "%TITLE%" ) != 0)
		{
		fgets( raobtitle, 80, fp );
		break;
		}
	     }

	   printf( "Got Herex: %s\n", raobtitle);

	   /* ----- Look for RAW data ----- */
	   while(fgets( rlin, 80, fp ) != 0)
	     {
	     if(strstr( rlin, "%RAW%" ) != 0)
		{
		i=0;
		strcpy( fs, "%f,%f,%f,%f,%f,%f" );
		while ((fgets( rlin, 80, fp) != 0) && (strstr(rlin, "%END%") == 0))
			{
			printf( "Before---> %s", rlin);
			j = sscanf( rlin, "%f,%f,%f,%f,%f,%f", &snd[i][0], &snd[i][1], 
				    &snd[i][2], &snd[i][3], &snd[i][4], &snd[i][5] ); 
			printf( "After ---> %7.1f, %7.1f\n", snd[i][1], snd[i][2]);
			i++;
			}
		}
	     }

	   newlev = i;
	   fclose(fp);

	   stype = SNDG_ARCH;
	   strcpy(parms, "PRES;HGHT;TEMP;DWPT;DRCT;SPED");
	   parmlist = defineParms(parms, &nparms);
	   s = newSounding(nparms, newlev);
	
	   s->parms    = parmlist;
           s->nparms   = nparms;
           s->datatype = stype;
           s->nlev     = newlev;
           s->noriglev = s->nlev;
           strcpy(s->stid, "ARCH");
           strcpy(s->dattim, "ARCH");

           /* Populate */
           for (i=0;i<newlev;i++) 
	     {
             for (j=0;j<nparms;j++) 
		{
                s->data[i][j]     = snd[i][j];
                s->origdata[i][j] = snd[i][j];
          	}
             }

	   changeGlobalSounding(s);

           xtnd_sndg();

           /* Reset levels in sounding since the global var numlvl is updated in xtnd_sndg() */
           s->nlev     = numlvl;
           s->noriglev = numlvl;

           history_add(&hist, s);

	   return 0;
	   }
	return 1;
	}



	/*NP*/
	short read_config( void )
	/*************************************************************/
	/*  READ_CONFIG                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine will read the SHARP.CFG file and store      */
	/*  the values into the CONFIG struct.                       */
	/*                                                           */
	/*  Return Codes:  0 = No Errors, values stored              */
	/*                 1 = File 'SHARP.CFG' Not Found            */
	/*                 2 = Error Reading File                    */
	/*************************************************************/
	{
	FILE *fp;
	short i;
	char ch1[82], ch2[42];

	/* ----- Open File if it exists ----- */
	fp = fopen( "sharp.cfg", "r" );
	if (!fp) { return 1; }

	/* ----- Read Title Lines ----- */
	for( i=1; i<8; i++ )
	   { fgets( ch1, 80, fp ); }

	/* ----- Now read configuration data into structure ----- */

	/* FILENAME */
	fgets( ch1, 40, fp ); fgets( ch2, 40, fp );
	strncpy( config.filename, ch2, strlen(ch2) - 1);
	printf( "Default Sounding Data File:  %s\n", config.filename);

	/* PRINTER TYPE */
	fgets( ch1, 40, fp ); fgets( ch2, 40, fp );
	strncpy( config.prntype, ch2, strlen(ch2) - 1);
	printf( "Default Printer Type:  %s\n", config.prntype);

	/* PRINTER PORT */
	fgets( ch1, 40, fp ); fgets( ch2, 40, fp );
	strncpy( config.lptname, ch2, strlen(ch2) - 1);
	printf( "Default Printer Port:  %s\n", config.lptname);

	fclose(fp);
	return 0;
	}



	/*NP*/
	void read_command( int argc, char *argv[], char *envp[] )
	/*************************************************************/
	/*  READ_COMMAND                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine will read the SHARP command line and        */
	/*  set the program variables as requested.                  */
	/*************************************************************/
	{
	if( argc > 0 && strlen(argv[1]) > 0)
	   {
	   strncpy( config.filename, argv[1], strlen(argv[1]));
	   config.filename[strlen(argv[1])] = 0;
	   }
	}

	/*NP*/
	short get_sndg( void )
	/*************************************************************/
	/*  GET_SNDG                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine will read the input file and determine      */
	/*  which type of file to read.  The following sounding      */
	/*  file types are available to be read.                     */
	/*                                                           */
	/*  1)  SHARP v2.0 File Format                               */
	/*  2)  SHARP v1.5 File Format                               */
	/*                                                           */
	/*  Return Codes:  0 = No Errors, array stored               */
	/*                 1 = Unknown Sounding File Format          */
	/*                 2 = Error Reading File                    */
	/*************************************************************/
	{
	FILE *fp;
	short ok, ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, i;
	char rlin[82];

	/* ----- Open File if it exists ----- */
	fp = fopen( config.filename, "r" );
	if (fp)
	   {
	   ok1 = 0;
	   ok2 = 0;
	   ok3 = 0;
	   ok4 = 0;
	   ok5 = 0;
	   ok6 = 0;
	   ok7 = 0;
	   ok8 = 0;
	   ok9 = 0;

	   while(fgets( rlin, 80, fp ) != 0)
	      {
	      /* ----- Search for SHARP v2.0 File Format ----- */
	      if( strstr( rlin, "%TITLE%" ))  { ok1 = 1; }
	      if( strstr( rlin, "%RAW%" ))    { ok2 = 1; }

	      /* ----- Search for SHARP v1.0 File Format ----- */
	      if( strstr( rlin, "WETBULB" ))  { ok3 = 1; }
	      if( strstr( rlin, "RAW" ))      { ok4 = 1; }
	      if( strstr( rlin, "SMOOTHED" )) { ok5 = 1; }

	      /* ----- Search for RAW WMO coded Format ----- */
	      if( strstr( rlin, "TTAA " ))  { ok6 = 1; }
	      if( strstr( rlin, "TTBB " ))  { ok7 = 1; }
	      if( strstr( rlin, "PPBB " ))  { ok8 = 1; }

	      /* ----- Search for another NSHARP txt File Format ----- */
	      if( strstr( rlin, "DWPT      DRCT      SPED")) {ok9 = 1; }

	      }
	   fclose(fp);
	   }
	else
	   { return 2; }

	printf( "%d   %d   %d   %d   %d\n", ok1, ok2, ok3, ok4, ok5);
	if(ok1 && ok2)        { return read_sndg1(); }

	if(ok3 && ok4 && ok5) { return read_sndg3( "RAW" ); }

	if(ok3 && ok5)        { return read_sndg3( "SMOOTHED" ); }

	if(ok6 && ok7)        { return decode_sounding( config.filename ); }

	if(ok9)               { return read_sndg2(); }

	printf( "Unknown sounding file format.  Read aborted.\n" );
	return 1;
	}

	/*NP*/
	short read_sndg2( void )
	/*************************************************************/
	/*  READ_SNDG2                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine will read the SHARP v1.5 input file and     */
	/*  store the necessary sounding information in the array    */
	/*  SNDG.                                                    */
	/*                                                           */
	/*  NOTE:  Array starts at ZERO for surface data.            */
	/*                                                           */
	/*  Input filename is read from configuration structure.     */
	/*                                                           */
	/*  Routines have also been included that will extend a      */
	/*  complete sounding above 100mb, assuming an isothermal    */
	/*  layer and using a hydrostatic routine for the heights.   */
	/*                                                           */
	/*  Return Codes:  0 = No Errors, array stored               */
	/*                 1 = Input File Not Found                  */
	/*                 2 = Error Reading File                    */
	/*************************************************************/
	{
        FILE *fp;
        short ok, i, aok, nparms = 0;
        int stype, newlev, ind, j;
        float x1, x2, x3, x4, x5, x6, snd[150][6];
        char rlin[82], fs[25], parms[128], **parmlist = NULL;
        Sounding  *s = NULL;

	/* ----- Open File if it exists ----- */
	fp = fopen( config.filename, "r" );
	if (fp)
	   {
	   /* ----- Set Title Line ----- */
	   strcpy( raobtitle, config.filename );

	   /* ----- Read data ----- */
	   printf("[SNDG2] Reading sounding\n");
	   fgets(rlin, 80, fp );
	   strcpy( fs, "  %f %f %f %f %f %f\n" );
	   i=0;
	   while(fscanf( fp, fs, &sndg[i][1], &sndg[i][2], &sndg[i][3], &sndg[i][4], &sndg[i][5], &sndg[i][6] ) != NULL)
	     {
	     printf("#%d   %.0f   %.0f   %.1f   %.1f    %.0f   %.0f\n", i, sndg[i][1], sndg[i][2], sndg[i][3], sndg[i][4], sndg[i][5], sndg[i][6]); 
	     i++;
	     }

	   numlvl = i;
	   fclose(fp);
	   printf("[SNDG2] Finished reading sounding\n");

           stype = SNDG_ARCH;
	   strcpy(parms, "PRES;HGHT;TEMP;DWPT;DRCT;SPED");
	   parmlist = defineParms(parms, &nparms);
	   s = newSounding(nparms, newlev);

	   s->nparms   = nparms;
	   s->datatype = stype;
	   s->nlev     = newlev;
	   s->noriglev = s->nlev;
	   /*
	   s->parms    = parmlist;
	   strcpy(s->stid, "ARCH");
	   strcpy(s->dattim, "ARCH");

	   for (i=0;i<newlev;i++)
	       {
	       for (j=0;j<nparms;j++)
	            {
		    s->data[i][j]     = snd[i][j];
		    s->origdata[i][j] = snd[i][j];
		    }
	       }

           changeGlobalSounding(s);

           xtnd_sndg();
           s->nlev     = numlvl;
           s->noriglev = numlvl;

           history_add(&hist, s);
	   */

           return 0;
           }
        return 1;
	}

	/*NP*/
	short read_sndg3( char *searchtag )
	/*************************************************************/
	/*  READ_SNDG2                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine will read the SHARP v1.5 input file and     */
	/*  store the necessary sounding information in the array    */
	/*  SNDG.                                                    */
	/*                                                           */
	/*  NOTE:  Array starts at ZERO for surface data.            */
	/*                                                           */
	/*  Input filename is read from configuration structure.     */
	/*                                                           */
	/*  Routines have also been included that will extend a      */
	/*  complete sounding above 100mb, assuming an isothermal    */
	/*  layer and using a hydrostatic routine for the heights.   */
	/*                                                           */
	/*  Return Codes:  0 = No Errors, array stored               */
	/*                 1 = Input File Not Found                  */
	/*                 2 = Error Reading File                    */
	/*************************************************************/
	{
	FILE *fp;
	int stype, newlev, ind, j;
	short ok, i, aok, nparms = 0;
	float x1, x2, x3, x4, x5, snd[150][6];
	char rlin[82], fs[25], parms[128], **parmlist = NULL;
	Sounding  *s = NULL;

	/* ----- Open File if it exists ----- */
	fp = fopen( config.filename, "r" );
	if (fp)
	   {
	   /* ----- Read Title Line ----- */
	   fgets( raobtitle, 40, fp );

	   /* ----- Look for RAW data ----- */
	   while(fgets( rlin, 80, fp ) != NULL)
	     {
	     if(strstr( rlin, searchtag ) != 0)
		{
		i=0;
		strcpy( fs, "%f,%f,%f,%f,%f,%f,%f,%f" );
		while (fscanf( fp, fs, &sndg[i][1], &sndg[i][2], &sndg[i][3],
		&sndg[i][4], &x1, &x2, &sndg[i][5], &sndg[i][6] ) != 0) {i++;}
		}
	     numlvl = i;

	     /* ----- Look for STORM MOTION data ----- */
	     if(strstr( rlin, "STORM" ) != 0)
		{
		fscanf( fp, "%f, %f", &st_dir, &st_spd);
		printf( "Storm Motion = %3.0f/%3.0f\n", st_dir, st_spd);
		}
	     }
	   fclose(fp);

           stype = SNDG_ARCH;
	   strcpy(parms, "PRES;HGHT;TEMP;DWPT;DRCT;SPED");
	   parmlist = defineParms(parms, &nparms);
	   s = newSounding(nparms, newlev);

	   s->parms    = parmlist;
	   s->nparms   = nparms;
	   s->datatype = stype;
	   s->nlev     = newlev;
	   s->noriglev = s->nlev;
	   strcpy(s->stid, "ARCH");
	   strcpy(s->dattim, "ARCH");

           /* Populate */
	   for (i=0;i<newlev;i++)
	       {
	       for (j=0;j<nparms;j++)
	            {
		    s->data[i][j]     = snd[i][j];
		    s->origdata[i][j] = snd[i][j];
		    }
	       }

           changeGlobalSounding(s);

           xtnd_sndg();

           /* Reset levels in sounding since the global var numlvl is updated in xtnd_sndg() */
           s->nlev     = numlvl;
           s->noriglev = numlvl;

           history_add(&hist, s);

           return 0;
           }
        return 1;
	}


	/*NP*/
	void security(char *envp[] )
	/*************************************************************/
	/*  SECURITY                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine checks for clearance by programmer.         */
	/*************************************************************/
	{
	short i, cleared;

	/* ----- Search for necessary parameter in DOS ----- */
	i = 1 ;
	cleared = 0;
	while( envp[i] != NULL )
	   {
	   if( strstr( envp[i], "SHARPV2" ) != 0 ) { cleared = 1; }
	   i++;
	   }

	if ( cleared != 0 ) { return; }

	exit(0);
	}

void save_origsndg(void)
/*******************************************************/
/*  SAVE_ORIGSNDG				       */
/*******************************************************/
{
	int i, j;

	origlvl = numlvl;

	for (i=0; i < numlvl; i++ ) {
	  for ( j=0; j < 7; j++ ) {
	    origsndg[i][j] = sndg[i][j];
	  }
	}
}


void restore_origsndg(void)
/*******************************************************/
/*  RESTORE_ORIGSNDG				       */
/*******************************************************/
{
	int i, j;

	numlvl = origlvl;
	for (i=0; i < numlvl; i++ ) {
	  for ( j=0; j < 7; j++ ) {
	    sndg[i][j] = origsndg[i][j];
	  }
	}
}


	/*NP*/
void copy_sndg(void)
	/*******************************************************/
	/*  COPY_SNDG					       */
	/*******************************************************/
{
	short i,j;

	for (i=0; i< numlvl; i++) {
	  for (j=0;j<7;j++) {
	    sndg2[i][j] = sndg[i][j];
	  }
	}
}

/* Moved here by mkay 10/22/99 from xwvid6 */
void clean_uvvs(void)
{
	short i;
	for(i=0;i<numlvl;i++) sndg[i][0]=-9999.0;
}

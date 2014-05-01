
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
#include <sharp95.h>
#include <ctype.h>

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
	short ok, i, aok, j, k;
	float x1, x2, x3, x4, x5;
	char rlin[82], fs[25];

	/* ----- Open File if it exists ----- */
	fp = fopen( config.filename, "r" );
	if (fp)
	   {

           raob_type[0] = '\0'; raobtitle[0] = '\0';

	   /* ----- Read Title Line ----- *
	   while(fgets( rlin, 80, fp ) != 0)
	     {
	     if(strstr( rlin, "%TITLE%" ) != 0)
		{
		fgets( raobtitle, 80, fp );
		break;
		}
	     } */

	   /* ----- Look for RAW data ----- */
	   while(fgets( rlin, 80, fp ) != 0)
	     {
	     if(strstr( rlin, "%TITLE%" ) != 0)
		{
		fgets( raobtitle, 80, fp );
		continue;
		}
             if(strstr( rlin, "%SOURCE%" ) != 0)
                {
                fgets( raob_type, 80, fp);
                continue;
                }
	     if(strstr( rlin, "%RAW%" ) != 0)
		{
	        i=0; j = 1;
		strcpy( fs, "%f,%f,%f,%f,%f,%f,%f" );
		while ((fgets(rlin,80,fp) != NULL)&&(j > 0))
                   {
                   j = sscanf( rlin, fs, &sndg[i][1], &sndg[i][2], &sndg[i][3],
		      &sndg[i][4], &sndg[i][5], &sndg[i][6], &sndg[i][0]);
                   /*printf("look %d %d %f %f\n",i,j,sndg[i][1],sndg[i][0]); */
                   if(j == 6) sndg[i][0] = -999.;
                   for(k=j;k<6;k++) sndg[i][k] = -999.;
                   if(j>0) i++;
                   }
		}
	     }
	   numlvl = i;
	   fclose(fp);

	   /* ----- Extend complete soundings above 100mb ----- */
	   xtnd_sndg();

	   save_origsndg ();

	   return 0;
	   }
	return 1;
	}

	/*NP*/
	void xtnd_sndg( void )
	/*************************************************************/
	/*  XTND_SNDG                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine will extend a sounding to 50mb, if the      */
	/*  sounding made it at least to 150mb.                      */
	/*                                                           */
	/*  An isothermal/hydrostatic layer is assumed above the     */
	/*  last observed level.                                     */
	/*************************************************************/
	{
	short above, below;
	double nm1, nm2, nm4;

	if (sndg[numlvl-1][1] <= 150 && sndg[numlvl-1][1] >= 100. )
	   {
	   sndg[numlvl][0] = -999;
	   sndg[numlvl][1] = 75;
	   sndg[numlvl][3] = i_temp(150);
	   sndg[numlvl][4] = i_dwpt(150);
	   sndg[numlvl][5] = i_wdir(150);
	   sndg[numlvl][6] = i_wspd(150);

	   above = numlvl-1;
	   below = numlvl-2;
	   nm1 = sndg[above][2] - sndg[below][2];
	   nm2 = log( sndg[below][1] / sndg[above][1] );
	   nm4 = log( sndg[below][1] / sndg[numlvl][1] );
	   sndg[numlvl][2] = (float)(sndg[below][2] + (( nm4 / nm2) * nm1));
	   numlvl += 1;

	   sndg[numlvl][0] = -999;
	   sndg[numlvl][1] = 50;
	   sndg[numlvl][3] = i_temp(150);
	   sndg[numlvl][4] = i_dwpt(150);
	   sndg[numlvl][5] = i_wdir(150);
	   sndg[numlvl][6] = i_wspd(150);

	   above = numlvl-1;
	   below = numlvl-2;
	   nm1 = sndg[above][2] - sndg[below][2];
	   nm2 = log( sndg[below][1] / sndg[above][1] );
	   nm4 = log( sndg[below][1] / sndg[numlvl][1] );
	   sndg[numlvl][2] = (float)(sndg[below][2] + (( nm4 / nm2) * nm1));
	   numlvl += 1;
	   }
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
	void interp_sndg( void )
	/*************************************************************/
	/*  INTERP_SNDG                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine take the current sounding array and         */
	/*  interpolate it to 25mb increments.                       */
	/*************************************************************/
	{
	float nsndg[100][7], sfclvl, pres, cint, p1;
	short newnum, i, j;

	cint = 25;

	/* ----- Copy surface conditions to new array ----- */
	newnum = 0;
	sfclvl = sndg[sfc()][1];
	nsndg[newnum][0] = i_omeg( sfclvl );
	nsndg[newnum][1] = sfclvl;
	nsndg[newnum][2] = i_hght( sfclvl );
	nsndg[newnum][3] = i_temp( sfclvl );
	nsndg[newnum][4] = i_dwpt( sfclvl );
	nsndg[newnum][5] = i_wdir( sfclvl );
	nsndg[newnum][6] = i_wspd( sfclvl );

	/* ----- Determine first interpolated level above surface ----- */
	pres = (float)((short)((sfclvl / cint) + 0) * cint);
	if( pres == sfclvl ) { pres -= cint; }

	/* ----- Interpolate NSNDG array to prescribed increments ----- */
	for( p1 = pres; p1 >= sndg[numlvl-1][1]; p1 -= cint )
	   {
	   newnum++;
	   nsndg[newnum][0] = i_omeg( p1 );
	   nsndg[newnum][1] = p1;
	   nsndg[newnum][2] = i_hght( p1 );
	   nsndg[newnum][3] = i_temp( p1 );
	   nsndg[newnum][4] = i_dwpt( p1 );
	   nsndg[newnum][5] = i_wdir( p1 );
	   nsndg[newnum][6] = i_wspd( p1 );
	  
           /* 
	   printf( "%f   %f   %f   %f   %f   %f %f\n", p1, i_hght(p1),
	   i_temp(p1), i_dwpt(p1), i_wdir(p1), i_wspd(p1));
	   */ 
	   }

	/* ----- Copy NSNDG to SNDG array ----- */
	for( i=0; i <= newnum; i++ )
	   {
	   for( j=0; j<=6; j++)
	      {
	      sndg[i][j] = nsndg[i][j];
	      }
	   }
       numlvl = newnum + 1;

	/* ----- NULL out two levels above highest observation ----- */
	for( i=numlvl; i <= numlvl + 2; i++ )
	   {
	   for( j=0; j<=6; j++)
	      {
	      sndg[i][j] = -999;
	      }
	   }
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
	short ok, ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, i;
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
	      }
	   fclose(fp);
	   }
	else
	   { return 2; }

	if(ok1 && ok2)        { return read_sndg1(); }

	if(ok3 && ok4 && ok5) { return read_sndg2( "RAW" ); }

	if(ok3 && ok5)        { return read_sndg2( "SMOOTHED" ); }

	if(ok6 && ok7)        { return decode_sounding( config.filename ); }

	printf( "%d   %d   %d   %d   %d\n", ok1, ok2, ok3, ok4, ok5);
	printf( "Unknown sounding file format.  Read aborted.\n" );
	return 1;
	}

	/*NP*/
	short read_sndg2( char *searchtag )
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
	short ok, i, aok;
	float x1, x2, x3, x4, x5;
	char rlin[82], fs[25];

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

	   /* ----- Extend complete soundings above 100mb ----- */
	   xtnd_sndg();

	   save_origsndg ();

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

      void save_origsndg ()
	{
	int i, j;

	origlvl = numlvl;
	for (i=0; i < numlvl; i++ )
	  {
	  for ( j=0; j < 7; j++ )
	    {
	    origsndg[i][j] = sndg[i][j];
	    }
	  }
	}

      void restore_origsndg ()
	{
	int i, j;

	numlvl = origlvl;
	for (i=0; i < numlvl; i++ )
	  {
	  for ( j=0; j < 7; j++ )
	    {
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

	for (i=0; i< numlvl; i++)
		{
		for (j=0;j<7;j++)
			{
		        sndg2[i][j] = sndg[i][j];
			}
		}
	}

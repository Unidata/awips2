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
#include "gui.h"
#include "sharp95.h"

short read_sndg1 ( void )
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
	short i, j, k;
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
		strcpy ( sndgp->title, raobtitle);
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
                   j = sscanf( rlin, fs, &sndgp->sndg[i].pres, &sndgp->sndg[i].hght,
			&sndgp->sndg[i].temp, &sndgp->sndg[i].dwpt, &sndgp->sndg[i].drct,
			&sndgp->sndg[i].sped, &sndgp->sndg[i].omega);
		   if ( j < 1 ) continue;
		   /* fill in unread values, expect 7 if all were scanned */
                   for(k=j;k<7;k++)
		      {
		      switch ( k )
		         {
			 case 0:
		            sndgp->sndg[i].pres = -999.;
			    break;
			 case 1:
		            sndgp->sndg[i].hght = -999.;
			    break;
			 case 2:
		            sndgp->sndg[i].temp = -999.;
			    break;
			 case 3:
		            sndgp->sndg[i].dwpt = -999.;
			    break;
			 case 4:
		            sndgp->sndg[i].drct = -999.;
			    break;
			 case 5:
		            sndgp->sndg[i].sped = -999.;
			    break;
			 case 6:
		            sndgp->sndg[i].omega = -999.;
			    break;
		         }
		      }
                   i++;
                   }
		}
	     }
	   sndgp->numlev = i;
	   fclose(fp);

	   /* ----- Extend complete soundings above 100mb ----- */
	   xtnd_sndg();

	   save_origsndg ();

	   return 0;
	   }
	return 1;
	}

	/*NP*/
	void xtnd_sndg ( void )
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
	int nlev;

	nlev = sndgp->numlev;
	if (sndgp->sndg[nlev-1].pres <= 150 && sndgp->sndg[nlev-1].pres >= 100. )
	   {
	   sndgp->sndg[nlev].omega = -999;
	   sndgp->sndg[nlev].pres = 75;
	   sndgp->sndg[nlev].temp = i_temp(150);
	   sndgp->sndg[nlev].dwpt = i_dwpt(150);
	   sndgp->sndg[nlev].drct = i_wdir(150);
	   sndgp->sndg[nlev].sped = i_wspd(150);

	   above = nlev-1;
	   below = nlev-2;
	   nm1 = sndgp->sndg[above].hght - sndgp->sndg[below].hght;
	   nm2 = log( sndgp->sndg[below].pres / sndgp->sndg[above].pres );
	   nm4 = log( sndgp->sndg[below].pres / sndgp->sndg[nlev].pres );
	   sndgp->sndg[nlev].hght = (float)(sndgp->sndg[below].hght + (( nm4 / nm2) * nm1));
	   printf(" 75, nm1 %f  nm2 %f nm4 %f\n", nm1,nm2,nm4);
	   printf(" nlev   height %f pressure %f \n", sndgp->sndg[nlev].hght, sndgp->sndg[nlev].pres);
	   printf(" nlev-1 height %f pressure %f \n", sndgp->sndg[above].hght, sndgp->sndg[above].pres);
	   printf(" nlev-2 height %f pressure %f \n", sndgp->sndg[below].hght, sndgp->sndg[below].pres);
	   nlev += 1;

	   sndgp->sndg[nlev].omega = -999;
	   sndgp->sndg[nlev].pres = 50;
	   sndgp->sndg[nlev].temp = i_temp(150);
	   sndgp->sndg[nlev].dwpt = i_dwpt(150);
	   sndgp->sndg[nlev].drct = i_wdir(150);
	   sndgp->sndg[nlev].sped = i_wspd(150);

	   above = nlev-1;
	   below = nlev-2;
	   nm1 = sndgp->sndg[above].hght - sndgp->sndg[below].hght;
	   nm2 = log( sndgp->sndg[below].pres / sndgp->sndg[above].pres );
	   nm4 = log( sndgp->sndg[below].pres / sndgp->sndg[nlev].pres );
	   sndgp->sndg[nlev].hght = (float)(sndgp->sndg[below].hght + (( nm4 / nm2) * nm1));
	   printf(" 50, nm1 %f  nm2 %f nm4 %f\n", nm1,nm2,nm4);
	   printf(" nlev   height %f pressure %f \n", sndgp->sndg[nlev].hght, sndgp->sndg[nlev].pres);
	   printf(" nlev-1 height %f pressure %f \n", sndgp->sndg[above].hght, sndgp->sndg[above].pres);
	   printf(" nlev-2 height %f pressure %f \n", sndgp->sndg[below].hght, sndgp->sndg[below].pres);
	   nlev += 1;

	   sndgp->numlev = nlev;
	   }
	}


	/*NP*/
	short read_config ( void )
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
	void interp_sndg ( void )
	/*************************************************************/
	/*  INTERP_SNDG                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine take the current sounding array and         */
	/*  interpolate it to 25mb increments.                       */
	/*************************************************************/
	{
	float sfclvl, pres, cint, p1;
	int newnum;
/*	short i, j; */
	struct sndg_struct nsndg;

	cint = 25;

	/* ----- Copy surface conditions to new array ----- */
	sfclvl = sndgp->sndg[sfc()].pres;
	nsndg.sndg[0].omega = i_omeg( sfclvl );
	nsndg.sndg[0].pres = sfclvl;
	nsndg.sndg[0].hght = i_hght( sfclvl );
	nsndg.sndg[0].temp = i_temp( sfclvl );
	nsndg.sndg[0].dwpt = i_dwpt( sfclvl );
	nsndg.sndg[0].drct = i_wdir( sfclvl );
	nsndg.sndg[0].sped = i_wspd( sfclvl );
        newnum = 1;

	/* ----- Determine first interpolated level above surface ----- */
	pres = (float)((short)((sfclvl / cint) + 0) * cint);
	if( pres == sfclvl ) { pres -= cint; }

	/* ----- Interpolate NSNDG array to prescribed increments ----- */
	for( p1 = pres; p1 >= sndgp->sndg[sndgp->numlev-1].pres; p1 -= cint )
	   {
	   nsndg.sndg[newnum].omega = i_omeg( p1 );
	   nsndg.sndg[newnum].pres = p1;
	   nsndg.sndg[newnum].hght = i_hght( p1 );
	   nsndg.sndg[newnum].temp = i_temp( p1 );
	   nsndg.sndg[newnum].dwpt = i_dwpt( p1 );
	   nsndg.sndg[newnum].drct = i_wdir( p1 );
	   nsndg.sndg[newnum].sped = i_wspd( p1 );
	   newnum++;

           /*
	   printf( "%f   %f   %f   %f   %f   %f %f\n", p1, i_hght(p1),
	   i_temp(p1), i_dwpt(p1), i_wdir(p1), i_wspd(p1));
	   */
	   }

	/* ----- Copy NSNDG to SNDG array ----- */
        sndgp->numlev = newnum;
	memcpy ( sndgp->sndg, nsndg.sndg, LLMXLV*sizeof(struct sndg_parms) ) ;
	/*for( i=0; i <= newnum; i++ )
	   {
	   for( j=0; j<=6; j++)
	      {
	      sndg[i][j] = nsndg[i][j];
	      }
	   }*/

	/* ----- NULL out two levels above highest observation ----- */
	/*for( i=numlvl; i <= numlvl + 2; i++ )
	   {
	   for( j=0; j<=6; j++)
	      {
	      sndg[i][j] = -999;
	      }
	   }*/
       }


	/*NP*/
	void read_command ( int argc, char *argv[], char *envp[] )
	/*************************************************************/
	/*  READ_COMMAND                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine will read the SHARP command line and        */
	/*  set the program variables as requested.                  */
	/*************************************************************/
	{
	if( argc > 0 && strlen(argv[1]) > (size_t)0)
	   {
	   strncpy( config.filename, argv[1], strlen(argv[1]));
	   config.filename[strlen(argv[1])] = 0;
	   }
	}

	/*NP*/
	short get_sndg ( void )
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
	short ok1, ok2, ok3, ok4, ok5, ok6, ok7;
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
	      }
	   fclose(fp);
	   }
	else
	   { return 2; }

	if(ok1 && ok2)        { return read_sndg1(); }

	if(ok3 && ok4 && ok5) { return read_sndg2( "RAW" ); }

	if(ok3 && ok5)        { return read_sndg2( "SMOOTHED" ); }

	if(ok6 && ok7)        { return decode_sounding( config.filename ); }

	printf( "%d   %d   %d   %d   %d   %d   %d\n", ok1, ok2, ok3, ok4, ok5, ok6, ok7);
	printf( "Unknown sounding file format.  Read aborted.\n" );
	return 1;
	}

	/*NP*/
	short read_sndg2 ( char *searchtag )
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
	short i;
	float x1, x2;
	char rlin[82], fs[25];

	/* ----- Open File if it exists ----- */
	fp = fopen( config.filename, "r" );
	if (fp)
	   {
	   /* ----- Read Title Line ----- */
	   fgets( raobtitle, 40, fp );
	   strcpy ( sndgp->title, raobtitle);

	   /* ----- Look for RAW data ----- */
	   while(fgets( rlin, 80, fp ) != NULL)
	     {
	     if(strstr( rlin, searchtag ) != 0)
		{
		i=0;
		strcpy( fs, "%f,%f,%f,%f,%f,%f,%f,%f" );
		while (fscanf( fp, fs, &sndgp->sndg[i].pres, &sndgp->sndg[i].hght,
			&sndgp->sndg[i].temp, &sndgp->sndg[i].dwpt, &x1, &x2,
			&sndgp->sndg[i].drct, &sndgp->sndg[i].sped ) != 0)
			{
			sndgp->sndg[i].omega = -999.;
			i++;
			}
		}
	     sndgp->numlev = i;

	     /* ----- Look for STORM MOTION data ----- */
	     if(strstr( rlin, "STORM" ) != 0)
		{
		fscanf( fp, "%f, %f", &sndgp->st_dir, &sndgp->st_spd);
		printf( "Storm Motion = %3.0f/%3.0f\n", sndgp->st_dir, sndgp->st_spd);
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
	void security ( char *envp[] )
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

/*====================================================================*/

      void save_origsndg (void)
	{
	memcpy ( sndgp->orig, sndgp->sndg, LLMXLV*sizeof(struct sndg_parms) );
	sndgp->origlev = sndgp->numlev;
	}

/*====================================================================*/

      void restore_origsndg (void)
	{
	sndgp->numlev = sndgp->origlev;
	memcpy ( sndgp->sndg, sndgp->orig, LLMXLV*sizeof(struct sndg_parms) );
	}

/*====================================================================*/

	void copy_sndg ( void )
	/*******************************************************/
	/*  COPY_SNDG					       */
	/*******************************************************/
	{
	sndgp->ovrlev = sndgp->numlev;
	memcpy ( sndgp->ovrl, sndgp->sndg, LLMXLV*sizeof(struct sndg_parms) );
	}

/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  Sounding Data Decoding Routines                            */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  DECODE_SOUNDING                                            */
/*  READ_TTAA                                                  */
/*  READ_TTBB                                                  */
/*  READ_PPBB                                                  */
/*  DECODE_TTAA                                                */
/*  DECODE_TTBB                                                */
/*  DECODE_PPBB                                                */
/*  LVL_MAN                                                    */
/*  LVL_SIG                                                    */
/*  LVL_WND                                                    */
/*  I_H                                                        */
/*  I_P                                                        */
/*  UASORT                                                     */
/*  FIND_TEMP                                                  */
/*  FIND_DWPT                                                  */
/*  FIND_WDIR                                                  */
/*  FIND_WSPD                                                  */
/*  REMOVE_LVL                                                 */
/*  SWAP_ARRAYS                                                */
/*  WRITE_FILE                                                 */
/*  GET_SFC_ELEV                                               */
/*                                                             */
/***************************************************************/
#include "gui.h"
#include "sharp95.h"

float get_stn_info (long id);

short numttxx, lvltop, verbose = 1;
char ttxx[LLMXLV][6];
float sfcelev=300;
long  wmo_id=72349;
char  station_name[33]="";
short wmo_hour, wmo_day;

/*===================================================================*/

short decode_sounding ( char *filename )
	/*************************************************************/
	/*  DECODE_SOUNDING                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine will decode a standard WMO formatted        */
	/*  upper air sounding and create an array for use in the    */
	/*  SHARP95 program.                                         */
	/*                                                           */
	/*  Return Codes:  0 = No Errors, sounding decoded           */
	/*                 1 = Input File Not Found                  */
	/*                 2 = Error Reading File                    */
	/*************************************************************/
	{
	FILE *fin;
	short ok;


	printf( "Decoding Input File:  %s\n", filename );

	sndgp->numlev = 0;

	/* ----- Make sure File Exists ----- */
	if( (fin = fopen( filename, "rt" )) == NULL ) { return( 1 ); }
	fclose( fin );

	/* ----- TTAA ----- */
	printf( "Reading/Decoding TTAA (Mandatory Level) Data\n" );
	ok = read_ttaa(filename);
	if(ok) { return 1; }            /* ----- Exit if not found -----*/
	ok = decode_ttaa();
	if(ok) { return 1; }            /* ----- Exit if not readable -----*/

	/* ----- TTBB ----- */
	printf( "Reading/Decoding TTBB (Significant Level) Data\n" );
	ok = read_ttbb(filename);
	printf("\nflag 3 %d \n", ok);
	if(ok) { return 1; }            /* ----- Exit if not found -----*/
	ok = decode_ttbb();
	printf("\nflag 4 %d \n", ok);
	if(ok) { return 1; }            /* ----- Exit if not readable -----*/

	/* ----- PPBB ----- */
	printf( "Reading/Decoding PPBB (Wind Level) Data\n" );
	ok = read_ppbb(filename);
	printf("\nflag 1 %d \n", ok);
	/* if(ok) { return 1; } */           /* ----- Exit if not found -----*/
	ok = decode_ppbb();
	printf("\nflag 2 %d \n", ok);
	/* if(ok) { return 1; } */            /* ----- Exit if not readable -----*/

	/* ----- Sort in ascending order ----- */
	printf( "Weeding/Sorting Data Array Into Ascending Order\n" );
	uasort();

	/* ----- Write output to file ----- */
	printf( "Writing File DECDDROB to Disk\n" );

	/* ----- Extract RAOB Title from FileName ----- */
	get_title( filename );
	strcpy ( sndgp->title, raobtitle);
	sprintf(raob_type,"RAW");

	write_file();

	return 0;
	}

/*============================================================================*/

short read_ttaa ( char *filename )
	/*************************************************************/
	/*  READ_TTAA                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Reads TTAA data and parses it into ttaa array.           */
	/*************************************************************/
	{
	FILE *fp;
	char rlin[82],*spos;
	short ok, aok, i;

	numttxx = 0;
	aok = 0;

	/* ----- Open File if it exists ----- */
	fp = fopen( filename, "r" );
	if (fp)
	   {

	   /* ----- Find TTAA Bulletin ----- */
	   i = 0;
	   while(fgets( rlin, 80, fp ))
	      {

	      /* ----- Look for ending of msg ----- */
	      if(strlen( rlin ) < (size_t)5 ) {aok = 0;}

	      /* ----- Look for beginning of msg ----- */
	      spos = strstr( rlin, "TTAA " );
	      if ( ( spos != NULL ) && ( strlen(spos) ) ) { aok += 1; }
	      /*if(strlen(strstr( rlin, "TTAA " ))) { aok += 1; }*/

	      /* ----- Process msg ----- */
	      if( aok == 1 )
		 {
		 printf( " %s", rlin);
		 ok = sscanf( rlin, "%s %s %s %s %s %s %s %s %s %s\n",
		 ttxx[i], ttxx[i+1], ttxx[i+2], ttxx[i+3], ttxx[i+4],
		 ttxx[i+5], ttxx[i+6], ttxx[i+7], ttxx[i+8], ttxx[i+9]);
		 i += ok;
		 }
	      }
	   }
	numttxx = i;
	fclose(fp);

	if(numttxx == 0 ) { return 1; }
	return 0;
	}

/*=========================================================================*/

short read_ttbb ( char *filename )
	/*************************************************************/
	/*  READ_TTBB                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Reads TTBB data and parses it into ttbb array.           */
	/*************************************************************/
	{
	FILE *fp;
	char rlin[82];
	short ok, aok, i;

	numttxx = 0;
	aok = 0;

	/* ----- Open File if it exists ----- */
	fp = fopen( filename, "r" );
	if (fp)
	   {

	   /* ----- Find TTBB Bulletin ----- */
	   i = 0;
	   while(fgets( rlin, 80, fp ))
	      {
	      /* ----- Look for beginning of msg ----- */
	      if(strstr( rlin, "TTBB " ) != 0 ) {aok += 1;}

	      /* ----- Look for ending of msg ----- */
	      if(strlen( rlin ) < (size_t)5 ) {aok = 0;}

	      /* ----- Process msg ----- */
	      if( aok == 1 )
		 {
		 printf( " %s", rlin);
		 ok = sscanf( rlin, "%s %s %s %s %s %s %s %s %s %s\n",
		 ttxx[i], ttxx[i+1], ttxx[i+2], ttxx[i+3], ttxx[i+4],
		 ttxx[i+5], ttxx[i+6], ttxx[i+7], ttxx[i+8], ttxx[i+9]);
		 i += ok;
		 }
	      }
	   }

	numttxx = i;
	fclose(fp);

	if(numttxx == 0 ) { return 1; }
	return 0;
	}

/*=====================================================================*/

short read_ppbb ( char *filename )
	/*************************************************************/
	/*  READ_PPBB                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Reads PPBB data and parses it into ttbb array.           */
	/*************************************************************/
	{
	FILE *fp;
	char rlin[82];
	short ok, aok, i;

	numttxx = 0;
	aok=0;

	/* ----- Open File if it exists ----- */
	fp = fopen( filename, "r" );
	if (fp)
	   {

	   /* ----- Find PPBB Bulletin ----- */
	   i = 0;
	   while(fgets( rlin, 80, fp ))
	      {
	      /* ----- Look for beginning of msg ----- */
	      if(strstr( rlin, "PPBB " ) != 0 ) {aok += 1;}

	      /* ----- Look for ending of msg ----- */
	      if(strlen( rlin ) < (size_t)5 ) {aok = 0;}

	      /* ----- Process msg ----- */
	      if( aok == 1 )
		 {
		 printf( " %s", rlin);
		 ok = sscanf( rlin, "%s %s %s %s %s %s %s %s %s %s\n",
		 ttxx[i], ttxx[i+1], ttxx[i+2], ttxx[i+3], ttxx[i+4],
		 ttxx[i+5], ttxx[i+6], ttxx[i+7], ttxx[i+8], ttxx[i+9]);
		 i += ok;
		 }
	      }
	   }
	printf("\n -> %s %s %s %s \n", ttxx[0], ttxx[1], ttxx[2], ttxx[3] );
	numttxx = i;
	fclose(fp);

	if(numttxx == 0 ) { return 1; }
	return 0;
	}

/*=============================================================================*/

short decode_ttaa ( void )
	/*************************************************************/
	/*  DECODE_TTAA                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Decodes TTAA file and places it into tadat array.        */
	/*************************************************************/
	{
	char wmo[6], datim[6], dum[6];
	short i, p_wmo, p_datim, p_first, incx;
	float lvl, hght, temp, dwpt, wdir, wspd;

	/* ----- Standard Pointers for TTAA message----- */
	p_wmo = 0;
	p_datim = 2;
	p_first = 4;

	/* ----- Read WMO ID Number ----- */
	strcpy( wmo, ttxx[p_wmo]);
	wmo_id = atol(wmo);
	get_stn_info(wmo_id);

	/* ----- Read Date/Time/Top Group ----- */
	strcpy( datim, ttxx[p_datim]);
	strncpy( dum, datim, 2);
	dum[2] = 0;
	wmo_day = atoi( dum ) - 50;
	strncpy( dum, datim+2, 2);
	dum[2] = 0;
	wmo_hour = atoi( dum );
	strcpy( dum, datim+4);
	lvltop = atoi( dum ) * 100;
	if(verbose)
	   { printf( "Stn=%ld   Day=%d   Hour=%d   Top=%d\n", wmo_id, wmo_day, wmo_hour, lvltop); }

	/* ----- Loop through data levels and decode ----- */
	for(i=p_first; i<=numttxx; i+=incx)
	   {

	   /* ----- Check for EOM group ----- */
	   if (strstr(ttxx[i], "51515")) { break; }
	   if (strstr(ttxx[i], "31313")) { break; }

	   /* ----- Convert units and apply rules ----- */
	   incx = 0;
	   lvl_man( i, &incx, &lvl, &hght, &temp, &dwpt, &wdir, &wspd);

	   sndgp->sndg[sndgp->numlev].pres = lvl;
	   if(hght == -999) 
		sndgp->sndg[sndgp->numlev].hght = i_h( lvl );
	   else
	       sndgp->sndg[sndgp->numlev].hght = hght;
	   sndgp->sndg[sndgp->numlev].temp = temp;
	   sndgp->sndg[sndgp->numlev].dwpt = dwpt;
	   sndgp->sndg[sndgp->numlev].drct = wdir;
	   sndgp->sndg[sndgp->numlev].sped = wspd;
	   sndgp->sndg[sndgp->numlev].omega = (-999.0);

	   if(verbose)
	      {
	      printf( "%3d  ", sndgp->numlev);
              printf( "%8.1f %8.1f %8.1f %8.1f %8.1f %8.1f\n", 
		sndgp->sndg[sndgp->numlev].pres, sndgp->sndg[sndgp->numlev].hght, 
		sndgp->sndg[sndgp->numlev].temp, sndgp->sndg[sndgp->numlev].dwpt, 
		sndgp->sndg[sndgp->numlev].drct, sndgp->sndg[sndgp->numlev].sped);
	      }

	   sndgp->numlev = sndgp->numlev + 1;

	   }
	return 0;
	}

/*===========================================================================*/

short decode_ttbb ( void )
	/*************************************************************/
	/*  DECODE_TTBB                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Decodes TTBB file and places it into sndg  array.        */
	/*************************************************************/
	{
	char wmo[6], datim[6], dum[6];
	short i, p_wmo, p_datim, p_first, day, hour, inc;
	float lvl, hght, temp, dwpt, wdir, wspd;

	/* ----- Standard Pointers for TTAA message----- */
	p_wmo = 0;
	p_datim = 2;
	p_first = 4;

	/* ----- Read WMO ID Number ----- */
	strcpy( wmo, ttxx[p_wmo]);

	/* ----- Read Date/Time/Top Group ----- */
	strcpy( datim, ttxx[p_datim]);
	strncpy( dum, datim, 2);
	dum[2] = 0;
	day = atoi( dum ) - 50;
	strncpy( dum, datim+2, 2);
	dum[2] = 0;
	hour = atoi( dum );
	if(verbose) { printf( "Stn=%s   Day=%d   Hour=%d\n", wmo, day, hour); }
	if((day != wmo_day) || (hour != wmo_hour)) return 1;

	/* ----- Loop through data levels and decode ----- */
	inc=3;
	for(i=p_first; i<=numttxx; i+=inc)
	   {
	   /* ----- Check for EOM group ----- */
	   if (strstr(ttxx[i], "51515")) { break; }
	   if (strstr(ttxx[i], "31313")) { break; }

	   /* ----- Convert units and apply rules ----- */
	   lvl_sig( i, &lvl, &hght, &temp, &dwpt, &wdir, &wspd);

	   sndgp->sndg[sndgp->numlev].pres = lvl;
	   if(hght == -999) 
		sndgp->sndg[sndgp->numlev].hght = i_h( lvl );
	   else
	   	sndgp->sndg[sndgp->numlev].hght = hght;
	   sndgp->sndg[sndgp->numlev].temp = temp;
	   sndgp->sndg[sndgp->numlev].dwpt = dwpt;
	   sndgp->sndg[sndgp->numlev].drct = wdir;
	   sndgp->sndg[sndgp->numlev].sped = wspd;
	   sndgp->sndg[sndgp->numlev].omega = (-999.0);

	   if(verbose)
	      {
	      printf( "%3d  ", sndgp->numlev);
              printf( "%8.1f %8.1f %8.1f %8.1f %8.1f %8.1f\n", 
		sndgp->sndg[sndgp->numlev].pres, sndgp->sndg[sndgp->numlev].hght, 
		sndgp->sndg[sndgp->numlev].temp, sndgp->sndg[sndgp->numlev].dwpt, 
		sndgp->sndg[sndgp->numlev].drct, sndgp->sndg[sndgp->numlev].sped);
	      }

	   sndgp->numlev = sndgp->numlev + 1;

	   /* ----- Increment groups ----- */
	   inc = 2;
	   }
	return 0;
	}

/*===========================================================================*/

short decode_ppbb ( void )
	/*************************************************************/
	/*  DECODE_PPBB                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Decodes PPBB file and places it into sndg  array.        */
	/*************************************************************/
	{
	char wmo[6], datim[6], dum[6];
	short i, p_wmo, p_datim, p_first, day, hour, inc;
	long mod, h1, h2, h3;
	float lvl, hght, temp, dwpt, wdir, wspd;

	/* ----- Standard Pointers for PPBB message----- */
	p_wmo = 2;
	p_datim = 1;
	p_first = 3;

	/* ----- Read WMO ID Number ----- */
	strcpy( wmo, ttxx[p_wmo]);

	/* ----- Read Date/Time/Top Group ----- */
	strcpy( datim, ttxx[p_datim]);
	strncpy( dum, datim, 2);
	dum[2] = 0;
	day = atoi( dum ) - 50;
	strncpy( dum, datim+2, 2);
	dum[2] = 0;
	hour = atoi( dum );
	if(verbose) { printf( "Stn=%s   Day=%d   Hour=%d\n", wmo, day, hour); }
	if((day != wmo_day) || (hour != wmo_hour)) return 1;

	/* ----- Loop through data levels and decode ----- */
	inc=4;
	for(i=p_first; i<=numttxx; i+=inc)
	   {
	   /* ----- Check for EOM group ----- */
	   if (strstr(ttxx[i], "51515")) { break; }
	   if (strstr(ttxx[i], "31313")) { break; }

	   /* ----- Read indicator group to get heights ----- */
	   strncpy( dum, ttxx[i],1 );
	   dum[1] = 0;
	   if(!strstr(dum, "9")) { break; }

	   strncpy( dum, ttxx[i]+1, 1); (dum)[1] = 0; mod = atol(dum) * 10000l;

	   strncpy( dum, ttxx[i]+2, 1); (dum)[1] = 0;
	   if( strstr(dum,"/") ) { h1=-1; } else { h1 = (atol(dum) * 1000) + mod; }

	   strncpy( dum, ttxx[i]+3, 1); (dum)[1] = 0;
	   if( strstr(dum,"/") ) { h2=-1; } else { h2 = (atol(dum) * 1000) + mod; }

	   strncpy( dum, ttxx[i]+4, 1); (dum)[1] = 0;
	   if( strstr(dum,"/") ) { h3=-1; } else { h3 = (atol(dum) * 1000) + mod; }

	   inc = 1;
	   if(h1 != -1)
	      {
	      /* ----- Convert units and apply rules ----- */
	      lvl_wnd( i + 1, &lvl, &hght, &temp, &dwpt, &wdir, &wspd);

	      sndgp->sndg[sndgp->numlev].pres = lvl;
	      if(h1==0) { sndgp->sndg[sndgp->numlev].hght = sfcelev; } else { sndgp->sndg[sndgp->numlev].hght = (float)(h1 / 3.281); }
	      sndgp->sndg[sndgp->numlev].temp = temp;
	      sndgp->sndg[sndgp->numlev].dwpt = dwpt;
	      sndgp->sndg[sndgp->numlev].drct = wdir;
	      sndgp->sndg[sndgp->numlev].sped = wspd;
	      sndgp->sndg[sndgp->numlev].omega = (-999.0);
	      if (lvl == -999) 
		sndgp->sndg[sndgp->numlev].pres = i_p( sndgp->sndg[sndgp->numlev].hght );

	      if(verbose)
		 {
		 printf( "%3d  ", sndgp->numlev);
                 printf( "%8.1f %8.1f %8.1f %8.1f %8.1f %8.1f\n", 
		   sndgp->sndg[sndgp->numlev].pres, sndgp->sndg[sndgp->numlev].hght, 
		   sndgp->sndg[sndgp->numlev].temp, sndgp->sndg[sndgp->numlev].dwpt, 
		   sndgp->sndg[sndgp->numlev].drct, sndgp->sndg[sndgp->numlev].sped);
		 }

	      sndgp->numlev = sndgp->numlev + 1;

	      inc++;
	      }

	   if(h2 != -1)
	      {
	      /* ----- Convert units and apply rules ----- */
	      lvl_wnd( i + 2, &lvl, &hght, &temp, &dwpt, &wdir, &wspd);

	      sndgp->sndg[sndgp->numlev].pres = lvl;
	      sndgp->sndg[sndgp->numlev].hght = (float)(h2 / 3.281);
	      sndgp->sndg[sndgp->numlev].temp = temp;
	      sndgp->sndg[sndgp->numlev].dwpt = dwpt;
	      sndgp->sndg[sndgp->numlev].drct = wdir;
	      sndgp->sndg[sndgp->numlev].sped = wspd;
	      sndgp->sndg[sndgp->numlev].omega = (-999.0);
	      if(lvl == -999) { sndgp->sndg[sndgp->numlev].pres = i_p( sndgp->sndg[sndgp->numlev].hght ); }

	      if(verbose)
		 {
		 printf( "%3d  ", sndgp->numlev);
                 printf( "%8.1f %8.1f %8.1f %8.1f %8.1f %8.1f\n", 
		   sndgp->sndg[sndgp->numlev].pres, sndgp->sndg[sndgp->numlev].hght, 
		   sndgp->sndg[sndgp->numlev].temp, sndgp->sndg[sndgp->numlev].dwpt, 
		   sndgp->sndg[sndgp->numlev].drct, sndgp->sndg[sndgp->numlev].sped);
		 }

	      sndgp->numlev = sndgp->numlev + 1;

	      inc++;
	      }

	   if(h3 != -1)
	      {
	      /* ----- Convert units and apply rules ----- */
	      lvl_wnd( i + 3, &lvl, &hght, &temp, &dwpt, &wdir, &wspd);

	      sndgp->sndg[sndgp->numlev].pres = lvl;
	      sndgp->sndg[sndgp->numlev].hght = (float)(h3 / 3.281);
	      sndgp->sndg[sndgp->numlev].temp = temp;
	      sndgp->sndg[sndgp->numlev].dwpt = dwpt;
	      sndgp->sndg[sndgp->numlev].drct = wdir;
	      sndgp->sndg[sndgp->numlev].sped = wspd;
	      sndgp->sndg[sndgp->numlev].omega = (-999.0);
	      if(lvl == -999) { sndgp->sndg[sndgp->numlev].pres = i_p( sndgp->sndg[sndgp->numlev].hght ); }

	      if(verbose)
		 {
		 printf( "%3d  ", sndgp->numlev);
		 printf( "%8.1f %8.1f %8.1f %8.1f %8.1f %8.1f\n",
                   sndgp->sndg[sndgp->numlev].pres, sndgp->sndg[sndgp->numlev].hght,
                   sndgp->sndg[sndgp->numlev].temp, sndgp->sndg[sndgp->numlev].dwpt,
                   sndgp->sndg[sndgp->numlev].drct, sndgp->sndg[sndgp->numlev].sped);
		 }

	      sndgp->numlev = sndgp->numlev + 1;

	      inc++;
	      }


	   }
	return 0;
	}

/*==============================================================================*/

void lvl_man ( short i, short *incx, float *lvl, float *hght,
		      float *temp, float *dwpt, float *wdir, float *wspd )
	/*************************************************************/
	/*  LVL_MAN                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns level values from base file data.                */
	/*************************************************************/
	{
	float depp;
	char dum[10];
	short h1, l1, misg, p1, p2, trop;

	/* ----- Level (mb) ----- */
	strncpy( dum, ttxx[i], 2); (dum)[2]=0;
	if(strstr( dum, "/" ))
	   { l1 = -999; }
	else
	   { l1 = atoi( dum ); }

	/* ----- Height (m) ----- */
	strcpy( dum, ttxx[i]+2);
	if(strstr( dum, "/" ))
	   { h1 = -999; }
	else
	   { h1 = atoi( dum ); }

	misg=0;
	if(h1 == -999) { misg=1; }
	p1 = 1;
	p2 = 2;
	trop=0;
	switch(l1)
	   {
	   case 0:
	      *lvl = 1000;
	      *hght = h1;
	      break;
	   case 99:
	      if(h1 < 300) { h1 += 1000; }
	      *lvl = h1;
	      *hght = sfcelev;
	      break;
	   case 92:
	      *lvl = 925;
	      *hght = h1;
	      break;
	   case 85:
	      *lvl = 850;
	      *hght = h1 + 1000;
	      break;
	   case 70:
	      *lvl = 700;
	      if( h1 < 500 ) { h1 += 3000; } else { h1 += 2000; }
	      *hght = h1;
	      break;
	   case 50:
	      *lvl = 500;
	      *hght = h1 * 10;
	      break;
	   case 40:
	      *lvl = 400;
	      *hght = h1 * 10;
	      break;
	   case 30:
	      *lvl = 300;
	      *hght = h1 * 10;
	      if(*hght < 3000) { *hght += 10000; }
	      break;
	   case 25:
	      *lvl = 250;
	      *hght = (h1 * 10) + 10000;
	      break;
	   case 20:
	      *lvl = 200;
	      *hght = (h1 * 10) + 10000;
	      break;
	   case 15:
	      *lvl = 150;
	      *hght = (h1 * 10) + 10000;
	      break;
	   case 10:
	      *lvl = 100;
	      *hght = (h1 * 10) + 10000;
	      break;
	   case 88:
	      *lvl = h1;
	      *hght = -999;
	      trop = 1;
	      break;
	   case 77:
	      *lvl = h1;
	      *hght = -999;
	      p1 = -1;
	      p2 = 1;
	      break;
	    default:
	      *incx = 1;
	      *lvl = -999;
	      *hght = -999;
	      *temp = -999;
	      *dwpt = -999;
	      *wdir = -999;
	      *wspd = -999;
	      return;
	   }
	*incx = 1;
	if (misg) { *hght = -999; }
	if(p1 > 0)
	   {

	   *incx = *incx + 1;

	   /* ----- Temperature (c) ----- */
	   strncpy( dum, ttxx[i+p1], 3); (dum)[3]=0;
	   *temp = (float)(atoi(dum)) / 10;
	   if(atoi(dum) % 2) { *temp *= -1; }
	   if (strstr(dum, "/")) { *temp = -999; }

	   /* ----- Dew Point (c) ----- */
	   strcpy( dum, ttxx[i+p1]+3);
	   depp = (float)atoi(dum);
	   if(depp <= 55) { depp *= .1; } else { depp -= 50; }
	   if (strstr(dum, "/")) { *dwpt = -999; } else { *dwpt = *temp - depp; }
	   }
	else
	   {
	   *temp = -999;
	   *dwpt = -999;
	   }

	if(((short)*lvl >= lvltop) || (p2 == 1) || (trop == 1))
	   {

	   *incx = *incx + 1;

	   /* ----- Wind Direction (deg) ----- */
	   strncpy( dum, ttxx[i+p2], 3); (dum)[3]=0;
	   *wdir = (float)atoi(dum);
	   if (strstr(dum, "/")) { *wdir = -999; }

	   /* ----- Wind Speed (kt) ----- */
	   strcpy( dum, ttxx[i+p2]+3);
	   *wspd = (float)atoi(dum) + (((short)*wdir % 5) * 100);
	   if (strstr(dum, "/")) { *wspd = -999; }
	   }
	else
	   {
	   *wdir = -999;
	   *wspd = -999;
	   }

	if((*lvl == 999) && (*hght == -999))
	   {
	   *incx = 1;
	   *lvl  = -999;
	   *hght = -999;
	   *temp = -999;
	   *dwpt = -999;
	   *wdir = -999;
	   *wspd = -999;
	   }
	}

/*=============================================================================*/

void lvl_sig ( short i, float *lvl, float *hght, float *temp,
		       float *dwpt, float *wdir, float *wspd )
	/*************************************************************/
	/*  LVL_SIG                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns level values from TTBB file data.                */
	/*************************************************************/
	{
	float depp;
	char dum[10];

	/* ----- Level (mb) ----- */
	strcpy( dum, ttxx[i]+2);
	*lvl = atoi( dum );

	/* ----- Height (m) ----- */
	*hght = -999;

	/* ----- Temperature (c) ----- */
	strncpy( dum, ttxx[i+1], 3); (dum)[3]=0;
	*temp = (float)(atoi(dum)) / 10;
	if(atoi(dum) % 2) { *temp *= -1; }
	if (strstr(dum, "/")) { *temp = -999; }

	/* ----- Dew Point (c) ----- */
	strcpy( dum, ttxx[i+1]+3);
	depp = (float)atoi(dum);
	if(depp <= 55) { depp *= .1; } else { depp -= 50; }
	if (strstr(dum, "/")) { *dwpt = -999; } else { *dwpt = *temp - depp; }

	/* ----- Wind Direction (deg) ----- */
	*wdir = -999;

	/* ----- Wind Speed (kt) ----- */
	*wspd = -999;
	}

/*=============================================================================*/

void lvl_wnd ( short i, float *lvl, float *hght, float *temp,
		       float *dwpt, float *wdir, float *wspd )
	/*************************************************************/
	/*  LVL_WND                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns level values from PPBB file data.                */
	/*************************************************************/
	{
	char dum[10];

	/* ----- Level (mb) ----- */
	*lvl = -999;

	/* ----- Height (m) ----- */
	*hght = -999;

	/* ----- Temperature (c) ----- */
	*temp = -999;

	/* ----- Dew Point (c) ----- */
	*dwpt = -999;

	/* ----- Wind Direction (deg) ----- */
	strncpy( dum, ttxx[i], 3); (dum)[3]=0;
	*wdir = (float)atoi(dum);
	if (strstr(dum, "/")) { *wdir = -999; }

	/* ----- Wind Speed (kt) ----- */
	strcpy( dum, ttxx[i]+3);
	*wspd = (float)atoi(dum) + (((short)*wdir % 5) * 100);
	if (strstr(dum, "/")) { *wspd = -999; }
	}

/*=============================================================================*/

float i_h ( float pres )
	/*************************************************************/
	/*  I_H                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Interpolates the given data to calculate a height        */
	/*  at pressure level (pres).                                */
	/*                                                           */
	/*  pres             - Level(mb) to compute a Height         */
	/*************************************************************/
	{
	short below, above, i, ok, close;
	double nm1, nm2, nm4;

	below=0;
	above=0;

	/* ----- Find Height Immediately Above level ----- */
	ok=0;
	close = 999;
	for (i = 1; i < sndgp->numlev; i++)
	   {
	   if((sndgp->sndg[i].pres <= pres) && (sndgp->sndg[i].pres != -999) && (sndgp->sndg[i].pres != -999))
	      {
	      if((short)(pres - sndgp->sndg[i].pres) < close )
		 {
		 close = (short)(pres - sndgp->sndg[i].pres);
		 above = i; ok=1;
		 }
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Height Immediately Below level ----- */
	ok=0;
	close = 999;
	for (i = sndgp->numlev - 1; i > -1; i--)
	   {
	   if((sndgp->sndg[i].pres >= pres) && (sndgp->sndg[i].pres != -999.0F))
	      {
	      if((short)(sndgp->sndg[i].pres - pres) < close )
		 {
		 close = (short)(sndgp->sndg[i].pres - pres);
		 below = i; ok=1;
		 }
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( sndgp->sndg[above].pres == sndgp->sndg[below].pres)
	   {
	   return sndgp->sndg[above].hght;
	   }

	/* ----- Now we need to interpolate to get the height ----- */
	nm1 = sndgp->sndg[above].hght - sndgp->sndg[below].hght;
	nm2 = log( sndgp->sndg[below].pres / sndgp->sndg[above].pres );
	nm4 = log( sndgp->sndg[below].pres / pres );
	return (float)(sndgp->sndg[below].hght + (( nm4 / nm2) * nm1));
	}

/*=============================================================================*/

float i_p ( float hght )
	/*************************************************************/
	/*  I_P                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Interpolates the given data to calculate a pressure(mb)  */
	/*  at height (hght).                                        */
	/*                                                           */
	/*  hght             - Height(m) of level                    */
	/*************************************************************/
	{
	short below, above, i, ok, close;
	double nm1, nm2, nm3;

	printf("interp i_p %f\n",hght);

	below = 0;
	above = 0;

	/* ----- Find Pressure Immediately Above level ----- */
	ok = 0;
	close = 9999;
	for (i = 0; i < sndgp->numlev; i++)
	   {
	   if((sndgp->sndg[i].hght >= hght) && (sndgp->sndg[i].pres != -999.0F))
	      {
	      if((short)(sndgp->sndg[i].hght - hght) < close )
		 {
		 close = (short)(sndgp->sndg[i].hght - hght);
		 above = i; ok=1;
		 }
	      }
	   }

	if( ok == 0 ) return -999.0F;

	/* ----- Find Pressure Below level ----- */
	ok = 0;
	close = 9999;
	for (i = sndgp->numlev - 1; i > -1; i--)
	   {
	   if((sndgp->sndg[i].hght <= hght) && (sndgp->sndg[i].pres != -999.0F))
	      {
	      if((short)(hght - sndgp->sndg[i].hght) < close )
		 {
		 close = (short)(hght - sndgp->sndg[i].hght);
		 below = i; ok=1;
		 }
	      }
	   }

	if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( sndgp->sndg[above].hght == sndgp->sndg[below].hght)
	   {
	   return sndgp->sndg[above].pres;
	   }

	/* ----- Now we need to interpolate to get the Pressure ----- */
	nm1 = hght - sndgp->sndg[below].hght;
	nm2 = sndgp->sndg[above].hght - sndgp->sndg[below].hght;
	nm3 = log( sndgp->sndg[above].pres / sndgp->sndg[below].pres );
	return (float)(sndgp->sndg[below].pres * exp(( nm1 / nm2) * nm3));
	}

/*=============================================================================*/

void uasort ( void )
	/*************************************************************/
	/*  UASORT                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Sort sndg array into ascending order.                    */
	/*************************************************************/
	{
	short nl1=0, i, j, m_ptr;
	float sndg1[LLMXLV][7], max;

	/* keep surface level in position 0 */
        sndg1[0][0] = (-999.0F);
        sndg1[0][1] = sndgp->sndg[0].pres;
	sndg1[0][2] = sndgp->sndg[0].hght;
	sndg1[0][3] = find_temp( sndgp->sndg[0].pres );
	sndg1[0][4] = find_dwpt( sndgp->sndg[0].pres );
	sndg1[0][5] = find_wdir( sndgp->sndg[0].pres );
	sndg1[0][6] = find_wspd( sndgp->sndg[0].pres );

	if(verbose) printf( "SFC %8.2f %8.2f %8.2f\n", sndgp->sndg[0].pres,
			sndgp->sndg[0].hght, sndgp->sndg[0].temp);

	remove_lvl( sndgp->sndg[0].pres );
	sndgp->sndg[0].pres = -1;

	nl1++;

	for(j=1; j<sndgp->numlev; j++)
	   { 
	   if(verbose) { printf( "%8.2f %8.2f %8.2f\n", sndgp->sndg[j].pres, sndgp->sndg[j].hght, sndgp->sndg[j].temp); }

	   /* ----- Loop through every observation in sndg array ----- */
	   max = -1;
	   for(i=1; i<sndgp->numlev; i++)
	      {
	      if((sndgp->sndg[i].pres > max) && (sndgp->sndg[i].pres > 0 ))
		 {
		 max = sndgp->sndg[i].pres;
		 m_ptr = i;
		 }
	      }

	   if(max == -1) { break; }


	   sndg1[nl1][0] = (-999.0);
	   sndg1[nl1][1] = sndgp->sndg[m_ptr].pres;
	   sndg1[nl1][2] = sndgp->sndg[m_ptr].hght;
	   sndg1[nl1][3] = find_temp( sndgp->sndg[m_ptr].pres );
	   sndg1[nl1][4] = find_dwpt( sndgp->sndg[m_ptr].pres );
	   sndg1[nl1][5] = find_wdir( sndgp->sndg[m_ptr].pres );
	   sndg1[nl1][6] = find_wspd( sndgp->sndg[m_ptr].pres );

	   remove_lvl( sndgp->sndg[m_ptr].pres );
	   sndgp->sndg[m_ptr].pres = -1;

	   nl1++;
	   }

	sndgp->numlev = nl1;

	swap_arrays( sndg1 );

	if(verbose)
	   {
	   for( j=0; j<sndgp->numlev; j++)
	      {
	      printf( "%3d  ", j);
	      printf( "%8.1f %8.1f %8.1f %8.1f %8.1f %8.1f\n",
                   sndgp->sndg[j].pres, sndgp->sndg[j].hght,
                   sndgp->sndg[j].temp, sndgp->sndg[j].dwpt,
                   sndgp->sndg[j].drct, sndgp->sndg[j].sped);
	      }
	   }
	}

/*=============================================================================*/

float find_temp ( float lvl )
	/*************************************************************/
	/*  FIND_TEMP                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Given a lvl (mb), returns any observed temperature (c).  */
	/*************************************************************/
	{
	short i;
	for(i=1; i<sndgp->numlev; i++)
	   {
	   if((lvl == sndgp->sndg[i].pres) && (sndgp->sndg[i].temp != -999))
	      { return sndgp->sndg[i].temp;}
	   }
	return -999;
	}

/*=============================================================================*/

float find_dwpt ( float lvl )
	/*************************************************************/
	/*  FIND_DWPT                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Given a lvl (mb), returns any observed dew point (c).    */
	/*************************************************************/
	{
	short i;
	for(i=1; i<sndgp->numlev; i++)
	   {
	   if((lvl == sndgp->sndg[i].pres) && (sndgp->sndg[i].dwpt != -999))
	      { return sndgp->sndg[i].dwpt;}
	   }
	return -999;
	}

/*=============================================================================*/

float find_wdir ( float lvl )
	/*************************************************************/
	/*  FIND_WDIR                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Given a lvl (mb), returns any observed wind dir (deg)    */
	/*************************************************************/
	{
	short i;
	for(i=1; i<sndgp->numlev; i++)
	   {
	   if((lvl == sndgp->sndg[i].pres) && (sndgp->sndg[i].drct != -999))
	      { return sndgp->sndg[i].drct;}
	   }
	return -999;
	}

/*=============================================================================*/

float find_wspd ( float lvl )
	/*************************************************************/
	/*  FIND_WSPD                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Given a lvl (mb), returns any observed wind spd (kt).    */
	/*************************************************************/
	{
	short i;
	for(i=1; i<sndgp->numlev; i++)
	   {
	   if((lvl == sndgp->sndg[i].pres) && (sndgp->sndg[i].sped != -999))
	      { return sndgp->sndg[i].sped;}
	   }
	return -999;
	}

/*=============================================================================*/

void remove_lvl ( float lvl )
	/*************************************************************/
	/*  REMOVE_LVL                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Deletes all levels from sndg array of pressure (lvl, mb) */
	/*************************************************************/
	{
	short i;
	for(i=1; i<sndgp->numlev; i++)
	   {
	   if((lvl == sndgp->sndg[i].pres))
	      { sndgp->sndg[i].pres = -1;}
	   }
	}

/*=============================================================================*/

void swap_arrays ( float nsndg[LLMXLV][7] )
	/*************************************************************/
	/*  SWAP_ARRAYS                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Swaps contents of passed array into sndg.                */
	/*************************************************************/
	{
	short ii;

	for(ii=0; ii<sndgp->numlev; ii++)
	   {
	   sndgp->sndg[ii].pres = nsndg[ii][1]; 
	   sndgp->sndg[ii].hght = nsndg[ii][2]; 
	   sndgp->sndg[ii].temp = nsndg[ii][3]; 
	   sndgp->sndg[ii].dwpt = nsndg[ii][4]; 
	   sndgp->sndg[ii].drct = nsndg[ii][5]; 
	   sndgp->sndg[ii].sped = nsndg[ii][6]; 
	   sndgp->sndg[ii].omega = nsndg[ii][0]; 
	   }
	}

/*=============================================================================*/

void write_file ( void )
	/*************************************************************/
	/*  WRITE_FILE                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes contents of sndg array into SHARP95 file.         */
	/*************************************************************/
	{
	short i;
/*	short j; */
	FILE *fout;

	if ( ( sndgp == NULL ) || ( sndgp->numlev < 1 ) ) return;

	fout = fopen( "DECDDROB", "wt" );
	if (fout==NULL) 
	   {
	   printf("Unable to write output file!\n" );
	   return;
	   }
	fprintf(fout, "%%TITLE%%\n%s\n", raobtitle );
	fprintf(fout, "%%SOURCE%%\n%s\n\n", raob_type );
	fprintf( fout, "   LEVEL       HGHT       TEMP       DWPT       WDIR       WSPD         OMEG\n");
	fprintf( fout, "-----------------------------------------------------------------------------\n");
	fputs( "%RAW%\n", fout );
	/*printf("   LEVEL     HGHT     TEMP     DWPT     WDIR     WSPD        OMEG\n");*/
	for(i=0; i<sndgp->numlev; i++)
	   {
           fprintf( fout, "%8.2f,  %8.2f,  %8.2f,  %8.2f,  %8.2f,  %8.2f,  %11.6f\n", 
		sndgp->sndg[i].pres, sndgp->sndg[i].hght, sndgp->sndg[i].temp,
		sndgp->sndg[i].dwpt, sndgp->sndg[i].drct, sndgp->sndg[i].sped,
		sndgp->sndg[i].omega);
	   /*for(j=1; j<=6; j++) 
              {
              fprintf( fout, "%8.2f,  ", sndgp->sndg[i][j]);
              }
           fprintf( fout, "%11.6f\n",sndgp->sndg[i].omega);*/
	   }
	fputs( "%END%\n", fout );
	fclose( fout );

	/*
	printf( "Running KF routine\n" );
	kainfritsch(kain);
	printf( "Writing KF file\n" );
	fout = fopen("DEC2", "wt" );
	if (fout==NULL)
	   {
	   printf( "Unable to write output file!\n" );
	   return;
	   }
	for(i=0; i<sndgp->numlev; i++)
           {
           for(j=1; j<=4; j++) fprintf( fout, "%8.2f,  ", kain[i][j]);
           fprintf( fout, "%8.2f\n", kain[i].drct);
           }
	fclose(fout);
	*/
	}

/*=============================================================================*/

void kainfritsch ( float *kain[LLMXLV][6] )
	/************************************************************/
	/* Run the Kain-Fritsch convective scheme with NSHARP data  */
	/************************************************************/
	{
	short i;

	/* ----- Reconfigure sounding array ----- */
	for(i=0; i<sndgp->numlev; i++)
		{
		*kain[i][0] = sndgp->sndg[i].pres;
		*kain[i][1] = sndgp->sndg[i].temp;
		*kain[i][2] = mixratio(sndgp->sndg[i].pres, sndgp->sndg[i].dwpt);
		*kain[i][3] = kt_to_mps(ucomp(sndgp->sndg[i].drct, sndgp->sndg[i].sped));
		*kain[i][4] = kt_to_mps(vcomp(sndgp->sndg[i].drct, sndgp->sndg[i].sped));
		*kain[i][5] = sndgp->sndg[i].omega;
		}		
	}

/*=============================================================================*/
void open_stationtable ( int *ilun, int *ier );

void open_stationtable ( int *ilun, int *ier )
{
static char stntbl[]="snworld.tbl", defdir[]="stns";

fl_tbop ( stntbl, defdir, ilun, ier, strlen(stntbl), strlen(defdir));
}

float get_stn_info ( long id )
	/*************************************************************/
	/*  GET_SFC_ELEV                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Read LOCATION.DAT file and returns station elevation.    */
	/*************************************************************/
	{
	char stid[8], stnnam[32], tbchars[20];
	int i, istnm, ispri, ilun, ier;
	char stat[3], coun[3];
	float slat, slon, selv;

	sfcelev = 300.0; /* default 300m elevation */
	sprintf(station_name,"Unknown");

	/* ----- Open LOCATION.DAT File ----- */
        open_stationtable ( &ilun, &ier );

	while ( ier == 0 ) 
           {
           tb_rstn ( &ilun, stid, stnnam, &istnm, stat, coun,
		&slat, &slon, &selv, &ispri, tbchars, &ier,
		sizeof(stid), sizeof(stnnam), sizeof(stat),
		sizeof(coun), sizeof(tbchars) ) ;
	   if ( ( ier == 0 ) && ( istnm == id ) )
	      {
	      fl_clos ( &ilun, &ier);
	      memcpy ( station_name, stnnam, 32 );
	      station_name[32] = '\0'; 
	      i = 31;
	      while ( ( i > 0 ) && ( station_name[i] == ' ') )
		 station_name[i] = '\0';
	      sfcelev = selv;
	      return ( selv );
	      }
	   }

        if ( ilun != 0 ) fl_clos ( &ilun, &ier);

	printf( "\aSTATION: %ld  not found in station table...assuming 300m Elevation\n", id);
	return ( sfcelev );
	}

/*=============================================================================*/

void get_title ( char *finam )
	/*************************************************************/
	/*  GET_TITLE                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Determine RAOB information from file name.               */
	/*************************************************************/
	{
	char *f ;
/*	char mo[12], st[10], stn[25]; */
/*	short dy, yr, hr, i; */

	/* ----- Initialize variables ----- */
/*	mo[0]=0; */

	/* ----- Extract file name from total path ----- */
	f = strrchr( finam, '/' );
	if(f==NULL)
	   { f = finam; }
	else
	   { f = f+1; }

	printf( "Extracting Sounding Identity from %s\n", f);

	/*strncpy( mo, f  , 2 ); mo[2]=0;
	strncpy( st, f+2, 2 ); st[2]=0; dy = atoi(st);
	strncpy( st, f+4, 2 ); st[2]=0; yr = atoi(st);
	strncpy( st, f+6, 2 ); st[2]=0; hr = atoi(st);
	strncpy( stn,f+9, 3 ); stn[3]=0;

	-* ----- Convert to Upper Case ----- *-
	for(i=0;i<2;i++) { mo[i] = (char)toupper(mo[i]); }
	for(i=0;i<3;i++) { stn[i] = (char)toupper(stn[i]); }

	-* ----- Change Month Abbreviation ----- *-
	if(strstr(mo, "JA")) { strcpy(mo, "JAN" ); }
	if(strstr(mo, "FE")) { strcpy(mo, "FEB" ); }
	if(strstr(mo, "MA")) { strcpy(mo, "MAR" ); }
	if(strstr(mo, "MR")) { strcpy(mo, "MAR" ); }
	if(strstr(mo, "AP")) { strcpy(mo, "APR" ); }
	if(strstr(mo, "MY")) { strcpy(mo, "MAY" ); }
	if(strstr(mo, "JU")) { strcpy(mo, "JUN" ); }
	if(strstr(mo, "JN")) { strcpy(mo, "JUN" ); }
	if(strstr(mo, "JY")) { strcpy(mo, "JUL" ); }
	if(strstr(mo, "JL")) { strcpy(mo, "JUL" ); }
	if(strstr(mo, "AG")) { strcpy(mo, "AUG" ); }
	if(strstr(mo, "AU")) { strcpy(mo, "AUG" ); }
	if(strstr(mo, "SE")) { strcpy(mo, "SEP" ); }
	if(strstr(mo, "OC")) { strcpy(mo, "OCT" ); }
	if(strstr(mo, "NO")) { strcpy(mo, "NOV" ); }
	if(strstr(mo, "DE")) { strcpy(mo, "DEC" ); }
	sprintf( raobtitle, "%.2d %s %.2d     %.2d UTC     %s", dy, mo, yr, hr, station_name);*/

	sprintf( raobtitle, "%.2d  %.2d UTC     %s", wmo_day, wmo_hour, station_name);
	}

/*=============================================================================*/

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sharp95.h>
#include <ctype.h>

	short numttxx, lvltop, verbose = 1;
	char ttxx[200][6];
	float sfcelev=300;
	long  wmo_id=72349;
	short wmo_hour, wmo_day;

	/*NP*/
	short decode_sounding( char *filename )
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
	short ok, j;


	printf( "Decoding Input File:  %s\n", filename );

	/* ----- Make sure File Exists ----- */
	if( (fin = fopen( filename, "rt" )) == NULL ) { return( 1 ); }
	fclose( fin );

	/* ----- Extract RAOB Title from FileName ----- */
	get_title( filename );

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
	write_file("DECDDROB");

	return 0;
	}



	/*NP*/
	short read_ttaa(char *filename)
	/*************************************************************/
	/*  READ_TTAA                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Reads TTAA data and parses it into ttaa array.           */
	/*************************************************************/
	{
	FILE *fp;
	char rlin[82];
	short ok, aok, i, j;

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
	      if(strlen( rlin ) < 5 ) {aok = 0;}

	      /* ----- Look for beginning of msg ----- */
	      if(strlen(strstr( rlin, "TTAA " ))) { aok += 1; }

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


	/*NP*/
	short read_ttbb(char *filename)
	/*************************************************************/
	/*  READ_TTBB                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Reads TTBB data and parses it into ttbb array.           */
	/*************************************************************/
	{
	FILE *fp;
	char rlin[82];
	short ok, aok, i, j;

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
	      /* ----- Look for beginning of msg ----- */
	      if(strstr( rlin, "TTBB " ) != 0 ) {aok += 1;}

	      /* ----- Look for ending of msg ----- */
	      if(strlen( rlin ) < 5 ) {aok = 0;}

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


	/*NP*/
	short read_ppbb(char *filename)
	/*************************************************************/
	/*  READ_PPBB                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Reads PPBB data and parses it into ttbb array.           */
	/*************************************************************/
	{
	FILE *fp;
	char rlin[82];
	short ok, aok, i, j;

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
	      if(strlen( rlin ) < 5 ) {aok = 0;}

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




	/*NP*/
	short decode_ttaa( void )
	/*************************************************************/
	/*  DECODE_TTAA                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Decodes TTAA file and places it into tadat array.        */
	/*************************************************************/
	{
	char wmo[6], datim[6], dum[6];
	short i, p_wmo, p_datim, p_first, x, y, j, incx;
	float lvl, hght, temp, dwpt, wdir, wspd;

	/* ----- Standard Pointers for TTAA message----- */
	p_wmo = 0;
	p_datim = 2;
	p_first = 4;

	/* ----- Read WMO ID Number ----- */
	strcpy( wmo, ttxx[p_wmo]);
	wmo_id = atol(wmo);
	get_sfc_elev(wmo_id);

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
	   { printf( "Stn=%s   Day=%d   Hour=%d   Top=%d\n", wmo_id, wmo_day, wmo_hour, lvltop); }

	/* ----- Loop through data levels and decode ----- */
	for(i=p_first; i<=numttxx; i+=incx)
	   {

	   /* ----- Check for EOM group ----- */
	   if (strstr(ttxx[i], "51515")) { break; }
	   if (strstr(ttxx[i], "31313")) { break; }

	   /* ----- Convert units and apply rules ----- */
	   incx = 0;
	   lvl_man( i, &incx, &lvl, &hght, &temp, &dwpt, &wdir, &wspd);

	   numlvl++;
	   sndg[numlvl][1] = lvl;
	   sndg[numlvl][2] = hght;
	   sndg[numlvl][3] = temp;
	   sndg[numlvl][4] = dwpt;
	   sndg[numlvl][5] = wdir;
	   sndg[numlvl][6] = wspd;
	   if(hght == -999) { sndg[numlvl][2] = i_h( lvl ); }

	   if(verbose)
	      {
	      printf( "%3d  ", numlvl);
	      for(j=1; j<=6; j++) { printf( "%8.1f ", sndg[numlvl][j]); }
	      printf( "\n" );
	      }

	   }
	return 0;
	}



	/*NP*/
	short decode_ttbb( void )
	/*************************************************************/
	/*  DECODE_TTBB                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Decodes TTBB file and places it into sndg  array.        */
	/*************************************************************/
	{
	char wmo[6], datim[6], dum[6];
	short i, p_wmo, p_datim, p_first, day, hour, x, y, inc, j;
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

	   numlvl++;
	   sndg[numlvl][1] = lvl;
	   sndg[numlvl][2] = hght;
	   sndg[numlvl][3] = temp;
	   sndg[numlvl][4] = dwpt;
	   sndg[numlvl][5] = wdir;
	   sndg[numlvl][6] = wspd;
	   if(hght == -999) { sndg[numlvl][2] = i_h( lvl ); }

	   if(verbose)
	      {
	      printf( "%3d  ", numlvl);
	      for(j=1; j<=6; j++) { printf( "%8.1f ", sndg[numlvl][j]); }
	      printf( "\n" );
	      }

	   /* ----- Increment groups ----- */
	   inc = 2;
	   }
	return 0;
	}


	/*NP*/
	short decode_ppbb( void )
	/*************************************************************/
	/*  DECODE_PPBB                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Decodes PPBB file and places it into sndg  array.        */
	/*************************************************************/
	{
	char wmo[6], datim[6], dum[6];
	short i, p_wmo, p_datim, p_first, day, hour, x, y, inc, j;
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

	      numlvl++;
	      sndg[numlvl][1] = lvl;
	      if(h1==0) { sndg[numlvl][2] = sfcelev; } else { sndg[numlvl][2] = (float)(h1 / 3.281); }
	      sndg[numlvl][3] = temp;
	      sndg[numlvl][4] = dwpt;
	      sndg[numlvl][5] = wdir;
	      sndg[numlvl][6] = wspd;
	      if(lvl == -999) { sndg[numlvl][1] = i_p( sndg[numlvl][2] ); }

	      if(verbose)
		 {
		 printf( "%3d  ", numlvl);
		 for(j=1; j<=6; j++) { printf( "%8.1f ", sndg[numlvl][j]); }
		 printf( "\n" );
		 }

	      inc++;
	      }

	   if(h2 != -1)
	      {
	      /* ----- Convert units and apply rules ----- */
	      lvl_wnd( i + 2, &lvl, &hght, &temp, &dwpt, &wdir, &wspd);

	      numlvl++;
	      sndg[numlvl][1] = lvl;
	      sndg[numlvl][2] = (float)(h2 / 3.281);
	      sndg[numlvl][3] = temp;
	      sndg[numlvl][4] = dwpt;
	      sndg[numlvl][5] = wdir;
	      sndg[numlvl][6] = wspd;
	      if(lvl == -999) { sndg[numlvl][1] = i_p( sndg[numlvl][2] ); }

	      if(verbose)
		 {
		 printf( "%3d  ", numlvl);
		 for(j=1; j<=6; j++) { printf( "%8.1f ", sndg[numlvl][j]); }
		 printf( "\n" );
		 }

	      inc++;
	      }

	   if(h3 != -1)
	      {
	      /* ----- Convert units and apply rules ----- */
	      lvl_wnd( i + 3, &lvl, &hght, &temp, &dwpt, &wdir, &wspd);

	      numlvl++;
	      sndg[numlvl][1] = lvl;
	      sndg[numlvl][2] = (float)(h3 / 3.281);
	      sndg[numlvl][3] = temp;
	      sndg[numlvl][4] = dwpt;
	      sndg[numlvl][5] = wdir;
	      sndg[numlvl][6] = wspd;
	      if(lvl == -999) { sndg[numlvl][1] = i_p( sndg[numlvl][2] ); }

	      if(verbose)
		 {
		 printf( "%3d  ", numlvl);
		 for(j=1; j<=6; j++) { printf( "%8.1f ", sndg[numlvl][j]); }
		 printf( "\n" );
		 }

	      inc++;
	      }


	   }
	return 0;
	}

	/*NP*/
	void lvl_man( short i, short *incx, float *lvl, float *hght,
		      float *temp, float *dwpt, float *wdir, float *wspd)
	/*************************************************************/
	/*  LVL_MAN                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns level values from base file data.                */
	/*************************************************************/
	{
	float depp;
	char dum[10], stnid[4];
	short h1, l1, misg, p1, p2, trop, maxw;

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
	maxw=0;
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
	      maxw = 1;
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


	/*NP*/
	void lvl_sig( short i, float *lvl, float *hght, float *temp,
		       float *dwpt, float *wdir, float *wspd)
	/*************************************************************/
	/*  LVL_SIG                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns level values from TTBB file data.                */
	/*************************************************************/
	{
	float depp;
	char dum[10];
	short h1, l1, misg, p1, p2;

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


	/*NP*/
	void lvl_wnd( short i, float *lvl, float *hght, float *temp,
		       float *dwpt, float *wdir, float *wspd)
	/*************************************************************/
	/*  LVL_WND                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns level values from PPBB file data.                */
	/*************************************************************/
	{
	char dum[10];
	short h1, l1, misg, p1, p2;

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

	/*NP*/
	float i_h( float pres )
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
	short below, above, i, ok, itophght, close;
	double nm1, nm2, nm3, nm4;

	below=0;
	above=0;

	/* ----- Find Height Immediately Above level ----- */
	ok=0;
	close = 999;
	for (i = 1; i <= numlvl; i++)
	   {
	   if((sndg[i][1] <= pres) && (sndg[i][2] != -999) && (sndg[i][1] != -999))
	      {
	      if((short)(pres - sndg[i][1]) < close )
		 {
		 close = (short)(pres - sndg[i][1]);
		 itophght = i;
		 above = i; ok=1;
		 }
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Height Immediately Below level ----- */
	ok=0;
	close = 999;
	for (i = numlvl - 1; i > -1; i--)
	   {
	   if((sndg[i][1] >= pres) && (sndg[i][2] != -999.0F))
	      {
	      if((short)(sndg[i][1] - pres) < close )
		 {
		 close = (short)(sndg[i][1] - pres);
		 below = i; ok=1;
		 }
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( sndg[above][1] == sndg[below][1])
	   {
	   return sndg[above][2];
	   }

	/* ----- Now we need to interpolate to get the height ----- */
	nm1 = sndg[above][2] - sndg[below][2];
	nm2 = log( sndg[below][1] / sndg[above][1] );
	nm4 = log( sndg[below][1] / pres );
	return (float)(sndg[below][2] + (( nm4 / nm2) * nm1));
	}


	/*NP*/
	float i_p( float hght )
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
	double nm1, nm2, nm3, nm4;

	below = 0;
	above = 0;

	/* ----- Find Pressure Immediately Above level ----- */
	ok = 0;
	close = 9999;
	for (i = 0; i < numlvl; i++)
	   {
	   if((sndg[i][2] >= hght) && (sndg[i][1] != -999.0F))
	      {
	      if((short)(sndg[i][2] - hght) < close )
		 {
		 close = (short)(sndg[i][2] - hght);
		 above = i; ok=1;
		 }
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Pressure Below level ----- */
	ok = 0;
	close = 9999;
	for (i = numlvl - 1; i > -1; i--)
	   {
	   if((sndg[i][2] <= hght) && (sndg[i][1] != -999.0F))
	      {
	      if((short)(hght - sndg[i][2]) < close )
		 {
		 close = (short)(hght - sndg[i][2]);
		 below = i; ok=1;
		 }
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( sndg[above][2] == sndg[below][2])
	   {
	   return sndg[above][1];
	   }

	/* ----- Now we need to interpolate to get the Pressure ----- */
	nm1 = hght - sndg[below][2];
	nm2 = sndg[above][2] - sndg[below][2];
	nm3 = log( sndg[above][1] / sndg[below][1] );
	return (float)(sndg[below][1] * exp(( nm1 / nm2) * nm3));
	}

	/*NP*/
	void uasort( void )
	/*************************************************************/
	/*  UASORT                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Sort sndg array into ascending order.                    */
	/*************************************************************/
	{
	short nl1, i, j, k, m_ptr;
	float sndg1[100][6], min;

	nl1=0;
	for(j=1; j<=numlvl; j++)
	   {
	   if(verbose) { printf( "%8.2f %8.2f %8.2f\n", sndg[j][1], sndg[i][2], sndg[j][3]); }

	   /* ----- Loop through every observation in sndg array ----- */
	   min = -1;
	   for(i=1; i<=numlvl; i++)
	      {
	      if((sndg[i][1] > min) && (sndg[i][1] > 0 ))
		 {
		 min = sndg[i][1];
		 m_ptr = i;
		 }
	      }

	   if(min == -1) { break; }

	   nl1++;

	   sndg1[nl1][1] = sndg[m_ptr][1];
	   sndg1[nl1][2] = sndg[m_ptr][2];
	   sndg1[nl1][3] = find_temp( sndg[m_ptr][1] );
	   sndg1[nl1][4] = find_dwpt( sndg[m_ptr][1] );
	   sndg1[nl1][5] = find_wdir( sndg[m_ptr][1] );
	   sndg1[nl1][6] = find_wspd( sndg[m_ptr][1] );
	   remove_lvl( sndg[m_ptr][1] );
	   sndg[m_ptr][1] = -1;
	   }

	numlvl = nl1;

	swap_arrays( sndg1 );

	if(verbose)
	   {
	   for( j=1; j<=numlvl; j++)
	      {
	      printf( "%3d  ", j);
	      for(k=1; k<=6; k++) { printf( "%8.1f ", sndg[j][k]); }
	      printf( "\n" );
	      }
	   }
	}


	/*NP*/
	float find_temp( float lvl )
	/*************************************************************/
	/*  FIND_TEMP                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Given a lvl (mb), returns any observed temperature (c).  */
	/*************************************************************/
	{
	short i;
	for(i=1; i<=numlvl; i++)
	   {
	   if((lvl == sndg[i][1]) && (sndg[i][3] != -999))
	      { return sndg[i][3];}
	   }
	return -999;
	}


	/*NP*/
	float find_dwpt( float lvl )
	/*************************************************************/
	/*  FIND_DWPT                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Given a lvl (mb), returns any observed dew point (c).    */
	/*************************************************************/
	{
	short i;
	for(i=1; i<=numlvl; i++)
	   {
	   if((lvl == sndg[i][1]) && (sndg[i][4] != -999))
	      { return sndg[i][4];}
	   }
	return -999;
	}

	/*NP*/
	float find_wdir( float lvl )
	/*************************************************************/
	/*  FIND_WDIR                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Given a lvl (mb), returns any observed wind dir (deg)    */
	/*************************************************************/
	{
	short i;
	for(i=1; i<=numlvl; i++)
	   {
	   if((lvl == sndg[i][1]) && (sndg[i][5] != -999))
	      { return sndg[i][5];}
	   }
	return -999;
	}

	/*NP*/
	float find_wspd( float lvl )
	/*************************************************************/
	/*  FIND_WSPD                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Given a lvl (mb), returns any observed wind spd (kt).    */
	/*************************************************************/
	{
	short i;
	for(i=1; i<=numlvl; i++)
	   {
	   if((lvl == sndg[i][1]) && (sndg[i][6] != -999))
	      { return sndg[i][6];}
	   }
	return -999;
	}

	/*NP*/
	void remove_lvl( float lvl )
	/*************************************************************/
	/*  REMOVE_LVL                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Deletes all levels from sndg array of pressure (lvl, mb) */
	/*************************************************************/
	{
	short i;
	for(i=1; i<=numlvl; i++)
	   {
	   if((lvl == sndg[i][1]))
	      { sndg[i][1] = -1;}
	   }
	}


	/*NP*/
	void swap_arrays( float nsndg[100][6] )
	/*************************************************************/
	/*  SWAP_ARRAYS                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Swaps contents of passed array into sndg.                */
	/*************************************************************/
	{
	short i, j;

	for(i=1; i<=numlvl; i++)
	   {
	   for(j=1; j<=6; j++) { sndg[i-1][j] = nsndg[i][j]; }
	   }
	}


	/*NP*/
	void write_file( char *filename )
	/*************************************************************/
	/*  WRITE_FILE                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes contents of sndg array into SHARP95 file.         */
	/*************************************************************/
	{
	short i, j;
	float kain[100][6];
	FILE *fout;

	fout = fopen( filename, "wt" );
	if (fout==NULL) 
	   {
	   printf("Unable to write output file!\n" );
	   return;
	   }
	fprintf(fout, "%%TITLE%%\n%s\n", raobtitle );
	fprintf(fout, "%%SOURCE%%\n%s\n\n", raob_type );
	fprintf( fout, "   LEVEL       HGHT       TEMP       DWPT       WDIR       WSPD       OMEGA\n");
	fprintf( fout, "   (mb)        (m)        (C)        (C)        (deg)      (kts)      (mb/s)\n");
	fprintf( fout, "-----------------------------------------------------------------------------\n");
	fputs( "%RAW%\n", fout );
	/*printf("   LEVEL     HGHT     TEMP     DWPT     WDIR     WSPD        OMEG\n");*/
	for(i=0; i<numlvl; i++)
	   {
	   for(j=1; j<=6; j++) 
              {
              fprintf( fout, "%8.2f,  ", sndg[i][j]);
              /*printf( "%8.2f ", sndg[i][j]);*/
              }
           if (sndg[i][0]==sndg[i][2]) 
                fprintf( fout, "%11.6f\n",-999.0);
           else
           	fprintf( fout, "%11.6f\n",sndg[i][0]);
           /*printf("%11.6f\n",sndg[i][0]);*/
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
	for(i=0; i<numlvl; i++)
           {
           for(j=1; j<=4; j++) fprintf( fout, "%8.2f,  ", kain[i][j]);
           fprintf( fout, "%8.2f\n", kain[i][5]);
           }
	fclose(fout);
	*/
	}

	/*NP*/
	void kainfritsch( float *kain[100][6] )
	/************************************************************/
	/* Run the Kain-Fritsch convective scheme with NSHARP data  */
	/************************************************************/
	{
	short i;

	/* ----- Reconfigure sounding array ----- */
	for(i=0; i<=numlvl; i++)
		{
		*kain[i][0] = sndg[i][1];
		*kain[i][1] = sndg[i][3];
		*kain[i][2] = mixratio(sndg[i][1], sndg[i][4]);
		*kain[i][3] = kt_to_mps(ucomp(sndg[i][5], sndg[i][6]));
		*kain[i][4] = kt_to_mps(vcomp(sndg[i][5], sndg[i][6]));
		*kain[i][5] = sndg[i][0];
		}		
	}

	/*NP*/
	float get_sfc_elev( long id )
	/*************************************************************/
	/*  GET_SFC_ELEV                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Read LOCATION.DAT file and returns station elevation.    */
	/*************************************************************/
	{
	FILE *fp;
	char st[100], stn[4], dum1[50], dum2[20], dum3[20];
	long wmo1;
	short elev;

	/* ----- Open LOCATION.DAT File ----- */
	fp = fopen( "LOCATION.DAT", "rt" );
	if(fp == NULL)
	   {
	   printf( "\aLOCATION.DAT file not found...assuming a 300m station elevation\n");
	   return (float)300;
	   }

	while(fgets(st, 80, fp) != NULL)
	   {
	   /* ----- Decode each line of file ----- */
	   sscanf( st, "%ld, %s", &wmo1, stn);
	   stn[3]=0;
	   sscanf( st+29, "%d,", &elev);

	   if(id == wmo1)
	      {
	      fclose(fp);
	      printf( "STATION: %ld  %s  ELEV=%d\n", wmo1, stn, elev);
	      strcpy( stn_abbrev, stn);
	      sfcelev=(float)elev;
	      return (float)elev;
	      }
	   }

	printf( "\aSTATION: %ld  not found in LOCATION.DAT file...assuming 300m Elevation\n");
	fclose(fp);
	sfcelev=300;
	return (float)300;
	}


	/*NP*/
	void get_title( char *finam )
	/*************************************************************/
	/*  GET_TITLE                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Determine RAOB information from file name.               */
	/*************************************************************/
	{
	char *f, st[10], mo[12], stn[25];
	short dy, yr, hr, i;

	/* ----- Initialize variables ----- */
	mo[0]=0;

	/* ----- Extract file name from total path ----- */
	f = strrchr( finam, '/' );
	if(f==NULL)
	   { f = finam; }
	else
	   { f = f+1; }

	printf( "Extracting Sounding Identity from %s\n", f);

	strncpy( mo, f  , 2 ); mo[2]=0;
	strncpy( st, f+2, 2 ); st[2]=0; dy = atoi(st);
	strncpy( st, f+4, 2 ); st[2]=0; yr = atoi(st);
	strncpy( st, f+6, 2 ); st[2]=0; hr = atoi(st);
	strncpy( stn,f+9, 3 ); stn[3]=0;

	/* ----- Convert to Upper Case ----- */
	for(i=0;i<2;i++) { mo[i] = (char)_toupper(mo[i]); }
	for(i=0;i<3;i++) { stn[i] = (char)_toupper(stn[i]); }

	/* ----- Change Month Abbreviation ----- */
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

	strcpy( stn, get_stn_name(stn));

	sprintf( raobtitle, "%.2d %s %.2d     %.2d UTC     %s", dy, mo, yr, hr, stn);
	}


	/*NP*/
	char *get_stn_name( char *stn )
	/*************************************************************/
	/*  GET_STN_NAME                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Read LOCATION.DAT file and returns station name.         */
	/*************************************************************/
	{
	FILE *fp;
	char st[100], name1[4], dum2[20], dum3[20];
	static char name2[50];
	long wmo1;
	short elev;

	/* ----- Open LOCATION.DAT File ----- */
	fp = fopen( "LOCATION.DAT", "rt" );
	if(fp == NULL)
	   { 
	   printf( "LOCATION.DAT file not found\n" );
	   return stn; 
	   }

	while(fgets(st, 80, fp) != NULL)
	   {
	   /* ----- Decode each line of file ----- */
	   sscanf( st, "%ld, %s", &wmo1, name1);
	   strncpy( name2, st+11, 20);
	   name1[3]=0;
	   name2[17]=0;

	   if(strstr(stn, name1))
	      {
	      fclose(fp);
	      printf( "STATION: %ld  %s\n", wmo1, name2);
	      strcpy( stn_abbrev, stn);
	      return (char *)name2;
	      }
	   }

	fclose(fp);
	return stn;
	}

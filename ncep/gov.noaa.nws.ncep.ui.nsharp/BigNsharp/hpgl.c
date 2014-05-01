/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  HP-Laserjet III+ Very High Quality Printer Output          */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  PRINT_SOUNDING                                             */
/*  OPEN_PRINTER                                               */
/*  HPGL_SETUP                                                 */
/*  HPSELECT_PEN                                               */
/*  HPMOVETO                                                   */
/*  HPLINETO                                                   */
/*  HPRECTANGLE                                                */
/*  HPCLIPRGN                                                  */
/*  HPCLOSE_PRINTER                                            */
/*  HPDRAW_SKEWT                                               */
/*  HPISOBAR                                                   */
/*  HPISOTHERM                                                 */
/*  HPPRES_TO_PIX                                              */
/*  HPTEMP_TO_PIX                                              */
/*  HPDRY_ADIABAT                                              */
/*  HPTRACE_TEMP                                               */
/*  HPTRACE_VTMP                                               */
/*  HPTRACE_DWPT                                               */
/*  HPTRACE_WETBULB                                            */
/*  HPOUTTEXT                                                  */
/*  HPWIND_BARB                                                */
/*  HPPLOT_BARBS                                               */
/*  HPTRACE_PARCEL                                             */
/*  HPTRACE_PARCELX                                            */
/*  HPSET_FONT                                                 */
/*  HPTRIANGLE                                                 */
/*  HPDRAW_HODO                                                */
/*  HPHODO_TO_PIX                                              */
/*  HPTRACE_HODO                                               */
/*  HPLABEL_HODO                                               */
/*  HPPLOT_ELEVS                                               */
/*  HPHODO_CIRCS                                               */
/*  HPCIRCLE                                                   */
/*  HPWRITE_PARCEL                                             */
/*  HPWRITE_THERMO                                             */
/*  HPDISP_PARAM                                               */
/*  HPFILL_CAPE                                                */
/*  HPFIL_CAPEX                                                */
/*  HPCENTERTEXT                                               */
/*  HPLVLDATA                                                  */
/*  HPSHARP_LABEL                                              */
/*                                                             */
/***************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "sharp95.h"

struct _skewt hsk = { 500, 850, 5636, 5986, 100, 70, 60 , 1 };
struct _hodog hho = { 3500, 850, 5636, 2950, 0, 0, 120, 20 };

	/*NP*/
void print_sounding_hpgl( void )
	/*************************************************************/
	/*  PRINT_SOUNDING                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Prints skewt/hodograph to LPT1.                          */
	/*************************************************************/
{
	FILE *fp;
	char	unixcmd[200];

	/* ----- Laserjet Printer Stuff ----- */
	fp = open_printer();
	if (fp==NULL) {
	   printf("\a");
	   return;
	}

	hpgl_setup( fp );
	hpselect_pen( fp, 1, 1, 9, 100 );
        hpdraw_skewt( fp ); 
	hpdraw_hodo( fp );
	hpwrite_parcel( fp ); 
	hpwrite_thermo( fp ); 
	hpwrite_winds( fp );
	hpwrite_storm( fp );
	hpplot_uvvs( fp );
	sharp_label( fp, 75, 9845 ); 
	hpclose_printer( fp );
/* JL fixing the Linux lp command by using "-o raw" for the lp flag
	sprintf(unixcmd,"lp -onb -oletter %s", config.lptname );
JL */
	sprintf(unixcmd,"lp -o raw %s", config.lptname );
	system(unixcmd);
	sprintf(unixcmd,"rm -f %s", config.lptname );
	/* system(unixcmd); */
}

	/*NP*/
	FILE *open_printer( void )
	/*************************************************************/
	/*  OPEN_PRINTER                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Opens stream for output to printer.  Returns FILE handle.*/
	/*                                                           */
	/*  fin        - Name of file to open                        */
	/*************************************************************/
	{
	FILE *f1;
	sprintf (config.lptname, "/tmp/sharphpgl.plt" );
	f1 = fopen( config.lptname, "wb" );
	return f1;
	}

	/*NP*/
	void hpgl_setup( FILE *fp )
	/*************************************************************/
	/*  HPGL_SETUP                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Sets HP-Laserjet III+ to HP-GL/2 graphics mode.          */
	/*                                                           */
	/*  fp         - File handle                                 */
	/*************************************************************/
	{
	/* ----- Reset Printer ----- */
	fputs( "\033E", fp );

	/* ----- Set to HP-GL/2 mode ----- */
	fputs( "\033%0B", fp );

	/* ----- Initialize HP-GL/2 mode ----- */
	fputs( "IN;", fp );

	/* ----- Set Standard Font ----- */
	fputs( "SD1,21,2,1,4,10,5,0,6,0,7,4148;SS;", fp );

	/* ----- Tell Laserjet to begin plot ----- */
	fputs( "BP;", fp );

	/* ----- Transparency mode off ----- */
	fputs( "TR0;", fp );

	}

	/*NP*/
	void hpselect_pen( FILE *fp , short onoff, float width, short type,
			   short greyscale )
	/*************************************************************/
	/*  HPSELECT_PEN                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Selects the appropriate graphics pen in HP-GL/2 mode.    */
	/*                                                           */
	/*  fp         - File handle                                 */
	/*  onoff      - Selects pin color (1=black, 0=white)        */
	/*  width      - Width in pels                               */
	/*  type       - Line style (0-8), 9=Solid                   */
	/*                                                           */
	/*************************************************************/
	{
	/* ----- Select Pen ----- */
	fprintf( fp, "SP%d;", onoff );

	/* ----- Set Pen Width in millimeters----- */
	fprintf( fp, "PW%f;", width );

	/* ----- Set Pen Type ----- */
	if( type == 9 )
	   { fputs( "LT;", fp); }
	if( type == 10 )
	   {
	   fputs( "UL8,2,2,2,2,2,2,2,2;", fp);
	   fputs( "LT8,5;", fp);
	   }
	if( type == 11 )
	   {
	   fputs( "UL8,2,2,2,2,2,2,2,2;", fp);
	   fputs( "LT8,2;", fp);
	   }
	if( type == -2 )
	   { fprintf( fp, "LT%d, 1;", type ); }
	if( type == -1 )
	   { fprintf( fp, "LT%d, 1;", type ); }

        /* ----- Set Pen Density (0-100) ----- */
        fprintf( fp, "SV1,%d;", greyscale);
	}

	/*NP*/
	void hpmoveto( FILE *fp , short x, short y )
	/*************************************************************/
	/*  HPMOVETO                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Moves HPGL cursor to given coordinates (pels)            */
	/*************************************************************/
	{
	y = (1016 * 11) - 1050 - y;
	fprintf( fp, "PU%d,%d;", x, y);
	}

	/*NP*/
	void hplineto( FILE *fp , short x, short y )
	/*************************************************************/
	/*  HPLINETO                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Strikes a line from current cursor location to (x,y)     */
	/*************************************************************/
	{
	y = (1016 * 11) - 1050 - y;
	fprintf( fp, "PD%d,%d;", x, y);
	}

	/*NP*/
	void hprectangle( FILE *fp , short x1, short y1, short x3, short y3,
			  short fill, short pct)
	/*************************************************************/
	/*  HPRECTANGLE                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws a rectangle bounded by (x1,y1) and (x2,y2).        */
	/*                                                           */
	/*  fill   -   Fill Indicator (1=YES)                        */
	/*  pct    -   Percent coverage of fill (0-100%)             */
	/*************************************************************/
	{
	short x2, y2, x4, y4;

	x2 = x3;
	y2 = y1;

	x4 = x1;
	y4 = y3;

	hpmoveto( fp, x1, y1);
	fputs( "PM0;", fp );
	hplineto( fp, x2, y2);
	hplineto( fp, x3, y3);
	hplineto( fp, x4, y4);
	hplineto( fp, x1, y1);
	fputs( "PM2;", fp );
	if( fill == 1)
	   {
	   fprintf( fp, "FT10,%d;", pct );
	   fputs( "FP;", fp );
	   }
	else
	   {
	   fputs( "EP;", fp );
	   }
	}

	/*NP*/
	void hpcliprgn( FILE *fp , short x1, short y1, short x2, short y2 )
	/*************************************************************/
	/*  HPCLIPRGN                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Defines drawing area with points (x1,y1) and (x2,y2).    */
	/*************************************************************/
	{
	y1 = (1016 * 11) - 1050 - y1;
	y2 = (1016 * 11) - 1050 - y2;
	fprintf( fp, "IW%d,%d,%d,%d;", x1, y1, x2, y2);
	}

	/*NP*/
	void hpclose_printer( FILE *fp )
	/*************************************************************/
	/*  HPCLOSE_PRINTER                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Ends HP-GL/2 mode and closes printer.                    */
	/*************************************************************/
	{
	/* ----- Enters PCL mode ----- */
	fputs( "\033%0A", fp );

	/* ----- Reset Printer ----- */
	fputs( "\033E", fp );

	fclose( fp );
	}

	/*NP*/
	void hpdraw_skewt( FILE *fp )
	/*************************************************************/
	/*  HPDRAW_SKEWT                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws a standard Skew-T/LogP diagram on a HP Laserjet    */
	/*  printer.  All graphic calls are HP-GL/2 dependent.       */
	/*************************************************************/
	{
	short x1, y1, x2, y2, y, i, j;
	float a, thta, ix1, sbcp, mucp;
	float lvl[] = {1050,1000,850,700,500,300,200,100};
	char st[90], rtitle[80];
	struct _parcel pcl;

	/* ----- Write Title Line at top of chart ----- */
	if (overlay_previous == 1)
	   {
	   sprintf( rtitle, "Background:  %s (%s)", raobtitle2, raob_type2);
	   hpouttext( fp, rtitle, hsk.tlx, hsk.tly - 250, 12, 3);
	   sprintf( rtitle, "Foreground:  %s (%s)", raobtitle, raob_type);
	   hpouttext( fp, rtitle, hsk.tlx, hsk.tly - 65, 12, 3);
	   }
	else
 	   {
	   sprintf( rtitle, "%s (%s)", raobtitle, raob_type);
	   hpouttext( fp, rtitle, hsk.tlx, hsk.tly - 65, 12, 3);
	   }

	/* ----- Begin with outer boundary and frame ----- */
	hpselect_pen( fp, 1, .75F, 9, 100 );
	hprectangle( fp, hsk.tlx, hsk.tly, hsk.brx, hsk.bry, 0, 0 );
	hpcliprgn(   fp, hsk.brx, hsk.tly, hsk.tlx, hsk.bry );

	/* ----- Draw Horizontal Pressure Lines ----- */
	hpselect_pen( fp, 1, 0, 9 , 100);
	for(i=1; i<=7; i++) { hpisobar( fp, lvl[i], 0 ); }
	for(i=150; i<1050; i += 50) { hpisobar( fp, (float)i, 1 ); }

	/* ----- Draw Skewed Temperature Lines ----- */
	hpselect_pen( fp, 1, 0, -2 , 100);
	for( i=-160; i<=50; i += 10 ) { hpisotherm( fp, (float)i ); }

	/* ----- Draw Dry Adiabats ----- */
	hpselect_pen( fp, 1, 0, 9 , 100);
	for( thta=-70; thta<=350; thta += 20) { hpdry_adiabat( fp, thta); };

	/* ----- If overlay is ON, plot old temp/dwpt data ----- */
	if (overlay_previous == 1)
	   {
	   printf( "Drawing overlay data\n" );
	   hpselect_pen( fp, 1, .75F, 9 , 20);
	   hptrace_temp2( fp );
	   hptrace_dwpt2( fp );
           hpselect_pen( fp, 1, .25F, 9 , 30);
           hpplot_barbs2( fp );
	   }

	hpcliprgn(   fp, hsk.brx, hsk.tly, hsk.tlx, hsk.bry );

	/* ----- Plot Environmental Temperature Data ----- */
	hpselect_pen( fp, 1, .75F, 9 , 100);
	hptrace_temp( fp );

	/* ----- Plot Environmental Dew Point Data ----- */
	hpselect_pen( fp, 1, .75F, 9 , 100);
	hptrace_dwpt( fp );

	/* ----- Plot Environmental Virtual Temperature Data ----- */
	hpselect_pen( fp, 1, .25F, 11 , 100);
	hptrace_vtmp( fp );

	/* ----- Plot Environmental Wet Bulb Temperature Data ----- */
	hpselect_pen( fp, 1, .25F, 11 , 100);
	hptrace_wetbulb( fp );

	/* ----- Plot Environmental Wind Barbs ----- */
	hpselect_pen( fp, 1, .25F, 9 , 100);
	hpplot_barbs( fp );

	/* ----- Plot Elevation legend ----- */
	hpselect_pen( fp, 1, .25F, 9 , 100);
	hpplot_elevs( fp );

	/* ----- Trace Lifted Parcel ----- */
	define_parcel(1, 0);
	sbcp = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	define_parcel(3, 400);
        mucp = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	if (sbcp > mucp)
		{
		define_parcel(1,0);
		}
	hpselect_pen( fp, 1, .35F, 10 , 100);
	hptrace_parcel( fp, lplvals.pres, lplvals.temp, lplvals.dwpt);

	hpfill_cape( fp, 20 );
	}

	/*NP*/
	void hpisobar( FILE *fp, float pres, short flag )
	/*************************************************************/
	/*  HPISOBAR                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws pressure lines (pres, mb) on SkewT graphic.        */
	/*                                                           */
	/*  flag = 0    Draw complete horizontal line                */
	/*       = 1    Draw small tick marks along sides of chart   */
	/*************************************************************/
	{
	short y;
	char st[10];

	y = hppres_to_pix( pres );
	if( flag == 0 )
	   {
	   hpcliprgn( fp , 20000, 0, 0, 20000 );
	   itoa( (short)pres, st, 10 );
	   hpouttext( fp, st, hsk.tlx - 355, y + 60, 10, 0 );
	   hpcliprgn( fp , hsk.brx, hsk.tly, hsk.tlx, hsk.bry );

	   hpmoveto( fp, hsk.tlx, y);
	   hplineto( fp, hsk.brx, y);
	   }
	else
	   {
	   hpmoveto( fp, hsk.tlx, y);
	   hplineto( fp, hsk.tlx + 100, y);
	   hpmoveto( fp, hsk.brx, y);
	   hplineto( fp, hsk.brx - 100, y);
	   }
	}

	/*NP*/
	void hpisotherm( FILE *fp, float temp)
	/*************************************************************/
	/*  HPISOTHERM                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws temperature lines (temp, c) on SkewT graphic.      */
	/*************************************************************/
	{
	short x1, y1, x2, y2;
	char st[10];

	if((temp >= -30) && (temp <= 50))
	   {
	   x1 = hptemp_to_pix( temp, 1050 );
	   y1 = hsk.bry;
	   hpcliprgn( fp , 20000, 0, 0, 20000 );
	   itoa( (short)temp, st, 10 );
	   hpouttext( fp, st, x1, y1 + 160, 10, 0 );
	   hpcliprgn( fp , hsk.brx, hsk.tly, hsk.tlx, hsk.bry );
	   }
	x1 = hptemp_to_pix( temp, 1050 );
	y1 = hsk.bry;
	hpmoveto( fp, x1, y1 );
	x2 = hptemp_to_pix( temp, 100 );
	y2 = hsk.tly;
	hplineto( fp, x2, y2 );
	}

	/*NP*/
	short hppres_to_pix( float pres )
	/*************************************************************/
	/*  HPPRES_TO_PIX                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given pressure (mb) to an Y coordinate on       */
	/*  Skewt graphic.                                           */
	/*************************************************************/
	{
	double scl1, scl2;
	scl1 = log(1050) - log(100);
	scl2 = log(1050) - log(pres);
	return (short)(hsk.bry - (scl2 / scl1) * (hsk.bry - hsk.tly));
	}

	/*NP*/
	short hptemp_to_pix( float temp, float pres )
	/*************************************************************/
	/*  HPTEMP_TO_PIX                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given temperature (c) to an X coordinate on     */
	/*  Thermodynamic diagram.                                   */
	/*                                                           */
	/*  Depending on (hsk.type), relationship is for (1) Pseudo-  */
	/*  Adiabatic or (2) Skew-T/Log P diagram.                   */
	/*                                                           */
	/*  Skew:  90c spread across the bottom of chart.            */
	/*        120c spread up-and-down the chart.                 */
	/*         Temp at BR of chart = 50c                         */
	/*************************************************************/
	{
	float scl1, scl2;

	if( hsk.type == 1 )
	   {
	   scl1 = (float)hsk.brtemp - ((((float)hsk.bry -
	   (float)hppres_to_pix( pres )) / ((float)hsk.bry - (float)hsk.tly)) *
	   (float)hsk.vspread);
	   }
	else
	   {
	   scl1 = hsk.brtemp;
	   }
	scl2 = hsk.brx - (((scl1 - temp) / hsk.hspread) * (hsk.brx - hsk.tlx));
	return (short)scl2;
	}

	/*NP*/
	void hpdry_adiabat( FILE *fp, float thta)
	/*************************************************************/
	/*  DRY_ADIABAT                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws dry adiabat of theta (thta, c) on SkewT graphic.   */
	/*************************************************************/
	{
	float pres, temp;
	short x, y;

	for( pres = 1050; pres >= 100; pres = pres - 50 )
	   {
	   temp = ((thta + 273.15) / pow( 1000.0 / pres, ROCP )) - 273.15;

	   x = hptemp_to_pix( temp , pres );
	   y = hppres_to_pix( pres );

	   if(pres == 1050)
	      { hpmoveto( fp, x, y ); }
	   else
	      { hplineto( fp, x, y ); }
	   }
	}

	/*NP*/
	void hptrace_temp( FILE *fp )
	/*************************************************************/
	/*  HPTRACE_TEMP                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental temperature trace on SkewT.          */
	/*************************************************************/
	{
	short i, j, x, y, xold, yold, ok = 0, idxt, idxp;

        if (!sndg || numlvl < 1) return;

        idxt  = getParmIndex("TEMP");
        idxp  = getParmIndex("PRES");

        if (idxt == -1 || idxp == -1) return;

	for( i=0; i < numlvl; i++) {
	   if (sndg[i][idxt] > -200)
	      {
	      xold = x;
	      yold = y;
	      x = hptemp_to_pix( sndg[i][idxt], sndg[i][idxp] );
	      y = hppres_to_pix( sndg[i][idxp] );
	      if( ok == 0)
		 { hpmoveto( fp, x, y); ok = 1; }
	      else
		 { hplineto( fp, x, y); }
	      }
	   }
	}

        /*NP*/
        void hptrace_temp2( FILE *fp )
        /*************************************************************/
        /*  HPTRACE_TEMP                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Plots environmental temperature trace on SkewT.          */
        /*************************************************************/
        {
        short i, j, x, y, xold, yold, ok = 0, numlvl2, idxt, idxp;

        if (!sndg || numlvl < 1) return;

        idxt  = getParmIndex("TEMP");
        idxp  = getParmIndex("PRES");

        if (idxt == -1 || idxp == -1) return;

	numlvl2 = sndg2[0][0];
	if (numlvl2 == 0) return;

        for( i=0; i < numlvl2; i++)
           {
           if( sndg2[i][idxt] > -200)
              {
              xold = x;
              yold = y;
              x = hptemp_to_pix( sndg2[i][idxt], sndg2[i][idxp] );
              y = hppres_to_pix( sndg2[i][idxp] );
              if( ok == 0)
                 { hpmoveto( fp, x, y); ok = 1; }
              else
                 { hplineto( fp, x, y); }
              }
           }
        }

	/*NP*/
	void hptrace_vtmp( FILE *fp )
	/*************************************************************/
	/*  HPTRACE_VTMP                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental virtual temp trace on SkewT.         */
	/*************************************************************/
	{
	short i, j, x, y, xold, yold, ok = 0, idxt, idxp, idxtd;

        if (!sndg || numlvl < 1) return;

        idxt  = getParmIndex("TEMP");
        idxp  = getParmIndex("PRES");
        idxtd  = getParmIndex("DWPT");

        if (idxt == -1 || idxp == -1 || idxtd == -1) return;

	for( i=0; i < numlvl; i++)
	   {
	   if( sndg[i][idxtd] > -200)
	      {
	      xold = x;
	      yold = y;
	      x = hptemp_to_pix(virtemp(sndg[i][idxp], sndg[i][idxt], sndg[i][idxtd]), sndg[i][idxp]);
	      y = hppres_to_pix( sndg[i][idxp] );
	      if( ok == 0)
		 { hpmoveto( fp, x, y); ok = 1; }
	      else
		 { hplineto( fp, x, y); }
	      }
	   }
	}

	/*NP*/
	void hptrace_dwpt( FILE *fp )
	/*************************************************************/
	/*  HPTRACE_DWPT                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental Dew Point trace on SkewT.            */
	/*************************************************************/
	{
	short i, j, x, y, xold, yold, ok = 0, idxt, idxtd, idxp;

        if (!sndg || numlvl < 1) return;

        idxt  = getParmIndex("TEMP");
        idxp  = getParmIndex("PRES");
        idxtd  = getParmIndex("DWPT");

        if (idxt == -1 || idxp == -1 || idxtd == -1) return;

	for( i=0; i < numlvl; i++)
	   {
	   if( sndg[i][idxtd] > -200)
	      {
	      xold = x;
	      yold = y;
	      x = hptemp_to_pix( sndg[i][idxtd], sndg[i][idxp] );
	      y = hppres_to_pix( sndg[i][idxp] );
	      if( ok == 0)
		 { hpmoveto( fp, x, y); ok = 1; }
	      else
		 { hplineto( fp, x, y); }
	      }
	   }
	}

        /*NP*/
        void hptrace_dwpt2( FILE *fp )
        /*************************************************************/
        /*  HPTRACE_DWPT                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Plots environmental Dew Point trace on SkewT.            */
        /*************************************************************/
        {
        short i, j, x, y, xold, yold, ok = 0, numlvl2, idxt, idxtd, idxp;

        if (!sndg || numlvl < 1) return;

        idxt  = getParmIndex("TEMP");
        idxp  = getParmIndex("PRES");
        idxtd  = getParmIndex("DWPT");

        if (idxt == -1 || idxp == -1 || idxtd == -1) return;

	numlvl2 = sndg2[0][0];
	if (numlvl2 == 0) return;

        for( i=0; i < numlvl2; i++)
           {
           if( sndg2[i][idxtd] > -200)
              {
              xold = x;
              yold = y;
              x = hptemp_to_pix( sndg2[i][idxtd], sndg2[i][idxp] );
              y = hppres_to_pix( sndg2[i][idxp] );
              if( ok == 0)
                 { hpmoveto( fp, x, y); ok = 1; }
              else
                 { hplineto( fp, x, y); }
              }
           }
        }

	/*NP*/
	void hptrace_wetbulb( FILE *fp )
	/*************************************************************/
	/*  HPTRACE_WETBULB                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental Wet Bulb trace on SkewT.             */
	/*************************************************************/
	{
	short i, j, x, y, xold, yold, ok = 0, idxt, idxtd, idxp;
	float t1;

        if (!sndg || numlvl < 1) return;

        idxt  = getParmIndex("TEMP");
        idxp  = getParmIndex("PRES");
        idxtd  = getParmIndex("DWPT");

        if (idxt == -1 || idxp == -1 || idxtd == -1) return;

	for( i=0; i < numlvl; i++)
	   {
	   if( sndg[i][idxtd] > -200)
	      {
	      xold = x;
	      yold = y;

	      t1 = wetbulb( sndg[i][idxp], sndg[i][idxt], sndg[i][idxtd] );
	      x = hptemp_to_pix( t1, sndg[i][idxp] );
	      y = hppres_to_pix( sndg[i][idxp] );
	      if( ok == 0)
		 { hpmoveto( fp, x, y); ok = 1; }
	      else
		 { hplineto( fp, x, y); }
	      }
	   }
	}

	/*NP*/
	void hpouttext( FILE *fp, char *st, short x, short y, short pts, short bold)
	/*************************************************************/
	/*  HPOUTTEXT                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes given string at location (x,y).                   */
	/*  pts         -  Number of points for given font           */
	/*  bold        -  Typeface indicator (0=NORM, 3=BOLD)       */
	/*************************************************************/
	{
	hpmoveto(fp, x, y);

	fprintf( fp, "AD1,21,2,1,4,%d,5,0,6,%d,7,4148;SA;", pts, bold);

	fputs( "DT~;", fp);
	fputs( "LB", fp);
	fputs( st, fp );
	fputs( "~;", fp );
	}

	/*NP*/
	void hpwind_barb( FILE *fp, float wdir, float wspd, short x,
			  short y, short siz, short grayflag)
	/*************************************************************/
	/*  HPWIND_BARB                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots wind barb at location (x,y) for given wind.        */
	/*************************************************************/
	{
	short x1, y1, x2, y2, x3, y3, sped, flag, maxsiz=3;
	float dx, dy, spcx, spcy, wid, hgt;

	dx = -ucomp(wdir, 10) * siz / 1.5;
	dy = -vcomp(wdir, 10) * siz / 1.5;

	x1 = x;
	y1 = y;
	x2 = x1 + (short)dx;
	y2 = y1 - (short)dy;

	/* ----- Draw backbone of wind barb, along with origin dot ----- */
	hpmoveto(fp, x1, y1); hplineto(fp, x2, y2);

	sped = (short)wspd;
	x1 = x2;
	y1 = y2;

	wid = 6;                        /* Width of flags */
	spcx = dx / wid;
	spcy = dy / wid;
	x1 = x1 + (short)spcx;
	y1 = y1 - (short)spcy;

	/* ----- Draw wind flags (increments of 50kt) ----- */
	flag = 0;
	while (sped > 47)
	   {
	   flag = 1;
	   x1 = x1 - (short)spcx;
	   y1 = y1 + (short)spcy;

	   hgt = .5F;                   /* Heigth of flags */
	   x2 = x1 + (short)(dy * hgt);
	   y2 = y1 + (short)(dx * hgt);

	   x3 = x1 - (short)spcx;
	   y3 = y1 + (short)spcy;

	   if (grayflag==1)
	   	{ hptriangle( fp, x1, y1, x2, y2, x3, y3, 2); }
	   else
	   	{ hptriangle( fp, x1, y1, x2, y2, x3, y3, 1); }
	   hpmoveto( fp, x1, y1);
	   hplineto( fp, x2, y2);
	   hplineto( fp, x3, y3);

	   sped -= 50;
	   x1 = x3;
	   y1 = y3;
	   }

	/* ----- Draw wind barbs (increments of 5kt) ----- */
	while (sped > 7 )
	   {
	   hgt = .5F;                   /* Heigth of flags */
	   x2 = x1 + (short)(dy * hgt);
	   y2 = y1 + (short)(dx * hgt);

	   x3 = x1 - (short)spcx;
	   y3 = y1 + (short)spcy;

	   hpmoveto(fp, x3, y3); hplineto(fp, x2, y2);
	   sped -= 10;
	   x1 = x3;
	   y1 = y3;
	   }
	/* ----- Draw short barb ----- */
	if(sped > 3 )
	   {
	   hgt = .5F;                   /* Heigth of flags */
	   x2 = x1 + (short)(dy * hgt);
	   y2 = y1 + (short)(dx * hgt);

	   x3 = x1 - (short)spcx;
	   y3 = y1 + (short)spcy;

	   dx = (x3 - x2) / 2;
	   dy = (y3 - y2) / 2;

	   x2 = x3 - (short)dx;
	   y2 = y3 - (short)dy;

	   hpmoveto(fp, x3, y3); hplineto(fp, x2, y2);

	   sped -= 10;
	   x1 = x3;
	   y1 = y3;
	   }
	}


	/*NP*/
	void hpplot_barbs( FILE *fp )
	/*************************************************************/
	/*  HPPLOT_BARBS                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots wind barbs along side of thermo diagram.           */
	/*************************************************************/
	{
	short i, x, y, idxp, idxh, idxwd, idxws;
	float lasthght;

	idxp  = getParmIndex("PRES");
	idxh  = getParmIndex("HGHT");
	idxws = getParmIndex("SPED");
	idxwd = getParmIndex("DRCT");

	if (idxp == -1 || idxh == -1 || idxws == -1 || idxwd == -1) return;

	hpcliprgn( fp , 20000, 0, 0, 20000 );

	/* ----- Draw vertical line ----- */
	hpmoveto( fp, hsk.brx + 500, hsk.tly);
	hplineto( fp, hsk.brx + 500, hsk.bry);

	lasthght = 0;
	for( i = 0; i < numlvl; i++)
	   {
	   if( qc( sndg[i][idxwd] ) && sndg[i][idxp] >= 100 )
	      {
	      y = hppres_to_pix( sndg[i][idxp] );
	      x = hsk.brx + 500;

	      if((sndg[i][idxh]-lasthght) > 400)
		 {
		 hpwind_barb( fp, sndg[i][idxwd], sndg[i][idxws], x, y, 45, 0);
		 lasthght = sndg[i][idxh];
		 }
	      }
	   }
	}

        /*NP*/
        void hpplot_barbs2( FILE *fp )
        /*************************************************************/
        /*  HPPLOT_BARBS                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Plots wind barbs along side of thermo diagram.           */
        /*************************************************************/
        {
        short i, x, y, numlvl2, idxp, idxh, idxws, idxwd;
        float lastpres;

        idxp  = getParmIndex("PRES");
        idxh  = getParmIndex("HGHT");
        idxws = getParmIndex("SPED");
        idxwd = getParmIndex("DRCT");

        if (idxp == -1 || idxh == -1 || idxws == -1 || idxwd == -1) return;

        hpcliprgn( fp , 20000, 0, 0, 20000 );

        numlvl2 = sndg2[0][0];
        if (numlvl2 == 0) return;

        /* ----- Draw vertical line ----- */
        hpmoveto( fp, hsk.brx + 800, hsk.tly);
        hplineto( fp, hsk.brx + 800, hsk.bry);

        lastpres = 1100;
        for( i = 0; i < numlvl2; i++)
           {
           if( qc( sndg2[i][idxwd] ) && sndg2[i][idxp] >= 100 )
              {
              y = hppres_to_pix( sndg2[i][idxp] );
              x = hsk.brx + 800;

              if((sndg2[i][idxh]-i_hght(lastpres, I_PRES)) > 400)
                 {
                 hpwind_barb( fp, sndg2[i][idxwd], sndg2[i][idxws], x, y, 45, 1);
                 lastpres = sndg2[i][idxp];
                 }
              }
           }
        }


	/*NP*/
	void hptrace_parcel(FILE *fp, float pres, float temp, float dwpt)
	/*************************************************************/
	/*  HPTRACE_PARCEL                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots parcel(pres, temp, dwpt) trajectory on SkewT       */
	/*  graphic.                                                 */
	/*                                                           */
	/*  pres             - Pressure of initial parcel(mb)        */
	/*  temp             - Temperature of initial parcel (c)     */
	/*  dwpt             - Dew Point of initial parcel (c)       */
	/*************************************************************/
	{
	float i, p2, t2, t3;
	short x, y;

	x = hptemp_to_pix( virtemp(pres, temp, dwpt), pres );
	y = hppres_to_pix( pres );
	hpmoveto( fp, x, y );

	drylift(pres, temp, dwpt, &p2, &t2);
	x = hptemp_to_pix( virtemp(p2, t2, t2), p2 );
	y = hppres_to_pix( p2 );
	hplineto( fp, x, y );

	for(i = p2 - 50; i >= 100; i = i - 50)
	   {
	   t3 = wetlift(p2, t2, i);
	   x = hptemp_to_pix( virtemp(i, t3, t3), i );
	   y = hppres_to_pix( i );
	   hplineto( fp, x, y );
	   }
	   t3 = wetlift(p2, t2, 100);
	   x = hptemp_to_pix( virtemp(100, t3, t3), 100 );
	   y = hppres_to_pix( 100 );
	   hplineto( fp, x, y );
	}

	/*NP*/
	void hptrace_parcelx(FILE *fp, float pres, float temp, float dwpt)
	/*************************************************************/
	/*  HPTRACE_PARCELX                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots parcel(pres, temp, dwpt) trajectory on SkewT       */
	/*  graphic.                                                 */
	/*                                                           */
	/*  pres             - Pressure of initial parcel(mb)        */
	/*  temp             - Temperature of initial parcel (c)     */
	/*  dwpt             - Dew Point of initial parcel (c)       */
	/*************************************************************/
	{
	float i, p2, t2, t3;
	short x, y;

	x = hptemp_to_pix( temp, pres );
	y = hppres_to_pix( pres );
	hpmoveto( fp, x, y );

	drylift(pres, temp, dwpt, &p2, &t2);
	x = hptemp_to_pix( t2, p2 );
	y = hppres_to_pix( p2 );
	hplineto( fp, x, y );

	for(i = p2 - 50; i >= 100; i = i - 50)
	   {
	   t3 = wetlift(p2, t2, i);
	   x = hptemp_to_pix( t3, i );
	   y = hppres_to_pix( i );
	   hplineto( fp, x, y );
	   }
	   t3 = wetlift(p2, t2, 100);
	   x = hptemp_to_pix( t3, 100 );
	   y = hppres_to_pix( 100 );
	   hplineto( fp, x, y );
	}

	/*NP*/
	void hpset_font( FILE *fp, short typ, short points)
	/*************************************************************/
	/*  HPSET_FONT                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Defines and activates the given font.                    */
	/*                                                           */
	/*  typ         -  1 = Bold HELV                             */
	/*                 2 = Norm HELV                             */
	/*************************************************************/
	{
	if( typ == 1)
	   { fprintf( fp, "AD1,21,2,1,4,%d,5,0,6,0,7,4148;SA;", points );}
	if( typ == 2)
	   { fprintf( fp, "AD1,21,2,1,4,%d,5,0,6,3,7,4148;SA;", points );}
	}


	/*NP*/
	void hptriangle( FILE *fp, short x1, short y1, short x2, short y2,
			 short x3, short y3, short fill)
	/*************************************************************/
	/*  HPTRIANGLE                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws triangle of (x,y) coordinates.                     */
	/*                                                           */
	/*  fill   -  Fill indicator (1=YES)                         */
	/*			     (2=Gray)			     */
	/*************************************************************/
	{
	fputs( "PM0;", fp );
	hpmoveto( fp, x1, y1);
	hplineto( fp, x2, y2);
	hplineto( fp, x3, y3);
	fputs( "PM2;", fp );
	if( fill == 1)
	   {
	   fputs( "FP;", fp ); 
	   return;
	   }

	if( fill == 2)
	   {
	   fputs( "FT10,.3;", fp );
	   fputs( "FP;", fp ); 
	   return;
	   }

	fputs( "EP;", fp );

	}

	/*NP*/
	void hpdraw_hodo( FILE *fp )
	/*************************************************************/
	/*  HPDRAW_HODO                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws a standard Hodograph diagram on a HP Laserjet      */
	/*  printer.  All graphic calls are HP-GL/2 dependent.       */
	/*************************************************************/
	{
	short x1, y1, x2, y2, i;
	float scle, wdir, wspd, mnu, mnv;
	char st[10];

	/* ----- Transparency mode off ----- */
	fputs( "TR0;", fp );

	hpcliprgn(fp, hho.tlx, hho.tly, hho.brx, hho.bry);
	hprectangle( fp, hho.tlx, hho.tly, hho.brx, hho.bry, 1, 0);
	hpselect_pen( fp, 1, .75F, 9 , 100);
	hprectangle( fp, hho.tlx, hho.tly, hho.brx, hho.bry, 0, 0);

	/* ----- Plot crosshairs ----- */
	hpselect_pen( fp, 1, .1F, 9 , 100);
	hphodo_to_pix( 180, 60, &x1, &y1);
	hpmoveto( fp, x1, hho.tly);
	hplineto( fp, x1, hho.bry);

	hphodo_to_pix( 270, 60, &x1, &y1);
	hpmoveto( fp, hho.tlx, y1);
	hplineto( fp, hho.brx, y1);

	/* ----- Plot X-Coord hash marks ----- */
	for(i = hho.scale; i <= hho.hodomag; i = i + hho.scale)
	   {
	   hphodo_to_pix( 180, (float)i, &x1, &y1);
	   hpmoveto( fp, x1-30, y1);
	   hplineto( fp, x1+30, y1);
	   itoa( i, st, 10 );
	   hpouttext( fp, st, x1 + 50, y1 + 30, 6, 0);

	   hphodo_to_pix( 360, (float)i, &x1, &y1);
	   hpmoveto( fp, x1-30, y1);
	   hplineto( fp, x1+30, y1);
	   itoa( i, st, 10 );
	   hpouttext( fp, st, x1 + 50, y1 + 30, 6, 0);
	   }

	/* ----- Plot Y-Coord hash marks ----- */
	for(i = hho.scale; i <= hho.hodomag; i = i + hho.scale)
	   {
	   hphodo_to_pix( 90, (float)i, &x1, &y1);
	   hpmoveto( fp, x1, y1 - 30);
	   hplineto( fp, x1, y1 + 30);
	   itoa( i, st, 10 );
	   hpouttext( fp, st, x1 - 50, y1 + 80, 6, 0);

	   hphodo_to_pix( 270, (float)i, &x1, &y1);
	   hpmoveto( fp, x1, y1 - 30);
	   hplineto( fp, x1, y1 + 30);
	   itoa( i, st, 10 );
	   hpouttext( fp, st, x1 - 50, y1 + 80, 6, 0);
	   }

	hpselect_pen( fp, 1, .1F, 9 , 100);
	hphodo_circs( fp );

        /* ----- If overlay is ON, plot old shear vectors ----- */
        if (overlay_previous == 1)
           {
           hpselect_pen( fp, 1, .75F, 9 , 20);
           hptrace_hodo2( fp );
	   }

	/* ----- Plot Hodograph (Shear Vectors) ----- */
	hpselect_pen( fp, 1, .75F, 9 , 100);
	hptrace_hodo( fp );
	hplabel_hodo( fp );
	}

	/*NP*/
	void hphodo_to_pix( float dir, float mag, short *x, short *y )
	/*************************************************************/
	/*  HPHODO_COORDS                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the printer location (x,y) in pels of the     */
	/*  wind vector (dir,mag).                                   */
	/*************************************************************/
	{
	float midx, midy, uref, vref;
	float scle, ix1, ix2, ix3, ix4;

	uref = 0.0;
	vref = 0.0;

/*	scle = (hho.brx - hho.tlx) / hho.hodomag;
	midx = hho.tlx + ((hho.brx - hho.tlx) / 2) + (hho.xshift * scle);
	midy = hho.tly + ((hho.bry - hho.tly) / 2) - (hho.yshift * scle);

	*x = (short)(midx + (ucomp( dir, mag ) * scle));
	*y = (short)(midy - (vcomp( dir, mag ) * scle));
*/
        mean_wind(i_pres(msl(1500)), i_pres(msl(8000)), &ix1, &ix2, &ix3, &ix4);
        if (ix1 > 40.0) ix1 = 40.0;
        if (ix2 > 40.0) ix2 = 40.0;
        if (ix1 < -150) ix1 = 0.0;
        if (ix2 < -150) ix2 = 0.0;
        uref = ix1;
        vref = ix2;

        scle = (hho.brx - hho.tlx) / hho.hodomag;
        midx = hho.tlx + ((hho.brx - hho.tlx) / 2.0) + (hho.xshift * scle);
        midy = hho.tly + ((hho.bry - hho.tly) / 2.0) - (hho.yshift * scle);

        *x = (short)(midx + ((ucomp(dir, mag) - uref) * scle));
        *y = (short)(midy - ((vcomp(dir, mag) - vref) * scle));


	}

	/*NP*/
	void hptrace_hodo( FILE *fp )
	/*************************************************************/
	/*  HPTRACE_HODO                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental wind shear vectors on Hodograph.     */
	/*************************************************************/
	{
	short i, j, x, y, xold, yold, ok = 0, idxwd, idxws;

        idxws = getParmIndex("SPED");
        idxwd = getParmIndex("DRCT");

        if (idxws == -1 || idxwd == -1) return;

	for( i=0; i < numlvl; i++)
	   {
	   if( qc(sndg[i][idxwd]) && qc(sndg[i][idxws]))
	      {
	      xold = x;
	      yold = y;
	      hphodo_to_pix( sndg[i][idxwd], sndg[i][idxws], &x, &y);
	      if( ok == 0)
		 {
		 hpmoveto( fp, x, y);
		 ok = 1;
		 }
	      else
		 {
		 hplineto( fp, x, y );
		 }
	      }
	   }
	}

        /*NP*/
        void hptrace_hodo2( FILE *fp )
        /*************************************************************/
        /*  HPTRACE_HODO                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Plots environmental wind shear vectors on Hodograph.     */
        /*************************************************************/
        {
        short i, j, x, y, xold, yold, ok = 0, numlvl2, idxwd, idxws;

        idxws = getParmIndex("SPED");
        idxwd = getParmIndex("DRCT");

        if (idxws == -1 || idxwd == -1) return;

        numlvl2 = sndg2[0][0];
        if (numlvl2 == 0) return;

        for( i=0; i < numlvl2; i++)
           {
           if( qc(sndg2[i][idxwd]) && qc(sndg2[i][idxws]))
              {
              xold = x;
              yold = y;
              hphodo_to_pix( sndg2[i][idxwd], sndg2[i][idxws], &x, &y);
              if( ok == 0)
                 {
                 hpmoveto( fp, x, y);
                 ok = 1;
                 }
              else
                 {
                 hplineto( fp, x, y );
                 }
              }
           }
        }

	/*NP*/
	void hplabel_hodo( FILE *fp )
	/*************************************************************/
	/*  HPLABEL_HODO                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  places labels on hodograph at predetermined levels.      */
	/*************************************************************/
	{
	float wdi, wsp;
	short i, x, y;
	short lvl[] = {1000, 2000, 3000, 5000, 10000, 15000};
	char st[10];

	for(i = 0; i <= 5; i++)
	   {
	   wdi = i_wdir( i_pres(msl( lvl[i]) ), I_PRES );
	   wsp = i_wspd( i_pres(msl( lvl[i]) ), I_PRES );
	   hphodo_to_pix( wdi, wsp, &x, &y);
	   sprintf( st, "%d", lvl[i]/1000);
	   hpouttext( fp, st, x - 60, y - 60, 8, 3);
	   }
	
	/* ----- Display Storm Motion at Upper Left Corner ----- */
	if(qc(st_dir) && qc(st_spd))
	   { sprintf( st, "%d / %d kt", (short)st_dir, (short)st_spd); }
	else
	   { sprintf( st, "M / M"); }
	hpouttext( fp, st, hho.tlx + 50, hho.tly+150, 9, 0);

	
	
	}

	/*NP*/
	void hpplot_elevs( FILE *fp )
	/*************************************************************/
	/*  HPPLOT_ELEVS                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots elevations (m, ft) MSL on right side of SkewT.     */
	/*************************************************************/
	{
	short i, x, y, idxp, idxh;
	float lastpres, hgt;
	char st[80];

        idxp  = getParmIndex("PRES");
        idxh  = getParmIndex("HGHT");

        if (idxp == -1 || idxh == -1) return;

	hpcliprgn( fp , 20000, 0, 0, 20000 );

	/* ----- Draw base legend ----- */
	hpmoveto( fp, hsk.brx + 1200, hsk.tly);
	hplineto( fp, hsk.brx + 1200, hsk.bry);

	hpmoveto( fp, hsk.brx + 900, hsk.tly);
	hplineto( fp, hsk.brx + 1500, hsk.tly);

	hpouttext( fp, "MSL", hsk.brx + 1125, hsk.tly - 95, 7, 3);
	hpouttext( fp, "kft     km", hsk.brx + 1000, hsk.tly - 15, 7, 3);

	/* ----- Find SFC level and plot ----- */
	y = hppres_to_pix( sndg[sfc()][idxp] );
	hpmoveto( fp, hsk.brx + 1150, y);
	hplineto( fp, hsk.brx + 1250, y);
	sprintf( st, "%d m", (short)sndg[sfc()][idxh] );
	hpouttext( fp, st, hsk.brx + 1300, y + 30, 7, 0);
	sprintf( st, "%d ft", (short)mtof(sndg[sfc()][idxh]) );
	hpouttext( fp, st, hsk.brx + 800, y + 30, 7, 0);

	/* ----- Plot every 1km ----- */
	for( hgt=1000; hgt < sndg[numlvl-1][idxh]; hgt += 1000)
	   {
	   if(i_pres(hgt) > 100)
	      {
	      y = hppres_to_pix( i_pres( hgt ));
	      hpmoveto( fp, hsk.brx + 1200, y);
	      hplineto( fp, hsk.brx + 1250, y);
	      sprintf( st, "%d", (short)(hgt / 1000) );
	      hpouttext( fp, st, hsk.brx + 1300, y + 30, 7, 0);
	      }
	   }
	/* ----- Plot every 5kft ----- */
	for(hgt = 5000; hgt < mtof(sndg[numlvl-1][idxh]); hgt += 5000)
	   {
	   if(i_pres(ftom(hgt)) > 100)
	      {
	      y = hppres_to_pix( i_pres( ftom(hgt)));
	      hpmoveto( fp, hsk.brx + 1150, y);
	      hplineto( fp, hsk.brx + 1200, y);
	      sprintf( st, "%d", (short)(hgt / 1000) );
	      hpouttext( fp, st, hsk.brx + 1000, y + 30, 7, 0);
	      }
	   }
	}

	/*NP*/
	void hphodo_circs( FILE *fp )
	/*************************************************************/
	/*  HPHODO_CIRCS                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws speed rings on hodograph as scale increments.      */
	/*************************************************************/
	{
	short x1, y1, x2, y2, i;

	hphodo_to_pix( 180, 0, &x1, &y1);

	for(i = hho.scale; i <= hho.hodomag; i = i + hho.scale)
	   {
	   hphodo_to_pix( 180, (float)i, &x2, &y2);
	   hpcircle( fp, x1, y1, abs(y2 - y1));
	   }
	}

	/*NP*/
	void hpcircle( FILE *fp, short x, short y, short radius)
	/*************************************************************/
	/*  HPCIRCLE                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws a circle at (x,y) of radius (radius).              */
	/*************************************************************/
	{
	hpmoveto( fp, x, y );
	fprintf( fp, "CI%d;", radius );
	}

	/*NP*/
	void hpwrite_parcel( FILE *fp )
	/*************************************************************/
	/*  HPWRITE_PARCEL                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes parcel data to printer.                           */
	/*************************************************************/
	{
	char st[80];
	short x1, y1;
	float sfctemp, sfcdwpt, sfcpres, p, sbcp;
	struct _parcel pcl;

	define_parcel(1,0);
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	x1 = 100;
	y1 = hsk.bry + 500;

	hpcliprgn( fp , 20000, 0, 0, 20000 );
	hpcentertext( fp, "THERMODYNAMIC PARAMETERS", x1 + 1750, y1, 12, 3);
	hpselect_pen( fp, 1, .4F, 9 , 100);
	hprectangle( fp , x1 - 50, y1 + 20, x1 + 4000, y1 +1100, 0, 0);
        hpselect_pen( fp, 1, .2F, 9 , 50);
        hprectangle( fp , x1 - 25, y1 + 750, x1 + 3975, y1 + 900, 0, 0);

	/* ----- Selected parcel type ----- */
	/*y1 += 200;
	sprintf( st, "%s", lplvals.desc);
	hpouttext( fp, st, x1, y1, 9, 0);*/
					
        /* Parcel Data header */
        y1 += 100;
        sprintf( st, "						J/kg			J/kg    	  m AGL	  500 mb		m AGL		m AGL");
        hpouttext( fp, st, x1, y1, 5, 0);

	y1 += 120;
	sprintf( st, "		          CAPE       CINH       LCL       LI         LFC           EL");
        hpouttext( fp, st, x1, y1, 9, 2);

	/* Surface Parcel Data */
	sbcp = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	y1 += 200;
	hpouttext( fp, "SB PARCEL", x1, y1, 9, 0);

	strcpy( st, qc2( pcl.bplus, "", 0));
	hpdisp_param( fp, st, x1 + 1300, y1, 9, 0);
	hpouttext( fp, "", x1 + 1300, y1, 9, 0);

	strcpy( st, qc2( pcl.bminus, "", 0));
	hpdisp_param( fp, st, x1 + 1850, y1, 9, 0);
        hpouttext( fp, "", x1 + 1850, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.lclpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 2375, y1, 9, 0);
        hpouttext( fp, "", x1 + 2375, y1, 9, 0);

        strcpy( st, qc2( pcl.li5, "", 0));
        hpdisp_param( fp, st, x1 + 2725, y1, 9, 0);
        hpouttext( fp, "", x1 + 2725, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.lfcpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 3300, y1, 9, 0);
        hpouttext( fp, "", x1 + 3300, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.elpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 3900, y1, 9, 0);
        hpouttext( fp, "", x1 + 3900, y1, 9, 0);

	/* 100 mb mean parcel data */
	define_parcel(4, 100);
        parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	y1 += 150;
	hpouttext( fp, "ML PARCEL", x1, y1, 9, 0);

        strcpy( st, qc2( pcl.bplus, "", 0));
        hpdisp_param( fp, st, x1 + 1300, y1, 9, 0);
        hpouttext( fp, "", x1 + 1300, y1, 9, 0);

        strcpy( st, qc2( pcl.bminus, "", 0));
        hpdisp_param( fp, st, x1 + 1850, y1, 9, 0);
        hpouttext( fp, "", x1 + 1850, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.lclpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 2375, y1, 9, 0);
        hpouttext( fp, "", x1 + 2375, y1, 9, 0);

        strcpy( st, qc2( pcl.li5, "", 0));
        hpdisp_param( fp, st, x1 + 2725, y1, 9, 0);
        hpouttext( fp, "", x1 + 2725, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.lfcpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 3300, y1, 9, 0);
        hpouttext( fp, "", x1 + 3300, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.elpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 3900, y1, 9, 0);
        hpouttext( fp, "", x1 + 3900, y1, 9, 0);

        /* FCST SFC parcel data */
	define_parcel(2, 0);
        parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        y1 += 150;
        hpouttext( fp, "FCST SFC", x1, y1, 9, 0);

        strcpy( st, qc2( pcl.bplus, "", 0));
        hpdisp_param( fp, st, x1 + 1300, y1, 9, 0);
        hpouttext( fp, "", x1 + 1300, y1, 9, 0);

        strcpy( st, qc2( pcl.bminus, "", 0));
        hpdisp_param( fp, st, x1 + 1850, y1, 9, 0);
        hpouttext( fp, "", x1 + 1850, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.lclpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 2375, y1, 9, 0);
        hpouttext( fp, "", x1 + 2375, y1, 9, 0);

        strcpy( st, qc2( pcl.li5, "", 0));
        hpdisp_param( fp, st, x1 + 2725, y1, 9, 0);
        hpouttext( fp, "", x1 + 2725, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.lfcpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 3300, y1, 9, 0);
        hpouttext( fp, "", x1 + 3300, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.elpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 3900, y1, 9, 0);
        hpouttext( fp, "", x1 + 3900, y1, 9, 0);

        /* MU parcel data */
        define_parcel(3, 400);
        parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        y1 += 150;
        hpouttext( fp, "MU PARCEL", x1, y1, 9, 0);

	if (sbcp > pcl.bplus)
		{
		define_parcel(1,0);
		parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
		
		strcpy( st, qc2( pcl.bplus, "", 0));
        	hpdisp_param( fp, st, x1 + 1300, y1, 9, 0);
        	hpouttext( fp, "", x1 + 1300, y1, 9, 0);

        	strcpy( st, qc2( pcl.bminus, "", 0));
        	hpdisp_param( fp, st, x1 + 1850, y1, 9, 0);
        	hpouttext( fp, "", x1 + 1850, y1, 9, 0);

        	strcpy( st, qc2( agl(i_hght(pcl.lclpres, I_PRES)), "", 0));
        	hpdisp_param( fp, st, x1 + 2375, y1, 9, 0);
        	hpouttext( fp, "", x1 + 2375, y1, 9, 0);

        	strcpy( st, qc2( pcl.li5, "", 0));
        	hpdisp_param( fp, st, x1 + 2725, y1, 9, 0);
        	hpouttext( fp, "", x1 + 2725, y1, 9, 0);

        	strcpy( st, qc2( agl(i_hght(pcl.lfcpres, I_PRES)), "", 0));
        	hpdisp_param( fp, st, x1 + 3300, y1, 9, 0);
        	hpouttext( fp, "", x1 + 3300, y1, 9, 0);

        	strcpy( st, qc2( agl(i_hght(pcl.elpres, I_PRES)), "", 0));
        	hpdisp_param( fp, st, x1 + 3900, y1, 9, 0);
        	hpouttext( fp, "", x1 + 3900, y1, 9, 0);
		}

        strcpy( st, qc2( pcl.bplus, "", 0));
        hpdisp_param( fp, st, x1 + 1300, y1, 9, 0);
        hpouttext( fp, "", x1 + 1300, y1, 9, 0);

        strcpy( st, qc2( pcl.bminus, "", 0));
        hpdisp_param( fp, st, x1 + 1850, y1, 9, 0);
        hpouttext( fp, "", x1 + 1850, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.lclpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 2375, y1, 9, 0);
        hpouttext( fp, "", x1 + 2375, y1, 9, 0);

        strcpy( st, qc2( pcl.li5, "", 0));
        hpdisp_param( fp, st, x1 + 2725, y1, 9, 0);
        hpouttext( fp, "", x1 + 2725, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.lfcpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 3300, y1, 9, 0);
        hpouttext( fp, "", x1 + 3300, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.elpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 3900, y1, 9, 0);
        hpouttext( fp, "", x1 + 3900, y1, 9, 0);

        /* Mean EFFECTIVE parcel data */
        define_parcel(6, 100);
        parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        y1 += 150;
        hpouttext( fp, "EFF PARCEL", x1, y1, 9, 0);

        strcpy( st, qc2( pcl.bplus, "", 0));
        hpdisp_param( fp, st, x1 + 1300, y1, 9, 0);
        hpouttext( fp, "", x1 + 1300, y1, 9, 0);

        strcpy( st, qc2( pcl.bminus, "", 0));
        hpdisp_param( fp, st, x1 + 1850, y1, 9, 0);
        hpouttext( fp, "", x1 + 1850, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.lclpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 2375, y1, 9, 0);
        hpouttext( fp, "", x1 + 2375, y1, 9, 0);

        strcpy( st, qc2( pcl.li5, "", 0));
        hpdisp_param( fp, st, x1 + 2725, y1, 9, 0);
        hpouttext( fp, "", x1 + 2725, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.lfcpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 3300, y1, 9, 0);
        hpouttext( fp, "", x1 + 3300, y1, 9, 0);

        strcpy( st, qc2( agl(i_hght(pcl.elpres, I_PRES)), "", 0));
        hpdisp_param( fp, st, x1 + 3900, y1, 9, 0);
        hpouttext( fp, "", x1 + 3900, y1, 9, 0);

	}

	/*NP*/
	void hpwrite_thermo( FILE *fp )
	/*************************************************************/
	/*  HPWRITE_THERMO                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes thermo data to printer.                           */
	/*************************************************************/
	{
	char st[80];
	short x1, y1, idxp, tIndex, zIndex;
	float sfctemp, sfcdwpt, sfcpres, ix1, ix2, ix3;
	float mlcape, mucape, mucinh, fzlh, shr6, pbot, ptop, el, depth, lr75, mumixr;
	struct _parcel pcl;


        idxp  = getParmIndex("PRES");
        tIndex  = getParmIndex("TEMP");
        zIndex  = getParmIndex("HGHT");

        if (idxp == -1) return;

	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	x1 = 100;
        y1 = hsk.bry + 1600;

        hpcliprgn( fp , 20000, 0, 0, 20000 );
        hpselect_pen( fp, 1, .4F, 9 , 100);
        hprectangle( fp , x1 - 50, y1, x1 + 4000, y1 + 800, 0, 0);
        hpmoveto( fp, x1 + 900, y1);
        hplineto( fp, x1 + 900, y1 + 800);
        hpmoveto( fp, x1 + 2150, y1);
        hplineto( fp, x1 + 2150, y1 + 800);
        hpmoveto( fp, x1 + 3150, y1);
        hplineto( fp, x1 + 3150, y1 + 800);
        hprectangle( fp , x1 - 50, y1 + 800, x1 + 4000, y1 + 1600, 0, 0);
        hpmoveto( fp, x1 + 2600, y1 + 1600);
        hplineto( fp, x1 + 2600, y1 + 800);


	/* ----- row 1 (precip water, 0-3km cape, wet bulb zero, and WNDG ----- */
	y1 += 175;
	hpouttext( fp, "PW = ", x1, y1, 8, 0);
	strcpy( st, qc2( precip_water( &ix1, -1, -1), " in", 2 ));
	hpdisp_param( fp, st, x1 +800, y1, 8, 0);
	define_parcel(4, 100);
	parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	ix1 = pcl.cape3km;
	hpouttext( fp, "3CAPE = ", x1 + 1000, y1, 8, 0);
        strcpy( st, qc2(ix1, " J/kg", 0));
        hpdisp_param( fp, st, x1 + 2100, y1, 8, 0);
        hpouttext( fp, "WBZ = ", x1 + 2200, y1, 8, 0);
        strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 ), I_PRES))), " ft", 0 ));
        hpdisp_param( fp, st, x1 + 3100, y1, 8, 0);
        hpouttext( fp, "WNDG = ", x1 + 3200, y1, 8, 0);
	strcpy( st, qc2(damaging_wind(), "", 1));
        hpdisp_param( fp, st, x1 + 3975, y1, 8, 0);


	/* ----- row 2 (K index, dcape, freezing level, and ESP ----- */
	y1 += 175;
        hpouttext( fp, "K Index = ", x1, y1, 8, 0);
        strcpy( st, qc2( k_index( &ix1 ), "", 0));
        hpdisp_param( fp, st, x1 + 800, y1, 8, 0);
        hpouttext( fp, "DCAPE = ", x1 + 1000, y1, 8, 0);
        strcpy( st, qc2(dcape(&ix1, &ix2), " J/kg", 0));
        hpdisp_param( fp, st, x1 + 2100, y1, 8, 0);
        hpouttext( fp, "FZL = ", x1 + 2200, y1, 8, 0);
        strcpy( st, qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES))), " ft", 0 ));
        hpdisp_param( fp, st, x1 + 3100, y1, 8, 0);
        hpouttext( fp, "ESP = ", x1 + 3200, y1, 8, 0);
        strcpy( st, qc2(esp(), "", 1));
        hpdisp_param( fp, st, x1 + 3975, y1, 8, 0);


        /* ----- row 3 (mid RH, downdraft T, conv T, and MMP ----- */
        y1 += 175;
        hpouttext( fp, "mid RH = ", x1, y1, 8, 0);
        strcpy( st, qc2( mean_relhum(&ix1, sndg[sfc()][idxp]-150, sndg[sfc()][idxp]-350), "%", 0 ));
        hpdisp_param( fp, st, x1 + 800, y1, 8, 0);
        hpouttext( fp, "DownT = ", x1 + 1000, y1, 8, 0);
        strcpy( st, qc2( ctof(ix3), " F", 0 ));
        hpdisp_param( fp, st, x1 + 2100, y1, 8, 0);
        hpouttext( fp, "ConvT = ", x1 + 2200, y1, 8, 0);
        strcpy( st, qc2( ctof(cnvtv_temp(&ix1, -1)), "F", 0 ));
        hpdisp_param( fp, st, x1 + 3100, y1, 8, 0);
        hpouttext( fp, "MMP = ", x1 + 3200, y1, 8, 0);
        strcpy( st, qc2(coniglio1(), "", 2));
        hpdisp_param( fp, st, x1 + 3975, y1, 8, 0);


        /* ----- row 4 (low RH, mean mixing ratio, max temp, and normalized CAPE ----- */
        y1 += 175;
        hpouttext( fp, "low RH = ", x1, y1, 8, 0);
        strcpy( st, qc2( mean_relhum(&ix3, -1, sndg[sfc()][idxp]-150), "%", 0 ));
        hpdisp_param( fp, st, x1 + 800, y1, 8, 0);
        hpouttext( fp, "MeanW = ", x1 + 1000, y1, 8, 0);
        strcpy( st, qc2( mean_mixratio(&ix1, -1, -1 ), " g/kg", 1 ));
        hpdisp_param( fp, st, x1 + 2100, y1, 8, 0);
        hpouttext( fp, "MaxT = ", x1 + 2200, y1, 8, 0);
        strcpy( st, qc2( ctof(max_temp( &ix1, -1)), "F", 0 ));
        hpdisp_param( fp, st, x1 + 3100, y1, 8, 0);
        hpouttext( fp, "NCAPE = ", x1 + 3200, y1, 8, 0);
        define_parcel(3, 400);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        ix2 = agl(i_hght(pcl.elpres, I_PRES));
        ix3 = agl(i_hght(pcl.lfcpres, I_PRES));
        strcpy( st, qc2(ix1/(ix2 - ix3), "", 2));
        hpdisp_param( fp, st, x1 + 3975, y1, 8, 0);


        /* ----- 0-3 km Lapse Rates and SCP ----- */
        y1 += 300;
        hpouttext( fp, "SFC-3km AGL Lapse Rate = ", x1, y1, 8, 0);
        strcpy( st, qc2( sndg[sfc()][tIndex]-i_temp(i_pres(sndg[sfc()][zIndex]+3000),I_PRES), " C", 0 ));
        strcat( st, "  /  " );
        strcat( st, qc2( lapse_rate(&ix3, sndg[sfc()][idxp], i_pres(sndg[sfc()][zIndex]+3000)), " C/km", 1 ));
        hpouttext( fp, st, x1 + 1600, y1, 8, 0);
        hpouttext( fp, "SCP = ", x1 + 2750, y1, 9, 2);
        strcpy( st, qc2( scp(st_dir, st_spd), "", 1 ));
        hpdisp_param( fp, st, x1 + 3850, y1, 9, 2);


        /* ----- 3-6 km Lapse Rates ----- */
        y1 += 175;
        hpouttext( fp, "3-6 km AGL Lapse Rate = ", x1, y1, 8, 0);
        strcpy( st, qc2( i_temp(i_pres(sndg[sfc()][zIndex]+3000),I_PRES)-i_temp(i_pres(sndg[sfc()][zIndex]+6000),I_PRES), " C", 0 ));
        strcat( st, "  /  " );
        strcat( st, qc2( lapse_rate(&ix3, i_pres(sndg[sfc()][zIndex]+3000), i_pres(sndg[sfc()][zIndex]+6000)), " C/km", 1 ));
        hpouttext( fp, st, x1 + 1600, y1, 8, 0);
        hpouttext( fp, "STPC (eff) = ", x1 + 2750, y1, 9, 2);
        strcpy( st, qc2( sigtorn_cin(st_dir, st_spd), "", 1 ));
        hpdisp_param( fp, st, x1 + 3850, y1, 9, 2);


        /* ----- 700-500mb Lapse Rates ----- */
        y1 += 175;
        hpouttext( fp, "700-500mb Lapse Rate = ", x1, y1, 8, 0);
        strcpy( st, qc2( delta_t( &ix1 ), " C", 0 ));
        strcat( st, "  /  " );
        strcat( st, qc2( lapse_rate( &ix1, 700, 500 ), " C/km", 1 ));
        hpouttext( fp, st, x1 + 1600, y1, 8, 0);
        hpouttext( fp, "STP (fixed) = ", x1 + 2750, y1, 9, 2);
        strcpy( st, qc2( sigtorn_fixed(st_dir, st_spd), "", 1 ));
        hpdisp_param( fp, st, x1 + 3850, y1, 9, 2);


        /* ----- 850-500mb Lapse Rates ----- */
        y1 += 175;
        hpouttext( fp, "850-500mb Lapse Rate = ", x1, y1, 8, 0);
        strcpy( st, qc2( vert_tot( &ix1 ), " C", 0 ));
        strcat( st, "  /  " );
        strcat( st, qc2( lapse_rate( &ix1, 850, 500 ), " C/km", 1 ));
        hpouttext( fp, st, x1 + 1600, y1, 8, 0);
        wind_shear(sndg[sfc()][idxp], i_pres(msl(6000)), &ix1, &ix2, &ix3, &shr6);
	define_parcel(4, 100);
	mlcape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        define_parcel(3, 400);
        mucape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mucinh = pcl.bminus;
        mumixr = mixratio(lplvals.pres, lplvals.dwpt);
	lr75 = lapse_rate( &ix1, 700, 500 );
	fzlh = agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES));
        el = agl(i_hght(pcl.elpres, I_PRES));
	effective_inflow_layer(100, -250, &pbot, &ptop);
        if (agl(i_hght(pbot, I_PRES)) > 0.0){
                depth = el - agl(i_hght(pbot, I_PRES));
                wind_shear(pbot, i_pres(msl(depth*0.5)), &ix1, &ix2, &ix3, &shr6);
                }

        hpouttext( fp, "SHIP = ", x1 + 2750, y1, 9, 2);
        strcpy( st, qc2( sig_hail(mucape, mumixr, lr75, i_temp(500, I_PRES), kt_to_mps(shr6), fzlh, mucinh, 0, 0, 25, mlcape), "", 1));
        hpdisp_param( fp, st, x1 + 3850, y1, 9, 2);

	}

	/*NP*/
	void hpdisp_param( FILE *fp, char *st, short x, short y, short font, short tf)
	/*************************************************************/
	/*  HPDISP_PARAM                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Right justifies value at location x,y.                   */
	/*************************************************************/
	{
	fputs( "LO7;", fp );
	hpouttext( fp, st, x, y, font, tf);
	fputs( "LO;", fp );
	}

	/*NP*/
	void hpfill_cape( FILE *fp, short pct)
	/*************************************************************/
	/*  HPFILL_CAPE                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Fills the positive area with grey area.                  */
	/*************************************************************/
	{
	short x, y, i, j, xs, ys, idxp, idxt, idxtd;
	float t1, p1, t2, p2, sfcpres, sfctemp, sfcdwpt, pres;
	struct _parcel pcl;

        idxp  = getParmIndex("PRES");
        idxt  = getParmIndex("TEMP");
        idxtd = getParmIndex("DWPT");

        if (idxp == -1 || idxt == -1 || idxtd == -1) return;

	sfcpres = lplvals.pres;
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;

	/* ----- Calculate Parcel Data ----- */
	parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	if( pcl.bplus <= 0 ) { return; }
	if( pcl.lfcpres <= 0 ) { return; }

	/* ----- Account for partial soundings ----- */
	if( !qc(pcl.elpres)) { pcl.elpres = sndg[numlvl-1][idxp]; }

	/* ----- Set Polygon Mode ----- */
	fputs( "PM0;", fp );

	printf("Beginning to trace up VTMP line\n" );

	/* ----- Start at LFC ----- */
	p1 = pcl.lfcpres;
	t1 = virtemp(p1, i_temp(p1, I_PRES), i_dwpt(p1, I_PRES));
	x = hptemp_to_pix( t1, p1);
	y = hppres_to_pix( p1 );
	hpmoveto( fp, x, y);
	xs = x;
	ys = y;

	printf("Looping through VTMP\n" );

	/* ----- Trace up temperature line ----- */
	i=0;
	while(sndg[i][idxp] > pcl.lfcpres) { i++; }

	for( j=i; sndg[j][idxp] > pcl.elpres; j++)
	   {
	   if( qc(sndg[j][idxt]))
	      {
	      p1 = sndg[j][idxp];
	      t1 = virtemp(p1, sndg[j][idxt], sndg[j][idxtd]);
	      x = hptemp_to_pix( t1, p1);
	      y = hppres_to_pix( p1 );
	      printf("%d   %d\n", x, y);
	      hplineto( fp, x, y);
	      }
	   }

	/* ----- Now, starting at EL, go back down Thw to LFC ----- */
	drylift(sfcpres, sfctemp, sfcdwpt, &p2, &t2);
	for(pres = pcl.elpres; pres < pcl.lfcpres; pres += 50)
	   {
	   t1 = wetlift(p2, t2, pres);
	   x = hptemp_to_pix( virtemp(pres, t1, t1), pres );
	   y = hppres_to_pix( pres );
	   hplineto( fp, x, y );
	   }

	/* ----- Finish Polygon by returning to LFC ----- */
	hplineto( fp, xs, ys );

	/* ----- End Polygon Mode ----- */
	fputs( "PM2;", fp );

	/* ----- Transparency mode on ----- */
	fputs( "TR1;", fp );

	/* ----- Set Fill Density and Finish Fill ----- */
	fprintf( fp, "FT10,%d;", pct );
	fputs( "FP;", fp );
	}

	/*NP*/
	void hpfill_capex( FILE *fp, short pct)
	/*************************************************************/
	/*  HPFILL_CAPEX                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Fills the positive area with grey area.                  */
	/*************************************************************/
	{
	short x, y, i, j, xs, ys, idxp, idxt, idxtd;
	float t1, p1, t2, p2, sfcpres, sfctemp, sfcdwpt, pres;
	struct _parcel pcl;

        idxp  = getParmIndex("PRES");
        idxt  = getParmIndex("TEMP");
        idxtd = getParmIndex("DWPT");

        if (idxp == -1 || idxt == -1 || idxtd == -1) return;

	sfcpres = lplvals.pres;
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;

	/* ----- Calculate Parcel Data ----- */
	parcelx( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	if( pcl.bplus <= 0 ) { return; }

	/* ----- Set Polygon Mode ----- */
	fputs( "PM0;", fp );

	/* ----- Start at LFC ----- */
	p1 = pcl.lfcpres;
	t1 = i_temp(pcl.lfcpres, I_PRES);
	x = hptemp_to_pix( t1, p1);
	y = hppres_to_pix( p1 );
	hpmoveto( fp, x, y);
	xs = x;
	ys = y;

	/* ----- Trace up temperature line ----- */
	i=0;
	while(sndg[i][idxp] > pcl.lfcpres) { i++; }

	for( j=i; sndg[j][idxp] > pcl.elpres; j++)
	   {
	   if( qc(sndg[j][idxt]))
	      {
	      p1 = sndg[j][idxp];
	      t1 = i_temp( p1 , I_PRES);
	      x = hptemp_to_pix( t1, p1);
	      y = hppres_to_pix( p1 );
	      hplineto( fp, x, y);
	      }
	   }

	/* ----- Now, starting at EL, go back down Thw to LFC ----- */
	drylift(sfcpres, sfctemp, sfcdwpt, &p2, &t2);
	for(pres = pcl.elpres; pres < pcl.lfcpres; pres += 50)
	   {
	   t1 = wetlift(p2, t2, pres);
	   x = hptemp_to_pix( t1, pres );
	   y = hppres_to_pix( pres );
	   hplineto( fp, x, y );
	   }

	/* ----- Finish Polygon by returning to LFC ----- */
	hplineto( fp, xs, ys );

	/* ----- End Polygon Mode ----- */
	fputs( "PM2;", fp );

	/* ----- Transparency mode on ----- */
	fputs( "TR1;", fp );

	/* ----- Set Fill Density and Finish Fill ----- */
	fprintf( fp, "FT10,%d;", pct );
	fputs( "FP;", fp );
	}

	/*NP*/
	void hpcentertext( FILE *fp, char *st, short x, short y, short font, short tf)
	/*************************************************************/
	/*  HPCENTERTEXT                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes given string at location (x,y).                   */
	/*  pts         -  Number of points for given font           */
	/*  bold        -  Typeface indicator (0=NORM, 3=BOLD)       */
	/*************************************************************/
	{
	fputs( "LO4;", fp );
	hpouttext( fp, st, x, y, font, tf);
	fputs( "LO;", fp );
	}

	/*NP*/
	void hplvldata( FILE *fp, float pres, char *nam, short x, short y)
	/*************************************************************/
	/*  HPLVLDATA                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Write level data to printer (used in parcel area).       */
	/*************************************************************/
	{
	char st[80];
	if (qc(pres))
	   {
	   hpdisp_param( fp, nam, x + 400, y, 9, 0);
	   strcpy( st, qc2( pres, "mb", 0 ));
	   hpdisp_param( fp, st, x + 1100, y, 9, 0);
	   strcpy( st, qc2( mtof(agl(i_hght(pres , I_PRES))), "ft", 0 ));
	   hpdisp_param( fp, st, x + 2100, y, 9, 0);
	   }
	else
	   {
	   hpdisp_param( fp, nam, x + 400, y, 9, 0);
	   hpdisp_param( fp, "M", x + 1100, y, 9, 0);
	   hpdisp_param( fp, "M", x + 2100, y, 9, 0);
	   }
	}

	/*NP*/
	void sharp_label( FILE *fp, short x1, short y1)
	/*************************************************************/
	/*  SHARP_LABEL                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes SHARP label on page at specified (x,y)            */
	/*************************************************************/
	{
	char st[80];

	strcpy( st, "Output produced by:");
	hpouttext( fp, st, x1 + 25, y1 + 80, 5, 3);

	strcpy( st, "SHARP (SkewT-Hodograph Analysis and Research Program) v3.9");
	hpouttext( fp, st, x1 + 25, y1 + 160, 5, 3);

	strcpy( st, "J Hart/R Thompson et al., 2007, NWS/NCEP/Storm Prediction Center");
	hpouttext( fp, st, x1 + 25, y1 + 240, 5, 3);

	hpselect_pen( fp, 1, .3F, 9, 100 );
	hprectangle( fp, x1, y1, x1 + 2315, y1 + 280, 0, 0 );
	}


	/*NP*/
	void hpwrite_winds( FILE *fp )
	/*************************************************************/
	/*  HPWRITE_WINDS                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes wind data to printer.                             */
	/*************************************************************/
	{
	char st[80];
	short x1, y1, idxp;
	float sfctemp, sfcdwpt, sfcpres, p, ix1, ix2, ix3, ix4, ix5, ix6, ix7, ix8, jh1, jh2, el, base, depth, ptop, pbot;
	struct _parcel pcl;

        idxp  = getParmIndex("PRES");

        if (idxp == -1) return;

	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	x1 = 4250;
	y1 = hsk.bry + 500;

	hpcliprgn( fp , 20000, 0, 0, 20000 );
	hpcentertext( fp, "KINEMATIC PARAMETERS", x1 + 1600, y1, 12, 3);
        hpselect_pen( fp, 1, .4F, 9, 100 );
	hprectangle( fp , x1 - 50, y1 + 20, x1 + 3700, y1 + 2700, 0, 0);

        /* Parcel Data header */
        y1 += 100;
        hpouttext( fp, "m2/s2", x1 + 1150, y1, 5, 0);
        hpouttext( fp, "kt", x1 + 1850, y1, 5, 0);
        hpouttext( fp, "kt", x1 + 2550, y1, 5, 0);
        hpouttext( fp, "kt", x1 + 3300, y1, 5, 0);


        y1 += 120;
        hpouttext( fp, "SRH", x1 + 1150, y1, 9, 2);
        hpouttext( fp, "SHEAR", x1 + 1600, y1, 9, 2);
        hpouttext( fp, "MNWIND", x1 + 2250, y1, 9, 2);
        hpouttext( fp, "SRWIND", x1 + 3050, y1, 9, 2);

	
	/* ----- 0-1 km Mean Wind ----- */
	y1 += 150;
	hpouttext( fp, "Sfc - 1 km", x1, y1, 9, 0);
	jh1 = 0; 
        jh2 = 1000;
        ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
        strcpy( st, qc2(ix1, "", 0));
        hpdisp_param( fp, st, x1 + 1350, y1, 9, 0);
        wind_shear( sndg[sfc()][idxp], i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, "", 0));
        hpdisp_param( fp, st, x1 + 1950, y1, 9, 0);
        mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        hpdisp_param( fp, st, x1 + 2750, y1, 9, 0);
        sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        hpdisp_param( fp, st, x1 + 3500, y1, 9, 0);

        /* ----- 0-2 km Mean Wind ----- */
        y1 += 150;
        hpouttext( fp, "Sfc - 2 km", x1, y1, 9, 0);
        jh1 = 0;
        jh2 = 2000;
        ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
        strcpy( st, qc2(ix1, "", 0));
        hpdisp_param( fp, st, x1 + 1350, y1, 9, 0);
        wind_shear( sndg[sfc()][idxp], i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, "", 0));
        hpdisp_param( fp, st, x1 + 1950, y1, 9, 0);
        mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        hpdisp_param( fp, st, x1 + 2750, y1, 9, 0);
        sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        hpdisp_param( fp, st, x1 + 3500, y1, 9, 0);

        /* ----- 0-3 km Mean Wind ----- */
        y1 += 150;
        hpouttext( fp, "Sfc - 3 km", x1, y1, 9, 0);
        jh1 = 0;
        jh2 = 3000;
        ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
        strcpy( st, qc2(ix1, "", 0));
        hpdisp_param( fp, st, x1 + 1350, y1, 9, 0);
        wind_shear( sndg[sfc()][idxp], i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, "", 0));
        hpdisp_param( fp, st, x1 + 1950, y1, 9, 0);
        mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        hpdisp_param( fp, st, x1 + 2750, y1, 9, 0);
        sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        hpdisp_param( fp, st, x1 + 3500, y1, 9, 0);


        /* ----- Effective Inflow Layer ----- */
        y1 += 150;
        hpouttext( fp, "Eff Inflow Layer", x1, y1, 9, 0);
        effective_inflow_layer(100,-250, &pbot, &ptop);
        if (pbot > 0)
           {
           jh1 = agl(i_hght(pbot, I_PRES));
           jh2 = agl(i_hght(ptop, I_PRES));
           ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
           strcpy( st, qc2(ix1, "", 0));
           hpdisp_param( fp, st, x1 + 1350, y1, 9, 0);           
           wind_shear(i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           strcpy( st, qc2(ix4, "", 0));
           hpdisp_param( fp, st, x1 + 1950, y1, 9, 0);           
           mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
           hpdisp_param( fp, st, x1 + 2750, y1, 9, 0);
           sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
           hpdisp_param( fp, st, x1 + 3500, y1, 9, 0);
           }

        /* ----- 0-6 km Mean Wind ----- */
        y1 +=300;
        hpouttext( fp, "Sfc - 6 km", x1, y1, 9, 0);
        jh1 = 0;
        jh2 = 6000;
        wind_shear( sndg[sfc()][idxp], i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, "", 0));
        hpdisp_param( fp, st, x1 + 1950, y1, 9, 0);
        mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        hpdisp_param( fp, st, x1 + 2750, y1, 9, 0);
        sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        hpdisp_param( fp, st, x1 + 3500, y1, 9, 0);


        /* ----- 0-8 km Mean Wind ----- */
        y1 +=150;
        hpouttext( fp, "Sfc - 8 km", x1, y1, 9, 0);
        jh1 = 0;
        jh2 = 8000;
        wind_shear( sndg[sfc()][idxp], i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, "", 0));
        hpdisp_param( fp, st, x1 + 1950, y1, 9, 0);
        mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        hpdisp_param( fp, st, x1 + 2750, y1, 9, 0);
        sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        hpdisp_param( fp, st, x1 + 3500, y1, 9, 0);



	/* ----- Cloud layer ----- */
	y1 += 150;
        hpouttext( fp, "LCL - EL (Cloud Layer)", x1, y1, 9, 0);
	define_parcel(3, 400);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        if (pcl.bplus > 0)
           {
           jh1 = agl(i_hght(pcl.lclpres, I_PRES));
           jh2 = agl(i_hght(pcl.elpres, I_PRES));
           wind_shear(i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           strcpy( st, qc2(ix4, "", 0));
           hpdisp_param( fp, st, x1 + 1950, y1, 9, 0);           
           mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
           hpdisp_param( fp, st, x1 + 2750, y1, 9, 0);
           sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                  sprintf( st, "%4.0f/%.0f", ix3, ix4);
           hpdisp_param( fp, st, x1 + 3500, y1, 9, 0);           
           }

	/* ----- Lower Half of Storm Depth ----- */
	y1 += 150;
        hpouttext( fp, "Lower Half Storm Depth", x1, y1, 9, 0);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        el = agl(i_hght(pcl.elpres, I_PRES));
        if (pcl.bplus >= 100.0)
           {
           effective_inflow_layer(100, -250, &pbot, &ptop);
           base = agl(i_hght(pbot, I_PRES));
           depth = (el - base);
           wind_shear(pbot, i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &ix4);
           strcpy( st, qc2(ix4, "", 0));
           hpdisp_param( fp, st, x1 + 1950, y1, 9, 0);           
           mean_wind( i_pres(msl(base)), i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
           hpdisp_param( fp, st, x1 + 2750, y1, 9, 0);           
           sr_wind( i_pres(msl(base)), i_pres(msl(base + (depth * 0.5))), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
           hpdisp_param( fp, st, x1 + 3500, y1, 9, 0);           
	   }
	
	y1 += 150;
        hpmoveto( fp, x1 - 50, y1); 
        hplineto( fp, x1 + 3700, y1);

        /* ----- BRN Shear ----- */
        y1 += 150;
        pcl.bplus=1;
        pcl.lplpres = lplvals.pres;
        ix2 = bulk_rich( pcl, &ix1);
        hpouttext( fp, "BRN Shear = ", x1, y1, 9, 0);
        if(qc(ix3) && qc(ix4))
           {
           sprintf( st, "%d m2/s2", (short)ix1);
           hpdisp_param( fp, st, x1 + 1900, y1, 9, 0);
           }
        else
           {
           strcpy( st, "M");
           hpdisp_param( fp, st, x1 + 1900, y1, 9, 0);
           }

	/* ----- 4-6 km SR Wind ----- */
	y1 += 150;
        hpouttext( fp, "4-6 km SR Wind = ", x1, y1, 9, 0);
        sr_wind( i_pres(msl(4000)), i_pres(msl(6000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
        sprintf( st, "%4.0f/%.0f kt", ix3, ix4 );
	hpdisp_param( fp, st, x1 + 1900, y1, 9, 0);

	/* ----- Corfidi Vectors and Bunkers Storm Motion ----- */
	y1 += 150;
        hpouttext( fp, "Corfidi Downshear = ", x1, y1, 9, 0);
        corfidi_MCS_motion(&ix1, &ix2, &ix3, &ix4, &ix5, &ix6, &ix7, &ix8);
        sprintf( st, "%4.0f/%.0f kt", ix3, ix4 );
        hpdisp_param( fp, st, x1 + 1900, y1, 9, 0);	

	y1 += 150;
        hpouttext( fp, "Corfidi Upshear = ", x1, y1, 9, 0);
        corfidi_MCS_motion(&ix1, &ix2, &ix3, &ix4, &ix5, &ix6, &ix7, &ix8);
        sprintf( st, "%4.0f/%.0f kt", ix7, ix8 );
        hpdisp_param( fp, st, x1 + 1900, y1, 9, 0);

        y1 += 150;
        hpouttext( fp, "Bunkers Right = ", x1, y1, 9, 0);        
        bunkers_storm_motion(&ix1, &ix2, &ix3, &ix4);
        sprintf( st, "%4.0f/%.0f kt", ix3, ix4);
        hpdisp_param( fp, st, x1 + 1900, y1, 9, 0);

        y1 += 150;
        hpouttext( fp, "Bunkers Left = ", x1, y1, 9, 0);               
        bunkers_left_motion(&ix1, &ix2, &ix3, &ix4);
        sprintf( st, "%4.0f/%.0f kt", ix3, ix4 );
        hpdisp_param( fp, st, x1 + 1900, y1, 9, 0);

	}


	/*NP*/
	void hpwrite_storm( FILE *fp )
	/*************************************************************/
	/*  HPWRITE_STORM                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes storm data to printer.                            */
	/*************************************************************/
	{
/*	char st[80], st1[80];
	short x1, y1;
	float sfctemp, sfcdwpt, sfcpres, p, hel3;
	float ix1, ix2, ix3, ix4;
	struct _parcel pcl;

	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;
	parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	x1 = 3500;
	y1 = 8000;

	hpcliprgn( fp , 20000, 0, 0, 20000 );
	hpcentertext( fp, "STORM STRUCTURE PARAMETERS", x1 + 1500, y1, 12, 3);
        hpselect_pen( fp, 1, .4F, 9, 100 );
	hprectangle( fp , x1 - 50, y1 + 20, x1 + 3000, 9200, 0, 0);
*/
        /* ----- Sfc-3km SREH ----- */
/*        y1 += 200;
	ix1 = helicity( 0, 3000, st_dir, st_spd, &ix2, &ix3);
	hel3 = ix1;
        hpouttext( fp, "Sfc - 3km SREH:", x1, y1, 9, 0);
        if(qc(ix1))
           {
           sprintf( st, "%.0f m2/s2", ix1);
           hpdisp_param( fp, st, x1+1700, y1, 9, 0);
           }
        else
           {
           strcpy( st, "M");
           hpdisp_param( fp, st, x1+1700, y1, 9, 0);
	   }
*/
        /* ----- Effective SREH ----- */
/*        y1 += 150;
        ix1 = helicity( -1, -1, st_dir, st_spd, &ix2, &ix3);
        hpouttext( fp, "Effective SREH:", x1, y1, 9, 0);
        if(qc(ix1))
           {
           sprintf( st, "%.0f m2/s2", ix1);
	   sprintf( st1, "from %.0f m.", agl(i_hght(esfc(50), I_PRES)));
           hpdisp_param( fp, st, x1+1700, y1, 9, 0);
	   hpdisp_param( fp, st1, x1+2600, y1, 9, 0);
           }
        else
           {
           strcpy( st, "M");
           hpdisp_param( fp, st, x1+1700, y1, 9, 0);
           }

	y1 += 200;
	hpmoveto( fp, x1, y1);
	hplineto( fp, x1+3000, y1);
*/
	/* ----- 0-2 km Storm Relative Winds ----- */
/*	y1 += 150;
	hpouttext( fp, "0-2 km SRW:", x1, y1, 9, 0);
	sr_wind( -1, i_pres(msl(2000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix4))
	   {
	   sprintf( st, "%.0f kt", ix4);
	   hpdisp_param( fp, st, x1+1300, y1, 9, 0);
	   }
	else
	   {
	   hpdisp_param( fp, "M", x1+1300, y1, 9, 0);
	   }
*/
	/* ----- EHI ----- */
/*	hpouttext( fp, "EHI:", x1+1700, y1, 9, 0);
	ix1 = ehi(pcl.bplus, hel3);
	if(qc(ix1))
	   {
	   sprintf( st, "%.1f", ix1);
	   hpdisp_param( fp, st, x1+2900, y1, 9, 0);
	   }
	else
	   {
	   hpdisp_param( fp, "M", x1+2900, y1, 9, 0);
	   }
*/
        /* ----- 4-6 km Storm Relative Winds ----- */
/*        y1 += 150;
        hpouttext( fp, "4-6 km SRW:", x1, y1, 9, 0);
        sr_wind( i_pres(msl(4000)), i_pres(msl(6000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
        if(qc(ix4))
           {
           sprintf( st, "%.0f kt", ix4);
           hpdisp_param( fp, st, x1+1300, y1, 9, 0);
           }
        else
           {
           hpdisp_param( fp, "M", x1+1300, y1, 9, 0);
           }
*/
        /* ----- BRN ----- */
/*        hpouttext( fp, "BRN:", x1+1700, y1, 9, 0);
        ix1 = bulk_rich(pcl, &ix2);
        if(qc(ix1))
           {
           sprintf( st, "%.0f", ix1);
           hpdisp_param( fp, st, x1+2900, y1, 9, 0);
           }
        else
           {
           hpdisp_param( fp, "M", x1+2900, y1, 9, 0);
           }
*/
        /* ----- 6-10 km Storm Relative Winds ----- */
/*        y1 += 150;
        hpouttext( fp, "6-10 km SRW:", x1, y1, 9, 0);
        sr_wind( i_pres(msl(6000)), i_pres(msl(10000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
        if(qc(ix4))
           {
           sprintf( st, "%.0f kt", ix4);
           hpdisp_param( fp, st, x1+1300, y1, 9, 0);
           }
        else
           {
           hpdisp_param( fp, "M", x1+1300, y1, 9, 0);
           }
*/	}



	/*NP*/
	void hpplot_uvvs( FILE *fp )
	/*************************************************************/
	/*  HPPLOT_UVVS                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots VVs (Mbps) on right side of SkewT.                 */
	/*************************************************************/
	{
	short i, x, y, x1, y1, x2, avail, idxp, idxo;
	float leng, scal;


        idxp  = getParmIndex("PRES");
        idxo  = getParmIndex("OMEG");

        if (idxp == -1 || idxo == -1) return;

	scal = 1000;
	
	hpcliprgn( fp , 20000, 0, 0, 20000 );
	hpselect_pen( fp, 1, .25F, 9, 100 );	

	x1 = hsk.brx + 1900;

	/* ----- Plot every level ----- */
	avail = 0;
	for( i = 0; i < numlvl; i++)
	   {
	   if(qc(sndg[i][idxo]) && (sndg[i][idxo] < 1))
	      {
       	      avail++;
	      y = hppres_to_pix( sndg[i][idxp] );
	      leng = (sndg[i][idxo] * scal);   /* Convert to Mbs/sec */
	      hpselect_pen( fp, 1, .05F, 9, 100 );
	      if(abs(leng) >  5) hpselect_pen( fp, 1, .10F, 9, 100 );
 	      if(abs(leng) > 10) hpselect_pen( fp, 1, .35F, 9, 100 );
 	      if(abs(leng) > 15) hpselect_pen( fp, 1, .50F, 9, 100 );
	      x = x1 - (leng * 15);	    /* Determine screen scale */
	      hpmoveto(fp, x1, y);
	      hplineto(fp, x, y);
	      }
	   }

	if(avail > 5)
	   {	   
  	   /* ----- Draw base legend ----- */

           hpselect_pen( fp, 1, .20F, 9, 100 );  	   
	   hpmoveto( fp, x1, hsk.tly);
	   hplineto( fp, x1, hsk.bry);

           hpselect_pen( fp, 1, .10F, 11, 100 );
	   hpmoveto( fp, x1+150, hsk.tly);
	   hplineto( fp, x1+150, hsk.bry);

           hpselect_pen( fp, 1, .10F, 11, 100 );
	   hpmoveto( fp, x1-150, hsk.tly);
	   hplineto( fp, x1-150, hsk.bry);

           hpselect_pen( fp, 1, .20F, 9, 100 );  	   
	   hpmoveto( fp, x1-300, hsk.tly);
	   hplineto( fp, x1+300, hsk.tly);

	   hpcentertext( fp, "VVEL (Mbps)", x1, hsk.tly-95, 7, 3);
	   hpcentertext( fp, "Down     Up   ", x1, hsk.tly-15, 7, 3);
	   }
	}

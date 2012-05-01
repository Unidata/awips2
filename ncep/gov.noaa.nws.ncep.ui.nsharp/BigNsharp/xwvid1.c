/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  XW Video Graphics Routines (Part #1)                       */
/*  These are the routines that pertain specifically to        */
/*  representing the Skew/t and Hodograph on the screen.       */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  MAKE_SCREEN                                                */
/*  DRAW_SKEWT                                                 */
/*  DRAW_HODO                                                  */
/*  HODO_TO_PIX                                                */
/*  PIX_TO_HODO                                                */
/*  TRACE_HODO                                                 */
/*  DRY_ADIABAT                                                */
/*  ISOTHERM                                                   */
/*  HIGHLIGHT_MINUS20					       */	
/*  ISOBAR                                                     */
/*  TRACE_TEMP                                                 */
/*  TRACE_VTMP                                                 */
/*  TRACE_DWPT                                                 */
/*  TRACE_WETBULB                                              */
/*  PRES_TO_PIX                                                */
/*  PIX_TO_PRES                                                */
/*  TEMP_TO_PIX                                                */
/*  PIX_TO_TEMP                                                */
/*  TRACE_PARCEL                                               */
/*  WIND_BARB                                                  */
/*  PLOT_BARBS                                                 */
/*                                                             */
/***************************************************************/
#define GLOBAL
#define VIDEO
//#include "xwcmn.h"
#include "sharp95.h"

/* 
 * Global vars 
 *
 * These are all of the global XW related vars from globals_xw.h
 */

short drawing_mode;
short display_mode_left;
short display_mode_right;
short inset_mode;
short hodo_mode;
short auto_mode;

int xwdth, xhght, xdpth;
int raob_mod;

#ifndef _WIN32
Pixel pixels[40];  /* for colors */
GC gc;
Widget draw_reg;
Pixmap canvas;
XtAppContext app;
#endif

short pagenum = 1;
short overlay_previous;
short inset_previous;

struct _skewt skv = { 40,  25, 500,  500, 100, 70, 60, 1}; 
/*struct _hodog hov = { 40, 675, 350, 1025, -20, -20, 100, 10}; */
struct _hodog hov = { 40, 675, 350, 1025, 0, 0, 120, 10};
struct _stpinset stv = { 720, 520, 1080, 790, 0};
struct _startup autostart;
#ifndef _WIN32
typedef struct {       /* for drawing moving crosshairs or zoom boxes */
  int start_x, start_y,
      last_x,  last_y;
  GC  gc;
  XPoint   points[4];
  int itype; /* 0 = temp 1 = dwpt */
  int ilev; /* 0 = bottom 1 = top 2 = inbetween */
  short yy, i;
} rubber_band_data;

rubber_band_data rbdata;
#endif

/* mkay added functions */
void mixratline(float val);
int  GetWindSpeedColor(float speed);
void display_levels(short colr);
void display_effective_layer(short colr);

       /*NP*/
#ifndef _WIN32
void make_screen(int argc, char *argv[])
       /*************************************************************/
       /*  MAKE_SCREEN                                              */
       /*  John Hart  NSSFC KCMO                                    */
       /*                                                           */
       /*  Draws basic SHARP graphic template on screen, including  */
       /*  areas, buttons, and tables.                              */
       /*                                                           */
       /*************************************************************/
{
       char st[80];

       /* in xwvid6.c */
       X_Init(argc, argv);

       /* ----- Parameter Area -----  */
       setlinestyle(1, 2);
       setcolor(24);
       rectangle(1, skv.tlx, skv.bry + 25, hov.brx, xhght - 10);
       setcolor(1);
       rectangle(0, skv.tlx, skv.bry + 25, hov.brx, xhght - 10);

       /* ----- Begin Main Processing Loop ----- */
       set_font(2);
       XtAppMainLoop(app);
}

void draw_skewt(void)
	/*************************************************************/
	/*  DRAW_SKEWT                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws a standard Skew-T/LogP diagram.                    */
	/*************************************************************/
{
	FILE *fp1;
	short i, pIndex, dIndex, tIndex;
/*	short x, y1, y2; */
	float a, thta, ix1, ix2, ix3, ix4;
	float lvl[] = {1050,1000,850,700,500,300,200,100};
	float sfcpres, sfctemp, sfcdwpt;
	char rtitle[80], st[80];
	HistElmt *he;
	Sounding *cursndg=NULL, *prevsndg=NULL;
	Parcel pcl;

	pIndex = getParmIndex("PRES");
	tIndex = getParmIndex("TEMP");
	dIndex = getParmIndex("DWPT");

	printf("Current Parcel:  %d\n", lplvals.flag);

	/* commented out 10/25/06 by RLT */
	/*if (numlvl>5)
		{*/ 
		/* Here are a few parameters that are listed in the text screen
           	for various people in the SPC */
	        /*uncapped_cape(&ix1, &ix2, -50);
	        sfctemp = lplvals.temp;
	        sfcdwpt = lplvals.dwpt;
       		sfcpres = lplvals.pres;
        	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
		
		fp1 = fopen("/tmp/GLDJPR.txt","wt");
		fprintf( fp1, "-------------------------------------\n");
		fprintf( fp1, "%s (%s)\n", raobtitle, raob_type);
		fprintf( fp1, "-------------------------------------\n");
		printf( "-------------------------------------\n");
	                ix2 = mean_relhum(&ix1, sndg[sfc()][pIndex], 850);
                fprintf( fp1, "1.  Sfc-850mb RH:                %3.0f %\n", ix2);
                printf( "1.  Sfc-850mb RH:                %3.0f %\n", ix2);
                        ix2 = mean_relhum(&ix1, 850, 700);
                fprintf( fp1, "2.  850-700mb RH:                %3.0f %\n", ix2);
                printf( "2.  850-700mb RH:                %3.0f %\n", ix2);
                        ix2 = mean_relhum(&ix1, 700, 500);
                fprintf( fp1, "3.  700-500mb RH:                %3.0f %\n", ix2);
                printf( "3.  700-500mb RH:                %3.0f %\n", ix2);
                        strcpy( st, qc2( lapse_rate( &ix1, sndg[sfc()][pIndex], 850 ), " C/km", 1 ));
                fprintf( fp1, "4.  Sfc-850mb Lapse Rate:        %s\n", st);
                printf( "4.  Sfc-850mb Lapse Rate:        %s\n", st);
                        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "5.  Sfc-6km AGL Shear Vector:    %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "5.  Sfc-6km AGL Shear Vector:    %3.0f/ %.0f kt\n", ix3, ix4);
                        wind_shear(i_pres(msl(2000)), i_pres(msl(8000)), &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "6.  2km-8km AGL Shear Vector:    %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "6.  2km-8km AGL Shear Vector:    %3.0f/ %.0f kt\n", ix3, ix4);
                        wind_shear(sndg[sfc()][pIndex], i_pres(msl(2000)), &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "7.  Sfc-2km AGL Shear Vector:    %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "7.  Sfc-2km AGL Shear Vector:    %3.0f/ %.0f kt\n", ix3, ix4);
                        wind_shear(i_pres(msl(1500)), i_pres(msl(8000)), &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "8.  1.5km-8km AGL Shear Vector:  %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "8.  1.5km-8km AGL Shear Vector:  %3.0f/ %.0f kt\n", ix3, ix4);
                        ix4 = helicity(0, msl(2000), st_dir, st_spd, &ix1, &ix2);
                fprintf( fp1, "9.  Sfc-2km AGL SR Helicity:     %3.0f m2/s2\n", ix4);
                printf( "9.  Sfc-2km AGL SR Helicity:     %3.0f m2/s2\n", ix4);
                        mean_wind( sndg[sfc()][pIndex], i_pres(msl(2000)), &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "10. Sfc-2km AGL Mean Wind:       %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "10. Sfc-2km AGL Mean Wind:       %3.0f/ %.0f kt\n", ix3, ix4);
                        mean_wind( i_pres(msl(2000)), i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "11. 2km-6km AGL Mean Wind:       %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "11. 2km-6km AGL Mean Wind:       %3.0f/ %.0f kt\n", ix3, ix4);
                        mean_wind( i_pres(msl(9000)), i_pres(msl(11000)), &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "12. 9km-11km AGL Mean Wind:      %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "12. 9km-11km AGL Mean Wind:      %3.0f/ %.0f kt\n", ix3, ix4);
                        wind_shear(i_pres(msl(2000)), i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "13. 2km-6km AGL Shear Vector:    %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "13. 2km-6km AGL Shear Vector:    %3.0f/ %.0f kt\n", ix3, ix4);
                        mean_wind( i_pres(msl(2000)), i_pres(msl(8000)), &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "14. 2km-8km AGL Mean Wind:       %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "14. 2km-8km AGL Mean Wind:       %3.0f/ %.0f kt\n", ix3, ix4);
                        mean_wind( pcl.lfcpres, pcl.elpres, &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "15. LFC-EL Mean Wind:            %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "15. LFC-EL Mean Wind:            %3.0f/ %.0f kt\n", ix3, ix4);
                        mean_wind( pcl.lclpres, pcl.elpres, &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "16. LCL-EL Mean Wind:            %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "16. LCL-EL Mean Wind:            %3.0f/ %.0f kt\n", ix3, ix4);
                        wind_shear( pcl.lfcpres, pcl.elpres, &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "17. LFC-EL Shear Vector          %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "17. LFC-EL Shear Vector          %3.0f/ %.0f kt\n", ix3, ix4);
                        wind_shear( pcl.lclpres, pcl.elpres, &ix1, &ix2, &ix3, &ix4);
                fprintf( fp1, "18. LCL-EL Shear Vector          %3.0f/ %.0f kt\n", ix3, ix4);
                printf( "18. LCL-EL Shear Vector          %3.0f/ %.0f kt\n", ix3, ix4);
		fprintf( fp1, "-------------------------------------\n");
		printf( "-------------------------------------\n");
		fclose(fp1);*/
	       /* sfctemp = lplvals.temp;
	        sfcdwpt = lplvals.dwpt;
       		sfcpres = lplvals.pres; */

        	/* ----- Calculate Parcel Data ----- */
        	/*ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
		hailgrowth(&ix4); */	
/*		printf( "1.  PM Hail Aloft:        %.1f cm\n", ix4);
		printf( "2.  SBCAPE (J/kg):        %.0f J/kg\n", pcl.bplus);
		printf( "3.  SBCINH (J/kg):        %.0f J/kg\n", pcl.bminus);
			wind_shear(850, 500, &ix1, &ix2, &ix3, &ix4);
		printf( "4.  850-500mb Shear:      %.1f m/s\n", kt_to_mps(ix4));
			wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
		printf( "5.  Sfc-6km Shear:        %.1f m/s\n", kt_to_mps(ix4));
			wind_shear(-1, i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
		printf( "6.  Low-6km Shear:        %.1f m/s\n", kt_to_mps(ix4));
			mean_wind(sndg[sfc()][pIndex], i_pres(msl(1000)), &ix1, &ix2, &ix3, &ix4);
		printf( "7.  Sfc-1km Mean Wind:    %.1f m/s\n", kt_to_mps(ix4));
			sr_wind(sndg[sfc()][pIndex], i_pres(msl(1000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
		printf( "8.  Sfc-1km SR Wind:      %.1f m/s\n", kt_to_mps(ix4));
			sr_wind(sndg[sfc()][pIndex], i_pres(msl(2000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
		printf( "9.  Sfc-2km SR Wind:      %.1f m/s\n", kt_to_mps(ix4));
			ix4 = helicity(0, msl(3000), st_dir, st_spd, &ix1, &ix2);
		printf( "10. Sfc-3km SR Helicity:  %.0f m2/s2\n", ix4);
			strcpy( st, qc2( ehi(pcl.bplus, ix4), "" , 1));
		printf( "11. EHI:                  %s\n", st);
			strcpy( st, qc2( scp(st_dir, st_spd), "", 1 ));
		printf( "12. SCP:                  %s\n", st);
			ix4 = sndg[sfc()][dIndex] - i_temp(500, I_PRES);
		printf( "13. sfc Td - 500 T:       %.1f c\n", ix4);
			strcpy( st, qc2( agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES)), "m", 0 ));
		printf( "14. Freezing Level:       %s\n", st);
			strcpy( st, qc2( agl(i_hght(wb_lvl( 0, &ix1 ), I_PRES)), "m", 0 ));
		printf( "15. Wet Bulb Zero:        %s\n", st);
			strcpy( st, qc2( lapse_rate( &ix1, 700, 500 ), " C/km", 1 ));
		printf( "16. 700-500mb Lapse Rate: %s\n", st);
		printf( "17. 300mb Temp :          %.1f  C\n", i_temp(300, I_PRES));
			strcpy( st, qc2( lapse_rate( &ix1, 500, 300 ), " C/km", 1 ));
		printf( "18. 500-300mb Lapse Rate: %s\n", st);
		printf( "-------------------------------------\n\n\n\n");
*/
	/*	}*/		

	 setcliprgn(1, 1, xwdth, xhght);
	 setcolor(0);
	 rectangle(1, 1, 1, skv.brx, skv.bry+15);
	 setcolor(1);
	 set_font(3);
	 setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
	 setlinestyle(1, 1);
	 rectangle(0, skv.tlx, skv.tly, skv.brx, skv.bry);

	/* ----- Draw Skewed Temperature Lines ----- */
	 setlinestyle(2, 1);
	 for( i=-160; i<=50; i=i+10 ) { isotherm(i, 24); }
	 isotherm(0, 26);

	/* ----- Draw Dry Adiabats ----- */
	setcolor(24);
	setlinestyle(1, 1);
	for (thta=-70; thta<=350; thta=thta+20)
	  dry_adiabat(thta);

	/* ----- Draw Mixing Ratio Lines ----- */
	setcolor(23);
        setlinestyle(3, 1);
	/* First draw from 0.2 g/kg to 1.0 g/kg */
	/*for (a=0.2;a<=1.0;a+=0.2)
	  mixratline(a); */
	/* Next draw from 2 g/kg to 30.0(!) g/kg */
	for (a=1.0;a<=30.0;a+=4.0)
	  mixratline(a);

	/* ----- Draw Horizontal Pressure Lines ----- */
	 setcolor(1);
	 setlinestyle(1, 1);
	for(i=1; i<8; i++)
	  isobar(lvl[i], 0);
	for(i=100; i<=1050; i=i+50)
	  isobar((float)i, 1);

#define HISTORY
#ifdef HISTORY
	/* ----- Plot old sounding if exists ----- */
	if (overlay_previous == 1) {
	  cursndg = getGlobalSounding();
	  he = history_first(&hist);
	  if (he)
	    he = history_next(he);
	  if (he) {
	    prevsndg = (Sounding *)history_data(he);
	    if (prevsndg) {
	      changeGlobalSounding(prevsndg);
	      if (sndg && (numlvl > 0)) {
/*	        setcolor(28); */
	        trace_temp(3, 28);
/*		setcolor(28); */
	        trace_dwpt(3, 28);
		setcolor(28);
	        plot_barbs2();
		display_effective_layer(28);
	      }
	    }
	  }
	  /* Reset sounding */
	  changeGlobalSounding(cursndg);
	}
#endif

	/* ----- Plot Environmental Temperature Data ----- */
	setcolor(2);
	if (numlvl > 2)
	  trace_temp(3, 2);

	if (numlvl > 2)
	  highlight_temp(-20, 26);

	/* ----- Plot Environmental Dew Point Data ----- */
/*	setcolor(3); */
	if (numlvl > 2)
	  trace_dwpt(3, 3);

	/* ----- Plot Environmental Virtual Temperature Data ----- */
	setcolor(2);
	if (numlvl > 2)
	  trace_vtmp(2);

	/* ----- Plot Environmental Wetbulb Temperature Data ----- */
	setcolor(6);
	setlinestyle(1, 1);
	if (numlvl > 2)
	  trace_wetbulb(1);
	
	/* ----- Plot Wind Barbs ----- */
/*	setcolor(0);
        rectangle(1, skv.brx-52, skv.tly+1, skv.brx-1, skv.bry);
*/      setcolor(31);
	setlinestyle(1, 1);
	if (numlvl > 2)
	  plot_barbs();
	
	/* ----- If Available, plot VVEL profile ----- */
	if (numlvl > 2)
	  vvel_profile();

	/* ----- Display Skew-T Inset ----- */
	draw_skinset();

	setcliprgn(1, 1, xwdth, xhght);
	setcolor(1);
	set_font(1);
	if (*raobtitle)
 	  sprintf(rtitle, "%s (%s)", raobtitle, raob_type);
	else
	  strcpy(rtitle, " ");
	outgtext(rtitle, skv.tlx, 1);

	/* Add title for previous sounding */
	if (overlay_previous == 1) {
	   setcolor(28);
	   if (*raobtitle2)
 	     sprintf(rtitle, "%s (%s)", raobtitle2, raob_type2);
	   else
	     strcpy(rtitle, " ");
	   outgtext(rtitle, skv.brx - getgtextextent(rtitle), 1);
	}

	copytodisplay();
}

	/*NP*/
void draw_hodo(void)
	/*************************************************************/
	/*  DRAW_HODO                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
        /*  Draws a standard Hodograph display.                      */ 
        /*************************************************************/
{
	short x1, y1, x2, y2, i, y3;
	short pIndex;
	float wdir, wspd, mnu, mnv, ix1, ix2, ix3, ix4, ix5, ix6, ix7, ix8;
	float uref, vref, dd, ss;
	char  st[20], rtitle[80];

        setcliprgn(1, 1, xwdth, xhght);

	setcolor(0);
	rectangle( 1, hov.tlx, hov.tly, hov.brx, hov.bry);

        setcolor(1);
        setlinestyle(1, 1);
        rectangle(0, hov.tlx, hov.tly, hov.brx, hov.bry);

	setcliprgn(hov.tlx+2, hov.tly+1, hov.brx-3, hov.bry-3);
        /*setcliprgn(hov.tlx, hov.tly, hov.brx, hov.bry);
	setcolor(1);
	setlinestyle(1, 1);
	rectangle(0, hov.tlx, hov.tly, hov.brx, hov.bry);*/

	/* ----- Plot crosshairs ----- */
	setcolor(31);
	hodo_to_pix(180, 60, &x1, &y1);
	moveto(x1, hov.tly);
	lineto(x1, hov.bry);

	setcolor(31);
	hodo_to_pix(270, 60, &x1, &y1);
	moveto(hov.tlx, y1);
	lineto(hov.brx, y1);

	/* ----- Plot Radius circles ----- */
	setcolor(24);
        setlinestyle(2, 1);
	hodo_to_pix(0, 0, &x1, &y1);
	x2 = x1;
	y2 = y1;
	for(i = hov.scale; i <= hov.hodomag; i = i + hov.scale) {
	   hodo_to_pix(0, (float)i, &x1, &y1);
	   y3 = (y1 - y2);
	   ellipse(0, x2-y3, y2-y3, x2+y3, y2+y3);
	}

	setcolor(1);
	set_font(3);

	/* ----- Plot X-Coord hash marks ----- */
	for(i = hov.scale; i <= hov.hodomag; i = i + hov.scale) {
	   hodo_to_pix(180, (float)i, &x1, &y1);
	   moveto(x1-3, y1);
	   lineto(x1+3, y1);
	   itoa(i, st, 10);
	   outgtext(st, x1 - getgtextextent(st) - 4, y1 - 5);

	   hodo_to_pix(360, (float)i, &x1, &y1);
	   moveto(x1-3, y1);
	   lineto(x1+3, y1);
	   itoa(i, st, 10);
	   outgtext(st, x1 - getgtextextent(st) - 4, y1 - 5);
	   }

	/* ----- Plot Y-Coord hash marks ----- */
	setcolor(1);
	for (i = hov.scale; i <= hov.hodomag; i = i + hov.scale) {
	   hodo_to_pix(90, (float)i, &x1, &y1);
	   moveto(x1, y1-3);
	   lineto(x1, y1+3);
	   itoa(i, st, 10);
	   outgtext(st, x1 - (getgtextextent(st) / 2), y1 + 5);   

	   hodo_to_pix(270, (float)i, &x1, &y1);
	   moveto(x1, y1-3);
	   lineto(x1, y1+3);
	   itoa(i, st, 10);
	   outgtext(st,  x1 - (getgtextextent(st) / 2), y1 + 5);
	}

	/* ----- Plot Hodograph (Shear Vectors) ----- */
	setcolor(2);
	setlinestyle(1,2);
	if (numlvl > 0) {
	  trace_hodo(3);
	}
	else {
		return;
	}

	if ((hodo_mode == HODO_NORMAL) || (hodo_mode == HODO_MEANWIND) || (hodo_mode == HODO_BNDRY)) {
	/* ----- Plot Mean Wind Vector ----- */
	setcolor(5);
	set_font(3);
	mean_wind( -1, -1, &mnu, &mnv, &wdir, &wspd);
	hodo_to_pix( wdir, wspd, &x1, &y1);
	moveto(x1, y1);
	rectangle(0, x1-4, y1-4, x1+4, y1+4);
        sprintf( st, "%.0f/%.0f MW", wdir, wspd);
        outgtext(st, x1-2, y1+7);

	
	pIndex = getParmIndex("PRES");
/*      printf("draw_hodo: pIndex = %d\n", pIndex);

	if (sndg && pIndex != -1) {
*/      mean_wind(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
	  /* ----- Plot 30/75 Storm Motion Vector ----- */	
	  if (hodo_mode == HODO_STORMRELATIVE) {
		uref = ix1 - ucomp(st_dir, st_spd);
		vref = ix2 - vcomp(st_dir, st_spd);
		dd = angle(uref,vref);
		ss = speed(uref,vref);	}
	  else {
		dd = ix3;
		ss = ix4; }
	
/*	 printf("mean wind is from %.1f at %.1f kt\n", dd, ss); 
*/     	  setcolor(11);
	  setlinestyle(1, 1);
          ss *= .75;
          dd += 30;
          if(dd>360) dd -= 360;
	  hodo_to_pix(dd, ss, &x1, &y1);
	  moveto(x1-3, y1); lineto(x1+3, y1);
	  moveto(x1, y1-3); lineto(x1, y1+3);
          ellipse(0, x1-3, y1-3, x1+3, y1+3);

	
          mean_wind(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, 
	    &ix3, &ix4);
      	  setcolor(12);
	  setlinestyle(1, 1);
          ix4 *= .85;
          ix3 += 15;
          if(ix3>360) ix3 -= 360;
	  hodo_to_pix( ix3, ix4, &x1, &y1);
	  moveto(x1-3, y1); lineto(x1+3, y1);
	  moveto(x1, y1-3); lineto(x1, y1+3);
          ellipse(0, x1-3, y1-3, x1+3, y1+3);
/*	}
*/

	
	/* ----- Plot Current Storm Motion Vector ----- */
	setcolor(31);
	setlinestyle(1, 1);
	set_font(5);
	hodo_to_pix(st_dir, st_spd, &x1, &y1);
	moveto(x1-6, y1); lineto(x1+6, y1);
	moveto(x1, y1-6); lineto(x1, y1+6);
	ellipse(0, x1-6, y1-6, x1+6, y1+6);
	sprintf( st, "%.0f/%.0f", st_dir, st_spd);
	outgtext(st, x1-2, y1+7);

        /* ----- Plot Bunkers RIGHT Storm Motion Vector ----- */
	bunkers_storm_motion(&ix1, &ix2, &ix3, &ix4);
/*	printf("\n pre plot dir = %0f\n", ix3);
	printf("\n pre plot spd = %0f\n", ix4);
*/	setcolor(15);
        setlinestyle(1, 1);
        set_font(5);
        hodo_to_pix(ix3, ix4, &x1, &y1);
        moveto(x1-5, y1); lineto(x1+5, y1);
        moveto(x1, y1-5); lineto(x1, y1+5);
        ellipse(0, x1-5, y1-5, x1+5, y1+5);
        sprintf( st, "%.0f/%.0f RM", ix3, ix4);
        outgtext(st, x1-2, y1+7);

        /* ----- Plot Bunkers LEFT Storm Motion Vector ----- */
        bunkers_left_motion(&ix1, &ix2, &ix3, &ix4);
        setcolor(26);
        setlinestyle(1, 1);
        set_font(5);
        hodo_to_pix(ix3, ix4, &x1, &y1);
        moveto(x1-5, y1); lineto(x1+5, y1);
        moveto(x1, y1-5); lineto(x1, y1+5);
        ellipse(0, x1-5, y1-5, x1+5, y1+5);
        sprintf( st, "%.0f/%.0f LM", ix3, ix4);
        outgtext(st, x1-2, y1+7);
	}
	

	/* ----- Plot Corfidi Vectors ----- */
/*	corfidi_MCS_motion(&ix1, &ix2, &ix3, &ix4, &ix5, &ix6, &ix7, &ix8);
	setcolor(25);
	set_font(5);
	hodo_to_pix(ix3, ix4, &x1, &y1);
	ellipse(0, x1-3, y1-3, x1+3, y1+3);
	sprintf( st, "DP=%.0f/%.0f", ix3, ix4);
	outgtext(st, x1-2, y1+7);

	hodo_to_pix(ix7, ix8, &x1, &y1);
	ellipse(0, x1-3, y1-3, x1+3, y1+3);
	sprintf( st, "UP=%.0f/%.0f", ix7, ix8);
	outgtext(st, x1-2, y1+7);
*/
        
        /* ----- Draw final outline of hodograph ----- */
        setcolor(1);
	setlinestyle(1, 1);
	rectangle(0, hov.tlx, hov.tly, hov.brx, hov.bry);
	setcliprgn(1, 1, xwdth, xhght);
	setcolor(1);
	set_font(1);
	outgtext(raobtitle, skv.tlx, 1);

	/* Add title for previous sounding */
	if (overlay_previous == 1) {
	   setcolor(28);
	   if (*raobtitle2)
 	     sprintf(rtitle, "%s (%s)", raobtitle2, raob_type2);
	   else
	     strcpy(rtitle, " ");
	   outgtext(rtitle, skv.brx - getgtextextent(rtitle), 1);
	}

	if (hodo_mode == HODO_BNDRY) { trace_bndry(bd_dir, bd_spd); }
        trace_esrh();

	/* Draw mode buttons on hodograph */
	set_font(4);
	x1 = hov.tlx;
	y1 = hov.tly;
	sprintf( st, "NORMAL");
	setcolor(26);
	if (hodo_mode == HODO_NORMAL) setcolor(1);
	outgtext( st, x1+4, y1+4);
	rectangle(0, x1, y1, x1+68, y1+18);

	sprintf( st, "STORMREL");
	setcolor(26);
	if (hodo_mode == HODO_STORMRELATIVE) setcolor(1);
	outgtext( st, x1+4, y1+22);
	rectangle(0, x1, y1+19, x1+68, y1+37);

	/*sprintf( st, "EFFECTIVE");
	setcolor(24);
	if (hodo_mode == HODO_EFFECTIVE) setcolor(1);
	outgtext( st, x1, y1+22);
	rectangle(0, x1-1, y1+21, x1+50, y1+30);*/
	
	/* 4jan07 */
        sprintf( st, "MEAN WIND");
        setcolor(26);
        if (hodo_mode == HODO_MEANWIND) setcolor(1);
        outgtext( st, x1+4, y1+40);
        rectangle(0, x1, y1+37, x1+68, y1+55);
        /*outgtext( st, x1, y1+33);
        rectangle(0, x1-1, y1+32, x1+50, y1+41);*/

	copytodisplay();

	/* do not plot motion vectors below if boundary motion entered by user */
	if ((hodo_mode == HODO_BNDRY) || (hodo_mode == HODO_STORMRELATIVE)) {
		return; }
	else {

          /* ----- Plot Corfidi Vectors ----- */
        corfidi_MCS_motion(&ix1, &ix2, &ix3, &ix4, &ix5, &ix6, &ix7, &ix8);
        setcolor(25);
        set_font(5);
        hodo_to_pix(ix3, ix4, &x1, &y1);
        ellipse(0, x1-3, y1-3, x1+3, y1+3);
        sprintf( st, "DP=%.0f/%.0f", ix3, ix4);
        outgtext(st, x1-32, y1+7);

        hodo_to_pix(ix7, ix8, &x1, &y1);
        ellipse(0, x1-3, y1-3, x1+3, y1+3);
        sprintf( st, "UP=%.0f/%.0f", ix7, ix8);
        outgtext(st, x1-2, y1+7);

	  /* ----- Plot 30R75 Storm Motion Vector ----- */
/*        printf("mean_wind1: pIndex = %d\n", pIndex);
	if (sndg && pIndex != -1) {
*/      mean_wind(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        setcolor(11);
        setlinestyle(1, 1);
        ix4 *= .75;
        ix3 += 30;
        if(dd>360) dd -= 360;
        hodo_to_pix(dd, ss, &x1, &y1);
        moveto(x1-3, y1); lineto(x1+3, y1);
        moveto(x1, y1-3); lineto(x1, y1+3);
        ellipse(0, x1-3, y1-3, x1+3, y1+3);
/*      }
*/
          /* ----- Plot 15R85 Storm Motion Vector ----- */
/*        printf("mean_wind2: pIndex = %d\n", pIndex);
	if (sndg && pIndex != -1) {
*/      mean_wind(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2,&ix3, &ix4);
        setcolor(12);
        setlinestyle(1, 1);
        ix4 *= .85;
        ix3 += 15;
        if(ix3>360) ix3 -= 360;
        hodo_to_pix( ix3, ix4, &x1, &y1);
        moveto(x1-3, y1); lineto(x1+3, y1);
        moveto(x1, y1-3); lineto(x1, y1+3);
        ellipse(0, x1-3, y1-3, x1+3, y1+3);
/*        }
*/	}

}
#endif
	/*NP*/
void hodo_to_pix(float dir, float mag, short *x, short *y)
	/*************************************************************/
	/*  HODO_TO_PIX                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the screen location (x,y) in pixels of the    */
	/*  wind vector (dir,mag).                                   */
	/*************************************************************/
{
	float midx, midy, scle;
	float uref, vref, ix1, ix2, ix3, ix4;

	uref=0;
	vref=0;
	/*if (hodo_mode == HODO_STORMRELATIVE) {
		uref = ucomp(st_dir, st_spd);
		vref = vcomp(st_dir, st_spd);
		}*/
	/* 4jan07 */
	if (hodo_mode == HODO_MEANWIND) {
		mean_wind(i_pres(msl(1500)), i_pres(msl(8000)), &ix1, &ix2, &ix3, &ix4);
		if (ix1 > 40.0) ix1 = 40.0;
		if (ix2 > 40.0) ix2 = 40.0;
		if (-998 < ix1 < -40) ix1 = -40.0;
		if (ix1 <= -999) ix1 = 0.0; 
		if (-998 < ix2 < -40) ix2 = -40.0;
		if (ix2 <= -999) ix2 = 0.0;
		uref = ix1;
		vref = ix2;
		
		}

	scle = (hov.brx - hov.tlx) / hov.hodomag;
	midx = hov.tlx + ((hov.brx - hov.tlx) / 2.0) + (hov.xshift * scle);
	midy = hov.tly + ((hov.bry - hov.tly) / 2.0) - (hov.yshift * scle);

	*x = (short)(midx + ((ucomp(dir, mag) - uref) * scle));
	*y = (short)(midy - ((vcomp(dir, mag) - vref) * scle));
}

	/*NP*/
void pix_to_hodo(short x, short y, float *dir, float *mag)
	/*************************************************************/
	/*  PIX_TO_HODO                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the wind vector (dir, mag) in knots of the    */
	/*  screen location (x,y).                                   */
	/*************************************************************/
{
	float midx, midy, scle, u, v;
	float uref, vref, ix1, ix2, ix3, ix4;

	uref=0;
	vref=0;
	/*if (hodo_mode == HODO_STORMRELATIVE) {
		uref = ucomp(st_dir, st_spd);
		vref = vcomp(st_dir, st_spd);
		}*/
	/* 4jan07 */
        if (hodo_mode == HODO_MEANWIND) {
                mean_wind(i_pres(msl(1500)), i_pres(msl(8000)), &ix1, &ix2, &ix3, &ix4);
                if (ix1 > 40.0) ix1 = 40.0;
                if (ix2 > 40.0) ix2 = 40.0;
                if (-998 < ix1 < -40) ix1 = -40.0;
                if (ix1 <= -999) ix1 = 0.0;
                if (-998 < ix2 < -40) ix2 = -40.0;
                if (ix2 <= -999) ix2 = 0.0;
                /*if (ix1 < -150) ix1 = 0.0;
                if (ix2 < -150) ix2 = 0.0;*/
                uref = ix1;
                vref = ix2;
		}

	scle = (hov.brx - hov.tlx) / hov.hodomag;   /* pixels/knot */
	midx = hov.tlx + ((hov.brx - hov.tlx) / 2.0) + (hov.xshift * scle);
	midy = hov.tly + ((hov.bry - hov.tly) / 2.0) - (hov.yshift * scle);

	u = ((x - midx) / scle) + uref;
	v = ((midy - y) / scle) + vref;

	*dir = angle(u, v) ;
	*mag = speed(u, v);
}

#ifndef _WIN32
	/*NP*/
void trace_hodo(short width)
	/*************************************************************/
	/*  TRACE_HODO                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental wind shear vectors on Hodograph.     */
	/*************************************************************/
{
	short i, x, y, xold, yold, ok = 0;
	short zIndex, dIndex, sIndex, pIndex;
	float lvl, u, v, dd, ss, uref, vref;
	char  st[20];
	Sounding *cursndg=NULL, *prevsndg=NULL;
	HistElmt *he;

	if (!sndg || numlvl < 2)
	  return;

	setlinestyle(1, width);

	zIndex = getParmIndex("HGHT");
	dIndex = getParmIndex("DRCT");
	sIndex = getParmIndex("SPED");

	if (zIndex == -1 || dIndex == -1 || sIndex == -1)
	  return;

	
	setlinestyle(1,width);
       	if (hodo_mode == HODO_BNDRY) {
	setlinestyle(1,1);
        for( i=0; i < numlvl; i++) {
           /* Don't plot data above 12km (~200mb) */
           if (sndg[i][zIndex] > msl(12000)) break;
 
           if (qc(sndg[i][dIndex]) && qc(sndg[i][sIndex])) {
              if (sndg[i][zIndex] <= msl(12000))
                setcolor(14);
/*              else if (sndg[i][zIndex] > msl(3000) &&
                       sndg[i][zIndex] <= msl(6000))
                setcolor(14);
              else if (sndg[i][zIndex] > msl(6000) &&
                       sndg[i][zIndex] <= msl(9000))
                setcolor(14);
              else if (sndg[i][zIndex] > msl(9000) &&
                       sndg[i][zIndex] <= msl(12000))
                setcolor(14);
              else if (sndg[i][zIndex] > msl(12000))
                setcolor(28);
*/
              xold = x;
              yold = y;
              if (hodo_mode == HODO_STORMRELATIVE) {
                        uref = ucomp(sndg[i][dIndex], sndg[i][sIndex]) - ucomp(st_dir, st_spd);
                        vref = vcomp(sndg[i][dIndex], sndg[i][sIndex]) - vcomp(st_dir, st_spd);
                        dd = angle(uref,vref);
                        ss = speed(uref,vref);
                        hodo_to_pix(dd, ss, &x, &y); }

              else {
			hodo_to_pix(sndg[i][dIndex], sndg[i][sIndex], &x, &y); }

              if (ok == 0) {
                 moveto(x, y);
                 ok = 1;
              }
              else {
                moveto(xold, yold);
                 lineto(x, y);
              }

           }
        }

            /* Plot height labels on hodograph */
            for( i=0; i < numlvl; i++)
              {
              if (sndg[i][zIndex] > msl(9000)) break;

              lvl = (float)((int)(sndg[i][zIndex]/1000) * 1000);
              dd = i_wdir(i_pres(msl(lvl)), I_PRES);
              ss = i_wspd(i_pres(msl(lvl)), I_PRES);
              hodo_to_pix(dd, ss, &x, &y);
              setcolor(14);
              set_font(4);
              sprintf( st, "%.0f", lvl/1000);
              outgtext(st, x, y);
              }
        }
	

else {
	setlinestyle(1,width);
	for( i=0; i < numlvl; i++) {
	   /* Don't plot data above 12km (~200mb) */
	   if (sndg[i][zIndex] > msl(12000)) break;

	   if (qc(sndg[i][dIndex]) && qc(sndg[i][sIndex])) {
	      if (sndg[i][zIndex] <= msl(3000))
	        setcolor(2);
	      else if (sndg[i][zIndex] > msl(3000) && 
	               sndg[i][zIndex] <= msl(6000))
	        setcolor(3);
	      else if (sndg[i][zIndex] > msl(6000) && 
	               sndg[i][zIndex] <= msl(9000))
	        setcolor(5);
	      else if (sndg[i][zIndex] > msl(9000) && 
	               sndg[i][zIndex] <= msl(12000))
                setcolor(6);
/*	      else if (sndg[i][zIndex] > msl(12000))
	        setcolor(28);
*/
	      xold = x;
	      yold = y;
	      if (hodo_mode == HODO_STORMRELATIVE) {
			uref = ucomp(sndg[i][dIndex], sndg[i][sIndex]) - ucomp(st_dir, st_spd);
			vref = vcomp(sndg[i][dIndex], sndg[i][sIndex]) - vcomp(st_dir, st_spd);
			dd = angle(uref,vref);
			ss = speed(uref,vref);	
	   	  	hodo_to_pix(dd, ss, &x, &y); }
	
	      else { 
	   	  	hodo_to_pix(sndg[i][dIndex], sndg[i][sIndex], &x, &y); }

	      if (ok == 0) {
		 moveto(x, y);
		 ok = 1;
	      }
	      else {
	        moveto(xold, yold);
		 lineto(x, y);
	      }

	   }
	}
	    /* Plot height labels on hodograph */
	    if (hodo_mode == HODO_STORMRELATIVE) {	
	    for( i=0; i < numlvl; i++) 
	      {
	      if (sndg[i][zIndex] > msl(9000)) break;

              lvl = (float)((int)(sndg[i][zIndex]/1000) * 1000);
	      	
	      dd = angle((i_wndu(i_pres(msl(lvl)), I_PRES) - ucomp(st_dir, st_spd)), i_wndv(i_pres(msl(lvl)), I_PRES) - vcomp(st_dir, st_spd)); 
	      ss = speed((i_wndu(i_pres(msl(lvl)), I_PRES) - ucomp(st_dir, st_spd)), i_wndv(i_pres(msl(lvl)), I_PRES) - vcomp(st_dir, st_spd));	
	      /*printf("\ndd = %0.1f\n", dd);
	      printf("\nss = %0.1f\n", ss);*/	
	      /*dd = i_wdir(i_pres(msl(lvl)), I_PRES);
	      ss = i_wspd(i_pres(msl(lvl)), I_PRES);*/
              hodo_to_pix(dd, ss, &x, &y);
              setcolor(1); 
	      set_font(5);
              sprintf( st, "%.0f", lvl/1000);
              outgtext(st, x, y);
	      }
	      }
	   else {
            /* Plot height labels on hodograph */
            /*for( i=0; i < numlvl; i++)
              {
              if (sndg[i][zIndex] > msl(500)) break;

              lvl = (float)((int)(sndg[i][zIndex]/500) * 500);*/
              dd = i_wdir(i_pres(msl(500)), I_PRES);
              ss = i_wspd(i_pres(msl(500)), I_PRES);
              hodo_to_pix(dd, ss, &x, &y);
              setcolor(7);
              set_font(5);
              ellipse(0, x-2, y-2, x+2, y+2);
              /*sprintf( st, "%.1f", lvl/1000);
              outgtext(st, x, y);*/
             /* }*/
            for( i=0; i < numlvl; i++)
              {
              if (sndg[i][zIndex] > msl(9000)) break;

              lvl = (float)((int)(sndg[i][zIndex]/1000) * 1000);
              dd = i_wdir(i_pres(msl(lvl)), I_PRES);
              ss = i_wspd(i_pres(msl(lvl)), I_PRES);
              hodo_to_pix(dd, ss, &x, &y);
              setcolor(1);
              set_font(5);
              sprintf( st, "%.0f", lvl/1000);
              outgtext(st, x, y);
              }
	   }		
	}

#ifdef HISTORY
	if (overlay_previous != 1)
	  return;

	cursndg = getGlobalSounding();
	he = history_first(&hist);
	if (he)
	  he = history_next(he);
	if (!he) {
	  return;
	}

	prevsndg = (Sounding *)history_data(he);
	if (!prevsndg) {
	  return;
	}

	changeGlobalSounding(prevsndg);
	if (!sndg || numlvl < 2) {
	  changeGlobalSounding(cursndg);
	  return;
	}

	zIndex = getParmIndex("HGHT");
	dIndex = getParmIndex("DRCT");
	sIndex = getParmIndex("SPED");

	if (zIndex == -1 || dIndex == -1 || sIndex == -1) {
	  changeGlobalSounding(cursndg);
	  return;
	}

	ok = 0;

	for (i=0; i < numlvl; i++) {
	   /* Don't plot data above 12km (~200mb) */
	   if (sndg[i][zIndex] > msl(12000))
	     break;

	   if (qc(sndg[i][dIndex]) && qc(sndg[i][sIndex])) {
	      if (sndg[i][zIndex] <= msl(3000))
	        setcolor(14);
	      else if (sndg[i][zIndex] > msl(3000) && 
	               sndg[i][zIndex] <= msl(6000))
	        setcolor(23);
	      else if (sndg[i][zIndex] > msl(6000) && 
	               sndg[i][zIndex] <= msl(9000))
	        setcolor(19);
	      else if (sndg[i][zIndex] > msl(9000) && 
	               sndg[i][zIndex] <= msl(12000))
	        setcolor(25);
/*	      else if (sndg[i][zIndex] > msl(12000))
	        setcolor(30);
*/
	      xold = x;
	      yold = y;
	      hodo_to_pix(sndg[i][dIndex], sndg[i][sIndex], &x, &y);
	      if (ok == 0) {
		moveto(x, y);
		ok = 1;
	      }
	      else {
	        moveto(xold, yold);
	        lineto(x, y);
	      }
	   }
	}

	/* Reset our sounding again */
	changeGlobalSounding(cursndg);
#endif
}

	/*NP*/
	void trace_esrh(void)
        /*************************************************************/
        /*  TRACE_ESRH                                               */
        /*  Rich Thompson SPC OUN                                    */
        /*                                                           */
        /*  Traces upper and lower bounds of effective SRH layer on  */
	/*  hodograph, based on the effective Bunkers motion.	     */
        /*************************************************************/
{
	float pbot, ptop, bot_spd, bot_dir, top_spd, top_dir, ix1, ix2, ix3, ix4;
	float u_bot, v_bot, u_top, v_top, ca, shr_dir, sr_dir;
	short x, y, x2, y2, bx, by, pIndex;
        char  st[20];

	if (hodo_mode == HODO_BNDRY) return;

        pIndex = getParmIndex("PRES");

	/* calculate pressure of effective layer top and bottom */

/* 24 Mar 2008 */
/*        effective_inflow_layer(100,-250, &p_bot, &p_top);

	printf("\ninflow base  = %0.1f", agl(i_hght(p_bot, I_PRES)));
	printf("\ninflow top  = %0.1f\n", agl(i_hght(p_top, I_PRES)));
*/
	if (p_bot < 1) return;	

	if ((hodo_mode == HODO_MEANWIND) || (hodo_mode == HODO_NORMAL)) {
	/* interpolate wind direction and speed at top and bottom of effective layer */
	bot_spd = i_wspd(p_bot, I_PRES);
	bot_dir = i_wdir(p_bot, I_PRES);
        top_spd = i_wspd(p_top, I_PRES);
	top_dir = i_wdir(p_top, I_PRES);

	/* convert effective layer top and bottom to pixel location on hodograph */
	hodo_to_pix(bot_dir, bot_spd, &x, &y);
	hodo_to_pix(top_dir, top_spd, &x2, &y2);
	/* convert effective Bunkers motion to pixel location on hodograph */
/*	bunkers_storm_motion(&ix1, &ix2, &st_dir, &st_spd);
*/	hodo_to_pix(st_dir, st_spd, &bx, &by);

	/*draw lines from storm motion to top and bottom of effective layer */
	setcliprgn(hov.tlx, hov.tly, hov.brx, hov.bry);
	setcolor(26);
	setlinestyle(1,1);	
	moveto(bx, by);
	lineto(x, y);
	moveto(bx, by);
	lineto(x2, y2);

        if (agl(i_hght(p_bot, I_PRES)) > 1) return;
        /* interpolate 500 m wind direction and speed */
        bot_spd = i_wspd(p_bot, I_PRES);
        bot_dir = i_wdir(p_bot, I_PRES);
        top_spd = i_wspd(i_pres(msl(500)), I_PRES);
        top_dir = i_wdir(i_pres(msl(500)), I_PRES);
        hodo_to_pix(bot_dir, bot_spd, &x, &y);
        hodo_to_pix(top_dir, top_spd, &x2, &y2);

        /*draw 0-500 m shear vector */
        setcolor(7);
        setlinestyle(1,1);
        moveto(x2, y2);
        lineto(x, y);
	}


	/* critical angle from Guiliano and Esterheld (2007) */
        sr_wind( p_bot, p_bot, st_dir, st_spd, &ix1, &ix2, &sr_dir, &ix4);
	wind_shear( p_bot, i_pres(msl(500)), &ix1, &ix2, &shr_dir, &ix4);
	if ((sr_dir <= 180) && (shr_dir <= 180)){ ca  = (sr_dir + 180) - shr_dir;}
        if ((sr_dir <= 180) && (shr_dir > 180)){ ca  = (sr_dir + 180)  - shr_dir;}      
        if ((sr_dir > 180) && (shr_dir <= 180)){ ca  = (sr_dir - 180) - shr_dir;}
        if ((sr_dir > 180) && (shr_dir > 180)){ ca  = 180 - ((shr_dir - 180) - (sr_dir - 180));}
	/*printf("\nsfc DIR = %0.1f \ sfc spd = %0.1f \ sfc u (ucomp) = %0.1f \ sfc v (vcomp) = %0.1f", bot_dir, bot_spd,
		ucomp(bot_dir, bot_spd), vcomp(bot_dir, bot_spd));
	printf("\n500m DIR = %0.1f \ 500m spd = %0.1f \ 500m u (ucomp) = %0.1f \ 500m v (vcomp) = %0.1f", top_dir, top_spd, 
		ucomp(top_dir, top_spd), vcomp(top_dir, top_spd));
	printf("\nsfc SR dir = %0.1f", sr_dir);
        printf("\n0-500m shr dir = %0.1f", shr_dir);
	printf("\nCRITICAL ANGLE = %0.1f\n", ca);*/
	setcolor(6);
	set_font(4);
        sprintf( st, "Critical Angle = %.0f", ca);
        outgtext(st, hov.tlx + 10, hov.bry - 20);

	if (hodo_mode == HODO_STORMRELATIVE) {	

	/* interpolate wind dir/spd for top and bottom of effective inflow layer, and adjust for storm motion */
	u_bot = i_wndu(p_bot, I_PRES) - ucomp(st_dir, st_spd);
	v_bot = i_wndv(p_bot, I_PRES) - vcomp(st_dir, st_spd);
        u_top = i_wndu(p_top, I_PRES) - ucomp(st_dir, st_spd);
        v_top = i_wndv(p_top, I_PRES) - vcomp(st_dir, st_spd);

	bot_spd = speed(u_bot, v_bot);
	bot_dir = angle(u_bot, v_bot);
	top_spd = speed(u_top, v_top);
	top_dir = angle(u_top, v_top);

        /* convert effective layer top and bottom to pixel location on hodograph */
        hodo_to_pix(bot_dir, bot_spd, &x, &y);
        hodo_to_pix(top_dir, top_spd, &x2, &y2);

	/* convert storm motion to origin of hodograph */
	hodo_to_pix(0, 0, &bx, &by);

        /*draw lines from storm motion to top and bottom of effective layer */
        setcliprgn(hov.tlx, hov.tly, hov.brx, hov.bry);
        setcolor(26);
        setlinestyle(1,1);
        moveto(bx, by);
        lineto(x, y);
        moveto(bx, by);
        lineto(x2, y2); 
	} 
}

/*NP*/
void trace_bndry(float dir, float spd)
        /*************************************************************/
        /*  TRACE BOUNDARY MOTION AND ORIENTATION                    */
        /*  Rich Thompson  SPC OUN                                   */
        /*                                                           */
        /*  Plots boundary orientation and motion on hodograph.      */
        /*************************************************************/
{
        float d, u_bndry, v_bndry, u_right, v_right, u_left, v_left;
	float ix1, ix2, ix3, ix4, u6, v6, u6rel, v6rel, u9, v9, u9rel, v9rel, rel_dir, rel_spd;
        float dir_right, spd_right, dir_left, spd_left;
        short x, y, xr, yr, xl, yl, x6, y6, x9, y9;
	short pIndex;
	char st[20];

	/* make sure a boundary dir/spd is provided */
	if ((dir < -999.0) || (spd < -999.0)){
	  return; }
	else { 
	hodo_mode = HODO_BNDRY;	}
	
        /* d = length of boundary segments on hodograph (kt) */
        d = 40.0;

        u_bndry = ucomp(dir, spd);
        v_bndry = vcomp(dir, spd);

        /* compute left and right orthogonal vectors from boundary motion */
        /* orthogonal right segment */
        u_right = u_bndry + ((d / pow((u_bndry * u_bndry) + (v_bndry * v_bndry), 0.5)) * v_bndry);
        v_right = v_bndry - ((d / pow((u_bndry * u_bndry) + (v_bndry * v_bndry), 0.5)) * u_bndry);
        dir_right = angle(u_right, v_right);
        spd_right = speed(u_right, v_right);

        /* orthogonal left segment */
        u_left = u_bndry - ((d / pow((u_bndry * u_bndry) + (v_bndry * v_bndry), 0.5)) * v_bndry);
        v_left = v_bndry + ((d / pow((u_bndry * u_bndry) + (v_bndry * v_bndry), 0.5)) * u_bndry);
        dir_left = angle(u_left, v_left);
        spd_left = speed(u_left, v_left);

        /* convert boundary motion and orientation segments to pixel location on hodograph */
        hodo_to_pix(dir, spd, &x, &y);
        hodo_to_pix(dir_right, spd_right, &xr, &yr);
        hodo_to_pix(dir_left, spd_left, &xl, &yl);

        /*draw lines from boundary motion orthogonal left and right (segment length of 25 kt) */
        setcliprgn(hov.tlx, hov.tly, hov.brx, hov.bry);
        setcolor(18);
        setlinestyle(1,2);
        moveto(x, y);
        lineto(xr, yr);
        moveto(x, y);
        lineto(xl, yl);

        pIndex = getParmIndex("PRES");

	/* 0-6 km vector shear relative to boundary */
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
	u6 = ix1;
	v6 = ix2;

	if ((u6 < -999.0) || (v6 < -999.0)){
		return;
	}

        u6rel = u6 + u_bndry;
        v6rel = v6 + v_bndry;
        rel_dir = angle(u6rel, v6rel);
        rel_spd = speed(u6rel, v6rel);

        hodo_to_pix(rel_dir, rel_spd, &x6, &y6);

        setcliprgn(hov.tlx, hov.tly, hov.brx, hov.bry);
        setcolor(26);
        setlinestyle(1,2);
        moveto(x, y);
        lineto(x6, y6);
        set_font(5);
/*        sprintf( st, "%.0f/%.0f 0-6km shr", ((dir+90)-rel_dir), ix4);
*/
	sprintf( st, "%.0f/%.0f 0-6km shr", rel_dir, ix4);
        outgtext(st, x6-30, y6-8);

	/* 9-11 km SR wind relative to boundary */
	sr_wind(i_pres(msl(9000)), i_pres(msl(11000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	u9 = ix1; 
	v9 = ix2;

        if ((u9 < -999.0) || (v9 < -999.0)){
                return;
        }

	u9rel = u9 + u_bndry;
	v9rel = v9 + v_bndry;
	rel_dir = angle(u9rel, v9rel);
        rel_spd = speed(u9rel, v9rel);

	hodo_to_pix(rel_dir, rel_spd, &x9, &y9);

        setcliprgn(hov.tlx, hov.tly, hov.brx, hov.bry);
        setcolor(7);
        setlinestyle(1,2);
        moveto(x, y);
        lineto(x9, y9);
	set_font(5);
        sprintf( st, "%.0f/%.0f 9-11km SR", rel_dir, ix4);
        outgtext(st, x9-30, y9+8);

}

	/*NP */
	void dry_adiabat(float thta)
	/*************************************************************/
	/*  DRY_ADIABAT                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws dry adiabat of theta (thta, c) on SkewT graphic.   */
	/*************************************************************/
{
	float pres, temp;
	short x, y;

	setcliprgn(skv.tlx+2, skv.tly+2, skv.brx-55, skv.bry-2);
	for( pres = 1050.0; pres >= 100.0; pres-=50.0 ) {
	   temp = ((thta + 273.15) / pow( 1000.0 / pres, ROCP )) - 273.15;

	   x = temp_to_pix(temp , pres);
	   y = pres_to_pix(pres);

	   if (pres == 1050.0)
	      moveto(x, y);
	   else
	      lineto(x, y);
	   }
}

	/*NP*/
void isotherm(float temp, short colr)
	/*************************************************************/
	/*  ISOTHERM                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws temperature lines (temp, c) on SkewT graphic.      */
	/*  Color variable is for line, labels in white.             */
	/*************************************************************/
{
	short x, y;
	char st[10];

	x = temp_to_pix(temp, 1045.0);
	y = skv.bry;
	if ((temp >= -30) && (temp <= 50)) {
	    setcliprgn(1, 1, xwdth, xhght);
	    itoa((short)temp, st, 10);
            setcolor(1);
	    outgtext(st, x - (getgtextextent( st ) / 2), y+2);
	}
	setcliprgn(skv.tlx+3, skv.tly+3, skv.brx-55, skv.bry-5);
	setcolor(colr);
	moveto(x, y);
	x = temp_to_pix(temp, 100.0);
	y = skv.tly;
	lineto(x, y);
}

	/*NP*/
void highlight_temp(float temp, short colr)
        /*************************************************************/
        /*  HIGHLIGHT_TEMP                                           */
        /*  Rich Thompson SPC OUN                                    */
        /*                                                           */
        /*  Highlights portion of isotherm through chosen sounding   */
	/*  temperature.  Temp is temperature (C) and colr is the    */
	/*  line color.					             */
        /*************************************************************/
{
	short x, y1, y2;
	float pres;

        y1 = temp_lvl(temp, &pres);
        y2 = pres_to_pix(pres);
        x = temp_to_pix(temp, pres);

	setcolor(colr);
	setlinestyle(1, 2);
        ellipse(0, x-3, y2-3, x+3, y2+3);
	
	setlinestyle(2, 1);
        setcliprgn(x, skv.tly, skv.brx, y2);
        moveto(x, y2);
        x = temp_to_pix(temp, pres-150);
        y2 = pres_to_pix(pres-150);
        lineto(x, y2);
}
	
	/*NP*/
void isobar(float pres, short flag)
	/*************************************************************/
	/*  ISOBAR                                                   */
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

	setcliprgn(1, 1, xwdth, xhght);
	y = pres_to_pix(pres);
	if (flag == 0) {
	   moveto(skv.tlx, y);
	   itoa((short)pres, st, 10);
	   outgtext(st, skv.tlx - getgtextextent( st ) - 2, y - 5);
	   setcliprgn(skv.tlx, skv.tly, skv.brx-55, skv.bry);
	   moveto(skv.tlx, y);
	   lineto(skv.brx, y);
	}
	else {
	   moveto(skv.tlx, y);
	   lineto(skv.tlx + 5, y);
	   moveto(skv.brx, y);
	   lineto(skv.brx - 5, y);
	}
}

void mixratline(float val)
{
	float top=600.0;
	float bot=1045.0;
	float temp;
	short x0, y0, x1, y1;

	/* bottom */
	temp = temp_at_mixrat(val, bot);
	x0 = temp_to_pix(temp, bot);
	y0 = pres_to_pix(bot);

	/* top */
	temp = temp_at_mixrat(val, top);
	x1 = temp_to_pix(temp, top);
	y1 = pres_to_pix(top);

	moveto(x0, y0);
	lineto(x1, y1);
}

	/*NP*/
void trace_temp(short width, short colr)
	/*************************************************************/
	/*  TRACE_TEMP                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental temperature trace on SkewT.          */
	/*************************************************************/
{
	short i, x, y, x2, y2, x3, y3, xold, yold, ok = 0;
	short pIndex, tIndex;
	char st[20];

	float minus20;

	pIndex = getParmIndex("PRES");
	tIndex = getParmIndex("TEMP");

	if (!sndg || pIndex == -1 || tIndex == -1)
	  return;

        setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry+8);
        setlinestyle(1, width);
   	setcolor(colr);
	for( i=0; i < numlvl; i++) {
	   if (sndg[i][tIndex] > -200.0) {
	      xold = x;
	      yold = y;
	      x = temp_to_pix(sndg[i][tIndex], sndg[i][pIndex]);
	      y = pres_to_pix(sndg[i][pIndex]);

	      if (ok == 0) {
		 moveto(x, y);
		 ok = 1;
	      }
	      else {
	        moveto(xold, yold);
	        lineto(x, y);
	      }
	   }
	}

	/* Plot Surface T along bottom */
	set_font(4);
	sprintf( st, "%.0f", ctof(sndg[sfc()][tIndex]));
	x = temp_to_pix(sndg[sfc()][tIndex], sndg[sfc()][pIndex]);
	y = pres_to_pix(sndg[sfc()][pIndex]);
	x2 = x + getgtextextent(st)+2;
	y2 = y + 12;
	setcolor(0);
	rectangle(1, x+1, y+3, x2, y+13);
	setcolor(colr);
	outgtext(st, x+2, y+2);

	/* Highlight -20C isotherm near actual temp trace */
/*	temp_lvl(-20, &minus20);
        printf("\npres lvl of -20c (mb) = %.1f\n", minus20);
	x3 = temp_to_pix(-20, minus20);
	y3 = pres_to_pix(minus20);
	setcolor(5);
	setlinestyle(2, 4);
	moveto(skv.tlx+x3, skv.tly+y3);
	lineto(skv.tlx+x3+10, skv.tly+y3+10);
*/

	if ((display_mode_left == DISPLAY_WINTER_LEFT) && (colr == 2)) display_dendritic_zone(width);
	if (display_mode_left == DISPLAY_CONVECTION_LEFT) display_effective_layer(27);

        /* added 25OCT06 by RLT */
        if (display_mode_left == DISPLAY_SARS_LEFT) display_effective_layer(27);		
        if (display_mode_left == DISPLAY_HAIL_LEFT) display_effective_layer(27);
        if (display_mode_left == DISPLAY_SHIP_LEFT) display_effective_layer(27);
        if (display_mode_left == DISPLAY_STP_LEFT) display_effective_layer(27);
        if (display_mode_left == DISPLAY_EBS_LEFT) display_effective_layer(27);
        if (display_mode_left == DISPLAY_FIRE_LEFT) display_effective_layer(27);

	display_levels(colr);
	
}

/* 

This routine has same problems as trace_hodo

*/

	/*NP*/
void trace_temp2(short width)
	/*************************************************************/
	/*  TRACE_TEMP2                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental temperature trace on SkewT.          */
	/*************************************************************/
{
	short i, x, y, xold, yold, ok = 0;

	if (!sndg)
	  return;

        setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
        setlinestyle(1, width);
	for (i=0; i < numlvl; i++) {
	   if (sndg2[i][3] > -200.0) {
	      xold = x;
	      yold = y;
	      x = temp_to_pix(sndg[i][3], sndg[i][1]);
	      y = pres_to_pix(sndg[i][1]);
	      if (ok == 0) {
		 moveto(x, y);
		 ok = 1;
	      }
	      else {
	        moveto(xold, yold);
	        lineto(x, y);
	      }
	   }
	}
}

	/*NP*/
void trace_vtmp(short width)
	/*************************************************************/
	/*  TRACE_VTMP                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots virtual temperature trace on SkewT.                */
	/*************************************************************/
{
	short i, x, y, xold, yold, ok = 0;
	short pIndex, tIndex, tdIndex;

	pIndex = getParmIndex("PRES");
	tIndex = getParmIndex("TEMP");
	tdIndex = getParmIndex("DWPT");

	if (!sndg || pIndex == -1 || tIndex == -1 || tdIndex == -1)
	  return;

	setlinestyle(10, width);
	for (i = 0; i < numlvl; i++) {
	   if (qc(sndg[i][tIndex]) && qc(sndg[i][tdIndex])) {
	      xold = x;
	      yold = y;
	      /* x = temp_to_pix(i_vtmp(sndg[i][pIndex], I_PRES), sndg[i][pIndex]); */
	      x = temp_to_pix(virtemp(sndg[i][pIndex], sndg[i][tIndex], sndg[i][tdIndex]), sndg[i][pIndex]);
	      y = pres_to_pix(sndg[i][pIndex]);
	      if (ok == 0) {
		 moveto(x, y);
		 ok = 1;
	      }
	      else {
		 moveto(xold, yold);
		 lineto(x, y);
	      }
	   }
	}
}

	/*NP*/
void trace_dwpt(short width, short colr)
	/*************************************************************/
	/*  TRACE_DWPT                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental Dew Point trace on SkewT.            */
	/*************************************************************/
{
	short i, x, y, x2, y2, xold, yold, ok = 0;
	short pIndex, tdIndex;
	char st[20];

	pIndex = getParmIndex("PRES");
	tdIndex = getParmIndex("DWPT");

	if (!sndg || pIndex == -1 || tdIndex == -1)
	  return;

	setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
        setlinestyle(1, width);
	setcolor(colr);
	for (i=0; i < numlvl; i++) {
	   if (sndg[i][tdIndex] > -200.0) {
	      xold = x;
	      yold = y;
	      x = temp_to_pix(sndg[i][tdIndex], sndg[i][pIndex]);
	      y = pres_to_pix(sndg[i][pIndex]);
	      if (ok == 0) {
		 moveto(x, y);
		 ok = 1;
	      }
	      else {
	        moveto(xold, yold);
	        lineto(x, y);
	      }
	   }
	}

/*        set_font(4);
        sprintf( st, "%.0f", ctof(sndg[sfc()][tIndex]));
        x = temp_to_pix(sndg[sfc()][tIndex], sndg[sfc()][pIndex]);
        y = pres_to_pix(sndg[sfc()][pIndex]);
        x2 = x + getgtextextent(st)+4;
        y2 = y + 12;
        setcolor(0);
        rectangle(1, x, y+3, x2, y2+3);
        setcolor(colr);
        outgtext(st, x+2, y+3);
*/

	/* Plot Surface Td along bottom */
	set_font(4);
	sprintf( st, "%.0f", ctof(sndg[sfc()][tdIndex]));
	x = temp_to_pix(sndg[sfc()][tdIndex], sndg[sfc()][pIndex]);
	y = pres_to_pix(sndg[sfc()][pIndex]);
        x2 = x + getgtextextent(st)-12;
        y2 = y + 12;
        setcolor(0);
        rectangle(1, x-13, y+3, x2, y+13);
        setcolor(colr);
        outgtext(st, x-12, y+2);	
}

/* 

This routine has same problems as trace_hodo

*/

	/*NP*/
void trace_dwpt2(short width)
	/*************************************************************/
	/*  TRACE_DWPT2                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental Dew Point trace on SkewT.            */
	/*************************************************************/
{
	short i, x, y, xold, yold, ok = 0;

	if (!sndg)
	  return;

	setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
        setlinestyle(1, width);
	for (i=0; i < numlvl; i++) {
	   if (sndg[i][4] > -200) {
	      xold = x;
	      yold = y;
	      x = temp_to_pix(sndg[i][4], sndg[i][1]);
	      y = pres_to_pix(sndg[i][1]);
	      if (ok == 0) {
		 moveto(x, y);
		 ok = 1;
	      }
	      else {
	        moveto(xold, yold);
	        lineto(x, y);
	      }
	   }
	}
}

	/*NP*/
void trace_wetbulb(short width)
	/*************************************************************/
	/*  TRACE_WETBULB                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental Wetbulb trace on SkewT.              */
	/*************************************************************/
{
	short i, x, y, xold, yold, ok = 0;
	float t1;
	short pIndex, tIndex, tdIndex;

	pIndex = getParmIndex("PRES");
	tIndex = getParmIndex("TEMP");
	tdIndex = getParmIndex("DWPT");

	if (!sndg || pIndex == -1 || tIndex == -1 || tdIndex == -1)
	  return;

	setlinestyle(1, width);
	for (i=0; i < numlvl; i++) {
	   if (sndg[i][tdIndex] > -200.0) {
	      xold = x;
	      yold = y;

	      t1 = wetbulb(sndg[i][pIndex], sndg[i][tIndex], sndg[i][tdIndex]);

	      x = temp_to_pix(t1, sndg[i][pIndex]);
	      y = pres_to_pix(sndg[i][pIndex]);
	      if (ok == 0) {
		 moveto(x, y);
		 ok = 1;
	      }
	      else {
		 moveto(xold, yold);
		 lineto(x, y);
	      }
	   }
	}
}
#endif
	/*NP*/
short pres_to_pix(float pres)
	/*************************************************************/
	/*  PRES_TO_PIX                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given pressure (mb) to an Y coordinate on       */
	/*  Skewt graphic.                                           */
	/*************************************************************/
{
	double scl1, scl2;
	scl1 = log(1050) - log(100);
	scl2 = log(1050) - log(pres);
	return (short)(skv.bry - (scl2 / scl1) * (skv.bry - skv.tly));
}

	/*NP*/
float pix_to_pres(short pix)
	/*************************************************************/
	/*  PIX_TO_PRES                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given Y coordinate on Skewt graphic to          */
	/*  pressure(mb).                                            */
	/*************************************************************/
{
	double scl1, scl2, scl3;
	scl1 = log(1050) - log(100);
	scl2 = skv.bry - (double)pix;
	scl3 = skv.bry - skv.tly + 1;
	return (float)(1050 / exp((scl2 / scl3) * scl1));
}

	/*NP*/
short temp_to_pix(float temp, float pres)
	/*************************************************************/
	/*  TEMP_TO_PIX                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given temperature (c) to an X coordinate on     */
	/*  Thermodynamic diagram.                                   */
	/*                                                           */
	/*  Depending on (skv.type), relationship is for (1) Pseudo-  */
	/*  Adiabatic or (2) Skew-T/Log P diagram.                   */
	/*                                                           */
	/*  Skew:  90c spread across the bottom of chart.            */
	/*        120c spread up-and-down the chart.                 */
	/*         Temp at BR of chart = 50c                         */
	/*************************************************************/
{
	float scl1, scl2;

	if (skv.type == 1) {
	   scl1 = (float)skv.brtemp - ((((float)skv.bry -
	   (float)pres_to_pix( pres )) / ((float)skv.bry - (float)skv.tly)) *
	   (float)skv.vspread);
	}
	else {
	   scl1 = skv.brtemp;
	}
	scl2 = skv.brx - (((scl1 - temp) / skv.hspread) * (skv.brx - skv.tlx));
	return (short)scl2;
	}

	/*NP*/
float pix_to_temp(short x, short y)
	/*************************************************************/
	/*  PIX_TO_TEMP                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given X/Y coordinates to temperature(c) on      */
	/*  Thermodynamic diagram.                                   */
	/*************************************************************/
{
	float scl1, scl2, scl3;

	scl1 = 1 - (((float)x - (float)skv.tlx) / 
	       ((float)skv.brx - (float)skv.tlx));
	scl2 = (float)skv.brtemp - (scl1 * skv.hspread);
	scl1 = 1 - (((float)y - (float)skv.tly) / ((float)skv.bry - 
	       (float)skv.tly));
	scl3 = scl2 - (scl1 * (float)skv.vspread);
	return scl3;
}

	/*NP*/
#ifndef _WIN32
void trace_parcel(float pres, float temp, float dwpt)
	/*************************************************************/
	/*  TRACE_PARCEL                                             */
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

	setcolor(31);
        setlinestyle(4, 2);

	/* ----- Draw Virtual Temperature Profile ----- */
	x = temp_to_pix(virtemp(pres, temp, dwpt), pres);
	y = pres_to_pix(pres);
	moveto(x, y);

	drylift(pres, temp, dwpt, &p2, &t2);
	x = temp_to_pix(virtemp(p2, t2, t2), p2);
	y = pres_to_pix(p2);
	lineto(x, y);

	for (i = p2 - 50; i >= 100; i = i - 50) {
	   t3 = wetlift(p2, t2, i);
	   x = temp_to_pix(virtemp(i, t3, t3), i);
	   y = pres_to_pix(i);
	   lineto(x, y);
	}
	t3 = wetlift(p2, t2, 100);
	x = temp_to_pix(virtemp(100, t3, t3), 100);
	y = pres_to_pix(100);
	lineto(x, y);

	setcolor(18);
	setlinestyle(4, 1);

	/* ----- Draw Basic Parcel Ascent ----- */
        x = temp_to_pix(temp, pres);
        y = pres_to_pix(pres);
        moveto(x, y);

        drylift(pres, temp, dwpt, &p2, &t2);
        x = temp_to_pix(t2, p2);
        y = pres_to_pix(p2);
        lineto( x, y );

        for(i = p2 - 50; i >= 100; i = i - 50) {
           t3 = wetlift(p2, t2, i);
           x = temp_to_pix(t3, i);
           y = pres_to_pix(i);
           lineto(x, y);
           }
        t3 = wetlift(p2, t2, 100);
        x = temp_to_pix(t3, 100);
        y = pres_to_pix(100);
        lineto(x, y);
}

	/*NP*/
void wind_barb(float wdir, float wspd, short x, short y, short siz)
	/*************************************************************/
	/*  WIND_BARB                                                */
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
	if (siz > maxsiz)
	   setlinestyle(1, 2);
	else
           setlinestyle(1, 1);

        rectangle(0, x1-1, y1-1, x1+1, y1+1);
	moveto(x1, y1);
	lineto(x2, y2);

	sped = (short)wspd;
	x1 = x2;
	y1 = y2;

	wid = 5;                        /* Number of flags that will fit */
	spcx = dx / wid;
	spcy = dy / wid;
	x1 = x1 + (short)spcx;
	y1 = y1 - (short)spcy;

	/* ----- Draw wind flags (increments of 50kt) ----- */
	flag = 0;
	while (sped > 47) {
	   flag = 1;
	   x1 = x1 - (short)spcx;
	   y1 = y1 + (short)spcy;

	   hgt = .5F;                   /* Height of flags */
	   x2 = x1 + (short)(dy * hgt);
	   y2 = y1 + (short)(dx * hgt);

	   x3 = x1 - (short)spcx;
	   y3 = y1 + (short)spcy;

	   moveto(x1, y1);
	   lineto(x2, y2);
	   lineto(x3, y3);

	   x2 = (x1 + x2 + x3) / 3;
	   y2 = (y1 + y2 + y3) / 3;

	   sped -= 50;
	   x1 = x3;
	   y1 = y3;
	}

	/* ----- Draw wind barbs (increments of 5kt) ----- */
	while (sped > 7) {
	   hgt = .5F;                   /* Height of flags */
	   x2 = x1 + (short)(dy * hgt);
	   y2 = y1 + (short)(dx * hgt);

	   x3 = x1 - (short)spcx;
	   y3 = y1 + (short)spcy;

	   moveto(x3, y3);
	   lineto(x2, y2);

	   sped -= 10;
	   x1 = x3;
	   y1 = y3;
	}

	/* ----- Draw short barb ----- */
	if (sped > 3) {
	   hgt = .5F;                   /* Height of flags */
	   x2 = x1 + (short)(dy * hgt);
	   y2 = y1 + (short)(dx * hgt);

	   x3 = x1 - (short)spcx;
	   y3 = y1 + (short)spcy;

	   dx = (x3 - x2) / 2;
	   dy = (y3 - y2) / 2;

	   x2 = x3 - (short)dx;
	   y2 = y3 - (short)dy;

	   moveto(x3, y3);
	   lineto(x2, y2);
	}
}

	/*NP*/
void plot_barbs(void)
	/*************************************************************/
	/*  PLOT_BARBS                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots wind barbs along side of thermo diagram.           */
	/*************************************************************/
{
        short i, x, y;
        short dIndex, sIndex, pIndex, zIndex;
        float lastpres;

        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");
        dIndex = getParmIndex("DRCT");
        sIndex = getParmIndex("SPED");

        if (!sndg || pIndex == -1 || zIndex == -1 || dIndex == -1 ||
            sIndex == -1)
          return;

        setcliprgn(1, 1, xwdth, xhght);
        lastpres = 1100;
        for (i = 0; i < numlvl; i++) {
           /* Only plot barbs up to 100 mb */
           if (sndg[i][pIndex] < 100.0) break;

           if (qc(sndg[i][dIndex])) {
              y = pres_to_pix(sndg[i][pIndex]);
              x = skv.brx - 28;

              if ((sndg[i][zIndex]-i_hght(lastpres, I_PRES)) > 400) {
                 wind_barb(sndg[i][dIndex], sndg[i][sIndex], x, y, 3.5);
                 lastpres = sndg[i][pIndex];
              }
           }
        }

}

/*	short i, x, y;
	float lasthght;
	short pIndex, dIndex, sIndex, zIndex;

	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");
	dIndex = getParmIndex("DRCT");
	sIndex = getParmIndex("SPED");

	if (!sndg || dIndex == -1 || sIndex == -1 || zIndex == -1)
	  return;

	setcliprgn(1, 1, xwdth, xhght);
	lasthght = 0;
	for (i = 0; i < numlvl; i++) {

	if (sndg[i][pIndex] < 100.0 ) break;

           if (qc(sndg[i][dIndex])) {
              y = pres_to_pix(sndg[i][pIndex]);
              x = skv.brx - 28;

	      if ((sndg[i][zIndex]-lasthght) > 400.0) 
			{
			wind_barb( sndg[i][dIndex], sndg[i][sIndex], x, y, 3.5);
		 	lasthght = sndg[i][zIndex];
	      		}
	   }
	}
}
*/
              /* RLT attempt to color code wind barbs to match hodograph 2/25/05 */
/*            if (((sndg[i][zIndex]-lasthght) > 400.0) && ((sndg[i][zIndex] >= 0) && (sndg[i][zIndex] < 3000))) 
                        {
                        setcolor(2);
                        wind_barb( sndg[i][dIndex], sndg[i][sIndex], x, y, 3.5);
                        lasthght = sndg[i][zIndex];
                        }
              if (((sndg[i][zIndex]-lasthght) > 400.0) && ((sndg[i][zIndex] >= 3000) && (sndg[i][zIndex] < 6000)))
                        {
                        setcolor(3);
                        wind_barb( sndg[i][dIndex], sndg[i][sIndex], x, y, 3.5);
                        lasthght = sndg[i][zIndex];
                        }
              if (((sndg[i][zIndex]-lasthght) > 400.0) && ((sndg[i][zIndex] >= 6000) && (sndg[i][zIndex] < 9000)))
                        {
                        setcolor(5);
                        wind_barb( sndg[i][dIndex], sndg[i][sIndex], x, y, 3.5);
                        lasthght = sndg[i][zIndex];
                        }
              if (((sndg[i][zIndex]-lasthght) > 400.0) && ((sndg[i][zIndex] >= 9000) && (sndg[i][zIndex] < 12000)))
                        {
                        setcolor(6);
                        wind_barb( sndg[i][dIndex], sndg[i][sIndex], x, y, 3.5);
                        lasthght = sndg[i][zIndex];
                        }
              if (((sndg[i][zIndex]-lasthght) > 400.0) && (sndg[i][zIndex] >= 12000))
                        {
                        setcolor(29);
                        wind_barb( sndg[i][dIndex], sndg[i][sIndex], x, y, 3.5);
                        lasthght = sndg[i][zIndex];
                        }
               }
           }
}

*/

/*

This routine has problems like trace_hodo

*/
        /*NP*/
void plot_barbs2(void)
        /*************************************************************/
        /*  PLOT_BARBS                                               */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Plots wind barbs along side of thermo diagram.           */
        /*************************************************************/
{
        short i, x, y;
	short dIndex, sIndex, pIndex, zIndex;
        float lastpres;

	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");
	dIndex = getParmIndex("DRCT");
	sIndex = getParmIndex("SPED");

	if (!sndg || pIndex == -1 || zIndex == -1 || dIndex == -1 || 
	    sIndex == -1)
	  return;

        setcliprgn(1, 1, xwdth, xhght);
        lastpres = 1100;
        for (i = 0; i < numlvl; i++) {
	   /* Only plot barbs up to 100 mb */
	   if (sndg[i][pIndex] < 100.0) break;

           if (qc(sndg[i][dIndex])) {
              y = pres_to_pix(sndg[i][pIndex]);
              x = skv.brx - 80;

              if ((sndg[i][zIndex]-i_hght(lastpres, I_PRES)) > 400) {
                 wind_barb(sndg[i][dIndex], sndg[i][sIndex], x, y, 3.5);
                 lastpres = sndg[i][pIndex];
              }
           }
        }
}


	/*NP*/
void vvel_profile(void)
	/*************************************************************/
	/*  VVEL_PROFILE                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots vertical velocity profile                          */
	/*************************************************************/
{
	short i, x1, x2, y, avail, leng;
	short pIndex, oIndex;
	float lastpres;

	pIndex = getParmIndex("PRES");
	oIndex = getParmIndex("OMEG");

	if (!sndg || pIndex == -1 || oIndex == -1)
	  return;

        avail=0;
	setcliprgn(1, 1, xwdth, xhght);
	lastpres = 1100;

	for (i = 0; i < numlvl; i++) {
	   if (qc(sndg[i][oIndex]) && (sndg[i][oIndex] < 1)) {
	      avail++;
	      y = pres_to_pix(sndg[i][pIndex]);
	      x1 = skv.tlx + 40;
	      leng = (sndg[i][oIndex] * 1000);   /* Convert to Mbs/sec */
	      x2 = x1 - (leng * 2);	    /* Determine screen scale */
	      setcolor(25);
	      if (sndg[i][oIndex] < 0)
	        setcolor(12);
	      moveto(x1, y);
	      lineto(x2, y);
	   }
	}
	
	/* ----- Draw Scales ----- */
	if (avail > 5) {
	   setcolor(7);
           x1 = skv.tlx + 40;
	   moveto(x1, skv.tly+40);
	   lineto(x1, skv.bry-10);
	   
	   setlinestyle(3, 1);
	   x1 = skv.tlx + 20;
	   moveto(x1, skv.tly+40);
	   lineto(x1, skv.bry-10);
	   x1 = skv.tlx + 60;
	   moveto(x1, skv.tly+40);
	   lineto(x1, skv.bry-10);
	   
	   set_font(2);
	   outgtext("OMEGA", skv.tlx+18, skv.tlx);
	   outgtext("+10", skv.tlx+3, skv.tlx+15);
  	   outgtext("-10", skv.tlx+43, skv.tlx+15);
	}
}

	/*NP*/
void trace_dcape(void)
	/*************************************************************/
	/*  TRACE_DCAPE                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots downdraft parcel trajectory                        */
	/*************************************************************/
{
	float mine, minep, ix1, tp1, tp2, te1, te2, pe1, pe2, h1, h2;
	float ent2, tote, lyre, upper, p5;
	short i, uptr, x, y;
	short pIndex, zIndex, tIndex, tdIndex;

	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");
	tIndex = getParmIndex("TEMP");
	tdIndex = getParmIndex("DWPT");

	if (!sndg || numlvl < 1 || pIndex == -1 || zIndex == -1 || 
	    tIndex == -1 || tdIndex == -1)
	  return;

	setcolor(31);
        setlinestyle(4, 1);

        /* ----- Find highest observation in layer ----- */
        i=numlvl-1;
        while (sndg[i][pIndex] < sndg[sfc()][pIndex]-400) { i--;}
        p5 = i;
        if (sndg[i][pIndex] == sndg[sfc()][pIndex]-400 ) { p5--; }

	/* ----- Find min ThetaE layer ----- */
        mine=1000; minep=-999;
        for (i=0;i<p5;i++) {
           if (qc(sndg[i][tdIndex]) && 
	       (qc(i_temp(sndg[i][pIndex]+100, I_PRES)))) {
              Mean_thetae(&ix1, sndg[i][pIndex], sndg[i][pIndex]-100);
              if (qc(ix1) && (ix1 < mine)) { 
	        mine=ix1; minep=sndg[i][pIndex]-50;
	      }
           }
        }
        if (minep < 0)
	  return;

        upper = minep;

        /* ----- Find highest observation in layer ----- */
        i=numlvl-1;
        while (sndg[i][pIndex] < upper) { i--;}
        uptr = i;
        if (sndg[i][pIndex] == upper) { uptr--; }

        /* ----- Define parcel starting point ----- */
        tp1 = wetbulb(upper, i_temp(upper, I_PRES), i_dwpt(upper, I_PRES));
        pe1 = upper;
        te1 = i_temp(pe1, I_PRES);
        h1 =  i_hght(pe1, I_PRES);
        tote = 0;
        lyre = 0;

	x = temp_to_pix(tp1 , pe1);
	y = pres_to_pix(pe1);
	moveto(x, y);

        for (i=uptr;i>=sfc();i--) {
           pe2 = sndg[i][pIndex];
           te2 = sndg[i][tIndex];
           h2  = sndg[i][zIndex];
           tp2 = wetlift(pe1, tp1, pe2);

	   if (qc(te2) && qc(tp2)) {
              /* Account for Entrainment */
              ent2 = ENTRAIN_DEFAULT * ((h1 - h2) / 1000);
              tp2 = tp2 + ((te2 - tp2) * ent2);

	      x = temp_to_pix( tp2 , pe2 );
	      y = pres_to_pix( pe2 );
	      lineto(x, y);

              pe1 = pe2;
              te1 = te2;
              h1 = h2;
              tp1 = tp2;
	   }
        }
}

void copytodisplay(void)
{
	XCopyArea(XtDisplay(draw_reg), canvas, XtWindow(draw_reg), 
		  gc, 0, 0, xwdth, xhght, 0, 0);
	XFlush(XtDisplay(draw_reg));
}

void display_dendritic_zone(short width)
{
        short i, x, y, xold, yold;
        short pIndex, tIndex;
	float p_top, p_bot, ix1, ix2, ix3, ix4;
	float t1, p1;
 	char st[20];

        pIndex = getParmIndex("PRES");
        tIndex = getParmIndex("TEMP");

        if (!sndg || pIndex == -1 || tIndex == -1) return;

        setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
        setlinestyle(1, width + 1);
	setcolor(19);

	/* Determine height of -16 temp */
	p_top = temp_lvl(-17, &ix1);

	/* Determine height of -12 temp */
	p_bot = temp_lvl(-12, &ix1);

	/* Loop though each observation and draw lines */
        x = temp_to_pix(-17, p_top);
        y = pres_to_pix(p_top);
	moveto(x,y);
        for( i=p_top; i <= p_bot; i=i+5) 
		{
              	xold = x;
              	yold = y;

		t1 = i_temp(i, I_PRES);
              	x = temp_to_pix(t1, i);
              	y = pres_to_pix(i);

                lineto(x, y);
              	}

	/* Draw lines to define top/bottom of layer */
	setlinestyle(1, 1);
	setcolor(19);
	set_font(3);

	y = pres_to_pix(p_top);
	moveto(skv.brx - 80, y);
	lineto(skv.brx - 120, y);
	sprintf( st, "%.0f'", mtof(i_hght(p_top, I_PRES)));
	disp_param( st, skv.brx-130, y-10);

	y = pres_to_pix(p_bot);
	moveto(skv.brx - 80, y);
	lineto(skv.brx - 120, y);
	sprintf( st, "%.0f'", mtof(i_hght(p_bot, I_PRES)));
	disp_param( st, skv.brx-130, y-10);

	/* Draw marker for freezing level */
	setcolor(16);
	p_top = temp_lvl(0, &ix1);
	if (p_top < (sndg[sfc()][pIndex] - 1))
		{
		y = pres_to_pix(p_top);
        	moveto(skv.brx - 80, y);
        	lineto(skv.brx - 120, y);
        	sprintf( st, "FRZ = %.0f'", mtof(i_hght(p_top, I_PRES)));
		disp_param( st, skv.brx-130, y-10);
		}

	/* Draw marker for WBZ level */
	setcolor(21);
	p_top = wb_lvl(0, &ix1);
	if (p_top < (sndg[sfc()][pIndex] - 1)) 
		{
	        y = pres_to_pix(p_top);
       	 	moveto(skv.brx - 80, y);
        	lineto(skv.brx - 120, y);
        	sprintf( st, "WBZ = %.0f'", mtof(i_hght(p_top, I_PRES)));
		disp_param( st, skv.brx-130, y-10);
		}
}

void display_levels(short colr)
{
        short i, x, y, xold, yold;
        short pIndex, tIndex;
        float p_top, p_bot, ix1, ix2, ix3, ix4;
        float h;
        char st[20];

        pIndex = getParmIndex("PRES");
        tIndex = getParmIndex("TEMP");

        if (!sndg || pIndex == -1 || tIndex == -1) return;

        setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
        setlinestyle(1, 1);
        setcolor(colr);
        set_font(4);

        /* Draw sfc level */
        h = 0;
        y = pres_to_pix(i_pres(msl(h)));
        setcolor(0);
        rectangle(1, skv.tlx + 18, y-7, skv.tlx + 100, y+7);
        setcolor(colr);
        moveto(skv.tlx + 20, y);
        lineto(skv.tlx + 30, y);
        sprintf( st, "SFC (%.0fm)", i_hght(sndg[sfc()][pIndex], I_PRES));
        outgtext(st, skv.tlx+ 35, y-5);

	/* Draw 1km level */
	h = 1000;
        y = pres_to_pix(i_pres(msl(h)));
        setcolor(0);
        rectangle(1, skv.tlx + 18, y-7, skv.tlx + 60, y+7);
        setcolor(colr);
        moveto(skv.tlx + 20, y);
        lineto(skv.tlx + 30, y);
        sprintf( st, "1 km");
	outgtext(st, skv.tlx+ 35, y-5);

        /* Draw 3km level */
        h = 3000;
        y = pres_to_pix(i_pres(msl(h)));
        setcolor(0);
        rectangle(1, skv.tlx + 18, y-7, skv.tlx + 60, y+7);
        setcolor(colr);
        moveto(skv.tlx + 20, y);
        lineto(skv.tlx + 30, y);
        sprintf( st, "3 km");
        outgtext(st, skv.tlx+ 35, y-5);

        /* Draw 6km level */
        h = 6000;
        y = pres_to_pix(i_pres(msl(h)));
        setcolor(0);
        rectangle(1, skv.tlx + 18, y-7, skv.tlx + 60, y+7);
        setcolor(colr);
        moveto(skv.tlx + 20, y);
        lineto(skv.tlx + 30, y);
        sprintf( st, "6 km");
        outgtext(st, skv.tlx+ 35, y-5);

        /* Draw 9km level */
        h = 9000;
        y = pres_to_pix(i_pres(msl(h)));
        setcolor(0);
        rectangle(1, skv.tlx + 18, y-7, skv.tlx + 60, y+7);
        setcolor(colr);
        moveto(skv.tlx + 20, y);
        lineto(skv.tlx + 30, y);
        sprintf( st, "9 km");
        outgtext(st, skv.tlx+ 35, y-5);

        /* Draw 12km level */
        h = 12000;
        y = pres_to_pix(i_pres(msl(h)));
        setcolor(0);
        rectangle(1, skv.tlx + 18, y-7, skv.tlx + 65, y+7);
        setcolor(colr);
        moveto(skv.tlx + 20, y);
        lineto(skv.tlx + 30, y);
        sprintf( st, "12 km");
        outgtext(st, skv.tlx+ 35, y-5);

        /* Draw 15km level */
        h = 15000;
        y = pres_to_pix(i_pres(msl(h)));
        setcolor(0);
        rectangle(1, skv.tlx + 18, y-7, skv.tlx + 65, y+7);
        setcolor(colr);
        moveto(skv.tlx + 20, y);
        lineto(skv.tlx + 30, y);
        sprintf( st, "15 km");
        outgtext(st, skv.tlx+ 35, y-5);
}

void display_effective_layer(short colr)
{
        short i, x, y, xold, yold;
        short pIndex, tIndex, x1, x2, x3;
        float ptop, pbot, ix1, ix2, ix3, ix4;
        float h, y1, y2;
        char st[20];
	Parcel pcl;

/*	x1 = 350;
	x2 = x1 + 40;
	x3 = (x1 + x2) / 2;
*/

	x1 = (xwdth*.31);
	x2 = x1 + 40;
	x3 = (x1 + x2) / 2;

        pIndex = getParmIndex("PRES");
        tIndex = getParmIndex("TEMP");

        if (!sndg || pIndex == -1 || tIndex == -1) return;

        setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
        setlinestyle(1, 2);
        setcolor(colr);
	set_font(4);

	/* Get Effective Layer Data */
/*	printf( "Getting Effective Layer Data\n");
*/

/* 24 Mar 2008 */
	effective_inflow_layer(100,-250, &p_bot, &p_top);

/*	printf("\n display elayer pbot = %.1f\n", p_bot);
        printf("\n display elayer pbot = %.1f\n", p_top);
*/
	if (p_bot < 1) return;

        /* Draw effective sfc level */
/*	printf( "Drawing Effective Layer Data\n");
*/      y = pres_to_pix(p_bot);
	y1 = y;
        moveto(skv.brx - x1, y);
        lineto(skv.brx - x2, y);
	if (agl(i_hght(p_bot, I_PRES)) < 1)
		{ sprintf( st, "SFC", agl(i_hght(p_bot, I_PRES))); }
	else
		{  sprintf( st, "%.0fm", agl(i_hght(p_bot, I_PRES))); }
        setcolor(0);
        rectangle(1, skv.brx - x2 - 42, y-3, skv.brx - x2 - 1, y+10);
	setcolor(colr);
        outgtext(st, skv.brx - x2 - 40, y-2);

        /* Draw effective top level */
        y = pres_to_pix(p_top);
	y2 = y;
        moveto(skv.brx - x1, y);
        lineto(skv.brx - x2, y);
        setcolor(0);
        rectangle(1, skv.brx - x2 - 42, y-13, skv.brx - x2 - 1, y-1);
        setcolor(colr);
        sprintf( st, "%.0fm", agl(i_hght(p_top, I_PRES)));
        outgtext(st, skv.brx - x2 - 40, y-12);

	/* Draw connecting line */
        moveto(skv.brx - x3, y1);
        lineto(skv.brx - x3, y2);

	/* Compute and display effective helicity */
	ix4 = helicity(agl(i_hght(p_bot, I_PRES)), agl(i_hght(p_top, I_PRES)), st_dir, st_spd, &ix1, &ix2);
        setcolor(0);
        rectangle(1, skv.brx-x2 -8, y2-12, skv.brx-x2+50, y2-1);
        setcolor(colr);
	sprintf( st, "%4.0f m%cs%c", ix4, 178, 178 );
	outgtext(st, skv.brx - x2, y2-13);

	/* Mark effective layer on hodograph */
	

	/* define_parcel(6, 100);
	ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        sprintf( st, "%4.0f J/kg", pcl.bplus);
        outgtext(st, skv.brx - x2, y1+5); */
}
#endif

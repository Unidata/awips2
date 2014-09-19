/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  DOS Video Graphics Routines (Part #5)                      */
/*  Inset graphic display routines.                            */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  INSET_OPTIONS                                              */
/*  VISUAL1                                                    */
/*  PLOT_VIS                                                   */
/*  VIS_XY                                                     */
/*  WRITE_VIS_DATA                                             */
/*  DRAW_HOINSET                                               */
/*  DRAW_SKINSET                                               */
/*  PLOT_STORMINFLOW                                           */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "xwcmn.h"
#include "sharp95.h"

float visual1(float lower, float upper, float pres, float temp, float dwpt, int ulx, int uly, int vwid);

short sk_mode = 0;
short ho_mode = 0;

	/*NP*/
void inset_options( short mode )
	/*************************************************************/
	/*  INSET_OPTIONS                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Allows the user to select which inset graphic that       */
	/*  will be displayed.                                       */
	/*************************************************************/
{
	if (mode == DRAW_SKEWT) {
	  /* ----- This is a Skew-T ----- */
	  sk_mode++;
	  if (sk_mode > 1) {sk_mode = 0;}
	  redraw_graph(DRAW_SKEWT);
	}
	else {
	  /* ----- This is a Hodograph ----- */
	  ho_mode++;
	  if (ho_mode > 3) {ho_mode = 0;}
	  redraw_graph(DRAW_HODO);
	}
}

	/*NP*/
float visual1(float lower, float upper, float pres, float temp, float dwpt, int ulx, int uly, int vwid)
	/*************************************************************/
	/*  VISUAL1                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Lifts specified parcel, given an initial 5 m/s push.     */
	/*  parcel trajectory is then calculated, using strict       */
	/*  parcel theory.  Updraft size is assumed 1km dia.         */
	/*                                                           */
	/*  All calculations use the virtual temperature correction. */
	/*                                                           */
	/*  lower       =  Bottom level of layer (mb)  [ -1=SFC]     */
	/*  upper       =  Top level of layer (mb)     [ -1=TOP]     */
	/*  pres        =  LPL pressure (mb)                         */
	/*  temp        =  LPL temperature (c)                       */
	/*  dwpt        =  LPL dew point (c)                         */
	/*************************************************************/
{
	short i, lptr, uptr, pIndex, zIndex, tIndex, tdIndex;
	float te1, pe1, te2, pe2, h1, h2, lyre, tdef1, tdef2, totp, totn;
	float te3, pe3, h3, tp1, tp2, tp3, tdef3, lyrf, lyrlast, pelast;
	float tote, dh, restim, uvv, ix1, ix2, tottim;
	float u, v, du, dv, tsu, tsv, tdist, tangle, disp, angl;
	short colrx;

	lyre = -1;
	totp = 25;
	totn = 0;

	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");
	tIndex = getParmIndex("TEMP");
	tdIndex = getParmIndex("DWPT");
	if (!sndg || pIndex == -1 || zIndex == -1 || tIndex == -1 || tdIndex == -1) return RMISSD;

	/* ----- See if default layer is specified ----- */
	if (lower == -1) { lower = sndg[sfc()][pIndex]; }
	if (upper == -1) { upper = sndg[numlvl-1][pIndex]; }

	/* ----- Make sure this is a valid layer ----- */
	if( lower > pres ) { lower = pres; }
	if( !qc( i_vtmp( upper , I_PRES))) { return RMISSD; }
	if( !qc( i_vtmp( lower , I_PRES))) { return RMISSD; }

	/* ----- Begin with Mixing Layer (LPL-LCL) ----- */
	te1 = i_vtmp(pres , I_PRES);
	pe1 = lower;
	h1 =  i_hght(pe1 , I_PRES);
	tp1 = virtemp(pres, temp, dwpt);

	drylift(pres, temp, dwpt, &pe2, &tp2);
	h2 =  i_hght(pe2 , I_PRES);
	te2 = i_vtmp(pe2 , I_PRES);

	if( lower > pe2 ) { lower = pe2; }

	/* ----- Find lowest observation in layer ----- */
	i = 0;
	while( sndg[i][pIndex] > lower)  { i++; }
	while ( !qc(sndg[i][tdIndex]) ) { i++; }
	lptr = i;
	if( sndg[i][pIndex] == lower ) { lptr++; }

	/* ----- Find highest observation in layer ----- */
	i=numlvl-1;
	while(sndg[i][pIndex] < upper) { i--; }
	uptr = i;
	if( sndg[i][pIndex] == upper ) { uptr--; }

	/* ----- Start with interpolated bottom layer ----- */
	pe1 = lower;
	h1 =  i_hght( pe1 , I_PRES);
	te1 = i_vtmp( pe1 , I_PRES);
	tp1 = wetlift(pe2, tp2, pe1);

	totp = 25;
	totn = 0;
	tsu = 0;
	tsv = 0;
	restim = 0;
	tottim = 0;
	for (i = lptr; i < numlvl; i++) {
	   if (qc(sndg[i][tIndex])) {
	      /* ----- Calculate every level that reports a temp ----- */
	      pe2 = sndg[i][pIndex];
	      h2 =  sndg[i][zIndex];
	      te2 = i_vtmp( pe2 , I_PRES);
	      tp2 = wetlift(pe1, tp1, pe2);
	      tdef1 = (virtemp(pe1, tp1, tp1) - te1) / (te1 + 273.15);
	      tdef2 = (virtemp(pe2, tp2, tp2) - te2) / (te2 + 273.15);
	      lyrlast = lyre;
	      lyre = 9.8F * (tdef1 + tdef2) / 2.0F * (h2 - h1);

	      if( lyre > 0 ) { totp += lyre; }
	      else { if(pe2 > 500) { totn += lyre; } }
	      tote += lyre;

	      uvv = (float)sqrt( totp * 2 );
	      dh = h2 - h1;
	      restim = dh / uvv;
	      tottim += restim;

	      sr_wind( pe1, pe2, st_dir, st_spd, &u, &v, &ix1, &ix2);
	      du = kt_to_mps(u) * restim;
	      dv = kt_to_mps(v) * restim;
	      tsu -= du;
	      tsv += dv;
	      tdist = (float)sqrt((tsu*tsu) + (tsv*tsv));
	      tangle = angle(tsu, tsv);

	      pelast = pe1;
	      pe1 = pe2;
	      h1 = h2;
	      te1 = te2;
	      tp1 = tp2;

	      /* ----- Is this the top of given layer ----- */
	      if(i >= uptr) {
		 pe3 = pe1;
		 h3 = h1;
		 te3 = te1;
		 tp3 = tp1;
		 lyrf = lyre;

		 if( lyrf > 0 )
		    { totp -= lyrf; }
		 else
		    { if(pe2 > 500) { totn -= lyrf; } }

		 pe2 = upper;
		 h2 = i_hght( pe2 , I_PRES);
		 te2 = i_vtmp( pe2 , I_PRES);
		 tp2 = wetlift(pe3, tp3, pe2);
		 tdef3 = (virtemp(pe3, tp3, tp3) - te3) / (te3 + 273.15);
		 tdef2 = (virtemp(pe2, tp2, tp2) - te2) / (te2 + 273.15);
		 lyrf = 9.8F * (tdef3 + tdef2) / 2.0F * (h2 - h3);
		 if( lyrf > 0 )
		    { totp += lyrf; }
		 else
		    { if(pe2 > 500) { totn -= lyrf; } }

		 if( totp == 0 ) { totn = 0; }

		 uvv = (float)sqrt( totp * 2 );
		 dh = h2 - h1;
		 restim = dh / uvv;
		 tottim += restim;

		 sr_wind( pe1, pe2, st_dir, st_spd, &u, &v, &ix1, &ix2);
		 du = kt_to_mps(u) * restim;
		 dv = kt_to_mps(v) * restim;
		 tsu -= du;
		 tsv += dv;
		 tdist = (float)sqrt((tsu*tsu) + (tsv*tsv));
		 tangle = angle(tsu, tsv);

		 colrx = 7;
		 vis_xy( -tsu, -tsv, colrx, ulx, uly, vwid);

		 angl = 90 - angle( tdist, agl(h2));

		 write_vis_data( tottim, angl, ulx, uly, vwid );
		 return 1;
	      }

	      colrx = 13;
	      if (h2>msl(3000)) colrx=3;
	      if (h2>msl(6000)) colrx=27;
	      if (h2>msl(9000)) colrx=20;
	      if (h2>msl(12000)) colrx=6;
	      vis_xy(-tsu, -tsv, colrx, ulx, uly, vwid);

	      if (sndg[i][pIndex] == 500) {
		 disp = tdist;
		 angl = 90 - angle( tdist, agl(sndg[i][zIndex]));
	      }
	   }
	}

	return 1.0;  /* ? mkay. there was no value before. bad thing */
}

        /*NP*/
float bndry_ci(float lift, float distance)
        /*************************************************************/
        /*  BOUNDARY CONVECTIVE INITIATION                           */
        /*  Rich Thompson and John Hart  SPC OUN                     */
        /*                                                           */
        /*  Lifts ML parcel to find LFC, then calculates parcel      */
        /*  trajectory relative to a boundary.  If parcel stays      */
        /*  within dist_to_bndry long enough to reach the LFC,       */
        /*  then convective initiation is likely                     */
        /*                                                           */
        /*  lift       =  ascent rate in m/min along boundary        */
        /*  dist_to_bndry       =  width of ascent zone (m)          */
        /*************************************************************/

{
	float ix1, ix2, u, v, pe1, pe2, lfc, height, pres;
	short oldlplchoice, i, min_lift;
	Parcel pcl;

	oldlplchoice = lplvals.flag;

	/* ----- Begin with ML parcel LFC height (m AGL) */
	define_parcel(4,100);
	ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	lfc = agl(i_hght(pcl.lfcpres, I_PRES));

	/* ----- Calculate minutes of lift needed to reach LFC */

	min_lift = (short)(lfc / lift);

	printf("\nminutes of lift to LFC = %d\n", min_lift);

	/* if parcel remains within distance (to bndry), continue ascent */
	for (i = 0; i < min_lift; i++){
		
                 height = i * lift;
/*		 pe1 = i_pres(msl(height));
		 pe2 = i_pres(msl(height));

                 restim = dh / uvv;
                 tottim += restim;

                 sr_wind( pe1, pe2, st_dir, st_spd, &u, &v, &ix1, &ix2);
                 du = kt_to_mps(u) * restim;
                 dv = kt_to_mps(v) * restim;
                 tsu -= du;
                 tsv += dv;
                 distance = (float)sqrt((tsu*tsu) + (tsv*tsv));
                 tangle = angle(tsu, tsv);
*/		

	}

        if (oldlplchoice == 3)
          pres = mu_layer;
        else if (oldlplchoice == 4)
          pres = mml_layer;
        else if (oldlplchoice == 5)
          pres = user_level;
        else
          pres = mml_layer;
        define_parcel(oldlplchoice, pres);


	return min_lift;
}
	/*NP*/
void plot_vis(void)
	/*************************************************************/
	/*  PLOT_VIS                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots the results of storm-relative parcel trajectory.   */
	/*************************************************************/
{
	float sfctemp, sfcdwpt, sfcpres, ix1;
	short x1, y1, x2, y2, ulx, uly, vwid;
	Parcel pcl;

	ulx = hov.tlx;
	uly = hov.bry;
	vwid = 120;

	setcliprgn( ulx, uly, ulx + vwid, uly + vwid - 1);
	setcolor(0);
	setlinestyle( 1, 1 );
	rectangle( 1, ulx, uly, ulx + vwid, uly + vwid - 1); 
	setcolor(1);
	rectangle( 0, ulx, uly, ulx + vwid, uly + vwid - 1);

       /* ----- Plot Crosshairs ----- */
	setcolor(24);
	moveto(ulx, uly + (vwid/2)); lineto(ulx + vwid, uly + (vwid/2));
	moveto(ulx + (vwid/2), uly); lineto(ulx + (vwid/2), uly + vwid);

	/* ----- Plot Label ----- */
	setcolor(1);
	/* outgtext( "Storm", hov.brx-145, hov.tly + 125 ); */
	outgtext( "Storm Slinky",  ulx + 5, uly + vwid-13 );


	/* ----- Calculate Parcel Data ----- */
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Plot storm motion ----- */
	x1 = ulx + (vwid/2);
	y1 = uly + (vwid/2);
	x2 = x1 + (short)ucomp( st_dir, 30);
	y2 = y1 - (short)vcomp( st_dir, 30);
	setcolor(31);
	moveto( x1, y1);
	lineto( x2, y2);

	/* ----- Run Visualization Routine ----- */
	ix1 = visual1( pcl.lfcpres, pcl.elpres, sfcpres, sfctemp, sfcdwpt, ulx, uly, vwid);
	
	setcliprgn(1, 1, xwdth, xhght);
}

	/*NP*/
void vis_xy( float x, float y, short colrx, int ulx, int uly, int vwid)
	/*************************************************************/
	/*  VIS_XY                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots the point (x,y) on the storm visualization chart.  */
	/*************************************************************/
{
	float xfactor, xs, ys;
	xfactor=.01F;

	xs = (ulx + (vwid/2)) + (x * xfactor);
	ys = (uly + (vwid/2)) + (y * xfactor);

/*	printf( "Plotting color: %d\n", colrx); */
	setcolor(colrx);
	setlinestyle (1, 1);
	ellipse( 0, (short)xs-5, (short)ys-5, (short)xs+5, (short)ys+5);
}


	/*NP*/
void write_vis_data( float tim, float ang, int ulx, int uly, int vwid )
	/*************************************************************/
	/*  WRITE_VIS_DATA                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes the numeric data on the storm visualization chart */
	/*************************************************************/
{
	short tr, tl;
	char st[10];

	setcolor(31);

	tr = ulx + 5;
	tl = uly + 5;
	strcpy( st, qc2( tim, " s", 0 ));
	disp_param( st, tr, tl);

	tr = ulx + vwid-5;
	strcpy( st, qc2( ang, " deg", 0 ));
	disp_param( st, tr, tl);
}

	/*NP*/
void draw_hoinset( void )
	/*************************************************************/
	/*  DRAW_HOINSET                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws the hodograph inset graphic selected by the user.  */
	/*************************************************************/
{
	switch(ho_mode)
	   {
	   case 0:
	      break;
	   case 1:
	      plot_vis();
	      break;
	   case 2:
	      plot_storminflow();
	      break;
	   case 3:
	      plot_vertsrw();
	      break;
	   }

	XCopyArea(XtDisplay(draw_reg), canvas, XtWindow(draw_reg), 
		  gc, hov.brx-150, hov.tly, 150, 150, 
		  hov.brx-150, hov.tly);
	XFlush(XtDisplay(draw_reg));
	setcliprgn(1, 1, xwdth, xhght);
}

	/*NP*/
void draw_skinset( void )
	/*************************************************************/
	/*  DRAW_SKINSET                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws the skew-t inset graphic selected by the user.     */
	/*************************************************************/
{
	switch(sk_mode)
	   {
	   case 0:
	      break;
	   case 1:
	      plot_thetae();
	      break;
	   }
}

	/*NP*/
void plot_storminflow( void )
	/*************************************************************/
	/*  PLOT_STORMINFLOW                                         */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots various inflow/sr wind vectors.                    */
	/*************************************************************/
{
	float sfctemp, sfcdwpt, sfcpres, ix1, ix2, ix3, ix4;
	short x1, y1, x2, y2, x3, y3, i;
	struct _parcel pcl;

	setcliprgn( hov.brx-150, hov.tly, hov.brx, hov.tly+150);
	setcolor(0);
	setlinestyle( 1, 1 );
	rectangle( 1, hov.brx-150, hov.tly, hov.brx, hov.tly+150);
	setcolor(1);
	rectangle( 0, hov.brx-150, hov.tly, hov.brx, hov.tly+150);
	setcolor(1);
	moveto(hov.brx-150, hov.tly + 75); lineto(hov.brx, hov.tly+75);
	moveto(hov.brx-75, hov.tly); lineto(hov.brx-75, hov.tly+150);
	
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	/* ----- Plot speed rings ----- */
	x1 = hov.brx - 75;
	y1 = hov.tly + 75;
	setlinestyle( 2, 1 );
	setcolor(24);
	for(i=20; i<=60; i+=20) { ellipse( 0, x1-i, y1-i, x1+i, y1+i); }
	rectangle( 0, hov.brx-150, hov.tly, hov.brx, hov.tly+150);

	/* ----- Plot Label ----- */
	setcolor(1);
	outgtext( "SR Wind", hov.brx-135, hov.tly + 3 );
	outgtext( "Vectors", hov.brx-135, hov.tly + 15 );

	/* ----- Calculate Parcel Data ----- */
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Plot 0-2km Inflow Vector ----- */
	sr_wind( -1, i_pres(msl(2000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3))
	   {
	   x1 = hov.brx - 75;
	   y1 = hov.tly + 75;
	   x2 = (hov.brx - 75) + (short)ix1;
	   y2 = (hov.tly + 75) - (short)ix2;
	   x3 = x2 + 1;
	   y3 = y2 + 1;

	   setlinestyle( 1, 2 );
	   setcolor(2);
	   moveto( x1, y1); lineto( x2, y2);
	   outgtext( "L", x3, y3 );
	   }

	/* ----- Plot 4-6km Inflow Vector ----- */
	sr_wind( i_pres(msl(4000)), i_pres(msl(6000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3))
	   {
	   x1 = hov.brx - 75;
	   y1 = hov.tly + 75;
	   x2 = (hov.brx - 75) + (short)ix1;
	   y2 = (hov.tly + 75) - (short)ix2;
	   x3 = x2 + 1;
	   y3 = y2 + 1;

	   setcolor(6);
	   setlinestyle( 1, 2 );
	   moveto( x1, y1); lineto( x2, y2);
	   outgtext( "M", x3, y3 );
	   }

	/* ----- Plot 9-11km Inflow Vector ----- */
	sr_wind( i_pres(msl(9000)), i_pres(msl(11000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3))
	   {
	   x1 = hov.brx - 75;
	   y1 = hov.tly + 75;
	   x2 = (hov.brx - 75) + (short)ix1;
	   y2 = (hov.tly + 75) - (short)ix2;
	   x3 = x2 + 1;
	   y3 = y2 + 1;

	   setcolor(7);
	   setlinestyle( 1, 2 );
	   moveto( x1, y1); lineto( x2, y2);
	   outgtext( "H", x3, y3 );
	   }
}

	/*NP*/
void plot_vertsrw(void)
	/*************************************************************/
	/*  PLOT_VERTSRW                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots vertical profile of sr-winds (0-9km AGL)           */
	/*************************************************************/
{
	float bothgt, tophgt, h, ix1, ix2, ix3, ix4, h1, h2;
	short x1, y1, x2, y2, tlx, tly, wid;
	char st[40];

	tlx = hov.tlx + 254;
	tly = hov.bry;
	wid = 135;

	setcliprgn( tlx+2, tly+2, tlx+wid+24, tly+wid-15);
	setcolor(0);
	setlinestyle( 1, 1 );
	rectangle( 1,tlx, tly, tlx+wid+27, tly+wid-15); 
	setcolor(1);
	rectangle( 0, tlx, tly, tlx+wid+27, tly+wid-15);

	/* ----- Set Layer (AGL) ----- */
	bothgt = 0;
	tophgt = 16000;

	/* ----- Plot Label ----- */
	set_font(5);
	setcolor(1);
	outgtext( "SR Winds (kt)", tlx + 20, tly + 3 );
	outgtext( "vs Height", tlx + 20, tly + 15 );

	/* ----- Plot height legend ----- */
	setcolor(1);
	for(h=bothgt; h<=tophgt; h += 2000)
	   {
	   x1 = tlx;
	   y1 = vert_coords(h, tophgt, tly);
	   moveto( x1, y1);
	   lineto(x1+5, y1);

	   if(h>0 && h<16000)
	      {
	      sprintf( st, "%d", (int)(h/1000));
	      outgtext( st, x1+5, y1-4 );
	      }
	   }

	/* ----- Plot horizontal legend ----- */
	setcolor(1);
	for(h=0; h<=80; h += 10)
	   {
	   x1 = tlx + (short)(h*2);
	   y1 = tly + wid - 15;
	   moveto( x1, y1);
	   lineto( x1, y1-5);
	   }

       /* ----- Plot vertical dashed line at 15kt ----- */
       setlinestyle(2, 1);
       moveto( tlx + 30, tly);
       lineto( tlx + 30, tly + wid);

       /* ----- Plot vertical dashed line at 40kt ----- */
       setcolor(7);     	
       moveto( tlx + 80, tly);
       lineto( tlx + 80, tly + (wid/2));

       /* ----- Plot vertical dashed line at 15kt ----- */
       moveto( tlx + 140, tly);
       lineto( tlx + 140, tly + (wid/2));


	/* ----- Plot vertical srw profile ----- */
        setlinestyle(1, 2);
	setcolor(2);
	x2 = 999;
	for(h=bothgt; h<=tophgt; h += 250)
	   {
	   sr_wind( i_pres(msl(h)), i_pres(msl(h)), 
			st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	   x1 = tlx + (short)(ix4*2);
	   y1 = vert_coords(h, tophgt, tly);
	   if(x2 == 999) { x2=x1; y2=y1; }

	   moveto( x1, y1);
	   lineto( x2, y2);

	   x2=x1;
	   y2=y1;
	   }

	/* ----- Plot Mean-Layer SRW value (Sfc-2km) ----- */
	h1 = 0;
	h2 = 2000;
	sr_wind( i_pres(msl(h1)), i_pres(msl(h2)),
			st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix4))
	   {
	   x1 = tlx + (short)(ix4*2);
	   y1 = vert_coords(h1, tophgt, tly);
	   y2 = vert_coords(h2, tophgt, tly);
	   setcolor(15);
	   moveto( x1, y1);
	   lineto( x1, y2);
	   }

	/* ----- Plot Mean-Layer SRW value (4-6km) ----- */
	h1 = 4000;
	h2 = 6000;
	sr_wind( i_pres(msl(h1)), i_pres(msl(h2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix4))
	   {
	   x1 = tlx + (short)(ix4*2);
	   y1 = vert_coords(h1, tophgt, tly);
	   y2 = vert_coords(h2, tophgt, tly);
	   setcolor(25);
	   moveto( x1, y1);
	   lineto( x1, y2);
	   }

	/* ----- Plot Mean-Layer SRW value (9-11km) ----- */
	h1 = 9000;
	h2 = 11000;
	sr_wind( i_pres(msl(h1)), i_pres(msl(h2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix4))
	   {
	   x1 = tlx + (short)(ix4*2);
	   y1 = vert_coords(h1, tophgt, tly);
	   y2 = vert_coords(h2, tophgt, tly);
	   setcolor(7);
	   moveto( x1, y1);
	   lineto( x1, y2);
	   }

	setcliprgn(1, 1, xwdth, xhght);
	copytodisplay();
}

	/*NP*/
short vert_coords( float hgt, float maxhgt, int tly )
	/*************************************************************/
	/*  VERT_COORDS                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Determines the y-pixel value for given height (m).       */
	/*************************************************************/
{
	float xfactor = 1/(maxhgt/120);
	return (short)((tly + 120) - (hgt * xfactor));
}

	/*NP*/
void plot_thetae(void)
	/*************************************************************/
	/*  PLOT_THETAE                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots vertical profile of Theta-E (sfc-500mb)            */
	/*************************************************************/
{
	float bothgt, tophgt, h, cthe, ix1;
	short x1, y1, x2, y2, i, tlx, tly;
	short pIndex, zIndex, tIndex, tdIndex;
	char st[10];

	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");
	tIndex = getParmIndex("TEMP");
	tdIndex = getParmIndex("DWPT");

	if (!sndg || pIndex == -1 || tIndex == -1 || tdIndex == -1 ||
	    zIndex == -1)
	  return;

	/* tlx = hov.brx - 150;
	tly = hov.tly; */

	tlx = hov.tlx + 120;
	tly = hov.bry;

	setcliprgn(tlx, tly, tlx+134, tly+120);
	setcolor(0);
	setlinestyle( 1, 1 );
	rectangle(1,tlx, tly, tlx+134, tly+120); 
	setcolor(1);
	rectangle(0, tlx, tly, tlx+134, tly+120);

	/* ----- Set Layer (AGL) ----- */
	bothgt = 0;
	tophgt = agl(i_hght(500, I_PRES));

	/* ----- Plot Label ----- */
	setcolor(1);
	set_font(4);
	outgtext("Theta-E vs", tlx+55, tly+3);
	outgtext("Pressure",  tlx+55, tly+15);

	/* ----- Plot horizontal legend ----- */
        if (800 < pIndex < 850){
                cthe = (thetae(800, i_temp(800, I_PRES), i_dwpt(800, I_PRES)) +
                        thetae(650, i_temp(650, I_PRES), i_dwpt(650, I_PRES)) +
                        thetae(sndg[sfc()][pIndex], sndg[sfc()][tIndex],
                        sndg[sfc()][tdIndex])) / 3.0;
                        }
        if (750 < pIndex < 800){
                cthe = (thetae(750, i_temp(750, I_PRES), i_dwpt(750, I_PRES)) +
                        thetae(600, i_temp(600, I_PRES), i_dwpt(600, I_PRES)) +
                        thetae(sndg[sfc()][pIndex], sndg[sfc()][tIndex],
                        sndg[sfc()][tdIndex])) / 3.0;
                        }
        if (700 < pIndex < 750){
                cthe = (thetae(700, i_temp(700, I_PRES), i_dwpt(700, I_PRES)) +
                        thetae(500, i_temp(500, I_PRES), i_dwpt(500, I_PRES)) +
                        thetae(sndg[sfc()][pIndex], sndg[sfc()][tIndex],
                        sndg[sfc()][tdIndex])) / 3.0;
                        }
        if (pIndex >= 850){
                cthe = (thetae(850, i_temp(850, I_PRES), i_dwpt(850, I_PRES)) +
                        thetae(700, i_temp(700, I_PRES), i_dwpt(700, I_PRES)) +
                        thetae(sndg[sfc()][pIndex], sndg[sfc()][tIndex],
                        sndg[sfc()][tdIndex])) / 3.0;
                        }
	setcolor(19);
	set_font(5);
	for(h=cthe - 30.0; h<=cthe + 30.0; h += 10) {
	   x1 = (short)(tlx + 60 + ((h-cthe)*2.5));
	   y1 = tly+120;
	   moveto( x1, y1);
	   lineto( x1, y1-5);
	   sprintf(st, "%.0f", h + 273.15);
	   outgtext(st, x1-6, y1-14);
	   }

	/* ----- Plot vertical theta-e profile ----- */
	setlinestyle(1, 2);
	setcolor(2);
	x2 = 999;
        if (sndg[numlvl-1][pIndex] < 500) {
	   for (i=0; sndg[i][pIndex] >= 500; i++) {
	      /*printf ("i = %d,    PRES = %.1f\n", i, sndg[i][pIndex]);*/
	      if (qc(sndg[i][tdIndex])) {
	         x1 = (short)(tlx + 60 + ((thetae(sndg[i][pIndex], 
	              sndg[i][tIndex], sndg[i][tdIndex])-cthe)*2.5));
   	         y1 = vert_coords(agl(sndg[i][zIndex]), tophgt, tly);
	         if(x2 == 999) { x2=x1; y2=y1; }

	         moveto(x1, y1);
	         lineto(x2, y2);

	         x2=x1;
	         y2=y1;
	      }
	   }
	}


	/* ----- Plot Vertical Legend ----- */
	setlinestyle(1, 1);
	setcolor(1);
	set_font(5);
	x2 = 999;
	for(i=1000; i >= 600; i -= 100) {
	   x1 = tlx;
	   y1 = vert_coords(agl(i_hght(i, I_PRES)), tophgt, tly);
	   moveto( x1, y1);
	   lineto( x1+5, y1);
	   sprintf(st, "%d", i);
	   if (i<1000) outgtext(st, x1+6, y1-5);
	}

	setcliprgn(1, 1, xwdth, xhght);
	copytodisplay();

	/* plot theta-e index */
	setcolor(19);
	set_font(4);
	sprintf( st, "TEI = %s", qc2( ThetaE_diff(&ix1), "", 0));
        outgtext( st, tlx + 80, tly + 50);
}

        /*NP*/
void plot_windspeedprofile(void)
        /*************************************************************/
        /*  PLOT_WINDSPEEDPROFILE                                    */
        /*  John Hart  SPC Norman                                    */
        /*                                                           */
        /*  Plots vertical profile of Wind Speeds                    */
        /*************************************************************/
{
	float lasthght;
        float bothgt, tophgt, h, wsp;
        short x1, y1, x2, y2, i, tlx, tly, wid, hgt;
        short pIndex, zIndex, wsIndex, wdIndex;
        char st[10];

        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");
        wsIndex = getParmIndex("SPED");

        if (!sndg || pIndex == -1 || zIndex == -1 || wsIndex == -1) return;

        tlx = skv.brx;
        tly = skv.tly;
	wid = 93;
	hgt = skv.bry - skv.tly;

        setcliprgn(tlx, tly, tlx+wid, tly+hgt+15);
        setcolor(0);
        setlinestyle( 1, 1 );
        rectangle(1,tlx, tly, tlx+wid, tly+hgt);
        setcolor(1);
        rectangle(0, tlx, tly, tlx+wid, tly+hgt);

	/* Plot scale */
        setlinestyle(2, 1);
	set_font(5);
	for (i=20;i<=120;i+=20) {
        	setcolor(8);
		x1 = tlx + (i/1.5);
		x2 = x1;
		y1 = tly + 1;
		y2 = tly + hgt;
              	moveto(x1, y1);
              	lineto(x2, y2);
        	setcolor(1);
		sprintf( st, "%d", i);	
		outgtext(st, x1-3, y2+2);
		}

        /* ----- Plot Label ----- */
        setcolor(1);
	set_font(5);
        outgtext("Wind Speed (kt)", tlx+5, tly+3);
        outgtext("vs Height",  tlx+5, tly+15);


	/* color code to match hodograph layers */
	lasthght = 0;
	setlinestyle(1, 1);
        for (i=0; i<numlvl; i++) 
	{
      		if ((lasthght >= 0) && (lasthght < 3000))
        		{
           		if (qc(sndg[i][wsIndex])) {
                		wsp = sndg[i][wsIndex];
                		x1 = tlx;
                		x2 = tlx + (short)(wsp / 1.5);
                		y2 = pres_to_pix(sndg[i][pIndex]);
                		y1 = y2;
                		setcolor(2);
                		moveto(x1, y1);
                		lineto(x2, y2);
				lasthght = agl(sndg[i][zIndex]);
           			}
		  	}

        	if ((lasthght >= 3000) && (lasthght < 6000))                                              
                	{
                	if (qc(sndg[i][wsIndex])) {
                        	wsp = sndg[i][wsIndex];
                        	x1 = tlx;
                        	x2 = tlx + (short)(wsp / 1.5);
                        	y2 = pres_to_pix(sndg[i][pIndex]);
                        	y1 = y2;
                        	setcolor(3);
                        	moveto(x1, y1);
                        	lineto(x2, y2);
                        	lasthght = agl(sndg[i][zIndex]);
				}
		  	}
        	if ((lasthght >= 6000) && (lasthght < 9000))
                	{
                	if (qc(sndg[i][wsIndex])) {
                        	wsp = sndg[i][wsIndex];
                        	x1 = tlx;
                        	x2 = tlx + (short)(wsp / 1.5);
                        	y2 = pres_to_pix(sndg[i][pIndex]);
                        	y1 = y2;
                        	setcolor(5);
                        	moveto(x1, y1);
                        	lineto(x2, y2);
                        	lasthght = agl(sndg[i][zIndex]);
				}
		  	}
        	if ((lasthght >= 9000) && (lasthght < 12000))
                	{
                	if (qc(sndg[i][wsIndex])) {
                        	wsp = sndg[i][wsIndex];
                        	x1 = tlx;
                        	x2 = tlx + (short)(wsp / 1.5);
                        	y2 = pres_to_pix(sndg[i][pIndex]);
                        	y1 = y2;
                        	setcolor(6);
                        	moveto(x1, y1);
                        	lineto(x2, y2);
                        	lasthght = agl(sndg[i][zIndex]);
				}
		  	}
        	if (lasthght >= 12000)
                	{
                	if (qc(sndg[i][wsIndex])) {
                        	wsp = sndg[i][wsIndex];
                        	x1 = tlx;
                        	x2 = tlx + (short)(wsp / 1.5);
                        	y2 = pres_to_pix(sndg[i][pIndex]);
                        	y1 = y2;
                        	setcolor(29);
                        	moveto(x1, y1);
                        	lineto(x2, y2);
                        	lasthght = agl(sndg[i][zIndex]);
				}
                  	}

	}
        setcliprgn(1, 1, xwdth, xhght);
        copytodisplay();
}

	/* PREVIOUS RLT VERSION OF COLOR CODED WIND SPEED WITH HEIGHT */
        /* ----- Plot vertical profile of wind speed ----- */
/*      setlinestyle(1, 1);
        for (i=0; i<numlvl; i++) {
           if (qc(sndg[i][wsIndex])) {
		wsp = sndg[i][wsIndex];
		x1 = tlx;
		x2 = tlx + (short)(wsp / 1.5);
              	y2 = pres_to_pix(sndg[i][pIndex]);
		y1 = y2;
		setcolor(31);
		setlinestyle(1, 1);
        	if (wsp >= 20) setcolor(18);
        	if (wsp >= 40) { setcolor(19); setlinestyle(1, 1); }
        	if (wsp >= 60) { setcolor(2); setlinestyle(1, 1); }
        	if (wsp >= 80) { setcolor(7); setlinestyle(1, 1); }
        	if (wsp >= 100) { setcolor(27); setlinestyle(1, 1); }
             	moveto(x1, y1);
              	lineto(x2, y2);
           }
        }

	setcliprgn(1, 1, xwdth, xhght);
	copytodisplay();
}
*/

        /*NP*/
void plot_advectionprofile(void)
        /*************************************************************/
        /*  PLOT_ADVECTIONPROFILE                                    */
        /*  John Hart  SPC Norman                                    */
        /*                                                           */
        /*  Plots vertical profile of Wind Speeds                    */
        /*************************************************************/
{
        float bothgt, tophgt, h, wsp, advt, ix1, z;
        short x1, y1, x2, y2, x3, i, tlx, tly, wid, hgt;
        short pIndex, zIndex, wsIndex, wdIndex; 
        char st[10];

        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");
        wsIndex = getParmIndex("SPED");

        if (!sndg || pIndex == -1 || zIndex == -1 || wsIndex == -1) return;

        tlx = skv.brx + 93;
        tly = skv.tly;
        wid = 67;
        hgt = skv.bry - skv.tly;

        setcliprgn(tlx, tly, tlx+wid, tly+hgt+15);
        setcolor(0);
        setlinestyle( 1, 1 );
        rectangle(1,tlx, tly, tlx+wid, tly+hgt);
        setcolor(1);
        rectangle(0, tlx, tly, tlx+wid, tly+hgt);

	/* Draw centerline */
        setlinestyle( 2, 1 );
	moveto( tlx + (wid/2), tly); lineto(tlx + (wid/2), tly + hgt);

	set_font(4);
        setlinestyle( 1, 1 );
	for (h=sndg[sfc()][pIndex]; h>=200; h-=100) {
		advt = advection_layer(&ix1, h, h - 100);

		/* Draw tick marks 
                x1 = tlx + (wid/2) + 5;
                x2 = tlx + (wid/2) - 5;
                y2 = pres_to_pix(h);
                y1 = pres_to_pix(h - 100);
		setcolor(1);
		moveto( x1, y1); lineto(x2, y1);
		moveto( x1, y2); lineto(x2, y2);
		*/
                x1 = tlx + (wid/2);
                x2 = tlx + (wid/2) + (advt*2.5);
                y2 = pres_to_pix(h);
                y1 = pres_to_pix(h - 100);
		setcolor(1);
		moveto( x1, y1); lineto(x2, y1);
		moveto( x1, y2); lineto(x2, y2);
		setcolor(26);
        	if (advt > 0) setcolor(13);
		sprintf( st, "%.1f", advt);
		if(advt > 0) rectangle(0, x1, y1, x2, y2); else rectangle(0, x2, y1, x1, y2);
		if (advt > 0) {
			x3 = x2 + 3;
			if (advt > 2) x3 = x1 + 3;
			}
		else {
			x3 = x2 - getgtextextent(st); 
			if (advt < -2) x3 = x1 - getgtextextent(st); 
			}
		if (advt > -999) outgtext( st, x3, ((y1+y2)/2)-4);
	}

        /* ----- Plot Label ----- */
        setcolor(1);
        set_font(5);
        outgtext("Inferred", tlx+5, tly+3);
        outgtext("Temp Advection",  tlx+5, tly+13);
        outgtext("(C / hr)",  tlx+5, tly+23);

	setcliprgn(1, 1, xwdth, xhght);
	copytodisplay();
}

        /* NP */
short ww_type(short *wwtype, short *dcp)
        /********************************************************************/
        /*      Watch type guidance                                         */
        /*      A decision tree to help with ww issuance                    */
        /*                                                                  */
        /*      Rich Thompson SPC OUN                                       */
        /********************************************************************/
        {
        float ix1, ix2, ix3, ix4, lr75, shr6, t500, fzlh, mumixr, lowrh, midrh, low_mid_rh;
        float mucn, mlcn, mlcp, sbcp, mucp, lr1, lr3, shr6_dir, sr46_dir, sr46_spd, shr6_sr46_diff, mmp;
        float sig_tor, sig_tor_winter, sighail, wind_dmg, rm_scp, cbsig, dncp, srh1, sblcl, mllcl;
        float oldlplpres, pres, pbot, ptop, shr8, bot, top, esrh, lm_scp;
        short oldlplchoice, ww_choice;
        short pIndex, tIndex, zIndex, tdIndex;
        short x1, y1, x2, y2, tlx, tly, wid;
        struct _parcel pcl;
        char st[40];

        tlx = hov.tlx + 409;
        tly = hov.bry;
        wid = 119;

        setcliprgn( tlx+2, tly+2, tlx+wid+24, tly+wid+1);
        setcolor(0);
        setlinestyle( 1, 1 );
        rectangle( 1,tlx, tly, tlx+wid+27, tly+wid+1);
        setcolor(1);
        rectangle( 0, tlx, tly, tlx+wid+27, tly+wid+1);
        setlinestyle( 1, 1 );
        moveto( tlx + 2, tly + 18);
	lineto(tlx + wid + 27, tly + 18);


        /* ----- Plot Label ----- */
        set_font(6);
        setcolor(1);
        outgtext( "Psbl Watch Type", tlx + 20, tly + 3 );

	*wwtype = 0;
	*dcp = 0;

        oldlplchoice = lplvals.flag;

        tIndex = getParmIndex("TEMP");
        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");
        tdIndex = getParmIndex("DWPT");

/* 24 Mar 2008 */
/*        effective_inflow_layer(100, -250, &pbot, &ptop);*/

	/* sb parcel */
        define_parcel(1,0);
        ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        sbcp = pcl.bplus;
        sblcl = agl(i_hght(pcl.lclpres, I_PRES));
        sig_tor_winter = sigtorn_fixed(st_dir, st_spd);

	/* ml parcel */
        define_parcel(4,100);
        ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mlcn = pcl.bminus;
        mlcp = pcl.bplus;
        mllcl = agl(i_hght(pcl.lclpres, I_PRES));
        sig_tor = sigtorn_cin(st_dir, st_spd);

	/* mu parcel */
        define_parcel(3,400);
        mucp = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mucn = pcl.bminus;

	dncp = dcape(&ix1, &ix2);

	/* sighail ingredients */
        lr75 = lapse_rate(&ix1, 700, 500);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        shr6 = ix4;
        shr6_dir = ix3;
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(8000)), &ix1, &ix2, &ix3, &ix4);
	shr8 = ix4;
        mumixr = mixratio(lplvals.pres, lplvals.dwpt);
        t500 =  i_temp(500, I_PRES);
        fzlh = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));
        sighail = sig_hail(pcl.bplus, mumixr, lr75, t500, kt_to_mps(shr6), fzlh, pcl.bminus, 0, 0, 25, mlcp);

        rm_scp = scp(st_dir, st_spd);
        wind_dmg = damaging_wind();

        sr_wind( i_pres(msl(4000)), i_pres(msl(6000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
        sr46_dir = ix3;
        sr46_spd = ix4;
        shr6_sr46_diff = (shr6_dir - sr46_dir);
	srh1 = helicity(0, 1000, st_dir, st_spd, &ix1, &ix2);
	bot = agl(i_hght(p_bot, I_PRES));
	top = agl(i_hght(p_top, I_PRES));	
	esrh = helicity(bot, top, st_dir, st_spd, &ix1, &ix2);

        lapse_rate(&ix2, sndg[sfc()][pIndex], i_pres(sndg[sfc()][zIndex]+1000));
        lr1 = ix2;
        lapse_rate(&ix2, sndg[sfc()][pIndex], i_pres(sndg[sfc()][zIndex]+3000));
        lr3 = ix2;

        mean_relhum(&ix1, sndg[sfc()][pIndex]-150, sndg[sfc()][pIndex]-350);
        midrh = ix1;
        mean_relhum(&ix1, sndg[sfc()][pIndex], sndg[sfc()][pIndex]-150);
        lowrh = ix1;
        low_mid_rh = ((lowrh + midrh)/2);
        mmp = coniglio1();
        cbsig = (mlcp * kt_to_mps(shr6));

/* 24 Mar 2008 */
/* all "p_bot" below were changed from "pbot" */

/* Decision tree below is identical to the operational "ww_type" flow chart documentation 9/23/09 RLT */
        if ((sig_tor >= 3.0) && (sig_tor_winter >= 4.0) && (srh1 >=200) && (esrh >= 200) && (sr46_spd >= 15.0) && (shr8 >= 45.0) && (sblcl < 1000) && (mllcl < 1200) && (lr1 >= 5.0) && (mlcn > -50.0) && (bot == 0.0)) {
		*dcp = 1;
		*wwtype = 5;
                ww_choice = 5;
        	/*printf("\n dcp (in PDS) = %d", *dcp);*/
        	set_font(6);
        	setcolor(7);
        	outgtext( "PDS TOR", tlx + 45, tly + 60 );
                }

/*        else
        if ((sig_tor_winter >= 4.0) && (sr46_spd >= 15.0) && (shr8 >= 40.0) && (sblcl < 1000) && (lr1 >= 5.0) && (bot == 0.0)) {
                *dcp = 2;
                *wwtype = 5;
                ww_choice = 5;
                set_font(6);
                setcolor(7);
                outgtext( "PDS TOR", tlx + 45, tly + 60 );
                }
*/	
        else
        if (((sig_tor >= 3.0) || (sig_tor_winter >= 4.0)) && (mlcn > -125.0) && (bot == 0.0)) {
                *dcp = 3;
                *wwtype = 4;
                ww_choice = 4;
                /*printf("\n dcp (in TOR) = %d", *dcp);*/
                set_font(6);
                setcolor(2);
                outgtext( "TOR", tlx + 45, tly + 60 );
                }
	
        else
        if (((sig_tor >= 1.0) || (sig_tor_winter >= 1.0)) && ((sr46_spd >= 15.0) || (shr8 >= 40.0)) && (mlcn > -75.0) && (bot == 0.0)) {
                *dcp = 4;
                *wwtype = 4;
                ww_choice = 4;
                /*printf("\n dcp (in TOR) = %d", *dcp);*/
                set_font(6);
                setcolor(2);
                outgtext( "TOR", tlx + 45, tly + 60 );
                }

        else
        if (((sig_tor >= 1.0) || (sig_tor_winter >= 1.0)) && (low_mid_rh >= 60) && (lr1 >= 5.0) && (mlcn > -50.0) && (bot == 0.0)) {
                *dcp = 5;
                *wwtype = 4;
                ww_choice = 4;
                /*printf("\n dcp (in TOR) = %d", *dcp);*/
                set_font(6);
                setcolor(2);
                outgtext( "TOR", tlx + 45, tly + 60 );
                }

        else
        if ((( sig_tor >= 1.0) || (sig_tor_winter >= 1.0)) && (mlcn > -150.0) && (bot == 0.0)) {
                *dcp = 6;
                *wwtype = 3;	
                ww_choice = 3; 
                /*printf("\n dcp (in mrgl TOR) = %d", *dcp);*/
	        set_font(6);
                setcolor(2);
                outgtext( "mrgl TOR", tlx + 40, tly + 60 );
                }

        else
        if (((( sig_tor >= 0.5) && (esrh >= 150)) || ((sig_tor_winter >= 0.5) && (srh1 >= 150))) && (mlcn > -50.0) && (bot == 0.0)) {
                *dcp = 7;
                *wwtype = 3;	
                ww_choice = 3;
                /*printf("\n dcp (in mrgl TOR) = %d", *dcp);*/
                set_font(6);
                setcolor(2);
                outgtext( "mrgl TOR", tlx + 40, tly + 60 );
                }

        else
        if ((( sig_tor_winter >= 1.0) || (rm_scp >= 4.0) || (sig_tor >= 1.0)) && (mucn >= -50.0)) {
                *dcp = 8;
                *wwtype = 2;
                ww_choice = 2;
                /*printf("\n dcp (in SVR) = %d", *dcp);*/
	        set_font(6);
                setcolor(6);
                outgtext( "SVR",  tlx + 60, tly + 60 );
                }

        else
        if ((rm_scp >= 2.0) && ((sighail >= 1.0) || (dncp >= 750)) && (mucn >= -50.0)) {
                *dcp = 9;
                *wwtype = 2;
                ww_choice = 2;
                /*printf("\n dcp (in SVR) = %d", *dcp);*/
                set_font(6);
                setcolor(6);
                outgtext( "SVR",  tlx + 60, tly + 60 );
                }

        else
        if ((cbsig >= 30000) && (mmp >= 0.6) && (mucn >= -50.0)) {
                *dcp = 10;
                *wwtype = 2;
                ww_choice = 2;
                /*printf("\n dcp (in SVR) = %d", *dcp);*/
                set_font(6);
                setcolor(6);
                outgtext( "SVR",  tlx + 60, tly + 60 );
                }
        
	else
        if ((mucn >= -75.0) && ((wind_dmg >= 0.5) || (sighail >= 0.5) || (rm_scp >= 0.5)))  {
                *dcp = 11;
                *wwtype = 1;
                ww_choice = 1;
                /*printf("\n dcp (in mrgl SVR) = %d", *dcp);*/
	        set_font(6);
                setcolor(26);
                outgtext( "MRGL SVR",  tlx + 40, tly + 60 );
                }
        else    {
                *dcp = 0;
                /*printf("\n dcp (after logic checks) = %d", *dcp);*/
                *wwtype = 0;
                ww_choice = 0;
		}

        if (*wwtype == 0) {
                set_font(6);
                setcolor(19);
                outgtext( "NONE",  tlx + 50, tly + 60 );
                }

/*      define_parcel(oldlplchoice, oldlplpres); */
        
        /* set parcel back to user selection */
        if (oldlplchoice == 1)
          pres = 0;
        else if (oldlplchoice == 2)
          pres = 0;
        else if (oldlplchoice == 3)
          pres = mu_layer;
        else if (oldlplchoice == 4)
          pres = mml_layer;
        else if (oldlplchoice == 5)
          pres = user_level;
        else if (oldlplchoice == 6)
          pres = mu_layer;
        define_parcel(oldlplchoice, pres);

        return ww_choice;

        }


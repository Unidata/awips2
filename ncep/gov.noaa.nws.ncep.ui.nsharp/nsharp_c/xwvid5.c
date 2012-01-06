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
#define VIDEO
#include "gui.h"
#include "sharp95.h"

/*
 *	Private functions
 */
void plot_storminflow ( void );
void plot_thetae ( void );
void plot_vertsrw ( void );
void plot_vis ( void );
short vert_coords ( float hgt, float maxhgt );
void vis_xy ( float x, float y );
void visual1 ( float lower, float upper, float pres, float temp, float dwpt );
void write_vis_data ( float tim, float ang );


short sk_mode = 0;
short ho_mode = 0;

/*===============================================================================*/

void inset_options ( short mode )
	/*************************************************************/
	/*  INSET_OPTIONS                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Allows the user to select which inset graphic that       */
	/*  will be displayed.                                       */
	/*************************************************************/
	{
	if(mode == 1)
	   {
	   /* ----- This is a SkewT ----- */
	   sk_mode++;
	   if( sk_mode > 1 ) {sk_mode = 0; }
	   /*redraw_graph(1);*/
	   }

	else
	   {
	   /* ----- This is a Hodograph ----- */
	   ho_mode++;
	   if( ho_mode > 3 ) {ho_mode = 0; }
	   /*redraw_graph(2);*/
	   }
	}

/*=========================================================================*/

void visual1 ( float lower, float upper, float pres, float temp, float dwpt )
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
	/*  dwpt        =  LPL dew point (c)                         *
	**								*
	* Log:								*
	* T. Piper/SAIC	02/04	Removed unused variable disp		*
	*************************************************************/
	{
	short i, lptr, uptr;
	float te1, pe1, te2, pe2, h1, h2, lyre, tdef1, tdef2, totp, totn;
	float te3, pe3, h3, tp1, tp2, tp3, tdef3, lyrf;
	float tote, dh, restim, uvv, ix1, ix2, tottim;
	float u, v, du, dv, tsu, tsv, tdist, angl;
/*	float tangle; */

	lyre = -1.0F;
	totp = 25.0F;
	totn = 0.0F;
        tote = 0.0F;

	/* ----- See if default layer is specified ----- */
	if( lower == -1.0F) { lower = sndgp->sndg[sfc()].pres; }
	if( upper == -1.0F) { upper = sndgp->sndg[sndgp->numlev-1].pres; }

	/* ----- Make sure this is a valid layer ----- */
	if( lower > pres ) { lower = pres; }
	if( !qc( i_vtmp( upper ))) { return; }
	if( !qc( i_vtmp( lower ))) { return; }

	/* ----- Begin with Mixing Layer (LPL-LCL) ----- */
	te1 = i_vtmp( pres );
	pe1 = lower;
	h1 =  i_hght( pe1 );
	tp1 = virtemp( pres, temp, dwpt);

	drylift(pres, temp, dwpt, &pe2, &tp2);
	h2 =  i_hght( pe2 );
	te2 = i_vtmp( pe2 );

	if( lower > pe2 ) { lower = pe2; }

	/* ----- Find lowest observation in layer ----- */
	i = 0;
	while( sndgp->sndg[i].pres > lower)  { i++; }
	while ( !qc(sndgp->sndg[i].dwpt) ) { i++; }
	lptr = i;
	if( sndgp->sndg[i].pres == lower ) { lptr++; }

	/* ----- Find highest observation in layer ----- */
	i=(short)(sndgp->numlev-1);
	while(sndgp->sndg[i].pres < upper) { i--; }
	uptr = i;
	if( sndgp->sndg[i].pres == upper ) { uptr--; }

	/* ----- Start with interpolated bottom layer ----- */
	pe1 = lower;
	h1 =  i_hght( pe1 );
	te1 = i_vtmp( pe1 );
	tp1 = wetlift(pe2, tp2, pe1);

	totp = 25.0F;
	totn = 0.0F;
	tsu = 0.0F;
	tsv = 0.0F;
	restim = 0.0F;
	tottim = 0.0F;
	for( i = lptr; i < sndgp->numlev; i++)
	   {
	   if( qc(sndgp->sndg[i].temp) )
	      {
	      /* ----- Calculate every level that reports a temp ----- */
	      pe2 = sndgp->sndg[i].pres;
	      h2 =  sndgp->sndg[i].hght;
	      te2 = i_vtmp( pe2 );
	      tp2 = wetlift(pe1, tp1, pe2);
	      tdef1 = (virtemp(pe1, tp1, tp1) - te1) / (te1 + 273.15F);
	      tdef2 = (virtemp(pe2, tp2, tp2) - te2) / (te2 + 273.15F);
	      lyre = 9.8F * (tdef1 + tdef2) / 2.0F * (h2 - h1);

	      if( lyre > 0.0F ) { totp += lyre; }
	      else { if(pe2 > 500.0F) { totn += lyre; } }
	      tote += lyre;

	      uvv = (float)sqrt( (double)(totp * 2.0F) );
	      dh = h2 - h1;
	      restim = dh / uvv;
	      tottim += restim;

	      sr_wind( pe1, pe2, sndgp->st_dir, sndgp->st_spd, &u, &v, &ix1, &ix2);
	      du = kt_to_mps(u) * restim;
	      dv = kt_to_mps(v) * restim;
	      tsu -= du;
	      tsv += dv;
	      tdist = (float)sqrt((double)(tsu*tsu) + (double)(tsv*tsv));
/*	      tangle = angle(tsu, tsv);  NOT used */

	      pe1 = pe2;
	      h1 = h2;
	      te1 = te2;
	      tp1 = tp2;

	      /* ----- Is this the top of given layer ----- */
	      if(i >= uptr)
		 {
		 pe3 = pe1;
		 h3 = h1;
		 te3 = te1;
		 tp3 = tp1;
		 lyrf = lyre;

		 if( lyrf > 0.0F )
		    { totp -= lyrf; }
		 else
		    { if(pe2 > 500.0F) { totn -= lyrf; } }

		 pe2 = upper;
		 h2 = i_hght( pe2 );
		 te2 = i_vtmp( pe2 );
		 tp2 = wetlift(pe3, tp3, pe2);
		 tdef3 = (virtemp(pe3, tp3, tp3) - te3) / (te3 + 273.15F);
		 tdef2 = (virtemp(pe2, tp2, tp2) - te2) / (te2 + 273.15F);
		 lyrf = 9.8F * (tdef3 + tdef2) / 2.0F * (h2 - h3);
		 if( lyrf > 0.0F )
		    { totp += lyrf; }
		 else
		    { if(pe2 > 500.0F) { totn -= lyrf; } }

		 if( totp == 0.0F ) { totn = 0.0F; }

		 uvv = (float)sqrt( (double)(totp * 2.0F) );
		 dh = h2 - h1;
		 restim = dh / uvv;
		 tottim += restim;

		 sr_wind( pe1, pe2, sndgp->st_dir, sndgp->st_spd, &u, &v, &ix1, &ix2);
		 du = kt_to_mps(u) * restim;
		 dv = kt_to_mps(v) * restim;
		 tsu -= du;
		 tsv += dv;
		 tdist = (float)sqrt((double)(tsu*tsu) + (double)(tsv*tsv));
/*		 tangle = angle(tsu, tsv);  NOT used */

		 vis_xy( tsu, tsv);
		 /* printf( "%8.2f %8.2f %8.2f    %8.2f %8.2f\n", pe2, uvv, tottim, tangle, tdist); */

		 angl = 90.0F - angle( tdist, agl(h2));

		 write_vis_data( tottim, angl );
		 return;
		 }

	      vis_xy( tsu, tsv);
	      /* printf( "%8.2f %8.2f %8.2f    %8.2f %8.2f\n", pe2, uvv, tottim, tangle, tdist); */

	      if( sndgp->sndg[i].pres == 500.0F )
		 {
		 angl = 90.0F - angle( tdist, agl(sndgp->sndg[i].hght));
		 }
	      }
	   }
	}

/*=============================================================================*/

void plot_vis ( void )
	/*************************************************************/
	/*  PLOT_VIS                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots the results of storm-relative parcel trajectory.   */
	/*************************************************************/
	{
	float sfctemp, sfcdwpt, sfcpres;
	short x1, y1, x2, y2;
	struct _parcel pcl;

        setcliprgn( (short)(hov.brx-150), hov.tly, hov.brx, (short)(hov.tly+150), draw_reg, gc);
	setcolor(0, draw_reg, gc);
	setlinestyle( 1, 1 );
	rectangle( 1, (short)(hov.brx-150), hov.tly, hov.brx, (short)(hov.tly+150));
	setcolor(1, draw_reg, gc);
	rectangle( 0, (short)(hov.brx-150), hov.tly, hov.brx, (short)(hov.tly+150));

       /* ----- Plot Crosshairs ----- */
	setcolor(24, draw_reg, gc);
	moveto((short)(hov.brx-150), (short)(hov.tly+75)); 
	lineto(hov.brx, (short)(hov.tly+75));
	moveto((short)(hov.brx-75), hov.tly); 
	lineto((short)(hov.brx-75), (short)(hov.tly+150));

	/* ----- Plot Label ----- */
	setcolor(1, draw_reg, gc);
	outgtext( "Storm", hov.brx-145, hov.tly+125 );
	outgtext( "Visualization",  hov.brx-145, hov.tly+137 );


	/* ----- Calculate Parcel Data ----- */
	sfctemp = sndgp->lplvals.temp;
	sfcdwpt = sndgp->lplvals.dwpt;
	sfcpres = sndgp->lplvals.pres;
	parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Plot storm motion ----- */
	x1 = (short)(hov.brx - 75);
	y1 = (short)(hov.tly + 75);
	x2 = (short)((hov.brx - 75) - ucomp( sndgp->st_dir, 30.0F));
	y2 = (short)((hov.tly + 75) + vcomp( sndgp->st_dir, 30.0F));
	setcolor(31, draw_reg, gc);
	moveto( x1, y1);
	lineto( x2, y2);

	/* ----- Run Visualization Routine ----- */
	visual1( pcl.lfcpres, pcl.elpres, sfcpres, sfctemp, sfcdwpt);
	}

	/*NP*/
	void vis_xy ( float x, float y )
	/*************************************************************/
	/*  VIS_XY                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots the point (x,y) on the storm visualization chart.  */
	/*************************************************************/
	{
	float xfactor, xs, ys;

	xfactor=.01F;

	xs = (float)(hov.brx - 75) + (x * xfactor);
	ys = (float)(hov.tly + 75) + (y * xfactor);

/*	_moveto( (short)xs, (short)ys);*/
	setcolor(3, draw_reg, gc);
	setlinestyle (1, 1);
	ellipse( 0, (short)(xs-5.0F), (short)(ys-5.0F), (short)(xs+5.0F), (short)(ys+5.0F));
	}


	/*NP*/
	void write_vis_data ( float tim, float ang )
	/*************************************************************/
	/*  WRITE_VIS_DATA                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes the numeric data on the storm visualization chart */
	/*************************************************************/
	{
	short tr, tl;
	char st[10];

	setcolor(31, draw_reg, gc);

	tr = (short)(hov.brx - 105);
	tl = (short)(hov.tly + 5);
	strcpy( st, qc2( tim, " s", 0 ));
	disp_param( st, tr, tl);

	tr = (short)(hov.brx - 5);
	strcpy( st, qc2( ang, " deg", 0 ));
	disp_param( st, tr, tl);
	}

	/*NP*/
	void draw_hoinset ( void )
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

	XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg), 
		    gc, hov.brx-150, (int)hov.tly, 150, 150, 
		    hov.brx-150, (int)hov.tly );
	setcliprgn( 1, 1, xwdth, xhght, draw_reg, gc );
	}

	/*NP*/
	void draw_skinset ( void )
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

/*====================================================================*/

void plot_storminflow ( void )
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

	setcliprgn( (short)(hov.brx-150), hov.tly, hov.brx, (short)(hov.tly+150), draw_reg, gc);
	setcolor(0, draw_reg, gc);
	setlinestyle( 1, 1 );
	rectangle( 1, (short)(hov.brx-150), hov.tly, hov.brx, (short)(hov.tly+150));
	setcolor(1, draw_reg, gc);
	rectangle( 0, (short)(hov.brx-150), hov.tly, hov.brx, (short)(hov.tly+150));
	setcolor(1, draw_reg, gc);
	moveto((short)(hov.brx-150), (short)(hov.tly + 75)); 
	lineto(hov.brx, (short)(hov.tly+75));
	moveto((short)(hov.brx-75), hov.tly); 
	lineto((short)(hov.brx-75), (short)(hov.tly+150));

	sfctemp = sndgp->lplvals.temp;
	sfcdwpt = sndgp->lplvals.dwpt;
	sfcpres = sndgp->lplvals.pres;

    /* ----- Plot speed rings ----- */
	x1 = (short)(hov.brx - 75);
	y1 = (short)(hov.tly + 75);
	setlinestyle( 2, 1 );
	setcolor(24, draw_reg, gc);
	for(i=20; i<=60; i+=20) { ellipse( 0, x1-i, y1-i, x1+i, y1+i); }
	rectangle( 0, (short)(hov.brx-150), hov.tly, hov.brx, (short)(hov.tly+150));

	/* ----- Plot Label ----- */
	setcolor(1, draw_reg, gc);
	outgtext( "SR Wind", (float)(hov.brx-135), (float)(hov.tly + 3 ));
	outgtext( "Vectors", (float)(hov.brx-135), (float)(hov.tly + 15 ));

	/* ----- Calculate Parcel Data ----- */
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Plot 0-2km Inflow Vector ----- */
	sr_wind( -1, i_pres(msl(2000)), sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3))
	   {
	   x1 = hov.brx - 75;
	   y1 = hov.tly + 75;
	   x2 = (hov.brx - 75) + (short)ix1;
	   y2 = (hov.tly + 75) - (short)ix2;
	   x3 = x2 + 1;
	   y3 = y2 + 1;

	   setlinestyle( 1, 2 );
	   setcolor(2, draw_reg, gc);
	   moveto( x1, y1); lineto( x2, y2);
	   outgtext( "L", x3, y3 );
	   }

	/* ----- Plot 4-6km Inflow Vector ----- */
	sr_wind( i_pres(msl(4000)), i_pres(msl(6000)), sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3))
	   {
	   x1 = hov.brx - (short)75;
	   y1 = hov.tly + (short)75;
	   x2 = (short)(hov.brx - 75) + (short)ix1;
	   y2 = (short)(hov.tly + 75) - (short)ix2;
	   x3 = x2 + (short)1;
	   y3 = y2 + (short)1;

	   setcolor(6, draw_reg, gc);
	   setlinestyle( 1, 2 );
	   moveto( x1, y1); lineto( x2, y2);
	   outgtext( "M", (int)x3, (int)y3 );
	   }

	/* ----- Plot 9-11km Inflow Vector ----- */
	sr_wind( i_pres(msl(9000.0F)), i_pres(msl(11000.0F)), sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3))
	   {
	   x1 = hov.brx - (short)75;
	   y1 = hov.tly + (short)75;
	   x2 = (short)(hov.brx - 75) + (short)ix1;
	   y2 = (short)(hov.tly + 75) - (short)ix2;
	   x3 = x2 + (short)1;
	   y3 = y2 + (short)1;

	   setcolor(7, draw_reg, gc);
	   setlinestyle( 1, 2 );
	   moveto( x1, y1); lineto( x2, y2);
	   outgtext( "H", (int)x3, (int)y3 );
	   }
	}

	/*NP*/
	void plot_vertsrw ( void )
	/*************************************************************/
	/*  PLOT_VERTSRW                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots vertical profile of sr-winds (0-9km AGL)           */
	/*************************************************************/
	{
	float bothgt, tophgt, h, ix1, ix2, ix3, ix4, h1, h2;
	short x1, y1, x2, y2;
	char st[40];

	setcliprgn( hov.brx-150, hov.tly, hov.brx, hov.tly+150, draw_reg, gc);
	setcolor(0, draw_reg, gc);
	setlinestyle( 1, 1 );
	rectangle( 1, hov.brx-150, hov.tly, hov.brx, hov.tly+150);
	setcolor(1, draw_reg, gc);
	rectangle( 0, hov.brx-150, hov.tly, hov.brx, hov.tly+150);

	/* ----- Set Layer (AGL) ----- */
	bothgt = 0;
	tophgt = 16000;

	/* ----- Plot Label ----- */
	setcolor(1, draw_reg, gc);
	outgtext( "SR Winds vs", hov.brx-110, hov.tly + 3 );
	outgtext( "Height", hov.brx-110, hov.tly + 15 );

	/* ----- Plot height legend ----- */
	setcolor(1, draw_reg, gc);
	for(h=bothgt; h<=tophgt; h += 2000)
	   {
	   x1 = hov.brx - 150;
	   y1 = vert_coords(h, tophgt);
	   moveto( x1, y1);
	   lineto(x1+5, y1);

	   if(h>0 && h<16000)
	      {
	      sprintf( st, "%d", (int)(h/1000));
	      outgtext( st, x1+5, y1-4 );
	      }
	   }

	/* ----- Plot horizontal legend ----- */
	setcolor(1, draw_reg, gc);
	for(h=0; h<=50; h += 10)
	   {
	   x1 = hov.brx - 150 + (short)(h*3);
	   y1 = hov.tly+150;
	   moveto( x1, y1);
	   lineto( x1, y1-5);
	   }

       /* ----- Plot vertical dashed line at 20kt ----- */
       setlinestyle(2, 1);
       moveto( hov.brx - 90, hov.tly);
       lineto( hov.brx - 90, hov.tly + 150);


	/* ----- Plot vertical srw profile ----- */
        setlinestyle(1, 2);
	setcolor(2, draw_reg, gc);
	x2 = 999;
	for(h=bothgt; h<=tophgt; h += 250)
	   {
	   sr_wind( i_pres(msl(h)), i_pres(msl(h)), 
			sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
	   x1 = hov.brx - 150 + (short)(ix4*3);
	   y1 = vert_coords(h, tophgt);
	   if(x2 == 999) { x2=x1; y2=y1; }

	   moveto( x1, y1);
	   lineto( x2, y2);

	   x2=x1;
	   y2=y1;
	   }

	/* ----- Plot Mean-Layer SRW value (Sfc-2km) ----- */
	h1 = 0.0F;
	h2 = 2000.0F;
	sr_wind( i_pres(msl(h1)), i_pres(msl(h2)),
			sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix4))
	   {
	   x1 = hov.brx - 150 + (short)(ix4*3);
	   y1 = vert_coords(h1, tophgt);
	   y2 = vert_coords(h2, tophgt);
	   setcolor(15, draw_reg, gc);
	   moveto( x1, y1);
	   lineto( x1, y2);
	   }

	/* ----- Plot Mean-Layer SRW value (4-6km) ----- */
	h1 = 4000.0F;
	h2 = 6000.0F;
	sr_wind( i_pres(msl(h1)), i_pres(msl(h2)),
			sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix4))
	   {
	   x1 = hov.brx - 150 + (short)(ix4*3);
	   y1 = vert_coords(h1, tophgt);
	   y2 = vert_coords(h2, tophgt);
	   setcolor(25, draw_reg, gc);
	   moveto( x1, y1);
	   lineto( x1, y2);
	   }

	/* ----- Plot Mean-Layer SRW value (9-11km) ----- */
	h1 = 9000.0F;
	h2 = 11000.0F;
	sr_wind( i_pres(msl(h1)), i_pres(msl(h2)),
			sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix4))
	   {
	   x1 = hov.brx - 150 + (short)(ix4*3);
	   y1 = vert_coords(h1, tophgt);
	   y2 = vert_coords(h2, tophgt);
	   setcolor(7, draw_reg, gc);
	   moveto( x1, y1);
	   lineto( x1, y2);
	   }

	/* ----- Plot Vertical RH profile ----- */
	/*
	setcolor(3, draw_reg, gc);
	setlinestyle(2, 1);
	x2=999;
	for(h=bothgt; h<=tophgt; h += 250)
	   {
	   mean_relhum( &ix1, i_pres(msl(h)), i_pres(msl(h))); 
	   x1 = hov.brx - 150 + (short)(ix1/2*3);
	   y1 = vert_coords(h, tophgt);
	   if(x2 == 999) { x2=x1; y2=y1; }

	   moveto( x1, y1);
	   lineto( x2, y2);

	   x2=x1;
	   y2=y1;
	   }
	*/
	}

	/*NP*/
	short vert_coords ( float hgt, float maxhgt )
	/*************************************************************/
	/*  VERT_COORDS                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Determines the y-pixel value for given height (m).       */
	/*************************************************************/
	{
	float xfactor = 1/(maxhgt/150);
	return (short)((hov.tly + 150) - (hgt * xfactor));
	}

	/*NP*/
	void plot_thetae ( void )
	/*************************************************************/
	/*  PLOT_THETAE                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots vertical profile of Theta-E (sfc-500mb)            */
	/*************************************************************/
	{
	float tophgt, h, cthe;
        float ct1, ct2, ct3, nct;
	short x1, y1, x2, y2, i;

	setcliprgn( (short)(hov.brx-150), hov.tly, hov.brx, (short)(hov.tly+150), draw_reg, gc);
	setcolor(0, draw_reg, gc);
	setlinestyle( 1, 1 );
	rectangle( 1, (short)(hov.brx-150), hov.tly, hov.brx, (short)(hov.tly+150));
	setcolor(1, draw_reg, gc);
	rectangle( 0, (short)(hov.brx-150), hov.tly, hov.brx, (short)(hov.tly+150));

	/* ----- Set Layer (AGL) ----- */
	tophgt = agl(i_hght(500.0F));
        if(! qc(tophgt)) tophgt = 5500.0F;

	/* ----- Plot Label ----- */
	setcolor(1, draw_reg, gc);
	outgtext( "Theta-E vs", hov.brx-105, hov.tly + 3 );
	outgtext( "Height",  hov.brx-105, hov.tly + 15 );

	/* ----- Plot horizontal legend ----- */
        ct1 = thetae( 850.0F, i_temp(850.0F), i_dwpt(850.0F));
        ct2 = thetae( 700.0F, i_temp(700.0F), i_dwpt(700.0F));
        ct3 = thetae( sndgp->sndg[sfc()].pres, sndgp->sndg[sfc()].temp, sndgp->sndg[sfc()].dwpt );
        cthe = 0.0F; nct = 0.0F;
        if(qc(ct1))
           {
           cthe = cthe + ct1;
           nct++;
           }
        if(qc(ct2))
           {
           cthe = cthe + ct2;
           nct++;
           }
        if(qc(ct3))
           {
           cthe = cthe + ct3;
           nct++;
           }
        if(nct < 1.0F) return;
        cthe = cthe / nct;

	setcolor(1, draw_reg, gc);
	for(h=cthe - 30.0F; h<=cthe + 30.0F; h += 10.0F)
	   {
	   x1 = (short)(hov.brx - 75) + (short)((h-cthe)*2.5F);
	   y1 = (short)(hov.tly+150);
	   moveto( x1, y1);
	   lineto( x1, (short)(y1-5));
	   }

	/* ----- Plot vertical theta-e profile ----- */
	setlinestyle( 1, 2 );
	setcolor(2, draw_reg, gc);
	x2 = 999;
	for(i=0; sndgp->sndg[i].pres >= 500.0F; i++)
	   {
	   if (qc(sndgp->sndg[i].dwpt))
	      {
	      x1 = (short)(hov.brx - 75 + (short)((thetae( sndgp->sndg[i].pres, sndgp->sndg[i].temp, sndgp->sndg[i].dwpt)-cthe)*2.5F));
	      y1 = vert_coords(agl(sndgp->sndg[i].hght), tophgt);
	      if(x2 == 999) { x2=x1; y2=y1; }

	      moveto( x1, y1);
	      lineto( x2, y2);

	      x2=x1;
	      y2=y1;
	      }
	   }
	   
	/* ----- Plot Vertical Legend ----- */
	setlinestyle( 1, 1 );
	setcolor(1, draw_reg, gc);
	x2 = 999;
	for(i=1000; i >= 600; i -= 100)
	   {
	   x1 = (short)(hov.brx - 150);
	   y1 = vert_coords((float)agl(i_hght((float)i)), tophgt);
	   moveto( x1, y1);
	   lineto( (short)(x1+5), y1);
	   }
	
	}


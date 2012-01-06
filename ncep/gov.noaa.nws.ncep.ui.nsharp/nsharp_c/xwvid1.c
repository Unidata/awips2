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
/*  OPC MODIFICATION - J. Morgan 5/12/05                       */
/*  header - Added isobar_nolabel                              */
/*  draw_skewt() - Added drawing of OPC diagnostic layers      */
/*  ISOBAR_NOLABEL - New function          				   */
/***************************************************************/
#define GLOBAL
#define VIDEO
#include "gui.h"
#include "sharp95.h"

/*
 *	Private functions
 */
void dry_adiabat (float thta);
void hodo_to_pix (float dir, float mag, short *x, short *y);
void isobar (float pres, short flag);
void isotherm (float temp);
void plot_barbs (void);
void trace_hodo (short width);
void trace_temp (struct sndg_struct *sp, short width);
void trace_temp2 (short width);
void trace_vtmp (short width);
void trace_wetbulb (short width);
void trace_dwpt (struct sndg_struct *sp, short width);
void trace_dwpt2 (short width);
void vvel_profile (void);
void wind_barb (float wdir, float wspd, short x, short y, short siz);
void isobar_nolabel (float pres);


/*===============================================================================*/

void
make_screen (void)
       /*************************************************************/
       /*  MAKE_SCREEN                                              */
       /*  John Hart  NSSFC KCMO                                    */
       /*                                                           */
       /*  Draws basic SHARP graphic template on screen, including  */
       /*  areas, buttons, and tables.                              */
       /*                                                           */
       /*************************************************************/
{
/*  char st[80]; */

  pagenum = 1;

  X_Init ();

  /* ----- RAOB Title Line ----- */
  setcolor (1, draw_reg, gc);
  set_font (1);

  /* ----- Parameter Area ----- */
  setcolor (4, draw_reg, gc);
  rectangle (1, (short) (skv.brx + 20), skv.tly, (short) (xwdth - 5),
	     (short) (xhght - 5));
  setcolor (1, draw_reg, gc);
  rectangle (0, (short) (skv.brx + 20), skv.tly, (short) (xwdth - 5),
	     (short) (xhght - 5));

  /* ----- Cursor Data Area ----- *-
     setcolor(3, draw_reg, gc);
     strcpy( st, "CURSOR DATA" );
     outgtext ( st,
     (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
     skv.tly + 5 );
     setcolor(0, draw_reg, gc);
     rectangle( 1, (short)(skv.brx+30), (short)(skv.tly+22), (short)(xwdth-15), 100);
     setcolor(1, draw_reg, gc);
     rectangle( 0, (short)(skv.brx+30), (short)(skv.tly+22), (short)(xwdth-15), 100); */

  mode = 1;
  pagenum = 1;

  /* ----- Begin Main Processing Loop ----- */
  StartLoop ();
}

/*==============================================================================*/

void draw_skewt (void)
/*************************************************************/
/*  DRAW_SKEWT                                               */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Draws a standard Skew-T/LogP diagram.                    */
/*************************************************************/
{
  short i;
  float thta;
  float ix1, ix2, ix3, ix4, ix5, ix6, ix7;
  float lvl[] =
    { 1050.0F, 1000.0F, 850.0F, 700.0F, 500.0F, 300.0F, 200.0F, 100.0F };
  char rtitle[200];

  setcliprgn (1, 1, xwdth, xhght, draw_reg, gc);
  setcolor (0, draw_reg, gc);
  rectangle (1, 1, 1, skv.brx + 14, skv.bry + skv.tly + 14);
  setcolor (1, draw_reg, gc);
  set_font (2);
  setcliprgn (skv.tlx, skv.tly, skv.brx, skv.bry, draw_reg, gc);
  setlinestyle (1, 1);
  rectangle (0, skv.tlx, skv.tly, skv.brx, skv.bry);

/* ----- Draw Skewed Temperature Lines ----- */
  setcolor (24, draw_reg, gc);
  setlinestyle (2, 1);
  for (i = (-160); i <= 50; i = i + 10)
    {
      isotherm ((float) i);
    }

/* ----- Draw Dry Adiabats ----- */
  setcolor (24, draw_reg, gc);
  setlinestyle (1, 1);
  for (thta = (-70.0F); thta <= 350.0F; thta = thta + 20.0F)
    {
      dry_adiabat (thta);
    };

/* ----- Draw Horizontal Pressure Lines ----- */
  setcolor (1, draw_reg, gc);
  setlinestyle (1, 1);
  for (i = 1; i < 8; i++)
    {
      isobar (lvl[i], 0);
    }
  for (i = 100; i <= 1050; i = i + 50)
    {
      isobar ((float) i, 1);
    }

/* ----- Draw OPC Horizontal Stability Lines ----- *
 ************************************************************************
 *  OPC MODIFICATION - J. Morgan 5/12/05				*
 *  Draws:								*
 *	 Lowest Inversion Height					*
 *	 Layer Based Mixing Height					*
 *	 Surface Based Mixing Height					*
 *  Calls:								*
 *	 xwvid.c: void isobar_nolabel ()				*
 *	skparams.c: void low_inv ()					*
 *	 skparams.c: void mix_height ()					*
 ***********************************************************************/

  low_inv( &ix1, &ix2 );
  setcolor (5, draw_reg, gc);
  setlinestyle (1, 1);
  isobar_nolabel (ix1);

  mix_height( &ix1, &ix2, &ix3, &ix4, &ix5, &ix6, &ix7, 1);
  setcolor (6, draw_reg, gc);
  setlinestyle (2, 1);
  isobar_nolabel (ix1);

  mix_height( &ix1, &ix2, &ix3, &ix4, &ix5, &ix6, &ix7, 0);
  setcolor (7, draw_reg, gc);
  setlinestyle (4, 1);
  isobar_nolabel (ix1);

/* Draw frame boarder */
  setcolor (1, draw_reg, gc);
  rectangle (0, skv.tlx, skv.tly, skv.brx, skv.bry);

  if (sndgp != NULL)
    {

      /* ----- Plot old sounding if exists ----- */
      setcolor (28, draw_reg, gc);
      if ((overlay_previous == 1) && (sndgp->ovrlev > 0))
	{
	  trace_temp2 (3);
	  trace_dwpt2 (3);
	}

      if (sndgp->numlev > 0)
	{
	  /* ----- Plot Environmental Temperature Data ----- */
	  setcolor (2, draw_reg, gc);
	  trace_temp (sndgp, 3);

	  /* ----- Plot Environmental Dew Point Data ----- */
	  setcolor (3, draw_reg, gc);
	  trace_dwpt (sndgp, 3);

	  /* ----- Plot Environmental Virtual Temperature Data ----- */
	  setcolor (2, draw_reg, gc);
	  trace_vtmp (1);

	  /* ----- Plot Environmental Wetbulb Temperature Data ----- */
	  setcolor (6, draw_reg, gc);
	  setlinestyle (1, 1);
	  trace_wetbulb (1);

	  /* ----- Plot Wind Barbs ----- */
	  setcolor (5, draw_reg, gc);
	  setlinestyle (1, 1);
	  plot_barbs ();

	  /* ----- If Available, plot VVEL profile ----- */
	  vvel_profile ();
	}




      /* ----- Display Skew-T Inset ----- */
      draw_skinset ();

      setcliprgn (1, 1, xwdth, xhght, draw_reg, gc);
      setcolor (1, draw_reg, gc);
      set_font (1);

      sprintf (rtitle, "%s (%s)", sndgp->title, raob_type);
      outgtext (rtitle, skv.tlx, 1);

      update_text_values ();
    }

  XCopyArea (XtDisplay (draw_reg), canvas, XtWindow (draw_reg),
	     gc, 0, 0, xwdth, xhght, 0, 0);
}

/*==============================================================================*/

void
draw_hodo (void)
	/*************************************************************/
	/*  DRAW_HODO                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws a standard Hodograph display.                      */
	/*************************************************************/
{
  short x1, y1, x2, y2, i, y3;
  float wdir, wspd, mnu, mnv, ix1, ix2, ix3, ix4;
  char st[10];

  setcolor (0, draw_reg, gc);
  rectangle (1, 1, 1, hov.brx + 14, hov.bry + 14);
  setcolor (1, draw_reg, gc);
  set_font (2);
  setcliprgn (hov.tlx, hov.tly, hov.brx, hov.bry, draw_reg, gc);
  setlinestyle (1, 1);
  rectangle (0, hov.tlx, hov.tly, hov.brx, hov.bry);

  /* ----- Plot crosshairs ----- */
  setcolor (31, draw_reg, gc);
  hodo_to_pix (180.0F, 60.0F, &x1, &y1);
  moveto (x1, hov.tly);
  lineto (x1, hov.bry);

  setcolor (31, draw_reg, gc);
  hodo_to_pix (270.0F, 60.0F, &x1, &y1);
  moveto (hov.tlx, y1);
  lineto (hov.brx, y1);

  /* ----- Plot Radius circles ----- */
  setcolor (24, draw_reg, gc);
  setlinestyle (2, 1);
  hodo_to_pix (0.0F, 0.0F, &x1, &y1);
  x2 = x1;
  y2 = y1;
  for (i = hov.scale; i <= hov.hodomag; i = i + hov.scale)
    {
      hodo_to_pix (0.0F, (float) i, &x1, &y1);
      y3 = (y1 - y2);
      ellipse (0, x2 - y3, y2 - y3, x2 + y3, y2 + y3);
    }

  setcolor (1, draw_reg, gc);
  /* ----- Plot X-Coord hash marks ----- */
  for (i = hov.scale; i <= hov.hodomag; i = i + hov.scale)
    {
      hodo_to_pix (180.0F, (float) i, &x1, &y1);
      moveto (x1 - 3, y1);
      lineto (x1 + 3, y1);
      itoa (i, st, 10);
      outgtext (st, x1 - getgtextextent (st) - 4, y1 - 5);

      hodo_to_pix (360.0F, (float) i, &x1, &y1);
      moveto (x1 - 3, y1);
      lineto (x1 + 3, y1);
      itoa (i, st, 10);
      outgtext (st, x1 - getgtextextent (st) - 4, y1 - 5);
    }

  /* ----- Plot Y-Coord hash marks ----- */
  setcolor (1, draw_reg, gc);
  for (i = hov.scale; i <= hov.hodomag; i = i + hov.scale)
    {
      hodo_to_pix (90.0F, (float) i, &x1, &y1);
      moveto (x1, y1 - 3);
      lineto (x1, y1 + 3);
      itoa (i, st, 10);
      outgtext (st, x1 - (getgtextextent (st) / 2), y1 + 5);

      hodo_to_pix (270.0F, (float) i, &x1, &y1);
      moveto (x1, y1 - 3);
      lineto (x1, y1 + 3);
      itoa (i, st, 10);
      outgtext (st, x1 - (getgtextextent (st) / 2), y1 + 5);
    }

  /* ----- Plot Hodograph (Shear Vectors) ----- */
  setcolor (2, draw_reg, gc);
  setlinestyle (1, 2);

  if ((sndgp != NULL) && (sndgp->numlev > 0))
    {
      trace_hodo (3);

      /* ----- Plot Mean Wind Vector ----- yellow square */
      setcolor (5, draw_reg, gc);
      mean_wind (-1.0F, -1.0F, &mnu, &mnv, &wdir, &wspd);
      hodo_to_pix (wdir, wspd, &x1, &y1);
      moveto (x1, y1);
      rectangle (0, (short) (x1 - 4), (short) (y1 - 4), (short) (x1 + 4),
		 (short) (y1 + 4));


      /* ----- Plot 30/75 Storm Motion Vector -----small light pink circle */
      mean_wind (sndgp->sndg[sfc ()].pres, i_pres (msl (6000.0F)), &ix1, &ix2,
		 &ix3, &ix4);
      setcolor (11, draw_reg, gc);
      setlinestyle (1, 1);
      ix4 *= .75F;
      ix3 += 30.0F;
      if (ix3 > 360.0F)
	ix3 -= 360.0F;
      hodo_to_pix (ix3, ix4, &x1, &y1);
      moveto (x1 - 3, y1);
      lineto (x1 + 3, y1);
      moveto (x1, y1 - 3);
      lineto (x1, y1 + 3);
      ellipse (0, x1 - 3, y1 - 3, x1 + 3, y1 + 3);


      /* ----- Plot 15/85 Storm Motion Vector ----- small light brick color circle*/
      mean_wind (sndgp->sndg[sfc ()].pres, i_pres (msl (6000.0F)), &ix1, &ix2,
		 &ix3, &ix4);
      setcolor (12, draw_reg, gc);
      setlinestyle (1, 1);
      ix4 *= .85F;
      ix3 += 15.0F;
      if (ix3 > 360.0F)
	ix3 -= 360.0F;
      hodo_to_pix (ix3, ix4, &x1, &y1);
      moveto (x1 - 3, y1);
      lineto (x1 + 3, y1);
      moveto (x1, y1 - 3);
      lineto (x1, y1 + 3);
      ellipse (0, x1 - 3, y1 - 3, x1 + 3, y1 + 3);


      /* ----- Plot Current Storm Motion Vector ----bigger-white  circle , initially at same location as 30/75 stomr */
      setcolor (31, draw_reg, gc);
      setlinestyle (1, 1);
      hodo_to_pix (sndgp->st_dir, sndgp->st_spd, &x1, &y1);
      moveto (x1 - 6, y1);
      lineto (x1 + 6, y1);
      moveto (x1, y1 - 6);
      lineto (x1, y1 + 6);
      ellipse (0, x1 - 6, y1 - 6, x1 + 6, y1 + 6);

      /* ----- Display Hodograph Inset ----- */
      draw_hoinset ();

      setcolor (1, draw_reg, gc);
      set_font (1);
      outgtext (sndgp->title, skv.tlx, 1);
    }


  /* ----- Draw final outline of hodograph ----- */
  setcolor (1, draw_reg, gc);
  setlinestyle (1, 1);
  rectangle (0, hov.tlx, hov.tly, hov.brx, hov.bry);

  /* reset clip region */
  setcliprgn (1, 1, xwdth, xhght, draw_reg, gc);

  XCopyArea (XtDisplay (draw_reg), canvas, XtWindow (draw_reg),
	     gc, 0, 0, xwdth, xhght, 0, 0);
}

/*==============================================================================*/

void
hodo_to_pix (float dir, float mag, short *x, short *y)
	/*************************************************************/
	/*  HODO_TO_PIX                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the screen location (x,y) in pixels of the    */
	/*  wind vector (dir,mag).                                   */
	/*************************************************************/
{
  float midx, midy;
  float scle;

  scle = (float) ((hov.brx - hov.tlx) / hov.hodomag);
  midx = hov.tlx + ((hov.brx - hov.tlx) / 2) + (hov.xshift * scle);
  midy = hov.tly + ((hov.bry - hov.tly) / 2) - (hov.yshift * scle);

  *x = (short) (midx - (ucomp (dir, mag) * scle));
  *y = (short) (midy + (vcomp (dir, mag) * scle));
}

/*==============================================================================*/

void
pix_to_hodo (short x, short y, float *dir, float *mag)
	/*************************************************************/
	/*  PIX_TO_HODO                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the wind vector (dir, mag) in knots of the    */
	/*  screen location (x,y).                                   */
	/*************************************************************/
{
  float midx, midy, scle, u, v;

  scle = (hov.brx - hov.tlx) / hov.hodomag;	/* pixels/knot */
  midx = hov.tlx + ((hov.brx - hov.tlx) / 2) + (hov.xshift * scle);
  midy = hov.tly + ((hov.bry - hov.tly) / 2) - (hov.yshift * scle);

  u = (midx - x) / scle;
  v = (y - midy) / scle;

  *dir = angle (u, v);
  *mag = (float) (sqrt ((u * u) + (v * v)));
}

/*==============================================================================*/

void
trace_hodo (short width)
	/*************************************************************/
	/*  TRACE_HODO                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental wind shear vectors on Hodograph.     */
	/*************************************************************/
{
  short i, x, y, xold, yold, ok = 0;

  if (sndgp == NULL)
    return;

  setlinestyle (1, width);
  for (i = 0; i < sndgp->numlev; i++)
    {
      if (qc (sndgp->sndg[i].drct) && qc (sndgp->sndg[i].sped))
	{

	  if (sndgp->sndg[i].hght <= msl (3000.0F))
	    setcolor (2, draw_reg, gc);
	  else if (sndgp->sndg[i].hght > msl (3000.0F)
		   && sndgp->sndg[i].hght <= msl (6000.0F))
	    setcolor (3, draw_reg, gc);
	  else if (sndgp->sndg[i].hght > msl (6000.0F)
		   && sndgp->sndg[i].hght <= msl (9000.0F))
	    setcolor (5, draw_reg, gc);
	  else if (sndgp->sndg[i].hght > msl (9000.0F)
		   && sndgp->sndg[i].hght <= msl (12000.0F))
	    setcolor (6, draw_reg, gc);
	  else if (sndgp->sndg[i].hght > msl (12000.0F))
	    setcolor (28, draw_reg, gc);

	  xold = x;
	  yold = y;
	  hodo_to_pix (sndgp->sndg[i].drct, sndgp->sndg[i].sped, &x, &y);
	  if (ok == 0)
	    {
	      moveto (x, y);
	      ok = 1;
	    }
	  else
	    {
	      moveto (xold, yold);
	      lineto (x, y);
	    }
	}
    }
}

/*==============================================================================*/

void
dry_adiabat (float thta)
	/*************************************************************/
	/*  DRY_ADIABAT                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws dry adiabat of theta (thta, c) on SkewT graphic.   */
	/*************************************************************/
{
  float pres, temp;
  short x, y;

  setcliprgn (skv.tlx, skv.tly, skv.brx, skv.bry, draw_reg, gc);
  for (pres = 1050; pres >= 100; pres = pres - 50)
    {
      temp = (float) ((((double) thta + 273.15) / pow (1000.0 /
						       (double) pres,
						       ROCP)) - 273.15);

      x = temp_to_pix (temp, pres);
      y = pres_to_pix (pres);

      /*
         if( pres <= 200) { printf( "%f   %f   %d\n", pres, temp, x);}
       */

      if (pres == 1050)
	{
	  moveto (x, y);
	}
      else
	{
	  lineto (x, y);
	}
    }
}

/*==============================================================================*/

void
isotherm (float temp)
	/*************************************************************/
	/*  ISOTHERM                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws temperature lines (temp, c) on SkewT graphic.      */
	/*************************************************************/
{
  short x, y;
  char st[10];

  setcolor (24, draw_reg, gc);
  x = temp_to_pix (temp, 1050);
  y = skv.bry;
  if ((temp >= -30) && (temp <= 50))
    {
      setcliprgn (1, 1, xwdth, xhght, draw_reg, gc);
      itoa ((short) temp, st, 10);
      setcolor (1, draw_reg, gc);
      outgtext (st, x - (getgtextextent (st) / 2), y);
      setcolor (24, draw_reg, gc);
    }
  setcliprgn (skv.tlx, skv.tly, skv.brx, skv.bry, draw_reg, gc);
  moveto (x, y);
  x = temp_to_pix (temp, 100);
  y = skv.tly;
  lineto (x, y);
}

/*==============================================================================*/

void
isobar (float pres, short flag)
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

  setcliprgn (1, 1, xwdth, xhght, draw_reg, gc);
  y = pres_to_pix (pres);
  if (flag == 0)
    {
      moveto (skv.tlx, y);
      itoa ((short) pres, st, 10);
      outgtext (st, skv.tlx - getgtextextent (st) - 2, y - 5);
      setcliprgn (skv.tlx, skv.tly, skv.brx, skv.bry, draw_reg, gc);
      moveto (skv.tlx, y);
      lineto (skv.brx, y);
    }
  else
    {
      moveto (skv.tlx, y);
      lineto (skv.tlx + 5, y);
      moveto (skv.brx, y);
      lineto (skv.brx - 5, y);
    }
}

/*==============================================================================*/

void
trace_temp (struct sndg_struct *sp, short width)
	/*************************************************************/
	/*  TRACE_TEMP                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental temperature trace on SkewT.          */
	/*************************************************************/
{
  short i, x, y, xold, yold, ok = 0;

  setcliprgn (skv.tlx, skv.tly, skv.brx, skv.bry, draw_reg, gc);
  setlinestyle (1, width);
  for (i = 0; i < sp->numlev; i++)
    {
      if (sp->sndg[i].temp > -200)
	{
	  xold = x;
	  yold = y;
	  x = temp_to_pix (sndgp->sndg[i].temp, sndgp->sndg[i].pres);
	  y = pres_to_pix (sndgp->sndg[i].pres);
	  if (ok == 0)
	    {
	      moveto (x, y);
	      ok = 1;
	    }
	  else
	    {
	      moveto (xold, yold);
	      lineto (x, y);
	    }
	}
    }
}

/*==============================================================================*/

void
trace_temp2 (short width)
	/*************************************************************/
	/*  TRACE_TEMP2                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental temperature trace on SkewT.          */
	/*************************************************************/
{
  short i, x, y, xold, yold, ok = 0, numlvl2;

  numlvl2 = sndgp->ovrlev;
  setcliprgn (skv.tlx, skv.tly, skv.brx, skv.bry, draw_reg, gc);
  setlinestyle (1, width);
  for (i = 0; i < numlvl2; i++)
    {
      if (sndgp->ovrl[i].temp > -200)
	{
	  xold = x;
	  yold = y;
	  x = temp_to_pix (sndgp->ovrl[i].temp, sndgp->ovrl[i].pres);
	  y = pres_to_pix (sndgp->ovrl[i].pres);
	  if (ok == 0)
	    {
	      moveto (x, y);
	      ok = 1;
	    }
	  else
	    {
	      moveto (xold, yold);
	      lineto (x, y);
	    }
	}
    }
}

/*==============================================================================*/

void
trace_vtmp (short width)
	/*************************************************************/
	/*  TRACE_VTMP                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots virtual temperature trace on SkewT.                */
	/*************************************************************/
{
  short i, x, y, xold, yold, ok = 0;

  setlinestyle (3, width);
  for (i = 0; i < sndgp->numlev; i++)
    {
      if (qc (sndgp->sndg[i].temp) && qc (sndgp->sndg[i].dwpt))
	{
	  xold = x;
	  yold = y;
	  x = temp_to_pix (i_vtmp (sndgp->sndg[i].pres), sndgp->sndg[i].pres);
	  y = pres_to_pix (sndgp->sndg[i].pres);
	  if (ok == 0)
	    {
	      moveto (x, y);
	      ok = 1;
	    }
	  else
	    {
	      moveto (xold, yold);
	      lineto (x, y);
	    }
	}
    }
}

/*==============================================================================*/

void
trace_dwpt (struct sndg_struct *sp, short width)
	/*************************************************************/
	/*  TRACE_DWPT                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental Dew Point trace on SkewT.            */
	/*************************************************************/
{
  short i, x, y, xold, yold, ok = 0;

  setcliprgn (skv.tlx, skv.tly, skv.brx, skv.bry, draw_reg, gc);
  setlinestyle (1, width);
  for (i = 0; i < sp->numlev; i++)
    {
      if (sp->sndg[i].dwpt > -200)
	{
	  xold = x;
	  yold = y;
	  x = temp_to_pix (sp->sndg[i].dwpt, sp->sndg[i].pres);
	  y = pres_to_pix (sp->sndg[i].pres);
	  if (ok == 0)
	    {
	      moveto (x, y);
	      ok = 1;
	    }
	  else
	    {
	      moveto (xold, yold);
	      lineto (x, y);
	    }
	}
    }
}

/*==============================================================================*/

void
trace_dwpt2 (short width)
	/*************************************************************/
	/*  TRACE_DWPT2                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental Dew Point trace on SkewT.            */
	/*************************************************************/
{
  short i, x, y, xold, yold, ok = 0, numlvl2;

  numlvl2 = sndgp->ovrlev;
  setcliprgn (skv.tlx, skv.tly, skv.brx, skv.bry, draw_reg, gc);
  setlinestyle (1, width);
  for (i = 0; i < numlvl2; i++)
    {
      if (sndgp->ovrl[i].dwpt > -200)
	{
	  xold = x;
	  yold = y;
	  x = temp_to_pix (sndgp->ovrl[i].dwpt, sndgp->ovrl[i].pres);
	  y = pres_to_pix (sndgp->ovrl[i].pres);
	  if (ok == 0)
	    {
	      moveto (x, y);
	      ok = 1;
	    }
	  else
	    {
	      moveto (xold, yold);
	      lineto (x, y);
	    }
	}
    }
}

/*==============================================================================*/

void
trace_wetbulb (short width)
	/*************************************************************/
	/*  TRACE_WETBULB                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental Wetbulb trace on SkewT.              */
	/*************************************************************/
{
  short i, x, y, xold, yold, ok = 0;
  float t1;

  setlinestyle (1, width);
  for (i = 0; i < sndgp->numlev; i++)
    {
      if (sndgp->sndg[i].dwpt > -200)
	{
	  xold = x;
	  yold = y;

	  t1 =
	    wetbulb (sndgp->sndg[i].pres, sndgp->sndg[i].temp,
		     sndgp->sndg[i].dwpt);

	  x = temp_to_pix (t1, sndgp->sndg[i].pres);
	  y = pres_to_pix (sndgp->sndg[i].pres);
	  if (ok == 0)
	    {
	      moveto (x, y);
	      ok = 1;
	    }
	  else
	    {
	      moveto (xold, yold);
	      lineto (x, y);
	    }
	}
    }
}

/*==============================================================================*/

short
pres_to_pix (float pres)
	/*************************************************************/
	/*  PRES_TO_PIX                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given pressure (mb) to an Y coordinate on       */
	/*  Skewt graphic.                                           */
	/*************************************************************/
{
  double scl1, scl2;
  scl1 = log (1050.) - log (100.);
  scl2 = log (1050.) - log ((double) pres);
  return (skv.bry - (short) ((scl2 / scl1) * (skv.bry - skv.tly)));
}

/*==============================================================================*/

float
pix_to_pres (short pix)
	/*************************************************************/
	/*  PIX_TO_PRES                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given Y coordinate on Skewt graphic to          */
	/*  pressure(mb).                                            */
	/*************************************************************/
{
  double scl1, scl2, scl3;
  scl1 = log (1050.) - log (100.);
  scl2 = (double) (skv.bry - pix);
  scl3 = (double) (skv.bry - skv.tly + 1);
  return (float) (1050.0 / exp ((scl2 / scl3) * scl1));
}

 /*NP*/ short
temp_to_pix (float temp, float pres)
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

  if (skv.type == 1)
    {
      scl1 = (float) skv.brtemp - ((((float) skv.bry -
				     (float) pres_to_pix (pres)) /
				    ((float) skv.bry -
				     (float) skv.tly)) * (float) skv.vspread);
    }
  else
    {
      scl1 = (float) skv.brtemp;
    }
  scl2 = skv.brx - (((scl1 - temp) / skv.hspread) * (skv.brx - skv.tlx));
  return (short) scl2;
}

 /*NP*/ float
pix_to_temp (short x, short y)
	/*************************************************************/
	/*  PIX_TO_TEMP                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given X/Y coordinates to temperature(c) on      */
	/*  Thermodynamic diagram.                                   */
	/*************************************************************/
{
  float scl1, scl2, scl3;

  scl1 =
    1 - (((float) x - (float) skv.tlx) / ((float) skv.brx - (float) skv.tlx));
  scl2 = (float) skv.brtemp - (scl1 * skv.hspread);
  scl1 =
    1 - (((float) y - (float) skv.tly) / ((float) skv.bry - (float) skv.tly));
  scl3 = scl2 - (scl1 * (float) skv.vspread);
  return scl3;
}

 /*NP*/ void
trace_parcel (float pres, float temp, float dwpt)
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

  if (!qc (pres) || !qc (temp) || !qc (dwpt))
    return;

  setcolor (31, draw_reg, gc);
  setlinestyle (4, 1);

  x = temp_to_pix (virtemp (pres, temp, dwpt), pres);
  y = pres_to_pix (pres);
  moveto (x, y);

  drylift (pres, temp, dwpt, &p2, &t2);
  x = temp_to_pix (virtemp (p2, t2, t2), p2);
  y = pres_to_pix (p2);
  lineto (x, y);

  for (i = p2 - 50; i >= 100; i = i - 50)
    {
      t3 = wetlift (p2, t2, i);
      x = temp_to_pix (virtemp (i, t3, t3), i);
      y = pres_to_pix (i);
      lineto (x, y);
    }
  t3 = wetlift (p2, t2, 100);
  x = temp_to_pix (virtemp (100, t3, t3), 100);
  y = pres_to_pix (100);
  lineto (x, y);
}

 /*NP*/ void
wind_barb (float wdir, float wspd, short x, short y, short siz)
	/*************************************************************/
	/*  WIND_BARB                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots wind barb at location (x,y) for given wind.        */
	/*************************************************************/
{
  short x1, y1, x2, y2, x3, y3, sped, maxsiz = 3;
  float dx, dy, spcx, spcy, wid, hgt;

  dx = (ucomp (wdir, 10) * (float) siz) / 1.5F;
  dy = (vcomp (wdir, 10) * (float) siz) / 1.5F;

  x1 = x;
  y1 = y;
  x2 = x1 + (short) dx;
  y2 = y1 - (short) dy;

  /* ----- Draw backbone of wind barb, along with origin dot ----- */
  if (siz > maxsiz)
    {
      setlinestyle (1, 2);
    }
  else
    {
      setlinestyle (1, 1);
    }

  rectangle (0, x1 - 1, y1 - 1, x1 + 1, y1 + 1);
  moveto (x1, y1);
  lineto (x2, y2);

  sped = (short) wspd;
  x1 = x2;
  y1 = y2;

  wid = 5;			/* Number of flags that will fit */
  spcx = dx / wid;
  spcy = dy / wid;
  x1 = x1 + (short) spcx;
  y1 = y1 - (short) spcy;

  /* ----- Draw wind flags (increments of 50kt) ----- */
  while (sped > 47)
    {
      x1 = x1 - (short) spcx;
      y1 = y1 + (short) spcy;

      hgt = .5F;		/* Heigth of flags */
      x2 = x1 + (short) (dy * hgt);
      y2 = y1 + (short) (dx * hgt);

      x3 = x1 - (short) spcx;
      y3 = y1 + (short) spcy;

      moveto (x1, y1);
      lineto (x2, y2);
      lineto (x3, y3);

      x2 = (x1 + x2 + x3) / 3;
      y2 = (y1 + y2 + y3) / 3;

      sped -= 50;
      x1 = x3;
      y1 = y3;
    }

  /* ----- Draw wind barbs (increments of 5kt) ----- */
  while (sped > 7)
    {
      hgt = .5F;		/* Heigth of flags */
      x2 = x1 + (short) (dy * hgt);
      y2 = y1 + (short) (dx * hgt);

      x3 = x1 - (short) spcx;
      y3 = y1 + (short) spcy;

      moveto (x3, y3);
      lineto (x2, y2);

      sped -= 10;
      x1 = x3;
      y1 = y3;
    }

  /* ----- Draw short barb ----- */
  if (sped > 3)
    {
      hgt = .5F;		/* Heigth of flags */
      x2 = x1 + (short) (dy * hgt);
      y2 = y1 + (short) (dx * hgt);

      x3 = x1 - (short) spcx;
      y3 = y1 + (short) spcy;

      dx = (x3 - x2) / 2;
      dy = (y3 - y2) / 2;

      x2 = x3 - (short) dx;
      y2 = y3 - (short) dy;

      moveto (x3, y3);
      lineto (x2, y2);
    }
}


 /*NP*/ void
plot_barbs (void)
	/*************************************************************/
	/*  PLOT_BARBS                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots wind barbs along side of thermo diagram.           */
	/*************************************************************/
{
  short ii, xx, yy;
  float lastpres;

  setcliprgn (1, 1, xwdth, xhght, draw_reg, gc);
  lastpres = 1100;
  for (ii = 0; ii < sndgp->numlev; ii++)
    {
      if (qc (sndgp->sndg[ii].drct))
	{
	  yy = pres_to_pix (sndgp->sndg[ii].pres);
	  xx = skv.brx - 40;

	  if ((sndgp->sndg[ii].hght - i_hght (lastpres)) > 400)
	    {
	      wind_barb (sndgp->sndg[ii].drct, sndgp->sndg[ii].sped, xx, yy,
			 4);
	      lastpres = sndgp->sndg[ii].pres;
	    }
	}
    }
}


/*==============================================================================*/

void
vvel_profile (void)
	/*************************************************************/
	/*  VVEL_PROFILE                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots vertical velocity profile                          */
	/*************************************************************/
{
  short i, x1, x2, y, avail, leng;

  avail = 0;
  setcliprgn (1, 1, xwdth, xhght, draw_reg, gc);
  for (i = 0; i < sndgp->numlev; i++)
    {
      if (qc (sndgp->sndg[i].omega) && (sndgp->sndg[i].omega < 1))
	{
	  avail++;
	  y = pres_to_pix (sndgp->sndg[i].pres);
	  x1 = skv.tlx + 40;
	  leng = (sndgp->sndg[i].omega * 1000);	/* Convert to Mbs/sec */
	  x2 = x1 - (leng * 2);	/* Determine screen scale */
	  setcolor (25, draw_reg, gc);
	  if (sndgp->sndg[i].omega < 0)
	    setcolor (12, draw_reg, gc);
	  moveto (x1, y);
	  lineto (x2, y);
	}
    }

  /* ----- Draw Scales ----- */
  if (avail > 5)
    {
      setcolor (7, draw_reg, gc);
      x1 = skv.tlx + 40;
      moveto (x1, skv.tly + 40);
      lineto (x1, skv.bry - 10);

      setlinestyle (3, 1);
      x1 = skv.tlx + 20;
      moveto (x1, skv.tly + 40);
      lineto (x1, skv.bry - 10);
      x1 = skv.tlx + 60;
      moveto (x1, skv.tly + 40);
      lineto (x1, skv.bry - 10);

      set_font (2);
      outgtext ("OMEGA", skv.tlx + 18, skv.tlx);
      outgtext ("+10", skv.tlx + 3, skv.tlx + 15);
      outgtext ("-10", skv.tlx + 43, skv.tlx + 15);
    }
}

void isobar_nolabel (float pres)
/*************************************************************/
/*  OPC MODIFICATION - J. Morgan 5/12/05                     */
/*  ISOBAR_NOLABEL - New function                     		 */
/*                                                           */
/*  ISOBAR_NOLABEL                                           */
/*  J. Morgan OPC                                            */
/*                                                           */
/*  Modification of void isobar() to draw pressure (pres)    */
/*    without a label on SkewT graphic                       */
/*                                                           */
/*  Called by 	xwvid1.c: draw_skewt()                        */
/*************************************************************/
	{
		short y;

		setcliprgn (1, 1, xwdth, xhght, draw_reg, gc);
		y = pres_to_pix (pres);
		moveto (skv.tlx, y);
		setcliprgn (skv.tlx, skv.tly, skv.brx, skv.bry, draw_reg, gc);
		moveto (skv.tlx, y);
		lineto (skv.brx, y);
	}

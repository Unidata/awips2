/***************************************************************/
/*  NSHARP                                                     */
/*  Postscript driver routine                                  */
/*  Adapted from hpgl.c driver                                 */
/*                                                             */
/*  Unidata/UCAR	Steve Chiswell	3/99                   */
/*                                                             */
/*      --------------------------------------------------     */
/** Public routines in this module:                            */
/*  print_sounding_ps                                          */
/*                                                             */
/** Private routines in this module:                           */
/*  ps_centertext                                              */
/*  ps_circle                                                  */
/*  ps_cliprgn                                                 */
/*  ps_close                                                   */
/*  ps_disp_param                                              */
/*  ps_draw_hodo                                               */
/*  ps_draw_skewt                                              */
/*  ps_dry_adiabat                                             */
/*  ps_fill_cape                                               */
/*  ps_fill_capex   (to be removed)                            */
/*  ps_hodo_circs                                              */
/*  ps_hodo_to_pix                                             */
/*  ps_isobar                                                  */
/*  ps_isotherm                                                */
/*  ps_label_hodo                                              */
/*  ps_lineto                                                  */
/*  ps_lvldata                                                 */
/*  ps_moveto                                                  */
/*  ps_open_printer                                            */
/*  ps_page                                                    */
/*  ps_plot_barbs                                              */
/*  ps_plot_elevs                                              */
/*  ps_plot_uvvs						*/
/*  ps_pres_to_pix                                             */
/*  ps_rectangle                                               */
/*  ps_select_pen                                              */
/*  ps_setup                                                   */
/*  ps_sharp_label                                             */
/*  ps_temp_to_pix                                             */
/*  ps_text                                                    */
/*  ps_trace_dwpt                                              */
/*  ps_trace_hodo                                              */
/*  ps_trace_parcel                                            */
/*  ps_trace_parcelX (to be removed)                           */
/*  ps_trace_temp                                              */
/*  ps_trace_vtmp                                              */
/*  ps_trace_wetbulb                                           */
/*  ps_triangle                                                */
/*  ps_wind_barb                                               */
/*  ps_write_parcel                                            */
/*  ps_write_storm						*/
/*  ps_write_thermo                                            */
/*  ps_write_winds						*/
/*                                                             */
/***************************************************************/
#include "gui.h"
#include "sharp95.h"

/*
 *	Private function prototypes
 */
void ps_centertext (FILE * fp, char *st, short x, short y, short font,
		    short tf);
void ps_circle (FILE * fp, short x, short y, short radius);
void ps_cliprgn (FILE * fp, short x1, short y1, short x2, short y2);
void ps_close (FILE * fp);
void ps_disp_param (FILE * fp, char *st, short x, short y, short font,
		    short tf);
void ps_draw_hodo (FILE * fp);
void ps_draw_skewt (FILE * fp);
void ps_dry_adiabat (FILE * fp, float thta);
void ps_fill_cape (FILE * fp, short pct);
void ps_fill_capex (FILE * fp, short pct);
void ps_hodo_circs (FILE * fp);
void ps_hodo_to_pix (float dir, float mag, short *x, short *y);
void ps_isobar (FILE * fp, float pres, short flag);
void ps_isotherm (FILE * fp, float temp);
void ps_label_hodo (FILE * fp);
void ps_lineto (FILE * fp, short x, short y);
void ps_lvldata (FILE * fp, float pres, char *nam, short x, short y);
void ps_moveto (FILE * fp, short x, short y);
FILE *ps_open_printer (void);
void ps_page (FILE * fp);
void ps_plot_barbs (FILE * fp);
void ps_plot_elevs (FILE * fp);
void ps_plot_uvvs ( FILE *fp );
short ps_pres_to_pix (float pres);
void ps_rectangle (FILE * fp, short x1, short y1, short x3, short y3,
		   short fill, short pct);
void ps_select_pen (FILE * fp, short onoff, float width, short type);
void ps_setup (FILE * fp);
void ps_sharp_label (FILE * fp, short x1, short y1);
short ps_temp_to_pix (float temp, float pres);
void ps_text (FILE * fp, char *textstr, short x, short y,
	      short pts, short bold);
void ps_trace_dwpt (FILE * fp);
void ps_trace_hodo (FILE * fp);
void ps_trace_parcel (FILE * fp, float pres, float temp, float dwpt);
void ps_trace_parcelx (FILE * fp, float pres, float temp, float dwpt);
void ps_trace_temp (FILE * fp);
void ps_trace_vtmp (FILE * fp);
void ps_trace_wetbulb (FILE * fp);
void ps_triangle (FILE * fp, short x1, short y1, short x2, short y2,
		  short x3, short y3, short fill);
void ps_wind_barb (FILE * fp, float wdir, float wspd, short x, short y, 
					short siz);
void ps_write_parcel ( FILE *fp);
void ps_write_storm ( FILE *fp);
void ps_write_thermo ( FILE *fp);
void ps_write_winds ( FILE *fp);

void _sounding_plot (FILE * fp);


extern int nnpage;

static int text_justify = 0;
static int Pfont;
static int linewrap;
int pscolor;

struct _skewt hsk = { 500, 850, 5636, 5986, 100, 70, 60, 1 };
struct _hodog hho = { 3500, 850, 5636, 2950, -25, -25, 120, 20 };

/*===================================================================*/

void
print_sounding_ps (int COLRMODE)
/*************************************************************/
/*  PRINT_SOUNDING_PS                                        */
/*                                                           */
/*  Prints skewt/hodograph to Postscript.                    */
/*************************************************************/
{
  int i = 1;
  FILE *fp;
  struct sndg_struct *save_sndgp;

  pscolor = COLRMODE;

  fp = ps_open_printer ();
  if (fp == NULL)
    {
      printf ("failed to open postscript file\n");
      return;
    }


  ps_setup (fp);

  if (NxmPrt_isPgFlgSet ())
    {
      save_sndgp = sndgp;
      while ((i < MAX_PIXMAP) && (sndgs[i] != NULL) && (sndgs[i]->numlev > 0))
	{
	  if (i > 1)
	    ps_page (fp);
	  sndgp = sndgs[i];
	  _sounding_plot (fp);
	  i++;
	}
      sndgp = save_sndgp;
    }
  else
    {
      _sounding_plot (fp);
    }

  ps_close (fp);

  return;
}

/*=========================================================================*/
FILE *
ps_open_printer (void)
/************************************************************************
 *  ps_open_printer                                            		*
 *                                                           		*
 *  Opens stream for output to printer.  Returns FILE handle.		*
 *                                                           		*
 *  fin        - Name of file to open                        		*
 **									*
 * T. Piper/SAIC	01/04	changed mktemp to mkstemp		*
 * S. Chiswell/UCAR	04/04	added fdopen for mkstemp descriptor	*
 ***********************************************************************/
{
  char *tempnam;
  FILE *f1 = NULL;
  int fdes;

  tempnam = (char *) malloc (20);
  tempnam[0] = '\0';
  sprintf (tempnam, "/tmp/.nsharp_XXXXXX");
  fdes = mkstemp (tempnam);

  strcpy (config.filename, tempnam);
  free (tempnam);
  f1 = fdopen (fdes, "w");
  return f1;
}

/*=========================================================================*/

void
ps_setup (FILE * fp)
/*************************************************************/
/*  ps_setup                                                 */
/*                                                           */
/*  fp         - File handle                                 */
/**									*
 * Log:									*
 * T. Piper/SAIC	03/05	Updated for Adobe 3.0			*
 ***********************************************************************/
{
  int iret, type=0;
  dttms_t dattim;

/*-----------------------------------------------------------------*/

/*      Write prolog commands to file. */

  fprintf (fp, "%%!PS-Adobe-3.0\n");
  fprintf (fp, "%%%%For:  %s\n", getenv("USER"));
  fprintf (fp, "%%%%Creator:  NSHARP\n");
  fprintf (fp, "%%%%Title:  NSHARP Graphic\n");
  css_gtim ( &type, dattim, &iret);
  fprintf (fp, "%%%%CreationDate:  %s\n", dattim);
  fprintf (fp, "%%%%EndComments\n");
  fprintf (fp, "%%%%BeginProlog\n");
  fprintf (fp, "%%%%Pages : (atend)\n");

  fprintf (fp, "/M {moveto} def\n");
  fprintf (fp, "/L {lineto} def\n");
  fprintf (fp, "/N {newpath} def\n");
  fprintf (fp, "/LW {setlinewidth} def\n");
  fprintf (fp, "/AF {arc fill} def\n");

  fprintf (fp, "/MS  {makefont setfont} def\n");
  fprintf (fp, "/FF {findfont} def\n");
  fprintf (fp, "/CF {currentfont} def\n");
  fprintf (fp, "/FL {closepath fill} def\n");
  fprintf (fp, "/RGB {setrgbcolor} def\n");
  fprintf (fp, "%%%%EndProlog\n");
  fprintf (fp, "%%%%BeginSetup\n");
  fprintf (fp, "<</PageSize[612 792]>>setpagedevice\n");
  fprintf (fp, "%%%%EndSetup\n");
  fprintf (fp, " 20 50 translate .075 .075 scale\n");
  fprintf (fp, " 1 setlinecap 1 setlinejoin newpath\n");
  fprintf (fp, "%%%%Page:    1  ?\n");

  nnpage = 1;
  Pfont = 0;
  linewrap = 0;

}

void
ps_select_pen (FILE * fp, short onoff, float width, short type)
/*************************************************************/
/*  ps_select_pen                                            */
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
  float red, green, blue;
  int irwdth, rgbcolor;

  if (pscolor != 1)
    rgbcolor = 1;
  else
    rgbcolor = (int) onoff;
  switch (rgbcolor)
    {
    case 0:
      red = 1;
      green = 1;
      blue = 1;
      break;
    case 1:
      red = 0;
      green = 0;
      blue = 0;
      break;
    case 2:
      red = .7;
      green = 0;
      blue = 0;
      break;
    case 3:
      red = 0;
      green = .7;
      blue = 0;
      break;
    case 4:
      red = 0;
      green = 0;
      blue = .7;
      break;
    case 5:
      red = .7;
      green = .7;
      blue = 0;
      break;
    case 6:
      red = 0;
      green = .7;
      blue = .7;
      break;
    default:
      red = 0;
      green = 0;
      blue = 0;
    }

  switch ((int) type)
    {
    case 2:
      fprintf (fp, "[40] 0 setdash\n");
      break;
    case 10:
      fprintf (fp, "[40 40 5 50] 0 setdash\n");
      break;
    case 11:
      fprintf (fp, "[20] 0 setdash\n");
      break;
    case 9:
      fprintf (fp, "[] 0 setdash\n");
      break;
    default:
      printf ("Unknown line type, defaulting to solid %d\n", type);
      fprintf (fp, "[] 0 setdash\n");
    }


  irwdth = (int) (width * 60);
  /*printf("look width %d color %f %f %f\n",irwdth,red,green,blue); */
  if (irwdth < 1)
    irwdth = 1;

  fprintf (fp, "%5.3f %5.3f %5.3f %s\n", red, green, blue, " RGB ");
  fprintf (fp, "%4d LW\n", irwdth);

}

void
ps_moveto (FILE * fp, short x, short y)
/*************************************************************/
/*  ps_moveto                                                */
/*                                                           */
/*  Moves cursor to given coordinates, newpath               */
/*************************************************************/
{
  y = (1016 * 11) - 1050 - y;
  fprintf (fp, "N %6d %6d M ", x, y);
}

void
ps_lineto (FILE * fp, short x, short y)
/*************************************************************/
/*  ps_lineto                                                */
/*                                                           */
/*  lineto from current cursor location to (x,y)             */
/*************************************************************/
{
  y = (1016 * 11) - 1050 - y;
  fprintf (fp, "%5d %5d L ", x, y);
  linewrap++;
  if (linewrap >= 4)
    {
      fprintf (fp, "\n");
      linewrap = 0;
    }
}

void
ps_rectangle (FILE * fp, short x1, short y1, short x3, short y3,
	      short fill, short pct)
/*************************************************************/
/*  ps_rectangle                                             */
/*                                                           */
/*  Draws a rectangle bounded by (x1,y1) and (x2,y2).        */
/*                                                           */
/*  fill   -   Fill Indicator (1=YES)                        */
/*  pct    -   Percent coverage of fill (0-100%)             */
/*************************************************************/
{
  short x2, y2, x4, y4;
  float grayf;

  x2 = x3;
  y2 = y1;

  x4 = x1;
  y4 = y3;

  if (fill == 1)
    {
      grayf = ((float) (100. - pct)) / 100.;
      fprintf (fp, "%5.3f %5.3f %5.3f RGB\n", grayf, grayf, grayf);
    }
  ps_moveto (fp, x1, y1);
  ps_lineto (fp, x2, y2);
  ps_lineto (fp, x3, y3);
  ps_lineto (fp, x4, y4);
  ps_lineto (fp, x1, y1);
  if (fill != 1)
    fprintf (fp, "stroke\n");
  else
    fprintf (fp, "FL\n");
}

void
ps_cliprgn (FILE * fp, short x1, short y1, short x2, short y2)
/*************************************************************/
/*  ps_cliprgn                                               */
/*                                                           */
/*  Defines drawing area with points (x1,y1) and (x2,y2).    */
/*************************************************************/
{
  y1 = (1016 * 11) - 1050 - y1;
  y2 = (1016 * 11) - 1050 - y2;

  fprintf (fp, "initclip\n");
  fprintf (fp,
	   "N %d %d M %d %d lineto %d %d lineto %d %d lineto %d %d lineto closepath clip N\n",
	   x1, y1, x1, y2, x2, y2, x2, y1, x1, y1);
}

void
ps_close (FILE * fp)
/*************************************************************/
/*  PS_CLOSE                                                 */
/*                                                           */
/*************************************************************/
{
  fprintf (fp, " gsave showpage grestore\n");
  fprintf (fp, " -576 -126 translate -90 rotate 60.6796 60.6796 scale\n");

  fprintf (fp, "%%%%Trailer:\n");
  fprintf (fp, "%%%%Pages: %5d\n", nnpage);

  fclose (fp);
}

void
ps_page (FILE * fp)
/*************************************************************/
/*  PS_PAGE                                                  */
/*                                                           */
/*************************************************************/
{
  fprintf (fp, " gsave showpage grestore\n");
  nnpage++;
  fprintf (fp, "%%%%Page:%5d  ?\n", nnpage);
}

void
ps_draw_skewt (FILE * fp)
/*************************************************************/
/*  ps_draw_skewt                                            */
/*                                                           */
/*  Draws a standard Skew-T/LogP diagram using postscript.   */
/*************************************************************/
{
  short i;
  float thta;
  float lvl[] = { 1050, 1000, 850, 700, 500, 300, 200, 100 };
  char st[128];

  /* ----- Write Title Line at top of chart ----- */
  sprintf (st, "%s (%s)", sndgp->title, raob_type);
  ps_text (fp, st, hsk.tlx, hsk.tly - 65, 12, 3);

  /* ----- Begin with outer boundary and frame ----- */
  ps_select_pen (fp, 1, .75F, 9);
  ps_rectangle (fp, hsk.tlx, hsk.tly, hsk.brx, hsk.bry, 0, 0);
  ps_cliprgn (fp, hsk.brx, hsk.tly, hsk.tlx, hsk.bry);

  ps_fill_cape (fp, 20);

  /* ----- Draw Horizontal Pressure Lines ----- */
  ps_select_pen (fp, 1, 0, 9);
  for (i = 1; i <= 7; i++)
    {
      ps_isobar (fp, lvl[i], 0);
    }
  for (i = 150; i < 1050; i += 50)
    {
      ps_isobar (fp, (float) i, 1);
    }

  /* ----- Draw Skewed Temperature Lines ----- */
  ps_select_pen (fp, 1, 0, 2);
  for (i = -160; i <= 50; i += 10)
    {
      ps_isotherm (fp, (float) i);
    }

  /* ----- Draw Dry Adiabats ----- */
  ps_select_pen (fp, 1, 0, 9);
  for (thta = -70; thta <= 350; thta += 20)
    {
      ps_dry_adiabat (fp, thta);
    };

  /* ----- Plot Environmental Temperature Data ----- */
  ps_select_pen (fp, 2, .75F, 9);
  ps_trace_temp (fp);

  /* ----- Plot Environmental Dew Point Data ----- */
  ps_select_pen (fp, 3, .75F, 9);
  ps_trace_dwpt (fp);

  /* ----- Plot Environmental Virtual Temperature Data ----- */
  ps_select_pen (fp, 5, .25F, 11);
  ps_trace_vtmp (fp);

  /* ----- Plot Environmental Wet Bulb Temperature Data ----- */
  ps_select_pen (fp, 6, .25F, 11);
  ps_trace_wetbulb (fp);

  /* ----- Plot Environmental Wind Barbs ----- */
  ps_select_pen (fp, 1, .25F, 9);
  ps_plot_barbs (fp);

  /* ----- Plot Elevation legend ----- */
  ps_select_pen (fp, 1, .25F, 9);
  ps_plot_elevs (fp);

  /* ----- Trace Lifted Parcel ----- */
  ps_select_pen (fp, 4, .35F, 10);
  ps_trace_parcel (fp, sndgp->lplvals.pres, sndgp->lplvals.temp,
		   sndgp->lplvals.dwpt);
}

/*============================================================================*/

void
ps_isobar (FILE * fp, float pres, short flag)
/*************************************************************/
/*  ps_isobar                                                */
/*                                                           */
/*  Draws pressure lines (pres, mb) on SkewT graphic.        */
/*                                                           */
/*  flag = 0    Draw complete horizontal line                */
/*       = 1    Draw small tick marks along sides of chart   */
/*************************************************************/
{
  short y;
  char st[10];

  y = ps_pres_to_pix (pres);
  if (flag == 0)
    {
      ps_cliprgn (fp, 20000, 0, 0, 20000);
      itoa ((short) pres, st, 10);
      ps_text (fp, st, hsk.tlx - 355, y + 60, 10, 0);
      ps_cliprgn (fp, hsk.brx, hsk.tly, hsk.tlx, hsk.bry);

      ps_moveto (fp, hsk.tlx, y);
      ps_lineto (fp, hsk.brx, y);
      fprintf (fp, " stroke\n");
    }
  else
    {
      ps_moveto (fp, hsk.tlx, y);
      ps_lineto (fp, hsk.tlx + 100, y);
      fprintf (fp, " stroke\n");
      ps_moveto (fp, hsk.brx, y);
      ps_lineto (fp, hsk.brx - 100, y);
      fprintf (fp, " stroke\n");
    }
}

void
ps_isotherm (FILE * fp, float temp)
/*************************************************************/
/*  ps_isotherm                                              */
/*                                                           */
/*  Draws temperature lines (temp, c) on SkewT graphic.      */
/*************************************************************/
{
  short x1, y1, x2, y2;
  char st[10];

  if ((temp >= -30) && (temp <= 50))
    {
      x1 = ps_temp_to_pix (temp, 1050);
      y1 = hsk.bry;
      ps_cliprgn (fp, 20000, 0, 0, 20000);
      itoa ((short) temp, st, 10);
      ps_text (fp, st, x1, y1 + 160, 10, 0);
      ps_cliprgn (fp, hsk.brx, hsk.tly, hsk.tlx, hsk.bry);
    }
  x1 = ps_temp_to_pix (temp, 1050);
  y1 = hsk.bry;
  ps_moveto (fp, x1, y1);
  x2 = ps_temp_to_pix (temp, 100);
  y2 = hsk.tly;
  ps_lineto (fp, x2, y2);
  fprintf (fp, " stroke\n");
}

/*======================================================================*/

short
ps_pres_to_pix (float pres)
/*************************************************************/
/*  ps_pres_to_pix                                           */
/*                                                           */
/*  Converts given pressure (mb) to an Y coordinate on       */
/*  Skewt graphic.                                           */
/*************************************************************/
{
  double scl1, scl2;
  scl1 = log (1050) - log (100);
  scl2 = log (1050) - log (pres);
  return (short) (hsk.bry - (scl2 / scl1) * (hsk.bry - hsk.tly));
}

/*======================================================================*/

short
ps_temp_to_pix (float temp, float pres)
/*************************************************************/
/*  ps_temp_to_pix                                           */
/*                                                           */
/*  Converts given temperature (c) to an X coordinate on     */
/*  Thermodynamic diagram.                                   */
/*                                                           */
/*  Depending on (hsk.type), relationship is for (1) Pseudo- */
/*  Adiabatic or (2) Skew-T/Log P diagram.                   */
/*                                                           */
/*  Skew:  90c spread across the bottom of chart.            */
/*        120c spread up-and-down the chart.                 */
/*         Temp at BR of chart = 50c                         */
/*************************************************************/
{
  float scl1, scl2;
  short tmpshrt;

/*----------------------------------------------------------------------*/
  if (hsk.type == 1)
    {
      tmpshrt = ps_pres_to_pix (pres);
      scl1 = (float) hsk.brtemp - ((((float)hsk.bry -
				     (float)tmpshrt) /
				    ((float)hsk.bry -
				     (float)hsk.tly)) * (float)hsk.vspread);
    }
  else
    {
      scl1 = hsk.brtemp;
    }
  scl2 = hsk.brx - (((scl1 - temp) / hsk.hspread) * (hsk.brx - hsk.tlx));
  return (short) scl2;
}

/*======================================================================*/

void
ps_dry_adiabat (FILE * fp, float thta)
/*************************************************************/
/*  PS_DRY_ADIABAT                                           */
/*                                                           */
/*  Draws dry adiabat of theta (thta, c) on SkewT graphic.   */
/*************************************************************/
{
  float pres, temp;
  short x, y;
  int ok = 0;

  fprintf (fp, "%% Dry Adiabat %5.1f\n", thta);
  for (pres = 1050; pres >= 100; pres = pres - 50)
    {
      temp = ((thta + 273.15) / pow (1000.0 / pres, ROCP)) - 273.15;

      x = ps_temp_to_pix (temp, pres);
      y = ps_pres_to_pix (pres);

      if (G_DIFF(pres, 1050.0F))
	{
	  ps_moveto (fp, x, y);
	}
      else
	{
	  ps_lineto (fp, x, y);
	  ok++;
	}
    }
  if (ok > 0)
    fprintf (fp, " stroke\n");
}

/*======================================================================*/

void
ps_trace_temp (FILE * fp)
/*************************************************************/
/*  ps_trace_temp                                            */
/*                                                           */
/*  Plots environmental temperature trace on SkewT.          */
/*************************************************************/
{
  short i, x, y, ok = 0;

  for (i = 0; i < sndgp->numlev; i++)
    {
      if (sndgp->sndg[i].temp > -200)
	{
	  x = ps_temp_to_pix (sndgp->sndg[i].temp, sndgp->sndg[i].pres);
	  y = ps_pres_to_pix (sndgp->sndg[i].pres);
	  if (ok == 0)
	    {
	      ps_moveto (fp, x, y);
	      ok = 1;
	    }
	  else
	    {
	      ps_lineto (fp, x, y);
	      ok++;
	    }
	}
    }
  if (ok > 1)
    fprintf (fp, " stroke\n");
}

/*======================================================================*/

void
ps_trace_vtmp (FILE * fp)
/*************************************************************/
/*  ps_trace_vtmp                                            */
/*                                                           */
/*  Plots environmental virtual temp trace on SkewT.         */
/*************************************************************/
{
  short i, x, y, ok = 0;

  for (i = 0; i < sndgp->numlev; i++)
    {
      if (sndgp->sndg[i].temp > -200)
	{
	  x =
	    ps_temp_to_pix (i_vtmp (sndgp->sndg[i].pres),
			    sndgp->sndg[i].pres);
	  y = ps_pres_to_pix (sndgp->sndg[i].pres);
	  if (ok == 0)
	    {
	      ps_moveto (fp, x, y);
	      ok = 1;
	    }
	  else
	    {
	      ps_lineto (fp, x, y);
	      ok++;
	    }
	}
    }
  if (ok > 1)
    fprintf (fp, " stroke\n");
}

/*====================================================================*/

void
ps_trace_dwpt (FILE * fp)
/*************************************************************/
/*  ps_trace_dwpt                                            */
/*                                                           */
/*  Plots environmental Dew Point trace on SkewT.            */
/*************************************************************/
{
  short i, x, y, ok = 0;

  for (i = 0; i < sndgp->numlev; i++)
    {
      if (sndgp->sndg[i].dwpt > -200)
	{
	  x = ps_temp_to_pix (sndgp->sndg[i].dwpt, sndgp->sndg[i].pres);
	  y = ps_pres_to_pix (sndgp->sndg[i].pres);
	  if (ok == 0)
	    {
	      ps_moveto (fp, x, y);
	      ok = 1;
	    }
	  else
	    {
	      ps_lineto (fp, x, y);
	      ok++;
	    }
	}
    }
  if (ok > 1)
    fprintf (fp, " stroke\n");
}

/*=========================================================================*/

void
ps_trace_wetbulb (FILE * fp)
/*************************************************************/
/*  ps_trace_wetbulb                                         */
/*                                                           */
/*  Plots environmental Wet Bulb trace on SkewT.             */
/*************************************************************/
{
  short i, x, y, ok = 0;
  float t1;

  for (i = 0; i < sndgp->numlev; i++)
    {
      if (sndgp->sndg[i].dwpt > -200)
	{

	  t1 =
	    wetbulb (sndgp->sndg[i].pres, sndgp->sndg[i].temp,
		     sndgp->sndg[i].dwpt);
	  x = ps_temp_to_pix (t1, sndgp->sndg[i].pres);
	  y = ps_pres_to_pix (sndgp->sndg[i].pres);
	  if (ok == 0)
	    {
	      ps_moveto (fp, x, y);
	      ok = 1;
	    }
	  else
	    {
	      ps_lineto (fp, x, y);
	      ok++;
	    }
	}
    }
  if (ok > 1)
    fprintf (fp, " stroke\n");
}

/*=======================================================================*/

void
ps_text (FILE * fp, char *textstr, short x, short y, short pts, short bold)
/*************************************************************/
/*  PS_TEXT                                                  */
/*                                                           */
/*  Writes given string at location (x,y).                   */
/*  pts         -  Number of points for given font           */
/*  bold        -  Typeface indicator (0=NORM, 3=BOLD)       */
/*************************************************************/
{
  int iftyp1, iftyp2;
  int font = 11;
  float size = 1.0F;
  static char *pfonts[12] = {
    "Courier", "Helvetica", "Times-Roman",
    "Courier-Oblique", "Helvetica-Oblique",
    "Times-Italic", "Courier-Bold",
    "Helvetica-Bold", "Times-Bold",
    "Courier-BoldOblique",
    "Helvetica-BoldOblique",
    "Times-BoldItalic"
  };
  static float Psize = 0.5F;

  iftyp1 = font / 10;
  iftyp2 = font % 10;
  if ((iftyp1 < 0) || (iftyp1 > 3))
    iftyp1 = 0;
  if ((iftyp2 < 1) || (iftyp2 > 3))
    iftyp2 = 1;

  font = iftyp1 * 3 + iftyp2;
  if ((Pfont != font) || (!G_DIFF(Psize, size)) )
    {
      fprintf (fp, "/%s FF\n", pfonts[font - 1]);
      fprintf (fp, "[%10.3f 0 0%10.3f 0 0] MS\n", size * 110, size * 110);
      Pfont = font;
      Psize = size;
    }

  ps_moveto (fp, x, y);

  /*      Write out the Text. */

  switch (text_justify)
    {
    case 1:			/* right justified */
      fprintf (fp, "(%s) dup stringwidth pop neg 0 rmoveto show\n", textstr);
      break;
    case 2:			/* cenetered text */
      fprintf (fp, "(%s) dup stringwidth pop neg 2 div 0 rmoveto show\n",
	       textstr);
      break;
    default:
      fprintf (fp, "(%s) show\n", textstr);
    }


}

/*======================================================================*/

void
ps_wind_barb (FILE * fp, float wdir, float wspd, short x, short y, short siz)
/*************************************************************/
/*  ps_wind_barb                                             */
/*                                                           */
/*  Plots wind barb at location (x,y) for given wind.        */
/*************************************************************/
{
  short x1, y1, x2, y2, x3, y3, sped;
  float dx, dy, spcx, spcy, wid, hgt;

  dx = ucomp (wdir, 10) * siz / 1.5;
  dy = vcomp (wdir, 10) * siz / 1.5;

  x1 = x;
  y1 = y;
  x2 = x1 + (short) dx;
  y2 = y1 - (short) dy;

  /* ----- Draw backbone of wind barb, along with origin dot ----- */
  ps_moveto (fp, x1, y1);
  ps_lineto (fp, x2, y2);
  fprintf (fp, " stroke\n");

  sped = (short) wspd;
  x1 = x2;
  y1 = y2;

  wid = 6;			/* Width of flags */
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

      ps_triangle (fp, x1, y1, x2, y2, x3, y3, 1);
      /*ps_moveto( fp, x1, y1);
         ps_lineto( fp, x2, y2);
         ps_lineto( fp, x3, y3); */

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

      ps_moveto (fp, x3, y3);
      ps_lineto (fp, x2, y2);
      fprintf (fp, " stroke\n");
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

      ps_moveto (fp, x3, y3);
      ps_lineto (fp, x2, y2);
      fprintf (fp, " stroke\n");

      sped -= 10;
      x1 = x3;
      y1 = y3;
    }
}

/*======================================================================*/

void
ps_plot_barbs (FILE * fp)
/*************************************************************/
/*  ps_plot_barbs                                            */
/*                                                           */
/*  Plots wind barbs along side of thermo diagram.           */
/*************************************************************/
{
  short i, x, y;
  float lastpres;

  ps_cliprgn (fp, 20000, 0, 0, 20000);

  /* ----- Draw vertical line ----- */
  ps_moveto (fp, hsk.brx + 500, hsk.tly);
  ps_lineto (fp, hsk.brx + 500, hsk.bry);
  fprintf (fp, " stroke\n");

  lastpres = 1100;
  for (i = 0; i < sndgp->numlev; i++)
    {
      if (qc (sndgp->sndg[i].drct) && sndgp->sndg[i].pres >= 100)
	{
	  y = ps_pres_to_pix (sndgp->sndg[i].pres);
	  x = hsk.brx + 500;

	  if ((sndgp->sndg[i].hght - i_hght (lastpres)) > 400)
	    {
	      ps_wind_barb (fp, sndgp->sndg[i].drct, sndgp->sndg[i].sped, x,
			    y, 45);
	      lastpres = sndgp->sndg[i].pres;
	    }
	}
    }
}

/*======================================================================*/

void
ps_trace_parcel (FILE * fp, float pres, float temp, float dwpt)
/*************************************************************/
/*  ps_trace_parcel                                          */
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

  if ((!qc (dwpt)) || (!qc (temp)) || (!qc (pres)))
    return;

  x = ps_temp_to_pix (virtemp (pres, temp, dwpt), pres);
  y = ps_pres_to_pix (pres);
  ps_moveto (fp, x, y);

  drylift (pres, temp, dwpt, &p2, &t2);
  x = ps_temp_to_pix (virtemp (p2, t2, t2), p2);
  y = ps_pres_to_pix (p2);
  ps_lineto (fp, x, y);

  for (i = p2 - 50; i >= 100; i = i - 50)
    {
      t3 = wetlift (p2, t2, i);
      x = ps_temp_to_pix (virtemp (i, t3, t3), i);
      y = ps_pres_to_pix (i);
      ps_lineto (fp, x, y);
    }

  t3 = wetlift (p2, t2, 100);
  x = ps_temp_to_pix (virtemp (100, t3, t3), 100);
  y = ps_pres_to_pix (100);
  ps_lineto (fp, x, y);
  fprintf (fp, " stroke\n");
}

/*======================================================================*/

void
ps_trace_parcelx (FILE * fp, float pres, float temp, float dwpt)
/*************************************************************/
/*  ps_trace_parcelX                                         */
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

  x = ps_temp_to_pix (temp, pres);
  y = ps_pres_to_pix (pres);
  ps_moveto (fp, x, y);

  drylift (pres, temp, dwpt, &p2, &t2);
  x = ps_temp_to_pix (t2, p2);
  y = ps_pres_to_pix (p2);
  ps_lineto (fp, x, y);

  for (i = p2 - 50; i >= 100; i = i - 50)
    {
      t3 = wetlift (p2, t2, i);
      x = ps_temp_to_pix (t3, i);
      y = ps_pres_to_pix (i);
      ps_lineto (fp, x, y);
    }
  t3 = wetlift (p2, t2, 100);
  x = ps_temp_to_pix (t3, 100);
  y = ps_pres_to_pix (100);
  ps_lineto (fp, x, y);
}

/*======================================================================*/

void
ps_triangle (FILE * fp, short x1, short y1, short x2, short y2,
	     short x3, short y3, short fill)
/*************************************************************/
/*  ps_triangle                                              */
/*                                                           */
/*  Draws triangle of (x,y) coordinates.                     */
/*                                                           */
/*  fill   -  Fill indicator (1=YES)                         */
/*************************************************************/
{
  ps_moveto (fp, x1, y1);
  ps_lineto (fp, x2, y2);
  ps_lineto (fp, x3, y3);
  fprintf (fp, " FL\n");

}

/*======================================================================*/

void
ps_draw_hodo (FILE * fp)
/*************************************************************/
/*  ps_draw_hodo                                             */
/*                                                           */
/*  Draws a standard Hodograph diagram on a HP Laserjet      */
/*  printer.  All graphic calls are HP-GL/2 dependent.       */
/*************************************************************/
{
  short x1, y1, i;
  char st[10];

  /* ----- Transparency mode off ----- */

  ps_cliprgn (fp, hho.tlx, hho.tly, hho.brx, hho.bry);
  ps_rectangle (fp, hho.tlx, hho.tly, hho.brx, hho.bry, 1, 0);
  ps_select_pen (fp, 1, .75F, 9);
  ps_rectangle (fp, hho.tlx, hho.tly, hho.brx, hho.bry, 0, 0);

  /* ----- Plot crosshairs ----- */
  ps_select_pen (fp, 1, .1F, 9);
  ps_hodo_to_pix (180, 60, &x1, &y1);
  ps_moveto (fp, x1, hho.tly);
  ps_lineto (fp, x1, hho.bry);
  fprintf (fp, " stroke\n");

  ps_hodo_to_pix (270, 60, &x1, &y1);
  ps_moveto (fp, hho.tlx, y1);
  ps_lineto (fp, hho.brx, y1);
  fprintf (fp, " stroke\n");

  /* ----- Plot X-Coord hash marks ----- */
  for (i = hho.scale; i <= hho.hodomag; i = i + hho.scale)
    {
      ps_hodo_to_pix (180, (float) i, &x1, &y1);
      ps_moveto (fp, x1 - 30, y1);
      ps_lineto (fp, x1 + 30, y1);
      fprintf (fp, " stroke\n");
      itoa (i, st, 10);
      ps_text (fp, st, x1 + 50, y1 + 30, 6, 0);

      ps_hodo_to_pix (360, (float) i, &x1, &y1);
      ps_moveto (fp, x1 - 30, y1);
      ps_lineto (fp, x1 + 30, y1);
      fprintf (fp, " stroke\n");
      itoa (i, st, 10);
      ps_text (fp, st, x1 + 50, y1 + 30, 6, 0);
    }

  /* ----- Plot Y-Coord hash marks ----- */
  for (i = hho.scale; i <= hho.hodomag; i = i + hho.scale)
    {
      ps_hodo_to_pix (90, (float) i, &x1, &y1);
      ps_moveto (fp, x1, y1 - 30);
      ps_lineto (fp, x1, y1 + 30);
      fprintf (fp, " stroke\n");
      itoa (i, st, 10);
      ps_text (fp, st, x1 - 50, y1 + 80, 6, 0);

      ps_hodo_to_pix (270, (float) i, &x1, &y1);
      ps_moveto (fp, x1, y1 - 30);
      ps_lineto (fp, x1, y1 + 30);
      fprintf (fp, " stroke\n");
      itoa (i, st, 10);
      ps_text (fp, st, x1 - 50, y1 + 80, 6, 0);
    }

  ps_select_pen (fp, 1, .1F, 9);
  ps_hodo_circs (fp);

  /* ----- Plot Hodograph (Shear Vectors) ----- */
  ps_select_pen (fp, 1, .15F, 9);
  ps_trace_hodo (fp);
  ps_select_pen (fp, 1, .15F, 9);
  ps_label_hodo (fp);
}

/*======================================================================*/

void
ps_hodo_to_pix (float dir, float mag, short *x, short *y)
/*************************************************************/
/*  ps_hodo_to_pix                                           */
/*                                                           */
/*  Calculates the printer location (x,y) in pels of the     */
/*  wind vector (dir,mag).                                   */
/*************************************************************/
{
  float midx, midy;
  float scle;

  scle = (hho.brx - hho.tlx) / hho.hodomag;
  midx = hho.tlx + ((hho.brx - hho.tlx) / 2) + (hho.xshift * scle);
  midy = hho.tly + ((hho.bry - hho.tly) / 2) - (hho.yshift * scle);

  *x = (short) (midx - (ucomp (dir, mag) * scle));
  *y = (short) (midy + (vcomp (dir, mag) * scle));
}

/*======================================================================*/

void
ps_trace_hodo (FILE * fp)
/*************************************************************/
/*  ps_trace_hodo                                            */
/*                                                           */
/*  Plots environmental wind shear vectors on Hodograph.     */
/*************************************************************/
{
  short i, x, y, ok = 0;

  for (i = 0; i < sndgp->numlev; i++)
    {
      if (qc (sndgp->sndg[i].drct) && qc (sndgp->sndg[i].sped))
	{
	  ps_hodo_to_pix (sndgp->sndg[i].drct, sndgp->sndg[i].sped, &x, &y);
	  if (ok == 0)
	    {
	      ps_moveto (fp, x, y);
	      ok = 1;
	    }
	  else
	    {
	      ps_lineto (fp, x, y);
	      ok++;
	    }
	}
    }
  if (ok > 1)
    fprintf (fp, " stroke\n");
}

/*======================================================================*/

void
ps_label_hodo (FILE * fp)
/*************************************************************/
/*  ps_label_hodo                                            */
/*                                                           */
/*  places labels on hodograph at predetermined levels.      */
/*************************************************************/
{
  float wdi, wsp;
  short i, x, y;
  short lvl[] = { 1000, 2000, 3000, 5000, 10000, 15000 };
  char st[10];

  for (i = 0; i <= 5; i++)
    {
      wdi = i_wdir (i_pres (msl (lvl[i])));
      wsp = i_wspd (i_pres (msl (lvl[i])));
      ps_hodo_to_pix (wdi, wsp, &x, &y);
      sprintf (st, "%d", lvl[i] / 1000);
      ps_text (fp, st, x - 60, y - 60, 8, 3);
    }

  /* ----- Display Storm Motion at Upper Left Corner ----- */
  if (qc (sndgp->st_dir) && qc (sndgp->st_spd))
    {
      sprintf (st, "%d / %d kt", (short) sndgp->st_dir,
	       (short) sndgp->st_spd);
    }
  else
    {
      sprintf (st, "M / M");
    }
  ps_text (fp, st, hho.tlx + 50, hho.tly + 150, 9, 0);

}

/*======================================================================*/

void
ps_plot_elevs (FILE * fp)
/*************************************************************/
/*  ps_plot_elevs                                            */
/*                                                           */
/*  Plots elevations (m, ft) MSL on right side of SkewT.     */
/*************************************************************/
{
  short y;
  float hgt, tmpflt;
  char st[80];

  ps_cliprgn (fp, 20000, 0, 0, 20000);

  /* ----- Draw base legend ----- */
  ps_moveto (fp, hsk.brx + 1200, hsk.tly);
  ps_lineto (fp, hsk.brx + 1200, hsk.bry);
  fprintf (fp, " stroke\n");

  ps_moveto (fp, hsk.brx + 900, hsk.tly);
  ps_lineto (fp, hsk.brx + 1500, hsk.tly);
  fprintf (fp, " stroke\n");

  ps_text (fp, "MSL", hsk.brx + 1125, hsk.tly - 95, 7, 3);
  ps_text (fp, "kft     km", hsk.brx + 1000, hsk.tly - 15, 7, 3);

  /* ----- Find SFC level and plot ----- */
  y = ps_pres_to_pix (sndgp->sndg[sfc ()].pres);
  ps_moveto (fp, hsk.brx + 1150, y);
  ps_lineto (fp, hsk.brx + 1250, y);
  fprintf (fp, " stroke\n");
  sprintf (st, "%d m", (short)sndgp->sndg[sfc ()].hght);
  ps_text (fp, st, hsk.brx + 1300, y + 30, 7, 0);
  tmpflt = mtof(sndgp->sndg[sfc ()].hght);
  sprintf (st, "%d ft", (short)tmpflt );
  ps_disp_param (fp, st, hsk.brx + 800, y + 30, 7, 0);
  /*ps_text( fp, st, hsk.brx + 800, y + 30, 7, 0); */

  /* ----- Plot every 1km ----- */
  for (hgt = 1000; hgt < sndgp->sndg[sndgp->numlev - 1].hght; hgt += 1000)
    {
      if (i_pres (hgt) > 100)
	{
	  y = ps_pres_to_pix (i_pres (hgt));
	  ps_moveto (fp, hsk.brx + 1200, y);
	  ps_lineto (fp, hsk.brx + 1250, y);
	  fprintf (fp, " stroke\n");
	  sprintf (st, "%d", (short) (hgt / 1000));
	  ps_text (fp, st, hsk.brx + 1300, y + 30, 7, 0);
	}
    }
  /* ----- Plot every 5kft ----- */
  tmpflt =  mtof(sndgp->sndg[sndgp->numlev - 1].hght);
  for (hgt = 5000; hgt < tmpflt; hgt += 5000)
    {
      if (i_pres (ftom (hgt)) > 100)
	{
	  y = ps_pres_to_pix (i_pres (ftom (hgt)));
	  ps_moveto (fp, hsk.brx + 1150, y);
	  ps_lineto (fp, hsk.brx + 1200, y);
	  fprintf (fp, " stroke\n");
	  sprintf (st, "%d", (short) (hgt / 1000));
	  ps_text (fp, st, hsk.brx + 1000, y + 30, 7, 0);
	}
    }
}

/*======================================================================*/

void
ps_hodo_circs (FILE * fp)
/*************************************************************/
/*  ps_hodo_circs                                            */
/*                                                           */
/*  Draws speed rings on hodograph as scale increments.      */
/*************************************************************/
{
  short x1, y1, x2, y2, i;

  ps_hodo_to_pix (180, 0, &x1, &y1);

  for (i = hho.scale; i <= hho.hodomag; i = i + hho.scale)
    {
      ps_hodo_to_pix (180, (float) i, &x2, &y2);
      ps_circle (fp, x1, y1, abs (y2 - y1));
    }
}

/*======================================================================*/

void
ps_circle (FILE * fp, short x, short y, short radius)
/*************************************************************/
/*  ps_circle                                                */
/*                                                           */
/*  Draws a circle at (x,y) of radius (radius).              */
/*************************************************************/
{
  fprintf (fp, "N %d %d M %d %d %d 0 360 arc\n", x, y, x, y, radius);
}

/*======================================================================*/

void
ps_write_parcel (FILE * fp)
/*************************************************************/
/*  ps_write_parcel                                          */
/*                                                           */
/*  Writes parcel data to printer.                           */
/*************************************************************/
{
  char st[80];
  short x1, y1;
  float sfctemp, sfcdwpt, sfcpres, p;
  struct _parcel pcl;

  sfctemp = sndgp->lplvals.temp;
  sfcdwpt = sndgp->lplvals.dwpt;
  sfcpres = sndgp->lplvals.pres;

  x1 = 100;
  y1 = hsk.bry + 500;

  ps_cliprgn (fp, 20000, 0, 0, 20000);
  ps_centertext (fp, "THERMODYNAMIC PARAMETERS", x1 + 1500, y1, 12, 3);
  ps_select_pen (fp, 1, .4F, 9);
  ps_rectangle (fp, x1 - 50, y1 + 20, x1 + 3000, 9800, 0, 0);

  /* ----- Selected parcel type ----- */
  y1 += 200;
  sprintf (st, "%s", sndgp->lplvals.desc);
  ps_text (fp, st, x1, y1, 9, 0);

  /* ----- Lifted Parcel Information ----- */
  y1 += 200;
  sprintf (st, "LPL:  %4.0fmb   %4.0fC/%4.0fC   %4.0fF/%4.0fF",
	   sndgp->lplvals.pres,
	   sndgp->lplvals.temp, sndgp->lplvals.dwpt,
	   ctof (sndgp->lplvals.temp), ctof (sndgp->lplvals.dwpt));
  ps_text (fp, st, x1, y1, 9, 0);

  /* ----- Calculate Parcel Data ----- */
  parcel (-1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

  /* ----- CAPE/LI ----- */
  y1 += 200;
  ps_text (fp, "CAPE:", x1, y1, 9, 0);
  strcpy (st, qc2 (pcl.bplus, " J/kg", 0));
  ps_disp_param (fp, st, x1 + 1200, y1, 9, 0);

  ps_text (fp, "LI:", x1 + 1500, y1, 9, 0);
  strcpy (st, qc2 (pcl.li5, " C @  500mb", 0));
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* ----- BFZL/LImin ----- */
  y1 += 150;
  ps_text (fp, "BFZL:", x1, y1, 9, 0);
  strcpy (st, qc2 (pcl.bfzl, " J/kg", 0));
  ps_disp_param (fp, st, x1 + 1200, y1, 9, 0);

  ps_text (fp, "LImin:", x1 + 1500, y1, 9, 0);
  if (qc (pcl.limax))
    {
      strcpy (st, qc2 (pcl.limax, " C @  ", 0));
      strcat (st, qc2 (pcl.limaxpres, "mb", 0));
    }
  else
    {
      strcpy (st, "M");
    }
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);


  /* ----- CINH/CAP ----- */
  y1 += 150;
  ps_text (fp, "CINH:", x1, y1, 9, 0);
  strcpy (st, qc2 (pcl.bminus, " J/kg", 0));
  ps_disp_param (fp, st, x1 + 1200, y1, 9, 0);

  ps_text (fp, "CAP:", x1 + 1500, y1, 9, 0);
  if (qc (pcl.cap))
    {
      strcpy (st, qc2 (pcl.cap, " C @  ", 0));
      strcat (st, qc2 (pcl.cappres, "mb", 0));
    }
  else
    {
      strcpy (st, "M");
    }
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* ----- Header for LEVEL data ----- */
  y1 += 200;
  ps_disp_param (fp, "LEVEL", x1 + 400, y1, 9, 0);
  ps_disp_param (fp, "PRES", x1 + 1100, y1, 9, 0);
  ps_disp_param (fp, "HGT(AGL)", x1 + 2100, y1, 9, 0);
  ps_disp_param (fp, "TEMP", x1 + 2900, y1, 9, 0);
  y1 += 20;
  ps_select_pen (fp, 1, .1F, 9);
  ps_moveto (fp, x1, y1);
  ps_lineto (fp, x1 + 2950, y1);
  fprintf (fp, " stroke\n");

  /* ----- LCL ----- */
  y1 += 150;
  p = pcl.lclpres;
  ps_lvldata (fp, p, "LCL", x1, y1);

  /* ----- LFC ----- */
  y1 += 150;
  p = pcl.lfcpres;
  ps_lvldata (fp, p, "LFC", x1, y1);
  strcpy (st, qc2 (i_temp (p), " C", 0));
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* ----- EL ----- */
  y1 += 150;
  p = pcl.elpres;
  ps_lvldata (fp, p, "EL", x1, y1);
  strcpy (st, qc2 (i_temp (p), " C", 0));
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* ----- MPL ----- */
  y1 += 150;
  p = pcl.mplpres;
  ps_lvldata (fp, p, "MPL", x1, y1);

  y1 += 50;
  ps_select_pen (fp, 1, .4F, 9);
  ps_moveto (fp, x1 - 50, y1);
  ps_lineto (fp, x1 + 3000, y1);
  fprintf (fp, " stroke\n");
}

/*=======================================================================*/

void
ps_write_thermo (FILE * fp)
/*************************************************************/
/*  ps_write_thermo                                          */
/*                                                           */
/*  Writes thermo data to printer.                           */
/*************************************************************/
{
  char st[80];
  short x1, y1;
  float ix1, ix2, ix3;
/*-----------------------------------------------------------------------*/

  x1 = 100;
  y1 = 8406;

  /* ----- PW and Mean RH ----- */
  ps_text (fp, "Precip Water:", x1, y1, 9, 0);
  strcpy (st, qc2 (precip_water (&ix1, -1, -1), " in", 2));
  ps_disp_param (fp, st, x1 + 1400, y1, 9, 0);
  ps_text (fp, "Mean RH:", x1 + 1600, y1, 9, 0);
  strcpy (st, qc2 (mean_relhum (&ix1, -1, -1), " %", 0));
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* ----- Mean MR and Mean LRH ----- */
  y1 += 150;
  ps_text (fp, "Mean Q:", x1, y1, 9, 0);
  strcpy (st, qc2 (mean_mixratio (&ix1, -1, -1), " g/kg", 1));
  ps_disp_param (fp, st, x1 + 1400, y1, 9, 0);
  ps_text (fp, "Mean LRH:", x1 + 1600, y1, 9, 0);
  strcpy (st,
	  qc2 (mean_relhum (&ix1, -1, sndgp->sndg[sfc ()].pres - 150), " %",
	       0));
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* ----- Top of Moist Layer ----- */
  y1 += 150;
  ps_text (fp, "Top of Moist Layer:", x1, y1, 9, 0);
  strcpy (st, qc2 (top_moistlyr (&ix1), " mb", 0));
  strcat (st, " / ");
  strcat (st, qc2 (mtof(agl (i_hght (ix1))), " ft", 0));
  ps_text (fp, st, x1 + 1600, y1, 9, 0);

  y1 += 50;
  ps_select_pen (fp, 1, .4F, 9);
  ps_moveto (fp, x1 - 50, y1);
  ps_lineto (fp, x1 + 3000, y1);
  fprintf (fp, " stroke\n");

  /* ----- 700-500mb Lapse Rates ----- */
  y1 += 150;
  ps_text (fp, "700-500mb Lapse Rate:", x1, y1, 9, 0);
  strcpy (st, qc2 (delta_t (&ix1), " C", 0));
  strcat (st, "  /  ");
  strcat (st, qc2 (lapse_rate (&ix1, 700, 500), " C/km", 1));
  ps_text (fp, st, x1 + 1600, y1, 9, 0);

  /* ----- 850-500mb Lapse Rates ----- */
  y1 += 150;
  ps_text (fp, "850-500mb Lapse Rate:", x1, y1, 9, 0);
  strcpy (st, qc2 (vert_tot (&ix1), " C", 0));
  strcat (st, "  /  ");
  strcat (st, qc2 (lapse_rate (&ix1, 850, 500), " C/km", 1));
  ps_text (fp, st, x1 + 1600, y1, 9, 0);

  y1 += 50;
  ps_moveto (fp, x1 - 50, y1);
  ps_lineto (fp, x1 + 3000, y1);
  fprintf (fp, " stroke\n");

  /* ----- Total-Totals and K-Index ----- */
  y1 += 150;
  ps_text (fp, "Total Totals:", x1, y1, 9, 0);
  strcpy (st, qc2 (t_totals (&ix1, &ix2, &ix3), "", 0));
  ps_disp_param (fp, st, x1 + 1400, y1, 9, 0);
  ps_text (fp, "K-Index:", x1 + 1600, y1, 9, 0);
  strcpy (st, qc2 (k_index (&ix1), "", 0));
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* -----SWEAT and Max Temp ----- */
  y1 += 150;
  ps_text (fp, "SWEAT Index:", x1, y1, 9, 0);
  strcpy (st, qc2 (sweat_index (&ix1), "", 0));
  ps_disp_param (fp, st, x1 + 1400, y1, 9, 0);
  ps_text (fp, "Max Temp:", x1 + 1600, y1, 9, 0);
  strcpy (st, qc2 (ctof (max_temp (&ix1, -1)), " F", 0));
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* ----- ThetaE Diff and Convective Temp ----- */
  y1 += 150;
  ps_text (fp, "ThetaE Diff:", x1, y1, 9, 0);
  ix2 = ThetaE_diff (&ix1);
  strcpy (st, qc2 (ix2, " C", 0));
  ps_disp_param (fp, st, x1 + 1400, y1, 9, 0);
  ix2 = cnvtv_temp (&ix1, -50);
  ps_text (fp, "*Conv Temp:", x1 + 1600, y1, 9, 0);
  strcpy (st, qc2 (ctof (ix2), " F", 0));
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* ----- FZL and WBZ ----- */
  y1 += 150;
  ps_text (fp, "FRZ Level:", x1, y1, 9, 0);
  strcpy (st, qc2 (mtof(agl (i_hght (temp_lvl (0, &ix1)))), " ft", 0));
  ps_disp_param (fp, st, x1 + 1400, y1, 9, 0);
  ps_text (fp, "WBZ Level:", x1 + 1600, y1, 9, 0);
  strcpy (st, qc2 (mtof(agl (i_hght (wb_lvl (0, &ix1)))), " ft", 0));
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);
}

/*====================================================================*/

void
ps_disp_param (FILE * fp, char *st, short x, short y, short font, short tf)
/*************************************************************/
/*  ps_disp_param                                            */
/*                                                           */
/*  Right justifies value at location x,y.                   */
/*************************************************************/
{
  text_justify = 1;
  fprintf (fp, "%% need to right justify %d\n", text_justify);
  ps_text (fp, st, x, y, font, tf);
  text_justify = 0;
}

/*======================================================================*/

void
ps_fill_cape (FILE * fp, short pct)
/*************************************************************/
/*  ps_fill_cape                                             */
/*                                                           */
/*  Fills the positive area with grey area.                  */
/*************************************************************/
{
  short x, y, i, j, xs, ys;
  float t1, p1, t2, p2, sfcpres, sfctemp, sfcdwpt, pres;
  float grayf;
  struct _parcel pcl;

  sfcpres = sndgp->lplvals.pres;
  sfctemp = sndgp->lplvals.temp;
  sfcdwpt = sndgp->lplvals.dwpt;

  /* ----- Calculate Parcel Data ----- */
  parcel (-1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
  if (pcl.bplus <= 0)
    {
      return;
    }

  /* ----- Account for partial soundings ----- */
  if (!qc (pcl.elpres))
    {
      pcl.elpres = sndgp->sndg[sndgp->numlev - 1].pres;
    }

  /* ----- Start at LFC ----- */
  fprintf (fp, "\n%% Plot cape\nN ");
  p1 = pcl.lfcpres;
  t1 = i_vtmp (pcl.lfcpres);
  x = ps_temp_to_pix (t1, p1);
  y = ps_pres_to_pix (p1);
  ps_moveto (fp, x, y);
  xs = x;
  ys = y;

  /* ----- Trace up temperature line ----- */
  i = 0;
  while (sndgp->sndg[i].pres > pcl.lfcpres)
    {
      i++;
    }

  for (j = i; sndgp->sndg[j].pres > pcl.elpres; j++)
    {
      if (qc (sndgp->sndg[j].temp))
	{
	  p1 = sndgp->sndg[j].pres;
	  t1 = i_vtmp (p1);
	  x = ps_temp_to_pix (t1, p1);
	  y = ps_pres_to_pix (p1);
	  ps_lineto (fp, x, y);
	}
    }

  grayf = ((float) (100. - pct)) / 100.;
  fprintf (fp, "%5.3f %5.3f %5.3f RGB\n", grayf, grayf, grayf);
  /* ----- Now, starting at EL, go back down Thw to LFC ----- */
  drylift (sfcpres, sfctemp, sfcdwpt, &p2, &t2);
  for (pres = pcl.elpres; pres < pcl.lfcpres; pres += 50)
    {
      t1 = wetlift (p2, t2, pres);
      x = ps_temp_to_pix (virtemp (pres, t1, t1), pres);
      y = ps_pres_to_pix (pres);
      ps_lineto (fp, x, y);
    }

  /* ----- Finish Polygon by returning to LFC ----- */
  ps_lineto (fp, xs, ys);
  fprintf (fp, "FL\n");

}

/*======================================================================*/

void
ps_fill_capex (FILE * fp, short pct)
/*************************************************************/
/*  ps_fill_capeX                                            */
/*                                                           */
/*  Fills the positive area with grey area.                  */
/*************************************************************/
{
  short x, y, i, j, xs, ys;
  float t1, p1, t2, p2, sfcpres, sfctemp, sfcdwpt, pres;
  struct _parcel pcl;

  sfcpres = sndgp->lplvals.pres;
  sfctemp = sndgp->lplvals.temp;
  sfcdwpt = sndgp->lplvals.dwpt;

  /* ----- Calculate Parcel Data ----- */
  parcelx (-1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
  if (pcl.bplus <= 0)
    {
      return;
    }

  /* ----- Set Polygon Mode ----- */
  fputs ("PM0;", fp);

  /* ----- Start at LFC ----- */
  p1 = pcl.lfcpres;
  t1 = i_temp (pcl.lfcpres);
  x = ps_temp_to_pix (t1, p1);
  y = ps_pres_to_pix (p1);
  ps_moveto (fp, x, y);
  xs = x;
  ys = y;

  /* ----- Trace up temperature line ----- */
  i = 0;
  while (sndgp->sndg[i].pres > pcl.lfcpres)
    {
      i++;
    }

  for (j = i; sndgp->sndg[j].pres > pcl.elpres; j++)
    {
      if (qc (sndgp->sndg[j].temp))
	{
	  p1 = sndgp->sndg[j].pres;
	  t1 = i_temp (p1);
	  x = ps_temp_to_pix (t1, p1);
	  y = ps_pres_to_pix (p1);
	  ps_lineto (fp, x, y);
	}
    }

  /* ----- Now, starting at EL, go back down Thw to LFC ----- */
  drylift (sfcpres, sfctemp, sfcdwpt, &p2, &t2);
  for (pres = pcl.elpres; pres < pcl.lfcpres; pres += 50)
    {
      t1 = wetlift (p2, t2, pres);
      x = ps_temp_to_pix (t1, pres);
      y = ps_pres_to_pix (pres);
      ps_lineto (fp, x, y);
    }

  /* ----- Finish Polygon by returning to LFC ----- */
  ps_lineto (fp, xs, ys);

  /* ----- End Polygon Mode ----- */
  fputs ("PM2;", fp);

  /* ----- Transparency mode on ----- */
  fputs ("TR1;", fp);

  /* ----- Set Fill Density and Finish Fill ----- */
  fprintf (fp, "FT10,%d;", pct);
  fputs ("FP;", fp);
}

/*======================================================================*/

void
ps_centertext (FILE * fp, char *st, short x, short y, short font, short tf)
/*************************************************************/
/*  ps_centertext                                            */
/*                                                           */
/*  Writes given string at location (x,y).                   */
/*  pts         -  Number of points for given font           */
/*  bold        -  Typeface indicator (0=NORM, 3=BOLD)       */
/*************************************************************/
{
  fprintf (fp, "%% need to center text\n");
  text_justify = 2;
  ps_text (fp, st, x, y, font, tf);
  text_justify = 0;
}

/*======================================================================*/

void
ps_lvldata (FILE * fp, float pres, char *nam, short x, short y)
/*************************************************************/
/*  ps_lvldata                                               */
/*                                                           */
/*  Write level data to printer (used in parcel area).       */
/*************************************************************/
{
  char st[80];
  if (qc (pres))
    {
      ps_disp_param (fp, nam, x + 400, y, 9, 0);
      strcpy (st, qc2 (pres, "mb", 0));
      ps_disp_param (fp, st, x + 1100, y, 9, 0);
      strcpy (st, qc2 (mtof(agl (i_hght (pres))), "ft", 0));
      ps_disp_param (fp, st, x + 2100, y, 9, 0);
    }
  else
    {
      ps_disp_param (fp, nam, x + 400, y, 9, 0);
      ps_disp_param (fp, "M", x + 1100, y, 9, 0);
      ps_disp_param (fp, "M", x + 2100, y, 9, 0);
    }
}

/*======================================================================*/

void
ps_sharp_label (FILE * fp, short x1, short y1)
/*************************************************************/
/*  ps_sharp_label                                           */
/*                                                           */
/*  Writes SHARP label on page at specified (x,y)            */
/*************************************************************/
{
  char st[80];

  strcpy (st, "Output produced by Unidata postscript driver:");
  ps_text (fp, st, x1 + 25, y1 + 80, 5, 3);
  ps_moveto (fp, x1 + 25, y1 + 80);
  fprintf (fp, " (%s)\n", st);
  fprintf (fp, "true charpath pathbbox 40 add /uy exch def pop pop pop\n");

  strcpy (st, "NSHARP (SkewT-Hodograph Analysis and Research Program)");
  ps_text (fp, st, x1 + 25, y1 + 160, 5, 3);
  ps_moveto (fp, x1 + 25, y1 + 160);
  fprintf (fp, " (%s)\n", st);
  fprintf (fp,
	   "true charpath pathbbox pop 60 add /ux exch def 60 sub /ly exch def 60 sub /lx exch def\n");
  fprintf (fp, "N lx ly M ux ly L ux uy L lx uy L lx ly L stroke\n");

  /*strcpy( st, "   ");
     ps_text( fp, st, x1 + 25, y1 + 240, 5, 3);

     ps_select_pen( fp, 1, .3F, 9 );
     ps_rectangle( fp, x1, y1, x1 + 2195, y1 + 280, 0, 0 ); */
}

/*====================================================================*/

void
ps_write_winds (FILE * fp)
/*************************************************************/
/*  ps_write_winds                                           */
/*                                                           */
/*  Writes wind data to printer.                             */
/*************************************************************/
{
  char st[80];
  short x1, y1;
  float ix1, ix2, ix3, ix4, tmpflt;
  struct _parcel pcl;

/*-------------------------------------------------------------------*/

  x1 = 3500;
  y1 = hsk.bry + 500;

  ps_cliprgn (fp, 20000, 0, 0, 20000);
  ps_centertext (fp, "KINEMATIC PARAMETERS", x1 + 1500, y1, 12, 3);
  ps_select_pen (fp, 1, .4F, 9);
  ps_rectangle (fp, x1 - 50, y1 + 20, x1 + 3000, 7700, 0, 0);

  /* ----- 0-6km Mean Wind ----- */
  y1 += 200;
  ps_text (fp, "Sfc - 6 km Mean Wind:", x1, y1, 9, 0);
  mean_wind (-1, i_pres (agl (6000)), &ix1, &ix2, &ix3, &ix4);
  if (qc (ix3) && qc (ix4))
    {
      tmpflt = kt_to_mps(ix4);
      sprintf (st, "%d / %d kt   (%d m/s)", (short)ix3, (short)ix4,
	       (short)tmpflt);
    }
  else
    {
      sprintf (st, "M / M");
    }
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* ----- LFC-EL Mean Wind ----- */
  y1 += 150;
  ps_text (fp, "LFC - EL Mean Wind:", x1, y1, 9, 0);
  mean_wind (-1, -1, &ix1, &ix2, &ix3, &ix4);
  if (qc (ix3) && qc (ix4))
    {
      tmpflt = kt_to_mps(ix4);
      sprintf (st, "%d / %d kt   (%d m/s)", (short) ix3, (short) ix4,
	       (short)tmpflt);
    }
  else
    {
      sprintf (st, "M / M");
    }
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  /* ----- 850-300 Mean Wind ----- */
  y1 += 150;
  ps_text (fp, "850 - 300 Mean Wind:", x1, y1, 9, 0);
  mean_wind (850, 300, &ix1, &ix2, &ix3, &ix4);
  if (qc (ix3) && qc (ix4))
    {
      tmpflt = kt_to_mps(ix4);
      sprintf (st, "%d / %d kt   (%d m/s)", (short) ix3, (short) ix4,
	       (short)tmpflt);
    }
  else
    {
      sprintf (st, "M / M");
    }
  ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);

  y1 += 200;
  ps_moveto (fp, x1, y1);
  ps_lineto (fp, x1 + 3000, y1);

  /* ----- Sfc-2km Shear ----- */
  y1 += 150;
  wind_shear (-1, i_pres (msl (2000)), &ix1, &ix2, &ix3, &ix4);
  ps_text (fp, "Sfc - 2km Shear:", x1, y1, 9, 0);
  if (qc (ix3) && qc (ix4))
    {
      sprintf (st, "%d kt", (short)ix4);
      ps_disp_param (fp, st, x1 + 1700, y1, 9, 0);
      tmpflt = kt_to_mps(ix4);
      sprintf (st, "(%d m/s)", (short)tmpflt);
      ps_disp_param (fp, st, x1 + 2500, y1, 9, 0);
    }
  else
    {
      strcpy (st, "M");
      ps_disp_param (fp, st, x1 + 1700, y1, 9, 0);
    }

  /* ----- Sfc-6km Shear ----- */
  y1 += 150;
  wind_shear (-1, i_pres (msl (6000)), &ix1, &ix2, &ix3, &ix4);
  ps_text (fp, "Sfc - 6km Shear:", x1, y1, 9, 0);
  if (qc (ix3) && qc (ix4))
    {
      sprintf (st, "%d kt", (short)ix4);
      ps_disp_param (fp, st, x1 + 1700, y1, 9, 0);
      tmpflt = kt_to_mps(ix4);
      sprintf (st, "(%d m/s)", (short)tmpflt);
      ps_disp_param (fp, st, x1 + 2500, y1, 9, 0);
    }
  else
    {
      strcpy (st, "M");
      ps_disp_param (fp, st, x1 + 1700, y1, 9, 0);
    }

  /* ----- BRN Shear ----- */
  y1 += 150;
  pcl.bplus = 1;
  pcl.lplpres = sndgp->lplvals.pres;
  ix2 = bulk_rich (pcl, &ix1);
  ps_text (fp, "*BRN Shear:", x1, y1, 9, 0);
  if (qc (ix3) && qc (ix4))
    {
      sprintf (st, "%d m2/s2", (short) ix1);
      ps_disp_param (fp, st, x1 + 2500, y1, 9, 0);
    }
  else
    {
      strcpy (st, "M");
      ps_disp_param (fp, st, x1 + 2100, y1, 9, 0);
    }
}

/*======================================================================*/

void
ps_write_storm (FILE * fp)
/*************************************************************/
/*  ps_write_storm                                           */
/*                                                           */
/*  Writes storm data to printer.                            */
/*************************************************************/
{
  char st[80], st1[80];
  short x1, y1;
  float sfctemp, sfcdwpt, sfcpres, hel3;
  float ix1, ix2, ix3, ix4;
  struct _parcel pcl;

  sfctemp = sndgp->lplvals.temp;
  sfcdwpt = sndgp->lplvals.dwpt;
  sfcpres = sndgp->lplvals.pres;
  parcel (-1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

  x1 = 3500;
  y1 = 8000;

  ps_cliprgn (fp, 20000, 0, 0, 20000);
  ps_centertext (fp, "STORM STRUCTURE PARAMETERS", x1 + 1500, y1, 12, 3);
  ps_select_pen (fp, 1, .4F, 9);
  ps_rectangle (fp, x1 - 50, y1 + 20, x1 + 3000, 9200, 0, 0);

  /* ----- Sfc-3km SREH ----- */
  y1 += 200;
  ix1 = helicity (0, 3000, sndgp->st_dir, sndgp->st_spd, &ix2, &ix3);
  hel3 = ix1;
  ps_text (fp, "Sfc - 3km SREH:", x1, y1, 9, 0);
  if (qc (ix1))
    {
      sprintf (st, "%.0f m2/s2", ix1);
      ps_disp_param (fp, st, x1 + 1700, y1, 9, 0);
    }
  else
    {
      strcpy (st, "M");
      ps_disp_param (fp, st, x1 + 1700, y1, 9, 0);
    }

  /* ----- Effective SREH ----- */
  y1 += 150;
  ix1 = helicity (-1, -1, sndgp->st_dir, sndgp->st_spd, &ix2, &ix3);
  ps_text (fp, "Effective SREH:", x1, y1, 9, 0);
  if (qc (ix1))
    {
      sprintf (st, "%.0f m2/s2", ix1);
      sprintf (st1, "from %.0f m.", agl (i_hght (esfc (50))));
      ps_disp_param (fp, st, x1 + 1700, y1, 9, 0);
      ps_disp_param (fp, st1, x1 + 2600, y1, 9, 0);
    }
  else
    {
      strcpy (st, "M");
      ps_disp_param (fp, st, x1 + 1700, y1, 9, 0);
    }

  y1 += 200;
  ps_moveto (fp, x1, y1);
  ps_lineto (fp, x1 + 3000, y1);

  /* ----- 0-2 km Storm Relative Winds ----- */
  y1 += 150;
  ps_text (fp, "0-2 km SRW:", x1, y1, 9, 0);
  sr_wind (-1, i_pres (msl (2000)), sndgp->st_dir, sndgp->st_spd, &ix1, &ix2,
	   &ix3, &ix4);
  if (qc (ix4))
    {
      sprintf (st, "%.0f kt", ix4);
      ps_disp_param (fp, st, x1 + 1300, y1, 9, 0);
    }
  else
    {
      ps_disp_param (fp, "M", x1 + 1300, y1, 9, 0);
    }

  /* ----- EHI ----- */
  ps_text (fp, "EHI:", x1 + 1700, y1, 9, 0);
  ix1 = ehi (pcl.bplus, hel3);
  if (qc (ix1))
    {
      sprintf (st, "%.1f", ix1);
      ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);
    }
  else
    {
      ps_disp_param (fp, "M", x1 + 2900, y1, 9, 0);
    }

  /* ----- 4-6 km Storm Relative Winds ----- */
  y1 += 150;
  ps_text (fp, "4-6 km SRW:", x1, y1, 9, 0);
  sr_wind (i_pres (msl (4000)), i_pres (msl (6000)), sndgp->st_dir,
	   sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
  if (qc (ix4))
    {
      sprintf (st, "%.0f kt", ix4);
      ps_disp_param (fp, st, x1 + 1300, y1, 9, 0);
    }
  else
    {
      ps_disp_param (fp, "M", x1 + 1300, y1, 9, 0);
    }

  /* ----- BRN ----- */
  ps_text (fp, "BRN:", x1 + 1700, y1, 9, 0);
  ix1 = bulk_rich (pcl, &ix2);
  if (qc (ix1))
    {
      sprintf (st, "%.0f", ix1);
      ps_disp_param (fp, st, x1 + 2900, y1, 9, 0);
    }
  else
    {
      ps_disp_param (fp, "M", x1 + 2900, y1, 9, 0);
    }

  /* ----- 6-10 km Storm Relative Winds ----- */
  y1 += 150;
  ps_text (fp, "6-10 km SRW:", x1, y1, 9, 0);
  sr_wind (i_pres (msl (6000)), i_pres (msl (10000)), sndgp->st_dir,
	   sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
  if (qc (ix4))
    {
      sprintf (st, "%.0f kt", ix4);
      ps_disp_param (fp, st, x1 + 1300, y1, 9, 0);
    }
  else
    {
      ps_disp_param (fp, "M", x1 + 1300, y1, 9, 0);
    }
}

/*======================================================================*/

void
ps_plot_uvvs (FILE * fp)
/*************************************************************/
/*  ps_plot_uvvs                                             */
/*                                                           */
/*  Plots VVs (Mbps) on right side of SkewT.                 */
/*************************************************************/
{
  short i, x, y, x1, avail;
  float leng, scal;

  scal = 1000;

  ps_cliprgn (fp, 20000, 0, 0, 20000);
  ps_select_pen (fp, 1, .25F, 9);

  x1 = hsk.brx + 1900;

  /* ----- Plot every level ----- */
  avail = 0;
  for (i = 0; i < sndgp->numlev; i++)
    {
      if (qc (sndgp->sndg[i].omega) && (sndgp->sndg[i].omega < 1))
	{
	  avail++;
	  y = ps_pres_to_pix (sndgp->sndg[i].pres);
	  leng = (sndgp->sndg[i].omega * scal);	/* Convert to Mbs/sec */
	  ps_select_pen (fp, 1, .05F, 9);
	  if (abs (leng) > 5)
	    ps_select_pen (fp, 1, .10F, 9);
	  if (abs (leng) > 10)
	    ps_select_pen (fp, 1, .35F, 9);
	  if (abs (leng) > 15)
	    ps_select_pen (fp, 1, .50F, 9);
	  x = x1 - (leng * 15);	/* Determine screen scale */
	  ps_moveto (fp, x1, y);
	  ps_lineto (fp, x, y);
	}
    }

  if (avail > 5)
    {
      /* ----- Draw base legend ----- */

      ps_select_pen (fp, 1, .20F, 9);
      ps_moveto (fp, x1, hsk.tly);
      ps_lineto (fp, x1, hsk.bry);

      ps_select_pen (fp, 1, .10F, 11);
      ps_moveto (fp, x1 + 150, hsk.tly);
      ps_lineto (fp, x1 + 150, hsk.bry);

      ps_select_pen (fp, 1, .10F, 11);
      ps_moveto (fp, x1 - 150, hsk.tly);
      ps_lineto (fp, x1 - 150, hsk.bry);

      ps_select_pen (fp, 1, .20F, 9);
      ps_moveto (fp, x1 - 300, hsk.tly);
      ps_lineto (fp, x1 + 300, hsk.tly);

      ps_centertext (fp, "VVEL (Mbps)", x1, hsk.tly - 95, 7, 3);
      ps_centertext (fp, "Down     Up   ", x1, hsk.tly - 15, 7, 3);
    }
}

/*======================================================================*/

void
_sounding_plot (FILE * fp)
{
  ps_draw_skewt (fp);
  ps_select_pen (fp, 1, 1, 9);
  ps_draw_hodo (fp);
  ps_write_parcel (fp);
  ps_write_thermo (fp);
  ps_write_winds (fp);
  ps_write_storm (fp);
  ps_plot_uvvs (fp);
  ps_sharp_label (fp, 75, 9845);
}

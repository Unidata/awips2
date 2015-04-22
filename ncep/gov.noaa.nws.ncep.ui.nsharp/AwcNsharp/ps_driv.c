/***************************************************************/
/*  NSHARP                                                     */
/*  Postscript driver routine                                  */
/*  Adapted from hpgl.c driver                                 */
/*                                                             */
/*  Unidata/UCAR	Steve Chiswell	3/99                   */
/*                                                             */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  PRINT_SOUNDING                                             */
/*  PSDRAW_SKEWT                                               */
/*  OPEN_PRINTER                                               */
/*  PS_SETUP                                                   */
/*  PS_CLOSE                                                   */
/*  PSMOVETO                                                   */
/*  PSTEXT                                                     */
/*  HPSELECT_PEN                                               */
/*  HPLINETO                                                   */
/*  HPRECTANGLE                                                */
/*  HPCLIPRGN                                                  */
/*  HPISOBAR                                                   */
/*  HPISOTHERM                                                 */
/*  HPPRES_TO_PIX                                              */
/*  HPTEMP_TO_PIX                                              */
/*  HPDRY_ADIABAT                                              */
/*  HPTRACE_TEMP                                               */
/*  HPTRACE_VTMP                                               */
/*  HPTRACE_DWPT                                               */
/*  HPTRACE_WETBULB                                            */
/*  HPWIND_BARB                                                */
/*  HPPLOT_BARBS                                               */
/*  HPTRACE_PARCEL                                             */
/*  HPTRACE_PARCELX                                            */
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
#include "gui.h"
#include "sharp95.h"

/*
 *	Private functions
 */
void psdraw_skewt ( FILE *fp );
void psmoveto ( FILE *fp , short x, short y );
void pstextc ( FILE *fp, char *textstr, short x, short y,
			short pts, short bold );
void ps_close ( FILE *fp );
void ps_setup ( FILE *fp );


extern int nnpage;

static int text_justify=0;
static int Pfont;
static int linewrap;
int pscolor;

struct _skewt hsk = { 500, 850, 5636, 5986, 100, 70, 60 , 1 };
struct _hodog hho = { 3500, 850, 5636, 2950, -25, -25, 120, 20 };

/*===================================================================*/

void print_sounding_ps ( int COLRMODE )
	/*************************************************************/
	/*  PRINT_SOUNDING                                           */
	/*                                                           */
	/*  Prints skewt/hodograph to LPT1.                          */
	/*************************************************************/
	{
	FILE *fp;
	char	unixcmd[200];
    char OS[20];
	pscolor = COLRMODE;

	fp = open_printer();
	if(fp==NULL)
	   {
	   printf("failed to open postscript file\n");
	   return;
	   }


	ps_setup( fp );
	psdraw_skewt( fp );
	hpselect_pen( fp, 1, 1, 9 );
	hpdraw_hodo( fp );
	hpwrite_parcel( fp ); 
	hpwrite_thermo( fp ); 
	hpwrite_winds( fp );
	hpwrite_storm( fp );
	hpplot_uvvs( fp );
	/* sharp_label( fp, 75, 9845 ); */
	sharp_label (fp, 3500, 9650);
	/* sharp_label( fp, 4305, 9620 ); */

	ps_close( fp );
	/* Resize Postscript to Letter size Paper */
	sprintf(unixcmd,"psresize -PA4 -pletter %s /tmp/.nsharprsz.out",config.filename);
	system(unixcmd);
	strcpy(OS,getenv("OS"));
    if (strncmp(OS,"Linux",5)==0) 
       sprintf(unixcmd,"lpr %s", "/tmp/.nsharprsz.out");
    else
       sprintf(unixcmd,"lp -onb -oletter %s", "/tmp/.nsharprsz.out");
    system(unixcmd);
	sprintf(unixcmd,"rm -f %s; rm -f /tmp/.nsharprsz.out", config.filename );
	system(unixcmd);
	return;
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

/*=========================================================================*/
FILE *open_printer ( void )
/************************************************************************
 *  OPEN_PRINTER                                             		*
 *                                                           		*
 *  Opens stream for output to printer.  Returns FILE handle.		*
 *                                                           		*
 *  fin        - Name of file to open                        		*
 **									*
 * T. Piper/SAIC	01/04	changed mktemp to mkstemp		*
 ***********************************************************************/
{
        char *tempnam;
	FILE *f1=NULL;
        tempnam = (char *)malloc(20);
        tempnam[0] = '\0';
        sprintf(tempnam,"/tmp/.nsharp_XXXXXX");
        mkstemp(tempnam);

        strcpy(config.filename, tempnam);
        free(tempnam);
	f1 = fopen( config.filename, "w" );
	return f1;
}

/*=========================================================================*/

void ps_setup ( FILE *fp )
	/*************************************************************/
	/*  PS_SETUP                                                 */
	/*                                                           */
	/*  fp         - File handle                                 */
	/*************************************************************/
	{
/*      Write prolog commands to file. */

                fprintf ( fp, "%%!PS-Adobe-1.0\n");
                fprintf ( fp, "%%%%Title:\n" );
                fprintf ( fp, "%%%%Creator: GEMPAK\n" );
                fprintf ( fp, "%%%%EndComments\n");
                fprintf ( fp, "%%%%Pages : (atend)\n" );

                fprintf ( fp, "/M {moveto} def\n");
                fprintf ( fp, "/L {lineto} def\n");
                fprintf ( fp, "/N {newpath} def\n");
                fprintf ( fp, "/LW {setlinewidth} def\n");
                fprintf ( fp, "/AF {arc fill} def\n");

                fprintf ( fp, "/MS  {makefont setfont} def\n");
                fprintf ( fp, "/FF {findfont} def\n");
                fprintf ( fp, "/CF {currentfont} def\n");
                fprintf ( fp, "/FL {closepath fill} def\n");
                fprintf ( fp, "/RGB {setrgbcolor} def\n");

                fprintf ( fp, " 20 50 translate .075 .075 scale\n");
                fprintf ( fp, " 1 setlinecap 1 setlinejoin newpath\n");
                fprintf ( fp, "%%%%Page:    1  ?\n");

                nnpage = 1;
                Pfont = 0;
		linewrap = 0;

	}

	/*NP*/
	void hpselect_pen ( FILE *fp , short onoff, float width, short type )
	/*************************************************************/
	/*  HPSELECT_PEN                                             */
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
        int irwdth,rgbcolor;

        if(pscolor != 1)
           rgbcolor = 1;
        else
           rgbcolor = (int)onoff;
        switch(rgbcolor)
           {
           case 0:
	      red = 1; green = 1; blue = 1;
              break;
           case 1:
              red = 0; green = 0; blue = 0;
              break;
           case 2:
              red = .7; green = 0; blue = 0;
              break;
           case 3:
              red = 0; green = .7; blue = 0;
              break;
           case 4:
              red = 0; green = 0; blue = .7;
              break;
           case 5:
              red = .7; green = .7; blue = 0;
              break;
           case 6:
              red = 0; green = .7; blue = .7;
              break;
           default:
              red = 0; green = 0; blue = 0;
           }

        switch((int)type)
           {
           case 2:
               fprintf ( fp, "[40] 0 setdash\n");
               break;
           case 10:
               fprintf ( fp, "[40 40 5 50] 0 setdash\n");
               break;
           case 11:
               fprintf ( fp, "[20] 0 setdash\n");
               break;
           case 9:
               fprintf ( fp, "[] 0 setdash\n");
               break;
		   case 12:
			   fprintf ( fp, "[10 10] 0 setdash\n");
			   break;
		   case -2:
		       fprintf ( fp, "[5 5] 0 setdash\n");
			   break;
           default:
               printf("Unknown line type, defaulting to solid %d\n",type);
               fprintf ( fp, "[] 0 setdash\n");
           }
 
 		/* ----- Set Pen Type ----- */
	/*	if( type == 9 )
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
		if ( type == 12)
		   {
		   fputs( "UL8,2,2,2,2,2,2,2,2;", fp);
		   fputs( "LT8,1;", fp);
		   }
		if( type == -2 )
		   { fprintf( fp, "LT%d, 1;", type ); }
		if( type == -1 )
		   { fprintf( fp, "LT%d, 1;", type ); }  */
         
   
        irwdth = (int)(width*60);
        /*printf("look width %d color %f %f %f\n",irwdth,red,green,blue);*/
        if(irwdth < 1) irwdth = 1;
       
        fprintf ( fp, "%5.3f %5.3f %5.3f %s\n", red, green, blue, " RGB ");
        fprintf ( fp, "%4d LW\n", irwdth );

	}

	/*NP*/
	void psmoveto ( FILE *fp , short x, short y )
	/*************************************************************/
	/*  PSMOVETO                                                 */
	/*                                                           */
	/*  Moves cursor to given coordinates, newpath               */
	/*************************************************************/
	{
	y = (1016 * 11) - 1050 - y;
	fprintf( fp, "N %6d %6d M ", x, y);
	}

	/*NP*/
	void hplineto ( FILE *fp , short x, short y )
	/*************************************************************/
	/*  HPLINETO                                                 */
	/*                                                           */
	/*  lineto from current cursor location to (x,y)             */
	/*************************************************************/
	{
	y = (1016 * 11) - 1050 - y;
        fprintf( fp, "%5d %5d L ",x,y);
        linewrap++;
        if(linewrap >= 4)
           {
           fprintf( fp, "\n");
           linewrap = 0;
           }
	}

	/*NP*/
	void hprectangle ( FILE *fp , short x1, short y1, short x3, short y3,
			  short fill, short pct )
	/*************************************************************/
	/*  HPRECTANGLE                                              */
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

        if(fill == 1)
           {
           grayf = ((float)(100. - pct))/100.;
           fprintf(fp,"%5.3f %5.3f %5.3f RGB\n",grayf,grayf,grayf);
           }
	psmoveto( fp, x1, y1);
	hplineto( fp, x2, y2);
	hplineto( fp, x3, y3);
	hplineto( fp, x4, y4);
	hplineto( fp, x1, y1);
        if(fill != 1)
           fprintf( fp,"stroke\n");
        else
           fprintf( fp,"FL\n");
	}

	/*NP*/
	void hpcliprgn ( FILE *fp , short x1, short y1, short x2, short y2 )
	/*************************************************************/
	/*  HPCLIPRGN                                                */
	/*                                                           */
	/*  Defines drawing area with points (x1,y1) and (x2,y2).    */
	/*************************************************************/
	{
	y1 = (1016 * 11) - 1050 - y1;
	y2 = (1016 * 11) - 1050 - y2;
      
        fprintf(fp,"initclip\n"); 
        fprintf(fp,"N %d %d M %d %d lineto %d %d lineto %d %d lineto %d %d lineto closepath clip N\n",
        x1,y1,x1,y2,x2,y2,x2,y1,x1,y1);
	}

	/*NP*/
	void ps_close ( FILE *fp )
	/*************************************************************/
	/*  PS_CLOSE                                                 */
	/*                                                           */
	/*************************************************************/
	{
        fprintf ( fp, " gsave showpage grestore\n" );
	fprintf ( fp,
                    " -576 -126 translate -90 rotate 60.6796 60.6796 scale\n");

        fprintf ( fp, "%%%%Trailer:\n" );
        fprintf ( fp, "%%%%Pages: %5d\n", nnpage);

	fclose( fp );
	}

	/*NP*/
	void psdraw_skewt ( FILE *fp )
	/*************************************************************/
	/*  HPDRAW_SKEWT                                             */
	/*                                                           */
	/*  Draws a standard Skew-T/LogP diagram using postscript.   */
	/*************************************************************/
	{
	short i;
	float thta,thtae;
	float lvl[] = {1050,1000,850,700,500,300,200,100};
	float mixratios[] = {0.5,1,2,4,8,16,32};
	int mixratiocount=7;	
	char st[128];

	/* ----- Write Title Line at top of chart ----- */
	sprintf(st,"%s (%s)",raobtitle,raob_type);
	pstextc( fp, st, hsk.tlx, hsk.tly - 65, 12, 3);

	/* ----- Begin with outer boundary and frame ----- */
	hpselect_pen( fp, 1, .75F, 9 );
	hprectangle( fp, hsk.tlx, hsk.tly, hsk.brx, hsk.bry, 0, 0 );
	hpcliprgn(   fp, hsk.brx, hsk.tly, hsk.tlx, hsk.bry );

	hpfill_cape( fp, 20 );

	/* ----- Draw Horizontal Pressure Lines ----- */
	hpselect_pen( fp, 1, 0, 9 );
	for(i=1; i<=7; i++) { hpisobar( fp, lvl[i], 0 ); }
	for(i=150; i<1050; i += 50) { hpisobar( fp, (float)i, 1 ); }

	/* ----- Draw Skewed Temperature Lines ----- */
	hpselect_pen( fp, 1, 0, 2 );
	for( i=-160; i<=50; i += 10 ) { hpisotherm( fp, (float)i ); }

	/* ----- Draw Dry Adiabats ----- */
	hpselect_pen( fp, 1, 0, 9 );
	for( thta=-70; thta<=350; thta += 20) { hpdry_adiabat( fp, thta); };
	
	/* ----- Draw Moist Adiabats ---- */
	hpselect_pen( fp, 1, 0, -2 );
    for (thtae=-2+273.16;thtae<=38+273.16;thtae+=4) 
	  {hpmoist_adiabat( fp, thtae); };
	
	/* ----- Draw Mixing ratio Lines ---- */
	hpselect_pen( fp, 1, 0, 12);
	for (i=0;i<mixratiocount;i++) {hpmixingratio( fp, mixratios[i]);};


	/* ----- Plot Environmental Temperature Data ----- */
	hpselect_pen( fp, 2, .75F, 9 );
	hptrace_temp( fp );

	/* ----- Plot Environmental Dew Point Data ----- */
	hpselect_pen( fp, 3, .75F, 9 );
	hptrace_dwpt( fp );

	/* ----- Plot Environmental Virtual Temperature Data ----- */
	hpselect_pen( fp, 5, .25F, 11 );
	hptrace_vtmp( fp );

	/* ----- Plot Environmental Wet Bulb Temperature Data ----- */
	hpselect_pen( fp, 6, .25F, 11 );
	hptrace_wetbulb( fp );

	/* ----- Plot Environmental Wind Barbs ----- */
	hpselect_pen( fp, 1, .25F, 9 );
	hpplot_barbs( fp );

	/* ----- Plot Elevation legend ----- */
	hpselect_pen( fp, 1, .25F, 9 );
	hpplot_elevs( fp );

	/* ----- Trace Lifted Parcel ----- */
	hpselect_pen( fp, 4, .35F, 10 );
	hptrace_parcel( fp, lplvals.pres, lplvals.temp, lplvals.dwpt);
	}

/*============================================================================*/

void hpisobar ( FILE *fp, float pres, short flag )
	/*************************************************************/
	/*  HPISOBAR                                                 */
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
	   pstextc( fp, st, hsk.tlx - 355, y + 60, 10, 0 );
	   hpcliprgn( fp , hsk.brx, hsk.tly, hsk.tlx, hsk.bry );

	   psmoveto( fp, hsk.tlx, y);
	   hplineto( fp, hsk.brx, y);
           fprintf(fp," stroke\n");
	   }
	else
	   {
	   psmoveto( fp, hsk.tlx, y);
	   hplineto( fp, hsk.tlx + 100, y);
           fprintf(fp," stroke\n");
	   psmoveto( fp, hsk.brx, y);
	   hplineto( fp, hsk.brx - 100, y);
           fprintf(fp," stroke\n");
	   }
	}

	/*NP*/
	void hpisotherm ( FILE *fp, float temp )
	/*************************************************************/
	/*  HPISOTHERM                                               */
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
	   pstextc( fp, st, x1, y1 + 160, 10, 0 );
	   hpcliprgn( fp , hsk.brx, hsk.tly, hsk.tlx, hsk.bry );
	   }
	x1 = hptemp_to_pix( temp, 1050 );
	y1 = hsk.bry;
	psmoveto( fp, x1, y1 );
	x2 = hptemp_to_pix( temp, 100 );
	y2 = hsk.tly;
	hplineto( fp, x2, y2 );
        fprintf(fp," stroke\n");
	}

	/*NP*/
	short hppres_to_pix ( float pres )
	/*************************************************************/
	/*  HPPRES_TO_PIX                                            */
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
	short hptemp_to_pix ( float temp, float pres )
	/*************************************************************/
	/*  HPTEMP_TO_PIX                                            */
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
	void hpdry_adiabat ( FILE *fp, float thta )
	/*************************************************************/
	/*  DRY_ADIABAT                                              */
	/*                                                           */
	/*  Draws dry adiabat of theta (thta, c) on SkewT graphic.   */
	/*************************************************************/
	{
	float pres, temp;
	short x, y;
        int ok=0;

        fprintf(fp,"%% Dry Adiabat %5.1f\n",thta);
	for( pres = 1050; pres >= 100; pres = pres - 50 )
	   {
	   temp = ((thta + 273.15) / pow( 1000.0 / pres, ROCP )) - 273.15;

	   x = hptemp_to_pix( temp , pres );
	   y = hppres_to_pix( pres );

	   if(pres == 1050)
	      { psmoveto( fp, x, y ); }
	   else
	      { hplineto( fp, x, y ); ok++; }
	   }
        if(ok > 0) fprintf(fp," stroke\n");
	}
	
	void hpmoist_adiabat(FILE *fp, float thtae)
	/**************************************************************/
	/* Draw moist adiabat of constant thetae from surface to top. */
	/**************************************************************/
	{
	  float pres, startpres, temp,t1,t2;
	  short x,y;
	  int ok = 0;
	  /* start at pres 1000 */
	  startpres=1000;
	  t1=thtae-273.16;
	  x=hptemp_to_pix(virtemp(startpres,t1,t1),startpres);
	  y=hppres_to_pix(startpres);
	  psmoveto(fp, x,y);
	  for(pres =950; pres>=100;pres-=50) {
	    t2=wetlift(startpres,t1,pres);
	    x=hptemp_to_pix(virtemp(pres,t2,t2),pres);
	    y=hppres_to_pix(pres);
	    hplineto(fp,x,y); 
		ok++;
 	  }
	  if(ok > 0) fprintf(fp," stroke\n");
 	}
 	
 	void hpmixingratio(FILE *fp, float w)
	/*****************************************************************/
	/* Draw line of constant mixing ratio from sfc to top of sounding*/
	/*****************************************************************/
	{
      float pres, startpres, temp,t1,t2;
	  short x,y;
	  int ok=0;
	  for (pres=1000;pres>=100;pres-=50) {
	    t1=temp_at_mixrat(w,pres);
	    x=hptemp_to_pix(t1,pres);
	    y=hppres_to_pix(pres);	
	    if (pres==1000)
	      psmoveto(fp,x,y);
	    else
	      { hplineto(fp,x,y); ok++; }
	  }
	  if(ok > 0) fprintf(fp," stroke\n");
	}

	/*NP*/
	void hptrace_temp ( FILE *fp )
	/*************************************************************/
	/*  HPTRACE_TEMP                                             */
	/*                                                           */
	/*  Plots environmental temperature trace on SkewT.          */
	/*************************************************************/
	{
	short i, x, y, ok = 0;

	for( i=0; i < numlvl; i++)
	   {
	   if( sndg[i][3] > -200)
	      {
	      x = hptemp_to_pix( sndg[i][3], sndg[i][1] );
	      y = hppres_to_pix( sndg[i][1] );
	      if( ok == 0)
		 { psmoveto( fp, x, y); ok = 1; }
	      else
		 { hplineto( fp, x, y); ok++; }
	      }
	   }
        if(ok > 1) fprintf(fp," stroke\n");
	}

	/*NP*/
	void hptrace_vtmp ( FILE *fp )
	/*************************************************************/
	/*  HPTRACE_VTMP                                             */
	/*                                                           */
	/*  Plots environmental virtual temp trace on SkewT.         */
	/*************************************************************/
	{
	short i, x, y, ok = 0;

	for( i=0; i < numlvl; i++)
	   {
	   if( sndg[i][3] > -200)
	      {
	      x = hptemp_to_pix( i_vtmp(sndg[i][1]), sndg[i][1] );
	      y = hppres_to_pix( sndg[i][1] );
	      if( ok == 0)
		 { psmoveto( fp, x, y); ok = 1; }
	      else
		 { hplineto( fp, x, y); ok++;}
	      }
	   }
        if(ok > 1) fprintf(fp," stroke\n");
	}

/*====================================================================*/

void hptrace_dwpt ( FILE *fp )
	/*************************************************************/
	/*  HPTRACE_DWPT                                             */
	/*                                                           */
	/*  Plots environmental Dew Point trace on SkewT.            */
	/*************************************************************/
	{
	short i, x, y, ok = 0;

	for( i=0; i < numlvl; i++)
	   {
	   if( sndg[i][4] > -200)
	      {
	      x = hptemp_to_pix( sndg[i][4], sndg[i][1] );
	      y = hppres_to_pix( sndg[i][1] );
	      if( ok == 0)
		 { psmoveto( fp, x, y); ok = 1; }
	      else
		 { hplineto( fp, x, y); ok++;}
	      }
	   }
        if(ok > 1) fprintf(fp," stroke\n");
	}

/*=========================================================================*/

void hptrace_wetbulb ( FILE *fp )
	/*************************************************************/
	/*  HPTRACE_WETBULB                                          */
	/*                                                           */
	/*  Plots environmental Wet Bulb trace on SkewT.             */
	/*************************************************************/
	{
	short i, x, y, ok = 0;
	float t1;

	for( i=0; i < numlvl; i++)
	   {
	   if( sndg[i][4] > -200)
	      {

	      t1 = wetbulb( sndg[i][1], sndg[i][3], sndg[i][4] );
	      x = hptemp_to_pix( t1, sndg[i][1] );
	      y = hppres_to_pix( sndg[i][1] );
	      if( ok == 0)
		 { psmoveto( fp, x, y); ok = 1; }
	      else
		 { hplineto( fp, x, y); ok++;}
	      }
	   }
       if(ok > 1) fprintf(fp," stroke\n");
	}

/*============================================================================*/

void pstextc ( FILE *fp, char *textstr, short x, short y, short pts, short bold )
	/*************************************************************/
	/*  pstextc                                                */
	/*                                                           */
	/*  Writes given string at location (x,y).                   */
	/*  pts         -  Number of points for given font           */
	/*  bold        -  Typeface indicator (0=NORM, 3=BOLD)       */
	/*************************************************************/
	{
        int iftyp1, iftyp2;
	int font=11;
        float size=1.0;
	static char     *pfonts[12] = {
                              "Courier", "Helvetica", "Times-Roman",
                              "Courier-Oblique", "Helvetica-Oblique",
                              "Times-Italic", "Courier-Bold",
                              "Helvetica-Bold", "Times-Bold",
                              "Courier-BoldOblique",
                              "Helvetica-BoldOblique",
                              "Times-BoldItalic"
                              };
	static float Psize=.5;

        iftyp1 = font / 10;
        iftyp2 = font % 10;
        if  ( ( iftyp1 < 0 ) || ( iftyp1 > 3 ) ) iftyp1 = 0;
        if  ( ( iftyp2 < 1 ) || ( iftyp2 > 3 ) ) iftyp2 = 1;

        font = iftyp1 * 3 + iftyp2;
        if( ( Pfont != font) || ( Psize != size) ){
            fprintf ( fp, "/%s FF\n", pfonts[font -1]);
            fprintf ( fp, "[%10.3f 0 0%10.3f 0 0] MS\n",
                                 size*110, size*110);
            Pfont = font;
            Psize = size;
            }

	psmoveto(fp, x, y);

        /*      Write out the Text. */

        switch(text_justify)
           {
           case 1: /* right justified */
                   fprintf ( fp, "(%s) dup stringwidth pop neg 0 rmoveto show\n",
                      textstr);
                   break;
           case 2: /* cenetered text */
                   fprintf ( fp, "(%s) dup stringwidth pop neg 2 div 0 rmoveto show\n",
                      textstr);
                   break;
           default:
                    fprintf ( fp, "(%s) show\n", textstr);
           }


	}

/*======================================================================*/

void hpwind_barb ( FILE *fp, float wdir, float wspd, short x,
			  short y, short siz )
	/*************************************************************/
	/*  HPWIND_BARB                                              */
	/*                                                           */
	/*  Plots wind barb at location (x,y) for given wind.        */
	/*************************************************************/
	{
	short x1, y1, x2, y2, x3, y3, sped;
	float dx, dy, spcx, spcy, wid, hgt;

	dx = ucomp(wdir, 10) * siz / 1.5;
	dy = vcomp(wdir, 10) * siz / 1.5;

	x1 = x;
	y1 = y;
	x2 = x1 + (short)dx;
	y2 = y1 - (short)dy;

	/* ----- Draw backbone of wind barb, along with origin dot ----- */
	psmoveto(fp, x1, y1); hplineto(fp, x2, y2); fprintf(fp," stroke\n");

	sped = (short)wspd;
	x1 = x2;
	y1 = y2;

	wid = 6;                        /* Width of flags */
	spcx = dx / wid;
	spcy = dy / wid;
	x1 = x1 + (short)spcx;
	y1 = y1 - (short)spcy;

	/* ----- Draw wind flags (increments of 50kt) ----- */
	while (sped > 47)
	   {
	   x1 = x1 - (short)spcx;
	   y1 = y1 + (short)spcy;

	   hgt = .5F;                   /* Heigth of flags */
	   x2 = x1 + (short)(dy * hgt);
	   y2 = y1 + (short)(dx * hgt);

	   x3 = x1 - (short)spcx;
	   y3 = y1 + (short)spcy;

	   hptriangle( fp, x1, y1, x2, y2, x3, y3, 1);
	   /*psmoveto( fp, x1, y1);
	   hplineto( fp, x2, y2);
	   hplineto( fp, x3, y3);*/

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

	   psmoveto(fp, x3, y3); hplineto(fp, x2, y2); fprintf(fp," stroke\n");
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

	   psmoveto(fp, x3, y3); hplineto(fp, x2, y2); fprintf(fp," stroke\n");

	   sped -= 10;
	   x1 = x3;
	   y1 = y3;
	   }
	}


	/*NP*/
	void hpplot_barbs ( FILE *fp )
	/*************************************************************/
	/*  HPPLOT_BARBS                                             */
	/*                                                           */
	/*  Plots wind barbs along side of thermo diagram.           */
	/*************************************************************/
	{
	short i, x, y;
	float lastpres;

	hpcliprgn( fp , 20000, 0, 0, 20000 );

	/* ----- Draw vertical line ----- */
	psmoveto( fp, hsk.brx + 500, hsk.tly);
	hplineto( fp, hsk.brx + 500, hsk.bry);
        fprintf(fp," stroke\n");

	lastpres = 1100;
	for( i = 0; i < numlvl; i++)
	   {
	   if( qc( sndg[i][5] ) && sndg[i][1] >= 100 )
	      {
	      y = hppres_to_pix( sndg[i][1] );
	      x = hsk.brx + 500;

	      if((sndg[i][2]-i_hght(lastpres)) > 400)
		 {
		 hpwind_barb( fp, sndg[i][5], sndg[i][6], x, y, 45);
		 lastpres = sndg[i][1];
		 }
	      }
	   }
	}

	/*NP*/
	void hptrace_parcel ( FILE *fp, float pres, float temp, float dwpt )
	/*************************************************************/
	/*  HPTRACE_PARCEL                                           */
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

        if((!qc(dwpt)) || (!qc(temp)) || (!qc(pres))) return;
           
	x = hptemp_to_pix( virtemp(pres, temp, dwpt), pres );
	y = hppres_to_pix( pres );
	psmoveto( fp, x, y );

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
        fprintf(fp," stroke\n");
	}

	/*NP*/
	void hptrace_parcelx ( FILE *fp, float pres, float temp, float dwpt )
	/*************************************************************/
	/*  HPTRACE_PARCELX                                          */
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
	psmoveto( fp, x, y );

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
	void hptriangle ( FILE *fp, short x1, short y1, short x2, short y2,
			 short x3, short y3, short fill )
	/*************************************************************/
	/*  HPTRIANGLE                                               */
	/*                                                           */
	/*  Draws triangle of (x,y) coordinates.                     */
	/*                                                           */
	/*  fill   -  Fill indicator (1=YES)                         */
	/*************************************************************/
	{
	psmoveto( fp, x1, y1);
	hplineto( fp, x2, y2);
	hplineto( fp, x3, y3);
        fprintf(fp," FL\n");

	}

	/*NP*/
	void hpdraw_hodo ( FILE *fp )
	/*************************************************************/
	/*  HPDRAW_HODO                                              */
	/*                                                           */
	/*  Draws a standard Hodograph diagram on a HP Laserjet      */
	/*  printer.  All graphic calls are HP-GL/2 dependent.       */
	/*************************************************************/
	{
	short x1, y1, i;
	char st[10];

	/* ----- Transparency mode off ----- */

	hpcliprgn(fp, hho.tlx, hho.tly, hho.brx, hho.bry);
	hprectangle( fp, hho.tlx, hho.tly, hho.brx, hho.bry, 1, 0);
	hpselect_pen( fp, 1, .75F, 9 );
	hprectangle( fp, hho.tlx, hho.tly, hho.brx, hho.bry, 0, 0);

	/* ----- Plot crosshairs ----- */
	hpselect_pen( fp, 1, .1F, 9 );
	hphodo_to_pix( 180, 60, &x1, &y1);
	psmoveto( fp, x1, hho.tly);
	hplineto( fp, x1, hho.bry);
        fprintf(fp," stroke\n");

	hphodo_to_pix( 270, 60, &x1, &y1);
	psmoveto( fp, hho.tlx, y1);
	hplineto( fp, hho.brx, y1);
        fprintf(fp," stroke\n");

	/* ----- Plot X-Coord hash marks ----- */
	for(i = hho.scale; i <= hho.hodomag; i = i + hho.scale)
	   {
	   hphodo_to_pix( 180, (float)i, &x1, &y1);
	   psmoveto( fp, x1-30, y1);
	   hplineto( fp, x1+30, y1);
           fprintf(fp," stroke\n");
	   itoa( i, st, 10 );
	   pstextc( fp, st, x1 + 50, y1 + 30, 6, 0);

	   hphodo_to_pix( 360, (float)i, &x1, &y1);
	   psmoveto( fp, x1-30, y1);
	   hplineto( fp, x1+30, y1);
           fprintf(fp," stroke\n");
	   itoa( i, st, 10 );
	   pstextc( fp, st, x1 + 50, y1 + 30, 6, 0);
	   }

	/* ----- Plot Y-Coord hash marks ----- */
	for(i = hho.scale; i <= hho.hodomag; i = i + hho.scale)
	   {
	   hphodo_to_pix( 90, (float)i, &x1, &y1);
	   psmoveto( fp, x1, y1 - 30);
	   hplineto( fp, x1, y1 + 30);
           fprintf(fp," stroke\n");
	   itoa( i, st, 10 );
	   pstextc( fp, st, x1 - 50, y1 + 80, 6, 0);

	   hphodo_to_pix( 270, (float)i, &x1, &y1);
	   psmoveto( fp, x1, y1 - 30);
	   hplineto( fp, x1, y1 + 30);
           fprintf(fp," stroke\n");
	   itoa( i, st, 10 );
	   pstextc( fp, st, x1 - 50, y1 + 80, 6, 0);
	   }

	hpselect_pen( fp, 1, .1F, 9 );
	hphodo_circs( fp );

	/* ----- Plot Hodograph (Shear Vectors) ----- */
	hpselect_pen( fp, 1, .15F, 9 );
	hptrace_hodo( fp );
	hpselect_pen( fp, 1, .15F, 9 );
	hplabel_hodo( fp );
	}

	/*NP*/
	void hphodo_to_pix ( float dir, float mag, short *x, short *y )
	/*************************************************************/
	/*  HPHODO_COORDS                                            */
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

	*x = (short)(midx - (ucomp( dir, mag ) * scle));
	*y = (short)(midy + (vcomp( dir, mag ) * scle));
	}

	/*NP*/
	void hptrace_hodo ( FILE *fp )
	/*************************************************************/
	/*  HPTRACE_HODO                                             */
	/*                                                           */
	/*  Plots environmental wind shear vectors on Hodograph.     */
	/*************************************************************/
	{
	short i, x, y, ok = 0;

	for( i=0; i < numlvl; i++)
	   {
	   if( qc(sndg[i][5]) && qc(sndg[i][6]))
	      {
	      hphodo_to_pix( sndg[i][5], sndg[i][6], &x, &y);
	      if( ok == 0)
		 {
		 psmoveto( fp, x, y);
		 ok = 1;
		 }
	      else
		 {
		 hplineto( fp, x, y );
                 ok++;
		 }
	      }
	   }
        if(ok > 1) fprintf(fp," stroke\n");
	}

	/*NP*/
	void hplabel_hodo ( FILE *fp )
	/*************************************************************/
	/*  HPLABEL_HODO                                             */
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
	   wdi = i_wdir( i_pres(msl( lvl[i]) ) );
	   wsp = i_wspd( i_pres(msl( lvl[i]) ) );
	   hphodo_to_pix( wdi, wsp, &x, &y);
	   sprintf( st, "%d", lvl[i]/1000);
	   pstextc( fp, st, x - 60, y - 60, 8, 3);
	   }
	
	/* ----- Display Storm Motion at Upper Left Corner ----- */
	if(qc(st_dir) && qc(st_spd))
	   { sprintf( st, "%d / %d kt", (short)st_dir, (short)st_spd); }
	else
	   { sprintf( st, "M / M"); }
	pstextc( fp, st, hho.tlx + 50, hho.tly+150, 9, 0);

	
	
	}

	/*NP*/
	void hpplot_elevs ( FILE *fp )
	/*************************************************************/
	/*  HPPLOT_ELEVS                                             */
	/*                                                           */
	/*  Plots elevations (m, ft) MSL on right side of SkewT.     */
	/*************************************************************/
	{
	short y;
	float hgt;
	char st[80];

	hpcliprgn( fp , 20000, 0, 0, 20000 );

	/* ----- Draw base legend ----- */
	psmoveto( fp, hsk.brx + 1200, hsk.tly);
	hplineto( fp, hsk.brx + 1200, hsk.bry);
        fprintf(fp," stroke\n");

	psmoveto( fp, hsk.brx + 900, hsk.tly);
	hplineto( fp, hsk.brx + 1500, hsk.tly);
        fprintf(fp," stroke\n");

	pstextc( fp, "MSL", hsk.brx + 1125, hsk.tly - 95, 7, 3);
	pstextc( fp, "kft  km", hsk.brx + 1000, hsk.tly - 15, 7, 3);

	/* ----- Find SFC level and plot ----- */
	y = hppres_to_pix( sndg[sfc()][1] );
	psmoveto( fp, hsk.brx + 1150, y);
	hplineto( fp, hsk.brx + 1250, y);
        fprintf(fp," stroke\n");
	sprintf( st, "%d m", (short)sndg[sfc()][2] );
	pstextc( fp, st, hsk.brx + 1300, y + 30, 7, 0);
	sprintf( st, "%d ft", (short)mtof(sndg[sfc()][2]) );
        hpdisp_param( fp, st, hsk.brx + 800, y + 30, 7, 0);
	/*pstextc( fp, st, hsk.brx + 800, y + 30, 7, 0);*/

	/* ----- Plot every 1km ----- */
	for( hgt=1000; hgt < sndg[numlvl-1][2]; hgt += 1000)
	   {
	   if(i_pres(hgt) > 100)
	      {
	      y = hppres_to_pix( i_pres( hgt ));
	      psmoveto( fp, hsk.brx + 1200, y);
	      hplineto( fp, hsk.brx + 1250, y);
              fprintf(fp," stroke\n");
	      sprintf( st, "%d", (short)(hgt / 1000) );
	      pstextc( fp, st, hsk.brx + 1300, y + 30, 7, 0);
	      }
	   }
	/* ----- Plot every 5kft ----- */
	for(hgt = 5000; hgt < mtof(sndg[numlvl-1][2]); hgt += 5000)
	   {
	   if(i_pres(ftom(hgt)) > 100)
	      {
	      y = hppres_to_pix( i_pres( ftom(hgt)));
	      psmoveto( fp, hsk.brx + 1150, y);
	      hplineto( fp, hsk.brx + 1200, y);
              fprintf(fp," stroke\n");
	      sprintf( st, "%d", (short)(hgt / 1000) );
	      pstextc( fp, st, hsk.brx + 1000, y + 30, 7, 0);
	      }
	   }
	}

	/*NP*/
	void hphodo_circs ( FILE *fp )
	/*************************************************************/
	/*  HPHODO_CIRCS                                             */
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
	void hpcircle ( FILE *fp, short x, short y, short radius )
	/*************************************************************/
	/*  HPCIRCLE                                                 */
	/*                                                           */
	/*  Draws a circle at (x,y) of radius (radius).              */
	/*************************************************************/
	{
	fprintf( fp, "N %d %d M %d %d %d 0 360 arc\n", x,y,x,y,radius );
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
	float sfctemp, sfcdwpt, sfcpres, p;
	struct _parcel pcl;

	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	x1 = 100;
	y1 = hsk.bry + 500;

	hpcliprgn( fp , 20000, 0, 0, 20000 );
	hpcentertext( fp, "THERMODYNAMIC PARAMETERS", x1 + 1500, y1, 12, 3);
	hpselect_pen( fp, 1, .4F, 9 );
	/* hprectangle( fp , x1 - 50, y1 + 20, x1 + 3000, 9800, 0, 0);*/
	hprectangle( fp , x1 - 50, y1 + 20, x1 + 3000, 9900, 0, 0);
	/* ----- Selected parcel type ----- */
	y1 += 200;
	sprintf( st, "%s", lplvals.desc);
	pstextc( fp, st, x1, y1, 9, 0);

	/* ----- Lifted Parcel Information ----- */
	y1 += 200;
	sprintf( st, "LPL:  %4.0fmb   %4.0fC/%4.0fC   %4.0fF/%4.0fF",
		 lplvals.pres,
		 lplvals.temp, lplvals.dwpt,
		 ctof(lplvals.temp), ctof(lplvals.dwpt));
	pstextc( fp, st, x1, y1, 9, 0);

	/* ----- Calculate Parcel Data ----- */
	parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- CAPE/LI ----- */
	y1 += 200;
	pstextc( fp, "CAPE:", x1, y1, 9, 0);
	strcpy( st, qc2( pcl.bplus, " J/kg", 0));
	hpdisp_param( fp, st, x1 + 1200, y1, 9, 0);

	pstextc( fp, "LI:", x1 + 1500, y1, 9, 0);
	strcpy( st, qc2( pcl.li5, " C @  500mb", 0));
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	/* ----- BFZL/LImin ----- */
	y1 += 150;
	pstextc( fp, "BFZL:", x1, y1, 9, 0);
	strcpy( st, qc2( pcl.bfzl, " J/kg", 0));
	hpdisp_param( fp, st, x1 + 1200, y1, 9, 0);

	pstextc( fp, "LImin:", x1 + 1500, y1, 9, 0);
	if( qc(pcl.limax))
	   {
	   strcpy( st, qc2( pcl.limax, " C @  ", 0));
	   strcat( st, qc2( pcl.limaxpres, "mb", 0));
	   }
	else
	   { strcpy( st, "M"); }
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);


	/* ----- CINH/CAP ----- */
	y1 += 150;
	pstextc( fp, "CINH:", x1, y1, 9, 0);
	strcpy( st, qc2( pcl.bminus, " J/kg", 0));
	hpdisp_param( fp, st, x1 + 1200, y1, 9, 0);

	pstextc( fp, "CAP:", x1 + 1500, y1, 9, 0);
	if( qc(pcl.cap))
	   {
	   strcpy( st, qc2( pcl.cap, " C @  ", 0));
	   strcat( st, qc2( pcl.cappres, "mb", 0));
	   }
	else
	   { strcpy( st, "M" ); }
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	/* ----- Header for LEVEL data ----- */
	y1 += 200;
/*	hpdisp_param( fp, "LEVEL", x1 + 400, y1, 9, 0);
	hpdisp_param( fp, "PRES", x1 + 1100, y1, 9, 0);
	hpdisp_param( fp, "HGT(AGL)", x1 + 2100, y1, 9, 0);
	hpdisp_param( fp, "TEMP", x1 + 2900, y1, 9, 0); */
	
	hpdisp_param( fp, "LEVEL", x1 + 400, y1, 9, 0);
	hpdisp_param( fp, "PRES", x1 + 1100, y1, 9, 0);
	hpdisp_param( fp, "HGT(AGL)", x1 + 1800, y1, 9, 0);
	hpdisp_param( fp, "HGT(MSL)", x1 + 2500, y1, 9, 0);
	hpdisp_param( fp, "TEMP", x1 + 2900, y1, 9, 0); 

	
	y1 += 20;
	hpselect_pen( fp, 1, .1F, 9 );
	psmoveto( fp, x1, y1 ); hplineto( fp, x1 + 2950, y1 );

	/* ----- LCL ----- */
	y1 += 150;
	p = pcl.lclpres;
	hplvldata( fp, p, "LCL", x1, y1);

	/* ----- LFC ----- */
	y1 += 150;
	p = pcl.lfcpres;
	hplvldata( fp, p, "LFC", x1, y1);
	strcpy( st, qc2(i_temp( p ), " C", 0));
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	/* ----- EL ----- */
	y1 += 150;
	p = pcl.elpres;
	hplvldata( fp, p, "EL", x1, y1);
	strcpy( st, qc2(i_temp( p ), " C", 0));
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	/* ----- MPL ----- */
	y1 += 150;
	p = pcl.mplpres;
	hplvldata( fp, p, "MPL", x1, y1);

	y1 += 50;
	hpselect_pen( fp, 1, .4F, 9 );
	psmoveto( fp, x1 - 50  , y1);
	hplineto( fp, x1 + 3000, y1);
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
	short x1, y1;
	float sfctemp, sfcdwpt, sfcpres, ix1, ix2, ix3;
	struct _parcel pcl;
	int fzgstatus,nparams;
	float params[2];
	
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	x1 = 100;
	y1 = 8406;

	/* ----- PW and Mean RH ----- */
	pstextc( fp, "Precip Water:", x1, y1, 9, 0);
	strcpy( st, qc2( precip_water( &ix1, -1, -1), " in", 2 ));
	hpdisp_param( fp, st, x1 + 1400, y1, 9, 0);
	pstextc( fp, "Mean RH:", x1 + 1600, y1, 9, 0);
	strcpy( st, qc2( mean_relhum( &ix1, -1, -1), " %", 0));
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	/* ----- Mean MR and Mean LRH ----- */
	y1 += 150;
	pstextc( fp, "Mean Q:", x1, y1, 9, 0);
	strcpy( st, qc2( mean_mixratio( &ix1, -1, -1), " g/kg", 1 ));
	hpdisp_param( fp, st, x1 + 1400, y1, 9, 0);
	pstextc( fp, "Mean LRH:", x1 + 1600, y1, 9, 0);
	strcpy( st, qc2( mean_relhum( &ix1, -1, sndg[sfc()][1] - 150), " %", 0));
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	/* ----- Top of Moist Layer ----- */
	y1 += 150;
	pstextc( fp, "Top of Moist Layer:", x1, y1, 9, 0);
	strcpy( st, qc2( top_moistlyr( &ix1 ), " mb", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( mtof(agl(i_hght(ix1))), " ft", 0 ));
	pstextc( fp, st, x1 + 1600, y1, 9, 0);

	y1 += 50;
	hpselect_pen( fp, 1, .4F, 9 );
	psmoveto( fp, x1 - 50  , y1);
	hplineto( fp, x1 + 3000, y1);

	/* ----- 700-500mb Lapse Rates ----- */
	y1 += 150;
	pstextc( fp, "700-500mb Lapse Rate:", x1, y1, 9, 0);
	strcpy( st, qc2( delta_t( &ix1 ), " C", 0 ));
	strcat( st, "  /  " );
	strcat( st, qc2( lapse_rate( &ix1, 700, 500 ), " C/km", 1 ));
	pstextc( fp, st, x1 + 1600, y1, 9, 0);

	/* ----- 850-500mb Lapse Rates ----- */
	y1 += 150;
	pstextc( fp, "850-500mb Lapse Rate:", x1, y1, 9, 0);
	strcpy( st, qc2( vert_tot( &ix1 ), " C", 0 ));
	strcat( st, "  /  " );
	strcat( st, qc2( lapse_rate( &ix1, 850, 500 ), " C/km", 1 ));
	pstextc( fp, st, x1 + 1600, y1, 9, 0);

	y1 += 50;
	psmoveto( fp, x1 - 50  , y1);
	hplineto( fp, x1 + 3000, y1);

	/* ----- Total-Totals and K-Index ----- */
	y1 += 150;
	pstextc( fp, "Total Totals:", x1, y1, 9, 0);
	strcpy( st,qc2( t_totals( &ix1, &ix2, &ix3 ), "", 0));
	hpdisp_param( fp, st, x1 + 1400, y1, 9, 0);
	pstextc( fp, "K-Index:", x1 + 1600, y1, 9, 0);
	strcpy( st, qc2( k_index( &ix1 ), "", 0));
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	/* -----SWEAT and Max Temp ----- */
	y1 += 150;
	pstextc( fp, "SWEAT Index:", x1, y1, 9, 0);
	strcpy( st, qc2( sweat_index( &ix1 ), "", 0));
	hpdisp_param( fp, st, x1 + 1400, y1, 9, 0);
	pstextc( fp, "Max Temp:", x1 + 1600, y1, 9, 0);
	strcpy( st, qc2( ctof(max_temp( &ix1, -1 )), " F", 0));
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	/* ----- ThetaE Diff and Convective Temp ----- */
	y1 += 150;
	pstextc( fp, "ThetaE Diff:", x1, y1, 9, 0);
	ix2 = ThetaE_diff(&ix1);
	strcpy( st, qc2( ix2, " C", 0 ));
	hpdisp_param( fp, st, x1 + 1400, y1, 9, 0);
	ix2 = old_cnvtv_temp( &ix1);
	pstextc( fp, "*Conv Temp(s):", x1 + 1600, y1, 9, 0);
	strcpy( st, qc2( ctof(ix2), "F/", 0 ));
	strcat( st, qc2(ctof(cnvtv_temp(&ix1, -50)),"F", 0));
	hpdisp_param( fp, st, x1 + 3000, y1, 9, 0);

	/* ----- FZL and WBZ ----- */
	y1 += 150;
/*	pstextc( fp, "FZG Level:", x1, y1, 9, 0);
	strcpy( st, qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 )))), " ft", 0 ));
	hpdisp_param( fp, st, x1 + 1400, y1, 9, 0);
	pstextc( fp, "WBZ Level:", x1 + 1600, y1, 9, 0);
	strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 )))), " ft", 0 ));
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0); */
	
	
	pstextc( fp, "WBZ Level:", x1, y1, 9, 0);
	strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 )))), " ft", 0 ));
	hpdisp_param( fp, st, x1 + 1400, y1, 9, 0); 
        pstextc( fp, "FZG Level:", x1+1600, y1, 9, 0);
	strcpy( st, qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 )))), " ft", 0 ));
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);
	

	/* ---- FZL MSL ----- */
	y1 += 150;
	fzgstatus=mult_temp_lvl(0,params,&nparams);
	if (fzgstatus<0) {
	  strcpy( st, "FZG LVL(MSL) = SNDG BLO FZG");
	  pstextc(fp, st, x1, y1, 9, 0);
	} else {
	  strcpy( st, "FZG LVL(MSL) = ");
	  strcat( st, qc2(mtof(i_hght(params[0])),"ft",0));
	  if( nparams==2) {
	    strcat(st,",");
	    strcat(st, qc2(mtof(i_hght(params[1])),"ft",0));
	  }
	  pstextc(fp, st, x1, y1, 9, 0);
	}
	}

/*====================================================================*/

void hpdisp_param ( FILE *fp, char *st, short x, short y, short font, 
								short tf )
	/*************************************************************/
	/*  HPDISP_PARAM                                             */
	/*                                                           */
	/*  Right justifies value at location x,y.                   */
	/*************************************************************/
	{
        text_justify = 1;
        fprintf(fp,"%% need to right justify %d\n",text_justify);
	pstextc( fp, st, x, y, font, tf);
        text_justify = 0;
	}

	/*NP*/
	void hpfill_cape ( FILE *fp, short pct )
	/*************************************************************/
	/*  HPFILL_CAPE                                              */
	/*                                                           */
	/*  Fills the positive area with grey area.                  */
	/*************************************************************/
	{
	short x, y, i, j, xs, ys;
	float t1, p1, t2, p2, sfcpres, sfctemp, sfcdwpt, pres;
        float grayf;
	struct _parcel pcl;

	sfcpres = lplvals.pres;
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;

	/* ----- Calculate Parcel Data ----- */
	parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	if( pcl.bplus <= 0 ) { return; }

	/* ----- Account for partial soundings ----- */
	if( !qc(pcl.elpres)) { pcl.elpres = sndg[numlvl-1][1]; }

	/* ----- Start at LFC ----- */
        fprintf(fp,"\n%% Plot cape\nN ");
	p1 = pcl.lfcpres;
	t1 = i_vtmp(pcl.lfcpres);
	x = hptemp_to_pix( t1, p1);
	y = hppres_to_pix( p1 );
	psmoveto( fp, x, y);
	xs = x;
	ys = y;

	/* ----- Trace up temperature line ----- */
	i=0;
	while(sndg[i][1] > pcl.lfcpres) { i++; }

	for( j=i; sndg[j][1] > pcl.elpres; j++)
	   {
	   if( qc(sndg[j][3]))
	      {
	      p1 = sndg[j][1];
	      t1 = i_vtmp( p1 );
	      x = hptemp_to_pix( t1, p1);
	      y = hppres_to_pix( p1 );
	      hplineto( fp, x, y);
	      }
	   }

        grayf = ((float)(100. - pct))/100.;
        fprintf(fp,"%5.3f %5.3f %5.3f RGB\n",grayf,grayf,grayf);
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
        fprintf(fp,"FL\n");

	}

	/*NP*/
	void hpfill_capex ( FILE *fp, short pct )
	/*************************************************************/
	/*  HPFILL_CAPEX                                             */
	/*                                                           */
	/*  Fills the positive area with grey area.                  */
	/*************************************************************/
	{
	short x, y, i, j, xs, ys;
	float t1, p1, t2, p2, sfcpres, sfctemp, sfcdwpt, pres;
	struct _parcel pcl;

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
	t1 = i_temp(pcl.lfcpres);
	x = hptemp_to_pix( t1, p1);
	y = hppres_to_pix( p1 );
	psmoveto( fp, x, y);
	xs = x;
	ys = y;

	/* ----- Trace up temperature line ----- */
	i=0;
	while(sndg[i][1] > pcl.lfcpres) { i++; }

	for( j=i; sndg[j][1] > pcl.elpres; j++)
	   {
	   if( qc(sndg[j][3]))
	      {
	      p1 = sndg[j][1];
	      t1 = i_temp( p1 );
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
	void hpcentertext ( FILE *fp, char *st, short x, short y, short font, short tf )
	/*************************************************************/
	/*  HPCENTERTEXT                                             */
	/*                                                           */
	/*  Writes given string at location (x,y).                   */
	/*  pts         -  Number of points for given font           */
	/*  bold        -  Typeface indicator (0=NORM, 3=BOLD)       */
	/*************************************************************/
	{
        fprintf(fp,"%% need to center text\n");
        text_justify = 2;
	pstextc( fp, st, x, y, font, tf);
        text_justify = 0;
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
	   /*hpdisp_param( fp, nam, x + 400, y, 9, 0);
	   strcpy( st, qc2( pres, "mb", 0 ));
	   hpdisp_param( fp, st, x + 1100, y, 9, 0);
	   strcpy( st, qc2( mtof(agl(i_hght(pres ))), "ft", 0 ));
	   hpdisp_param( fp, st, x + 2100, y, 9, 0);*/
	   
	   hpdisp_param( fp, nam, x + 400, y, 9, 0);
	   strcpy( st, qc2( pres, "mb", 0 ));
	   hpdisp_param( fp, st, x + 1100, y, 9, 0);
	   strcpy( st, qc2( mtof(agl(i_hght(pres ))), "ft", 0 ));
	   hpdisp_param( fp, st, x + 1800, y, 9, 0);
	   strcpy( st, qc2( mtof(i_hght(pres )), "ft", 0 ));
	   hpdisp_param(fp, st, x + 2500, y, 9, 0);
	   
	   }
	else
	   {
	   /*hpdisp_param( fp, nam, x + 400, y, 9, 0);
	   hpdisp_param( fp, "M", x + 1100, y, 9, 0);
	   hpdisp_param( fp, "M", x + 2100, y, 9, 0);*/
	   
	   hpdisp_param( fp, nam, x + 400, y, 9, 0);
	   hpdisp_param( fp, "M", x + 1100, y, 9, 0);
	   hpdisp_param( fp, "M", x + 1800, y, 9, 0);
	   hpdisp_param( fp, "M", x + 2500, y, 9, 0);
	  
	   }
	}



	/*NP*/
	void sharp_label ( FILE *fp, short x1, short y1 )
	/*************************************************************/
	/*  SHARP_LABEL                                              */
	/*                                                           */
	/*  Writes SHARP label on page at specified (x,y)            */
	/*************************************************************/
	{
	char st[80];

	strcpy( st, "Output produced by Unidata postscript driver:");
	pstextc( fp, st, x1 + 25, y1 + 80, 5, 3);
        psmoveto(fp,x1 + 25,y1 + 80);
        fprintf(fp," (%s)\n", st);
        fprintf(fp,"true charpath pathbbox 40 add /uy exch def pop pop pop\n");

	strcpy( st, "NSHARP (SkewT-Hodograph Analysis and Research Program)");
	pstextc( fp, st, x1 + 25, y1 + 160, 5, 3);
        psmoveto(fp,x1 + 25, y1 + 160);
        fprintf(fp," (%s)\n", st);
        fprintf(fp,"true charpath pathbbox pop 60 add /ux exch def 60 sub /ly exch def 60 sub /lx exch def\n");
        fprintf(fp,"N lx ly M ux ly L ux uy L lx uy L lx ly L stroke\n");

	/*strcpy( st, "   ");
	pstextc( fp, st, x1 + 25, y1 + 240, 5, 3);

	hpselect_pen( fp, 1, .3F, 9 );
	hprectangle( fp, x1, y1, x1 + 2195, y1 + 280, 0, 0 );*/
	}

/*====================================================================*/

void hpwrite_winds ( FILE *fp )
	/*************************************************************/
	/*  HPWRITE_WINDS                                            */
	/*                                                           */
	/*  Writes wind data to printer.                             */
	/*************************************************************/
	{
	char st[80];
	short x1, y1;
	float ix1, ix2, ix3, ix4;
	struct _parcel pcl;

/*-------------------------------------------------------------------*/

	x1 = 3500;
	y1 = hsk.bry + 500;

	hpcliprgn( fp , 20000, 0, 0, 20000 );
	hpcentertext( fp, "KINEMATIC PARAMETERS", x1 + 1500, y1, 12, 3);
        hpselect_pen( fp, 1, .4F, 9 );
	hprectangle( fp , x1 - 50, y1 + 20, x1 + 3000, 7700, 0, 0);
	
	/* ----- 0-6km Mean Wind ----- */
	y1 += 200;
	pstextc( fp, "Sfc - 6 km Mean Wind:", x1, y1, 9, 0);
	mean_wind( -1, i_pres(agl(6000)), &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3) && qc(ix4))
	   { sprintf( st, "%d / %d kt   (%d m/s)", (short)ix3, (short)ix4, 
	     (short)kt_to_mps(ix4)); }
	else
	   { sprintf( st, "M / M"); }
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	/* ----- LFC-EL Mean Wind ----- */
	y1 += 150;
	pstextc( fp, "LFC - EL Mean Wind:", x1, y1, 9, 0);
	mean_wind( -1, -1, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3) && qc(ix4))
	   { sprintf( st, "%d / %d kt   (%d m/s)", (short)ix3, (short)ix4,
   	     (short)kt_to_mps(ix4)); }
	else
	   { sprintf( st, "M / M"); }
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	/* ----- 850-300 Mean Wind ----- */
	y1 += 150;
	pstextc( fp, "850 - 300 Mean Wind:", x1, y1, 9, 0);
	mean_wind( 850, 300, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3) && qc(ix4))
	   { sprintf( st, "%d / %d kt   (%d m/s)", (short)ix3, (short)ix4,
   	     (short)kt_to_mps(ix4)); }
	else
	   { sprintf( st, "M / M"); }
	hpdisp_param( fp, st, x1 + 2900, y1, 9, 0);

	y1 += 200;
	psmoveto(fp, x1, y1);
	hplineto(fp, x1+3000, y1);

	/* ----- Sfc-2km Shear ----- */
	y1 += 150;
	wind_shear( -1, i_pres(msl(2000)), &ix1, &ix2, &ix3, &ix4);	
	pstextc( fp, "Sfc - 2km Shear:", x1, y1, 9, 0);	
	if(qc(ix3) && qc(ix4))
	   {
	   sprintf( st, "%d kt", (short)ix4);
	   hpdisp_param( fp, st, x1+1700, y1, 9, 0);
	   sprintf( st, "(%d m/s)", (short)kt_to_mps(ix4));
	   hpdisp_param( fp, st, x1+2500, y1, 9, 0);
	   }
	else
	   {
	   strcpy( st, "M");
	   hpdisp_param( fp, st, x1+1700, y1, 9, 0);
	   }

        /* ----- Sfc-6km Shear ----- */
        y1 += 150;
        wind_shear( -1, i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        pstextc( fp, "Sfc - 6km Shear:", x1, y1, 9, 0);
        if(qc(ix3) && qc(ix4))
           {
           sprintf( st, "%d kt", (short)ix4);
           hpdisp_param( fp, st, x1+1700, y1, 9, 0);
           sprintf( st, "(%d m/s)", (short)kt_to_mps(ix4));
           hpdisp_param( fp, st, x1+2500, y1, 9, 0);
           }
        else
           {
           strcpy( st, "M");
           hpdisp_param( fp, st, x1+1700, y1, 9, 0);
           }

        /* ----- BRN Shear ----- */
        y1 += 150;
	pcl.bplus=1;
	pcl.lplpres = lplvals.pres;
	ix2 = bulk_rich( pcl, &ix1);
        pstextc( fp, "*BRN Shear:", x1, y1, 9, 0);
        if(qc(ix3) && qc(ix4))
           {
           sprintf( st, "%d m2/s2", (short)ix1);
           hpdisp_param( fp, st, x1+2500, y1, 9, 0);
           }
        else
           {
           strcpy( st, "M");
           hpdisp_param( fp, st, x1+2100, y1, 9, 0);
           }
	}


	/*NP*/
	void hpwrite_storm ( FILE *fp )
	/*************************************************************/
	/*  HPWRITE_STORM                                            */
	/*                                                           */
	/*  Writes storm data to printer.                            */
	/*************************************************************/
	{
	char st[80], st1[80];
	short x1, y1;
	float sfctemp, sfcdwpt, sfcpres, hel3;
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
        hpselect_pen( fp, 1, .4F, 9 );
	hprectangle( fp , x1 - 50, y1 + 20, x1 + 3000, 9200, 0, 0);

        /* ----- Sfc-3km SREH ----- */
        y1 += 200;
	ix1 = helicity( 0, 3000, st_dir, st_spd, &ix2, &ix3);
	hel3 = ix1;
        pstextc( fp, "Sfc - 3km SREH:", x1, y1, 9, 0);
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

        /* ----- Effective SREH ----- */
        y1 += 150;
        ix1 = helicity( -1, -1, st_dir, st_spd, &ix2, &ix3);
        pstextc( fp, "Effective SREH:", x1, y1, 9, 0);
        if(qc(ix1))
           {
           sprintf( st, "%.0f m2/s2", ix1);
	   sprintf( st1, "from %.0f m.", agl(i_hght(esfc(50))));
           hpdisp_param( fp, st, x1+1700, y1, 9, 0);
	   hpdisp_param( fp, st1, x1+2600, y1, 9, 0);
           }
        else
           {
           strcpy( st, "M");
           hpdisp_param( fp, st, x1+1700, y1, 9, 0);
           }

	y1 += 200;
	psmoveto( fp, x1, y1);
	hplineto( fp, x1+3000, y1);

	/* ----- 0-2 km Storm Relative Winds ----- */
	y1 += 150;
	pstextc( fp, "0-2 km SRW:", x1, y1, 9, 0);
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

	/* ----- EHI ----- */
	pstextc( fp, "EHI:", x1+1700, y1, 9, 0);
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

        /* ----- 4-6 km Storm Relative Winds ----- */
        y1 += 150;
        pstextc( fp, "4-6 km SRW:", x1, y1, 9, 0);
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

        /* ----- BRN ----- */
        pstextc( fp, "BRN:", x1+1700, y1, 9, 0);
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

        /* ----- 6-10 km Storm Relative Winds ----- */
        y1 += 150;
        pstextc( fp, "6-10 km SRW:", x1, y1, 9, 0);
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
	}



	/*NP*/
	void hpplot_uvvs ( FILE *fp )
	/*************************************************************/
	/*  HPPLOT_UVVS                                              */
	/*                                                           */
	/*  Plots VVs (Mbps) on right side of SkewT.                 */
	/*************************************************************/
	{
	short i, x, y, x1, avail;
	float leng, scal;
	int ok=0;

	scal = 1000;
    if  (g_pfs_model_sel_sw)
      scal=10;
    else
      scal=1000;
	
	hpcliprgn( fp , 20000, 0, 0, 20000 );
	hpselect_pen( fp, 1, .25F, 9 );	

	x1 = hsk.brx + 1900;

	/* ----- Plot every level ----- */
	avail = 0;
	for( i = 0; i < numlvl; i++)
	   {
	   if(qc(sndg[i][0]) && (sndg[i][0] < 1))
	      {
       	      avail++;
	      y = hppres_to_pix( sndg[i][1] );
	      leng = (sndg[i][0] * scal);   /* Convert to Mbs/sec */
	      hpselect_pen( fp, 1, .05F, 9 );
	      if(abs(leng) >  5) hpselect_pen( fp, 1, .10F, 9 );
 	      if(abs(leng) > 10) hpselect_pen( fp, 1, .35F, 9 );
 	      if(abs(leng) > 15) hpselect_pen( fp, 1, .50F, 9 );
	      x = x1 - (leng * 15);	    /* Determine screen scale */
	      psmoveto(fp, x1, y);
	      hplineto(fp, x, y); 
          fprintf(fp," stroke\n");
	      }
	   }

	if(avail > 5)
	   {	   
  	   /* ----- Draw base legend ----- */

           hpselect_pen( fp, 1, .20F, 9 );  	   
	   psmoveto( fp, x1, hsk.tly);
	   hplineto( fp, x1, hsk.bry);
        fprintf(fp," stroke\n");

           hpselect_pen( fp, 1, .10F, 11 );
	   psmoveto( fp, x1+150, hsk.tly);
	   hplineto( fp, x1+150, hsk.bry);
        fprintf(fp," stroke\n");

           hpselect_pen( fp, 1, .10F, 11 );
	   psmoveto( fp, x1-150, hsk.tly);
	   hplineto( fp, x1-150, hsk.bry);
        fprintf(fp," stroke\n");

           hpselect_pen( fp, 1, .20F, 9 );  	   
	   psmoveto( fp, x1-300, hsk.tly);
	   hplineto( fp, x1+300, hsk.tly);
        fprintf(fp," stroke\n");

	   hpcentertext( fp, "VVEL (Mbps)", x1, hsk.tly-95, 7, 3);
	   hpcentertext( fp, "Down  Up   ", x1, hsk.tly-15, 7, 3);
	   }
	}

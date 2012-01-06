/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  DOS Video Graphics Routines (Part #2)                      */
/*  These are the routines that set up the VIDEO environment   */
/*  in DOS and provide a workplace for SHARP.                  */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  SET_FONT                                                   */
/*  SWITCH_MODES                                               */
/*  REDRAW_GRAPH                                               */
/*  DISP_PARAM                                                 */
/*  GRAB_LEVEL                                                 */
/*  SKEWT_CURSOR_DATA                                          */
/*  HODO_CURSOR_DATA                                           */
/*                                                             */
/***************************************************************/
#define VIDEO
#include "gui.h"
#include "sharp95.h"

/*==============================================================================*/

short switch_modes ( short mode )
        /*************************************************************/
        /*  SWITCH_MODES                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Changes screen output from Skew-T to Hodograph and back. */
        /*************************************************************/
        {

        /* ----- Erase Current Graphic Workplace ----- */
        setcolor(0, draw_reg, gc);
	rectangle( 0, 1, 1, (short)(skv.brx+19), (short)(skv.bry+19) );

        /* ----- Redraw Graphic ----- */
        if( mode == 2 )
           {
	   pagenum = 1;
	   clear_paramarea();
           draw_skewt();
	   show_page( pagenum );
           mode = 1;
	   }
        else
           {
	   pagenum = 2;
	   clear_paramarea();
           draw_hodo();
	   show_page( pagenum );
           mode = 2;
	   }


        /* ----- Display New Button ----- */
	XFlush ( XtDisplay(draw_reg) );
        return mode;
        }

        /*NP*/
        void redraw_graph ( short mode )
        /*************************************************************/
        /*  REDRAW_GRAPH                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Redraws current graph (Skew-T or Hodograph).             */
        /*************************************************************/
        {

        /* ----- Erase Current Graphic Workplace ----- */
        setcolor(0, draw_reg, gc);
	rectangle( 1, 1, 1, (short)(skv.brx+19), (short)(skv.bry+19) );

        /* ----- Redraw Graphic ----- */
	if( mode == 1 )
	   { 
		draw_skewt(); 
           }	
        else
	   { 
		draw_hodo(); 
           }

	set_font(2);
        }

        /*NP*/
        void disp_param ( char *value, short rcol, short rlin )
        /*************************************************************/
        /*  DISP_PARAM                                               */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Right justifies value at location rrow, rlin.            */
        /*************************************************************/
        {
	outgtext ( value, (int)(rcol - getgtextextent(value)), (int)rlin );
        }

	/*NP*/
	short grab_level ( float pres )
	/*************************************************************/
	/*  GRAB_LEVEL                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the array index of the closest data level that   */
	/*  contains temperature data.                               */
	/*************************************************************/
	{
	short i, ii;
	float dx, mindx;

	mindx = 9999.0F;
	for(i=0; i<sndgp->numlev; i++)
	   {
	   dx = (float)fabs((double)(pres - sndgp->sndg[i].pres));
	   /* printf( "p=%f   dx=%f   mindx=%f\n", sndgp->sndg[i].pres, dx, mindx); */
	   if((dx < mindx) && qc(sndgp->sndg[i].temp))
	      {
	      mindx = dx;
	      ii = i;
	      }
	   }
	return ii;
	}



	/*NP*/
	static XFontStruct *fs = NULL;
	int cursor_xwdth, cursor_xhght;
	void skewt_cursor_data ( short x, short y )
	/*************************************************************/
	/*  SKEWT_CURSOR_DATA                                        */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes cursor data at top of skewt screen.               */
	/*************************************************************/
	{
	float pres, temp, hght;
	char st[20];
	int skv_brx=0, skv_tly=-20;

	set_font_cursor ( 2, cursor_out, gc_cursor, &fs );
	setcliprgn ( 1, 1, cursor_xwdth, cursor_xhght, cursor_out, gc_cursor );

	/* ----- Write data to screen ----- */
	pres = pix_to_pres( y );
	temp = pix_to_temp( x, y);
	hght = i_hght( pres );

	setcolor( 31, cursor_out, gc_cursor );
	sprintf( st, "%5.0fmb", pres );
	outcursor( st, skv_brx + 185, skv_tly + 25 );

	setcolor( 6, cursor_out, gc_cursor );
	sprintf( st, "%5.0fm", hght);
	outcursor( st, skv_brx + 300, skv_tly + 25 );

	sprintf( st, "%5.0fft", mtof(hght));
	outcursor( st, skv_brx+300, skv_tly+40 );

	setcolor( 2, cursor_out, gc_cursor );
	sprintf( st, "%6.1fC", temp);
	outcursor( st, skv_brx+75, skv_tly+25 );

	sprintf( st, "%6.1fF", ctof(temp));
	outcursor( st, skv_brx+75, skv_tly+40 );

	setcolor( 3, cursor_out, gc_cursor );
	sprintf( st, "%5.1fg/kg", mixratio( pres, temp ));
	outcursor( st, skv_brx+180, skv_tly+57 );

	setcolor( 5, cursor_out, gc_cursor );
        /* changed from 216 courier to 113 symbol*/
        set_font_cursor(5, cursor_out, gc_cursor, &fs);
	sprintf( st, "%c=%5.1fK", 113, 
			theta( pres, temp, 1000.0F ) + 273.15F );
	/*outtext( st, skv_brx+50, skv_tly+57 );*/
	outcursor( st, skv_brx+85, skv_tly+57 );

	sprintf( st, "%c=%5.1fK", 113, 
			thetaw( pres, temp, temp) + 273.15F );
	/*outtext( st,  skv_brx+300, skv_tly+57 );*/
	outcursor( st,  skv_brx+310, skv_tly+57 );
        set_font_cursor(2, cursor_out, gc_cursor, &fs);
	XFlush ( XtDisplay(cursor_out) );
	}


	/*NP*/
	void hodo_cursor_data ( short x, short y )
	/*************************************************************/
	/*  HODO_CURSOR_DATA                                         */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes cursor data at top of hodograph screen.           */
	/*************************************************************/
	{
	float wdir, wspd, ix1, ix2, ix3, ix4;
	char st[20];
	int skv_brx=0, skv_tly=-20;

        set_font_cursor(2, cursor_out, gc_cursor, &fs);
	setcliprgn ( 1, 1, cursor_xwdth, cursor_xhght, cursor_out, gc_cursor );

	setcolor( 31, cursor_out, gc_cursor );
	pix_to_hodo( x, y, &wdir, &wspd );
	sprintf( st, "%3.0f%c%3.0fkt", wdir, 176, wspd );
	outcursor( st,  skv_brx+165, skv_tly+25 );

	ix1 = helicity( 0.0F, 3000.0F, wdir, wspd, &ix2, &ix3);
	sprintf( st, "%4.0f m%cs%c", ix1, 178, 178 );
	outcursor( st, skv_brx+165, skv_tly+37 );

	setcolor( 5, cursor_out, gc_cursor );
	outcursor( "0-2km", skv_brx+50, skv_tly+45 );
	sr_wind( -1.0F, -1.0F, wdir, wspd, &ix1, &ix2, &ix3, &ix4);
	sprintf( st, "%3.0f", ix4 );
	outcursor( st, skv_brx+50, skv_tly+60);

	outcursor( "2-5km", skv_brx+115, skv_tly+45 );
	sr_wind( i_pres(msl(2000.0F)), i_pres(msl(5000.0F)), wdir, wspd, &ix1, &ix2, &ix3, &ix4);
	sprintf( st, "%3.0f", ix4 );
	outcursor( st, skv_brx+115, skv_tly+60);

	outcursor( "5-9km", skv_brx+250, skv_tly+45 );
	sr_wind( i_pres(msl(5000.0F)), i_pres(msl(9000.0F)), wdir, wspd, &ix1, &ix2, &ix3, &ix4);
	sprintf( st, "%3.0f", ix4 );
	outcursor( st, skv_brx+250, skv_tly+60 );

	outcursor( "9-12km",  skv_brx+315, skv_tly+45 );
	sr_wind( i_pres(msl(9000.0F)), i_pres(msl(12000.0F)), wdir, wspd, &ix1, &ix2, &ix3, &ix4);
	sprintf( st, "%3.0f", ix4 );
	outcursor( st, skv_brx+315, skv_tly+60 );
	}

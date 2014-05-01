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
/*  CHECK_VICO                                                 */
/*  RESET_VIDEO                                                */
/*  SET_VIDEO                                                  */
/*  SET_FONT                                                   */
/*  INTERACTIVE                                                */
/*  SWITCH_MODES                                               */
/*  REDRAW_GRAPH                                               */
/*  DISP_PARAM                                                 */
/*  EDIT_SKEWT                                                 */
/*  GRAB_LEVEL                                                 */
/*  SKEWT_CURSOR_DATA                                          */
/*  HODO_CURSOR_DATA                                           */
/*                                                             */
/***************************************************************/

#define VIDEO
#include <sharp95.h>
#include <xwcmn.h>
#include "winline.h"

        /*NP*/
        short switch_modes( short mode )
        /*************************************************************/
        /*  SWITCH_MODES                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Changes screen output from Skew-T to Hodograph and back. */
        /*************************************************************/
        {
        short i;

        /* ----- Erase Current Graphic Workplace ----- */
        setcolor(0);
	rectangle( 0, 1, 1, skv.brx + 19, skv.bry + 19 );

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
        void redraw_graph( short mode )
        /*************************************************************/
        /*  REDRAW_GRAPH                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Redraws current graph (Skew-T or Hodograph).             */
        /*************************************************************/
        {
        short i;

        /* ----- Erase Current Graphic Workplace ----- */
        setcolor(0);
	rectangle( 1, 1, 1, skv.brx + 19, skv.bry + 19 );

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
        void disp_param( char *value, short rcol, short rlin)
        /*************************************************************/
        /*  DISP_PARAM                                               */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Right justifies value at location rrow, rlin.            */
        /*************************************************************/
        {
	outgtext ( value, rcol - getgtextextent( value ), rlin );
        }

	/*NP*/
	void edit_skewt( short pagenum )
	/*************************************************************/
	/*  EDIT_SKEWT                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine allows the user to edit the skewt chart.    */
	/*************************************************************/
	{
	short x, y, i, yy;
	float pres, temp, dwpt, tcur, d1, d2, chg1;
	char st[20];
	XEvent mse;

	/* ----- Get current mouse location ----- */
	GetPtrPos( &mse );

	/* ----- Determine whether to grab the T or TD trace ----- */
	pres = pix_to_pres( (short)mse.xbutton.y );
	tcur = pix_to_temp( (short)mse.xbutton.x, (short)mse.xbutton.y );
	if (pres > sndg[sfc()][1]) { pres = sndg[sfc()][1]; }

	temp = i_temp( pres );
	dwpt = i_dwpt( pres );
	d1 = (float)fabs(tcur - temp);
	d2 = (float)fabs(tcur - dwpt);
	printf( "tcur=%f     temp=%f\n", tcur, temp);
	if( d1 < d2 )
	   {
	   /* ----- Edit Temperatures ----- */
	   i = grab_level( pres );
	   if(!qc(sndg[i][3])) { sndg[i][3] = i_temp(sndg[i][1]); }

	   /* ----- Move mouse to starting spot ----- */
	   yy = pres_to_pix(sndg[i][1]);
	   SetPtrPos( temp_to_pix(sndg[i][3], sndg[i][1]), yy);
	   /*while(mse.fsBtn)
	      {
	      GetPtrPos(&mse);
	      if((mse.xbutton.y > yy + 10) || (mse.xbutton.y < yy - 10))
		 { SetPtrPos(mse.xbutton.x, yy); }
	      }*/

	   /* ----- Update sounding array with new temp ----- */
	   chg1 = pix_to_temp( (short)mse.xbutton.x, yy );
	   if(chg1 < i_dwpt(sndg[i][1])) { chg1 = i_dwpt(sndg[i][1]); }
	   sndg[i][3] = chg1;

	   /* ----- Redraw skewt and update parameters ----- */
	   printf( "Defining Parcel\n" );
	   define_parcel( -1, 850);
	   SetPtrVis( HIDE );
	   printf( "Redraw Graph\n" );
	   redraw_graph( 1 );
	   show_page( pagenum );
	   SetPtrVis( SHOW );
	   }
	else
	   {
	   /* ----- Edit Dew Points ----- */
	   i = grab_level( pres );
	   if(!qc(sndg[i][4])) { sndg[i][4] = i_dwpt(sndg[i][1]); }

	   /* ----- Move mouse to starting spot ----- */
	   yy = pres_to_pix(sndg[i][1]);
	   SetPtrPos( temp_to_pix(sndg[i][4], sndg[i][1]), yy );
	   /*while(mse.fsBtn)
	      {
	      GetPtrPos( &mse );
	      if((mse.y > yy + 10) || (mse.y < yy - 10))
		 { SetPtrPos(mse.x, yy); }
	      }*/

	   /* ----- Update sounding array with new temp ----- */
	   chg1 = pix_to_temp( (short)mse.xbutton.x, yy );
	   if(chg1 > i_temp(sndg[i][1])) { chg1 = i_temp(sndg[i][1]); }
	   sndg[i][4] = chg1;

	   /* ----- Redraw skewt and update parameters ----- */
	   printf( "Defining Parcel\n" );
	   define_parcel( -1, 850);
	   SetPtrVis( HIDE );
	   printf( "Redraw Graph\n" );
	   redraw_graph( 1 );
	   show_page( pagenum );
	   SetPtrVis( SHOW );
	   }
	}

	/*NP*/
	short grab_level( float pres )
	/*************************************************************/
	/*  GRAB_LEVEL                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the array index of the closest data level that   */
	/*  contains temperature data.                               */
	/*************************************************************/
	{
	short i, ii;
	float dx, mindx, t1;

	mindx = 9999;
	for(i=0; i<numlvl; i++)
	   {
	   dx = (float)fabs(pres - sndg[i][1]);
	   /* printf( "p=%f   dx=%f   mindx=%f\n", sndg[i][1], dx, mindx); */
	   if((dx < mindx) && qc(sndg[i][3]))
	      {
	      mindx = dx;
	      ii = i;
	      }
	   }
	return ii;
	}



	/*NP*/
	void skewt_cursor_data( short x, short y )
	/*************************************************************/
	/*  SKEWT_CURSOR_DATA                                        */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes cursor data at top of skewt screen.               */
	/*************************************************************/
	{
	float pres, temp, hght;
	char st[20];

	setcliprgn ( 1, 1, xwdth, xhght );

	/* ----- Write data to screen ----- */
	pres = pix_to_pres( y );
	temp = pix_to_temp( x, y);
	hght = i_hght( pres );

	setcolor( 31 );
	sprintf( st, "%5.0fmb", pres );

	outtext( st, skv.brx+130, skv.tly+25 );

	setcolor( 6 );
        
        sprintf( st, " MSL ");
        outtext( st, skv.brx+245, skv.tly+25 );

	sprintf( st, "%5.0fft", mtof(hght));
	outtext( st, skv.brx+245, skv.tly+40 );

        sprintf( st, "%5.0fm", hght);
        outtext( st, skv.brx+245, skv.tly+55);

        sprintf( st, " AGL ");
        outtext( st, skv.brx+315, skv.tly+25 );

	sprintf( st, "%5.0fft", mtof(agl(hght)));
	outtext( st, skv.brx+315, skv.tly+40 );

        sprintf( st, "%5.0fm", agl(hght));
        outtext( st, skv.brx+315, skv.tly+55 );

	setcolor( 2 );

	sprintf( st, "%6.1fC", temp);
	outtext( st, skv.brx+35, skv.tly+25 );

	sprintf( st, "%6.1fF", ctof(temp));
	outtext( st, skv.brx+35, skv.tly+40 ); 

	setcolor( 3 );
	sprintf( st, "%5.1fg/kg", mixratio( pres, temp ));
        outtext( st, skv.brx+125, skv.tly+40 );
        
	setcolor( 5 );
        set_font(5);
        sprintf(st,"%c=%5.1fK", 113,
			theta( pres, temp, 1000 ) + 273.15 );
        outtext( st, skv.brx+45, skv.tly+57 );

	sprintf( st, "%cw=%5.1fK", 113,
                        thetaw( pres, temp, temp) + 273.15 );
        /* outtext( st,  skv.brx+245, skv.tly+57 ); */
        outtext( st,  skv.brx+135, skv.tly+57 );
        set_font(2);
	XFlush ( XtDisplay(draw_reg) );
	}
	
	/*NP*/
	void icing_turb_cursor_data( short x, short y)
	/*****************************************************************/
	/* ICING_TURB_CURSOR_DATA (LJH)                                  */
	/*****************************************************************/
	{
	float pres, hght;
	float temp,cpw;
	char st[20];

	setcliprgn ( 1, 1, xwdth, xhght );

	/* ----- Write data to screen ----- */
	temp=pmap((float)x,2);
	cpw=pmap((float)y,3);
	pres=exp(log(1000.0)*cpw/1000.0);
	/*pres = pix_to_pres( y );*/
	hght = i_hght( pres );

	setcolor( 31 );
	sprintf( st, "%5.0fmb", pres );
	outtext( st, skv.brx+185, skv.tly+25 );

	setcolor( 6 );
	sprintf( st, "%5.0fm", hght);
	outtext( st, skv.brx+300, skv.tly+25 );

	sprintf( st, "%5.0fft", mtof(hght));
	outtext( st, skv.brx+300, skv.tly+40 );

	setcolor( 2 );
	sprintf( st, "%6.1fC", temp);
	outtext( st, skv.brx+75, skv.tly+25 );
	}

	/*NP*/
	void hodo_cursor_data( short x, short y )
	/*************************************************************/
	/*  HODO_CURSOR_DATA                                         */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes cursor data at top of hodograph screen.           */
	/*************************************************************/
	{
	float wdir, wspd, ix1, ix2, ix3, ix4;
	char st[20];

	setcolor( 31 );
	pix_to_hodo( x, y, &wdir, &wspd );
	sprintf( st, "%3.0f%c%3.0fkt", wdir, 176, wspd );
	outtext( st,  skv.brx+165, skv.tly+25 );

	ix1 = helicity( 0, 3000, wdir, wspd, &ix2, &ix3);
	sprintf( st, "%4.0f m%cs%c", ix1, 178, 178 );
	outtext( st, skv.brx+165, skv.tly+37 );

	setcolor( 5 );
	outtext( "0-2km", skv.brx+50, skv.tly+45 );
	sr_wind( -1, -1, wdir, wspd, &ix1, &ix2, &ix3, &ix4);
	sprintf( st, "%3.0f", ix4 );
	outtext( st, skv.brx+50, skv.tly+60);

	outtext( "2-5km", skv.brx+115, skv.tly+45 );
	sr_wind( i_pres(msl(2000)), i_pres(msl(5000)), wdir, wspd, &ix1, &ix2, &ix3, &ix4);
	sprintf( st, "%3.0f", ix4 );
	outtext( st, skv.brx+115, skv.tly+60);

	outtext( "5-9km", skv.brx+250, skv.tly+45 );
	sr_wind( i_pres(msl(5000)), i_pres(msl(9000)), wdir, wspd, &ix1, &ix2, &ix3, &ix4);
	sprintf( st, "%3.0f", ix4 );
	outtext( st, skv.brx+250, skv.tly+60 );

	outtext( "9-12km",  skv.brx+315, skv.tly+45 );
	sr_wind( i_pres(msl(9000)), i_pres(msl(12000)), wdir, wspd, &ix1, &ix2, &ix3, &ix4);
	sprintf( st, "%3.0f", ix4 );
	outtext( st, skv.brx+315, skv.tly+60 );
	}

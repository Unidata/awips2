/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  DOS Video Graphics Routines (Part #2)                      */
/*  These are the routines that set up the VIDEO environment   */
/*  in DOS and provide a workplace for SHARP.                  */
/*                                                             */
/*  John A. Hart and Richard L. Thompson                       */
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
/*  BUNKERS_STORM_MOTION				       */
/*  BUNKERS_LEFT_MOTION					       */
/*  EFFECTIVE_INFLOW_LAYER				       */
/*  SCP (Supercell Composite Parameter			       */
/*  SIGTORN_WINTER					       */	
/*  SIGTORN						       */
/*  SIGTORN_CIN						       */
/*  CB_SIGTOR						       */
/*  ESP (Enhanced Stretching Potential - Jon Davies)	       */
/*  DAMAGING_WIND					       */
/*  WW_TYPE						       */
/***************************************************************/

#ifndef _WIN32
#include "xwcmn.h"
#endif
#include "sharp95.h"

#ifndef _WIN32
short switch_modes(short mode)
        /*************************************************************/
        /*  SWITCH_MODES                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Changes screen output from Skew-T to Hodograph and back. */
        /*************************************************************/
{
        /* ----- Erase Current Graphic Workplace ----- */
        setcolor(0);
	rectangle(0, 1, 1, skv.brx + 10, skv.bry + 10);

        /* ----- Redraw Graphic ----- */
        if (mode == DRAW_HODO) {
	   if (display_mode_left == DISPLAY_WINTER_LEFT || display_mode_right == DISPLAY_WINTER_RIGHT)
	     pagenum = 5;
	   else
	     pagenum = 1;
	   clear_paramarea();
           draw_skewt();
	   show_page(pagenum);
           drawing_mode = DRAW_SKEWT;
	} else {
	   pagenum = 2;
	   clear_paramarea();
           draw_hodo();
	   show_page(pagenum);
           drawing_mode = DRAW_HODO;
	}

	copytodisplay();

        return drawing_mode;
}

        /*NP*/
void redraw_graph(short mode)
        /*************************************************************/
        /*  REDRAW_GRAPH                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Redraws current graph (Skew-T or Hodograph).             */
        /*************************************************************/
{
	float pres;
	int current_parcel;

        /* ----- Erase Current Graphic Workplace ----- */
        setcolor(0);
/*	rectangle(1, 1, 1, hov.brx + 10, hov.bry + 10); */
	/* RLT attempt to fix parcel plot 2/16/05 */
	rectangle(1, hov.tlx, hov.tly, hov.tlx + 250, skv.tly + 350);

        /* ----- Redraw Graphic ----- */
	draw_skewt(); 
	trace_parcel(lplvals.pres, lplvals.temp, lplvals.dwpt);
	draw_hodo(); 
	clear_paramarea();
	show_page(pagenum);
	set_font(2);
}

        /*NP*/
void disp_param(char *value, short rcol, short rlin)
        /*************************************************************/
        /*  DISP_PARAM                                               */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Right justifies value at location rrow, rlin.            */
        /*************************************************************/
{
	outgtext(value, (rcol - getgtextextent(value)), rlin);
}

#endif
/*

grab_level should return an error as needed 

-mkay
6/8/00

*/
	/*NP*/
short grab_level(float pres)
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
	short pIndex, tIndex;

	pIndex = getParmIndex("PRES");
	tIndex = getParmIndex("TEMP");

	if (!sndg || pIndex == -1 || tIndex == -1)
	  return 0;

	mindx = 9999;
	for(i=0; i<numlvl; i++) {
	   dx = (float)fabs(pres - sndg[i][pIndex]);
	   if((dx < mindx) && qc(sndg[i][tIndex])) {
	      mindx = dx;
	      ii = i;
	   }
	}
	return ii;
}


#ifndef _WIN32
	/*NP*/
void skewt_cursor_data( short x, short y )
	/*************************************************************/
	/*  SKEWT_CURSOR_DATA                                        */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Writes cursor data at top of skewt screen.               */
	/*************************************************************/
{
	float pres, temp, hght, ix1;
	short ii, x1, y1;
	char st[20];
	short pIndex, zIndex, tIndex, tdIndex, dIndex, sIndex;

        pIndex = getParmIndex("PRES");
        tIndex = getParmIndex("TEMP");
        tdIndex = getParmIndex("DWPT");
        zIndex = getParmIndex("HGHT");
        sIndex = getParmIndex("SPED");
        dIndex = getParmIndex("DRCT");

        setlinestyle(1, 1);


	/* ----- Write data to screen ----- */
	pres = pix_to_pres(y);
	temp = pix_to_temp(x, y);
	hght = i_hght(pres, I_PRES);

	/* Put values on the skewt for this level */
	set_font(4);
	setcolor(31);
	setcolor(5);
	copytodisplay();
	sprintf( st, "%4.0f/%.0f kt", i_wdir(pres, I_PRES), i_wspd(pres, I_PRES)); /* wind */
	outtext( st, skv.brx-35, y );
	sprintf( st, "%4.0fmb", pres); /* pres */
	outtext( st, skv.tlx+2, y );
	sprintf( st, "%5.0fft / %.0fm agl", mtof(agl(i_hght(pres, I_PRES))), agl(i_hght(pres, I_PRES))); /* hgt, agl */
	outtext( st, skv.tlx+45, y );
	sprintf( st, "%4.1f/%4.1fC", i_temp(pres, I_PRES), i_dwpt(pres, I_PRES)); /* t/td */
	outtext( st, skv.brx-110, y );

/*        sprintf( st, "RH = %s", qc2( relh(pres, &ix1), "%", 0 ));
        outgtext( st, skv.tlx+150, y);
*/
	if (i_temp(pres, I_PRES) > -9998.0 & i_dwpt(pres, I_PRES) > -9998.0){
		sprintf( st, "%2.0f%%", relh(pres, &ix1)); 
		outtext( st, skv.tlx+150, y);
		/*sprintf( st, "RH = %s", qc2( relh(pres, &ix1), "%", 0 ));
        	outgtext( st, skv.tlx+150, y);*/
	        }

	/* Plot a point on the hodograph for this level */
	hodo_to_pix(i_wdir(pres, I_PRES), i_wspd(pres, I_PRES), &x1, &y1);
	sprintf( st, "x -- %.0fm %4.0f/%.0f kt", agl(i_hght(pres, I_PRES)), i_wdir(pres, I_PRES), i_wspd(pres, I_PRES));
	outtext( st, x1, y1-3 );
	
	setcolor( 31 );
	sprintf( st, "%5.0fmb ", pres );
	outtext( st, skv.brx-240, skv.tly+25 );

	setcolor( 6 );
	sprintf( st, "  %5.0fm ", hght);
	outtext( st, skv.brx-180, skv.tly+25 );

	sprintf( st, "  %5.0fft ", mtof(hght));
	outtext( st, skv.brx-180, skv.tly+40 );

	setcolor( 2 );
	sprintf( st, "%6.1fC ", temp);
	outtext( st, skv.brx-240, skv.tly+40 );

	sprintf( st, "%6.1fF ", ctof(temp));
	outtext( st, skv.brx-240, skv.tly+57 );

	setcolor( 3 );
	sprintf( st, "%5.1fg/kg ", mixratio(pres, temp));
	outtext( st, skv.brx-180, skv.tly+57 );

	setcolor( 5 );
	sprintf( st, " %c=%5.1fK ", 216, theta(pres, temp, 1000.0) + 273.15 );
	outtext( st, skv.brx-120, skv.tly+25 );

	sprintf( st, "%cw=%5.1fK ", 216, thetaw(pres, temp, temp) + 273.15 );
	outtext( st,  skv.brx-120, skv.tly+40 );

	sprintf( st, "%ce=%5.1fK ", 216, thetae(pres, temp, temp) + 273.15 );
	outtext( st,  skv.brx-120, skv.tly+57 );

	ii = get_level_pointer(pres);

	if (sndg && pIndex != -1) {
	  setcolor(31);
          sprintf(st, "%5.0fmb %5.0fm %4.1f/%4.1fC %4.0f/%.0f kt", sndg[ii][pIndex],
             sndg[ii][zIndex], sndg[ii][tIndex], sndg[ii][tdIndex], sndg[ii][dIndex], sndg[ii][sIndex] );
          outtext(st, skv.brx-250, skv.tly+74 );
	}

	return;

	XFlush(XtDisplay(draw_reg));
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
	char  st[20];

	copytodisplay();

	setcolor( 31 );
	pix_to_hodo( x, y, &wdir, &wspd );
	sprintf( st, "%3.0f%c%3.0fkt", wdir, 176, wspd );
	outtext( st,  hov.brx-50, hov.tly+10 );
	return;

	ix1 = helicity( 0, agl(3000), wdir, wspd, &ix2, &ix3);
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
#endif

/*

should return an error condition if needed

-mkay
6/8/00
*/
	/*NP*/
short get_level_pointer(float pres)
	/**************************************************************/
	/*							      */
	/* GET_LEVEL_POINTER					      */
	/*							      */
	/* Returns pointer for level closest to pres(mb).	      */
	/**************************************************************/
{
	short i, ptr, pIndex;
	float pdif, mindif;

	pIndex = getParmIndex("PRES");

	if (!sndg || pIndex == -1)
	  return 0;

	mindif = 999;
	ptr = 0;
	for (i=0;i<numlvl;i++) {
	  pdif = abs(sndg[i][pIndex] - pres);
	  if (pdif < mindif) {
	    mindif = pdif;
	    ptr = i;
	  }
	}
 	return ptr;	
}

/*  ALL ROUTINES BELOW WERE MOVED FROM SNDGLIB SO THAT GLOBAL VARIABLES user_level, mu_layer, and mml_layer
    WOULD ALL WORK PROPERLY IN PARCEL SETBACK  
    Rich Thompson - 10/2/05	*/

void bunkers_storm_motion(float *u, float *v, float *dir, float *spd)
        /*************************************************************/
        /*  EFFECTIVE BUNKERS_STORM_MOTION                           */
        /*  Rich Thompson and John Hart  SPC OUN                     */
        /*                                                           */
        /*  Calculates the motion of a right-moving supercell using  */
        /*  a method developed by Bunkers et. al. (2000),            */
        /*  modified to account for storm depth                      */
        /*                                                           */
        /*  *u, *v   - Storm motion vector (kt)                      */
        /*************************************************************/
{
        float u_mw, v_mw, u_sv, v_sv, ix1, ix2;
        /* Deviation Value (emperically derived as 8 m/s) */
        float d = 7.5 / 0.51479;
/* 24 Mar 2008 */	
/*        float base, el, depth, p_bot, p_top, oldlplpres, pres, mucp;*/

	float base, el, depth, oldlplpres, pres, mucp;
        struct _parcel pcl;

        short pIndex, oldlplchoice;

        *u   = RMISSD;
        *v   = RMISSD;
        *dir = RMISSD;
        *spd = RMISSD;

        pIndex = getParmIndex("PRES");

        if (!sndg || pIndex == -1)
          return; 

	/*printf("\n beginning storm dir = %0f\n", *dir);
        printf("\n beggining storm spd = %0f\n", *spd);*/

        oldlplchoice = lplvals.flag;
       /* printf("bunkers_storm_motion 1 calling define_parcel  flag=3-pres=400...oldlplchoice=%d------------------------->\n", oldlplchoice);*/
        define_parcel(3, 400);
        mucp = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        el = agl(i_hght(pcl.elpres, I_PRES));

        if (mucp >= 100 && el > 0)
        {

/* 24 Mar 2008 */
/*	effective_inflow_layer(100,-250, &p_bot, &p_top);

        printf("\nBunkers R inflow base  = %0.1f", agl(i_hght(p_bot, I_PRES)));
        printf("\nBunkers R inflow top  = %0.1f\n", agl(i_hght(p_top, I_PRES)));
*/

	base = agl(i_hght(p_bot, I_PRES));
        if (base >= 750){
	    	depth = (el - base);
		mean_wind_npw( p_bot, i_pres(msl(base + (depth * 0.5))), &u_mw, &v_mw, &ix1, &ix2);
                wind_shear(p_bot, i_pres(msl(base + (depth * 0.5))), &u_sv, &v_sv, &ix1, &ix2);
				
                if (qc(u_sv) && qc(v_sv)) {
                *u = u_mw + ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * v_sv);
                *v = v_mw - ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * u_sv);
                *dir = angle(*u, *v);
                *spd = speed(*u, *v);
/*      printf("\n end elayer storm dir = %0f\n", *dir);
        printf("\n end elayer storm spd = %0f\n", *spd);
*/
                }
          }
	}
/*	if(p_bot < 0) {
                        base = agl(i_hght(pcl.lplpres, I_PRES));
                        depth = (el - base);

                        mean_wind_npw( pcl.lplpres, i_pres(msl(base + (depth * 0.5))), &u_mw, &v_mw, &ix1, &ix2);
                        wind_shear( pcl.lplpres, i_pres(msl(base + (depth * 0.5))), &u_sv, &v_sv, &ix1, &ix2);
                        if (qc(u_sv) && qc(v_sv)) {
                                *u = u_mw + ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * v_sv);
                                *v = v_mw - ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * u_sv);
                                *dir = angle(*u, *v);
                                *spd = speed(*u, *v);
                        }
        	}
	}
*/	
	
	if (mucp < 100 || base < 750 ||  el < 0) {
        /* default to standard 0-6 km layer if cape > 100 but EL height is missing */
        /* Sfc-6km mean wind */
        mean_wind_npw(sndg[sfc()][pIndex], i_pres(msl(6000.0)), &u_mw, &v_mw, &ix1, &ix2);

        /* Sfc-6km Shear Vector */
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000.0)), &u_sv, &v_sv, &ix1, &ix2);


                if (qc(u_sv) && qc(v_sv)) {
                *u = u_mw + ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * v_sv);
                *v = v_mw - ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * u_sv);
                *dir = angle(*u, *v);
                *spd = speed(*u, *v);

                }
        }
	if(oldlplchoice < 1 || oldlplchoice>6)
		return;
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
       // printf("bunkers_storm_motion 2 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);

        define_parcel(oldlplchoice, pres);

	/*printf("\n ending storm dir = %0f\n", *dir);
	printf("\n ending storm spd = %0f\n", *spd);*/

}

void bunkers_left_motion(float *ul, float *vl, float *dirl, float *spdl)
        /*************************************************************/
        /*  EFFECTIVE BUNKERS_STORM_MOTION                           */
        /*  Rich Thompson and John Hart  SPC OUN                     */
        /*                                                           */
        /*  Calculates the motion of a right-moving supercell using  */
        /*  a method developed by Bunkers et. al. (2000),            */
        /*  modified to account for storm depth                      */
        /*                                                           */
        /*  *u, *v   - Storm motion vector (kt)                      */
        /*************************************************************/
{
        float u_mw, v_mw, u_sv, v_sv, ix1, ix2;
        /* Deviation Value (emperically derived as 8 m/s) */
        float d = 7.5 / 0.51479;
/* 24 Mar 2008 */
/*        float base, el, depth, p_bot, p_top, oldlplpres, pres, mucp; */

	float base, el, depth, oldlplpres, pres, mucp;
        struct _parcel pcl;

        short pIndex, oldlplchoice;

        *ul   = RMISSD;
        *vl   = RMISSD;
        *dirl = RMISSD;
        *spdl = RMISSD;

        pIndex = getParmIndex("PRES");

        if (!sndg || pIndex == -1)
          return;

        oldlplchoice = lplvals.flag;

        define_parcel(3, 400);
        mucp = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);

        el = agl(i_hght(pcl.elpres, I_PRES));

        if (pcl.bplus >= 100 && el > 0)
        {
/* 24 Mar 2008 */
/*        effective_inflow_layer(100,-250, &p_bot, &p_top);
*/

/*      printf("\nBunkers L inflow base  = %0.1f", agl(i_hght(p_bot, I_PRES)));
        printf("\nBunkers L inflow top  = %0.1f\n", agl(i_hght(p_top, I_PRES)));
*/

	base = agl(i_hght(p_bot, I_PRES));
	if (base >= 750){
                depth = (el - base);
                mean_wind_npw( p_bot, i_pres(msl(base + (depth * 0.5))), &u_mw, &v_mw, &ix1, &ix2);
                wind_shear(p_bot, i_pres(msl(base + (depth * 0.5))), &u_sv, &v_sv, &ix1, &ix2);

                if (qc(u_sv) && qc(v_sv)) {
                *ul = u_mw - ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * v_sv);
                *vl = v_mw + ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * u_sv);
                *dirl = angle(*ul, *vl);
                *spdl = speed(*ul, *vl);
                }
          }
        }
/*        if(p_bot < 0) {
                        base = agl(i_hght(pcl.lplpres, I_PRES));
                        depth = (el - base);

                        mean_wind_npw( pcl.lplpres, i_pres(msl(base + (depth * 0.5))), &u_mw, &v_mw, &ix1, &ix2);
                        wind_shear( pcl.lplpres, i_pres(msl(base + (depth * 0.5))), &u_sv, &v_sv, &ix1, &ix2);
                        if (qc(u_sv) && qc(v_sv)) {
                                *u = u_mw + ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * v_sv);
                                *v = v_mw - ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * u_sv);
                                *dir = angle(*u, *v);
                                *spd = speed(*u, *v);
                        }
                }
        }
*/
        if (mucp < 100 || base < 750 || el < 0) {
        /* default to standard 0-6 km layer if cape > 100 but EL height is missing */
        /* Sfc-6km mean wind */
        mean_wind_npw(sndg[sfc()][pIndex], i_pres(msl(6000.0)), &u_mw, &v_mw, &ix1, &ix2);

        /* Sfc-6km Shear Vector */
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000.0)), &u_sv, &v_sv, &ix1, &ix2);


                if (qc(u_sv) && qc(v_sv)) {
                *ul = u_mw - ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * v_sv);
                *vl = v_mw + ((d / pow((u_sv * u_sv) + (v_sv * v_sv), 0.5)) * u_sv);
                *dirl = angle(*ul, *vl);
                *spdl = speed(*ul, *vl);
                }
        }
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
       // printf("5 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);
        define_parcel(oldlplchoice, pres);

/*        printf("\n ending storm dir = %0f\n", *dir);
        printf("\n ending storm spd = %0f\n", *spd);
*/
}


        void effective_inflow_layer(float ecape, float ecinh, float *bot, float *top)
        /******************************************************************/
        /*  Effective Inflow Layer                                        */
        /*  John Hart & Rich Thompson     SPC Norman OK                   */
        /*                                                                */
        /*  Calculates the effective layer top and bottom (mb).           */
        /*  Based on research by Thompson et. al. 2004                    */
        /*                                                                */
        /******************************************************************/
        {
        short i, j, tIndex, tdIndex, pIndex, oldlplchoice, ok;
        float ix1, mucape, mucin, pres, mucp, mucn;
        Parcel pcl;

        oldlplchoice = lplvals.flag;

	define_parcel(3, 300);
	mucp = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	mucn = pcl.bminus;
	
        define_parcel(3, 400);
        parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mucape = pcl.bplus;
        mucin = pcl.bminus;

	/* scenario where shallow buoyancy present for lesser theta parcel near ground */
	if (mucp > mucape){
		mucape = mucp;
		mucin = mucn;
		}	

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
        //printf("6 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);

        define_parcel(oldlplchoice, pres);

        *bot = RMISSD;
        *top = RMISSD;

        if (!sndg) { return; }

        pIndex  = getParmIndex("PRES");
        tIndex  = getParmIndex("TEMP");
        tdIndex = getParmIndex("DWPT");

        if (pIndex == -1 || tIndex == -1 || tdIndex == -1) { return; }

        if (mucape >= 100 && mucin >= -250)
        {

        		//     printf( "Determining Effective Surface\n");
        		// ----- Begin at surface and search upward for "Effective Surface" -----
                for(i=sfc();i<=numlvl-1;i++)
                {
                ix1 = parcel( -1, -1, sndg[i][pIndex], sndg[i][tIndex], sndg[i][tdIndex], &pcl);
                if((pcl.bplus >= ecape) && (pcl.bminus >= ecinh))
                        {
                        *bot = sndg[i][pIndex];
        				// printf( "EFFSFC = %f\n", *bot);
                        break;
                        }
                }

                if (*bot == RMISSD) return;

        		//             printf( "Determining Effective Top\n");
        		// ----- Keep searching upward for the "Effective Top" -----
                for(i=sfc();i<=numlvl-1;i++)
                {
                if (sndg[i][pIndex] <= *bot)
                        {
			if ((sndg[i][tIndex] != RMISSD) && (sndg[i][tdIndex] != RMISSD)) {
	                        ix1 = parcel( -1, -1, sndg[i][pIndex], sndg[i][tIndex], sndg[i][tdIndex], &pcl);
        					//printf("%.2f %.2f %.2f  - %.2f %.2f\n", sndg[i][pIndex], sndg[i][tIndex], sndg[i][tdIndex], pcl.bplus, pcl.bminus);
                	        if((pcl.bplus <= ecape) || (pcl.bminus <= ecinh))
        						//check for missing T/Td data with significant wind levels in obs soundings
                        	        {
					ok = 0;
					j=1;

        						while (!ok && i-j >=0 && i-j <= numlvl-1) {
						if ((sndg[i-j][tIndex] != RMISSD) && (sndg[i-j][tdIndex] != RMISSD)) { ok=1; } else { j++; }
						}
        						if(i-j >=0 && i-j <= numlvl-1)
                                	*top = sndg[i-j][pIndex];
        						//printf("inflow top = %f\n", *top);
        	                        break;
                	                }
				}
                        }
                }

        	}
        	//	printf( "EIL - %f\n", *bot);


        }

float scp(float stdir, float stspd)
/***************************************************************/
/*                                                             */
/*  Computes the Supercell Composite Parameter                 */
/*  (uses effective layer) - RLT 1/17/05                       */
/*                                                             */
/***************************************************************/
        {
/* 24 Mar 2008 */
/*        float ix1, ix2, ix3, ix4, lpl, el, depth, eshear, esrh, pbot, ptop, fudge, cape, scp_new;
        float eshear40, eshear50, eshear60, oldlplpres, pres, base;*/

	float ix1, ix2, ix3, ix4, lpl, el, depth, eshear, esrh, fudge, cape, scp_new;
        float oldlplpres, pres, base;
        int idxp;

        short oldlplchoice;
        struct _parcel pcl;
        //fprintf(stderr,"scp called wdir=%f, wspd=%f\n", stdir, stspd);
        idxp = getParmIndex("PRES");

        oldlplchoice = lplvals.flag;

        define_parcel(3,400);
        cape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        el = agl(i_hght(pcl.elpres, I_PRES));

/* 24 Mar 2008 */
/*	effective_inflow_layer(100, -250, &pbot, &ptop);

        base = agl(i_hght(pbot, I_PRES));

        printf("\nSCP inflow base  = %0.1f", agl(i_hght(p_bot, I_PRES)));
        printf("\nSCP inflow top  = %0.1f\n", agl(i_hght(p_top, I_PRES)));
*/

        base = agl(i_hght(p_bot, I_PRES));
        depth = (el - base);

        /*wind_shear(pbot, i_pres(msl(base + (depth * 0.4))), &ix1, &ix2, &ix3, &ix4);
        eshear40 = ix4;*/

/* 24 Mar 2008 */
/*        wind_shear(pbot, i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &ix4);
        eshear = ix4;
*/

        wind_shear(p_bot, i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &ix4);
        eshear = ix4;

        /*wind_shear(pbot, i_pres(msl(base + (depth * 0.6))), &ix1, &ix2, &ix3, &ix4);
        eshear60 = ix4;

        if ((eshear40 >= eshear50) && (eshear40 >= eshear60)) eshear=eshear40;
        else if ((eshear50 >= eshear40) && (eshear50 >= eshear60)) eshear=eshear50;
        else if ((eshear60 >= eshear40) && (eshear60 >= eshear50)) eshear=eshear60;*/

        if ((cape >= 100) && (el < 0)) {
                wind_shear( i_pres(msl(0)), i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
                eshear = ix4;
                }

/* 24 Mar 2008 */
/*        esrh = helicity(base, agl(i_hght(ptop, I_PRES)), stdir, stspd, &ix2, &ix3);
*/

          esrh = helicity(base, agl(i_hght(p_top, I_PRES)), stdir, stspd, &ix2, &ix3);
        if (!qc(cape)) return -9999;

        if (eshear < 20 ) eshear = 0;
        if (eshear > 40 ) eshear = 1;
        else
                eshear = (eshear / 40 );
        if (esrh < -998) esrh = 0;

        scp_new = (eshear * (esrh/50) * (cape/1000));

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
        //printf("7 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);

        define_parcel(oldlplchoice, pres);

        return scp_new;
        }

float sigtorn_fixed(float stdir, float stspd)
/***************************************************************/
/*                                                             */
/*      WAF 2003 Significant Tornado Parameter (fixed layer)   */
/*      Rich Thompson SPC OUN                                  */
/*      Adapted to sbCAPE and fixed layer shear terms          */
/***************************************************************/
	{
	float ix1, ix2, ix3, ix4, srh1, shr6, sbcp, lclh, pres, stpf;
	short pIndex, oldlplchoice;
	struct _parcel pcl;

	oldlplchoice = lplvals.flag;

        pIndex = getParmIndex("PRES");

	/* sbCAPE and LCL */
        define_parcel(1,0);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	sbcp = pcl.bplus;
        lclh = agl(i_hght(pcl.lclpres, I_PRES));	 	

	/* 0-6 km bulk shear and 0-1 km SRH */
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &shr6);
        srh1 = helicity(0, 1000, st_dir, st_spd, &ix2, &ix3);
      
	if (sbcp < 0) sbcp = 0;
	if (srh1 < 0) srh1 = 0;  
	if (lclh > 2000) lclh = 0;
        else
        if (lclh <= 1000) lclh = 1;
        else
                lclh = ((2000-lclh)/1000);

        if (shr6< 25 ) shr6 = 0;
        else
        if (shr6 >= 60) shr6 = 1.5;
        else
                shr6 = (shr6 / 40);

        stpf = (shr6) * (srh1/150) * (sbcp/1500) * (lclh);
        if (stpf < 0) stpf = 0;

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
        //printf("8 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);

        define_parcel(oldlplchoice, pres);

	return stpf;
	}

float sigtorn(float stdir, float stspd)
/***************************************************************/
/*                                                             */
/*      Significant Tornado Parameter (effective layer)        */
/*      Rich Thompson SPC OUN                                  */
/*                                                             */
/***************************************************************/
        {
/* 24 Mar 2008 */
/*      float ix1, ix2, ix3, ix4, eshear, esrh, mlcape, lclh, stp, cape3km, lpl, el, depth, pbot, ptop;
*/
	float ix1, ix2, ix3, ix4, eshear, esrh, mlcape, lclh, stp, cape3km, lpl, el, depth;
        float eshear40, eshear50, eshear60, pres, oldlplpres, base, cape;
        int idxp;
        short oldlplchoice;
        struct _parcel pcl;

        oldlplchoice = lplvals.flag;

        idxp = getParmIndex("PRES");

        define_parcel(3,400);
        cape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        el = agl(i_hght(pcl.elpres, I_PRES));

/* 24 Mar 2008 */
/*	effective_inflow_layer(100, -250, &pbot, &ptop);
	
        base = agl(i_hght(pbot, I_PRES));
*/
	base = agl(i_hght(p_bot, I_PRES));

        depth = (el - base);

        /*wind_shear(pbot, i_pres(msl(base + (depth * 0.4))), &ix1, &ix2, &ix3, &ix4);
        eshear40 = ix4;*/
        wind_shear(p_bot, i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &ix4);
        eshear = ix4;
        /*wind_shear(pbot, i_pres(msl(base + (depth * 0.6))), &ix1, &ix2, &ix3, &ix4);
        eshear60 = ix4;

        if ((eshear40 >= eshear50) && (eshear40 >= eshear60)) eshear=eshear40;
        else if ((eshear50 >= eshear40) && (eshear50 >= eshear60)) eshear=eshear50;
        else if ((eshear60 >= eshear40) && (eshear60 >= eshear50)) eshear=eshear60;*/

        if ((cape >= 100) && (el < 0)) {
                wind_shear( i_pres(msl(0)), i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
                eshear = ix4;
                }

/* 24 Mar 2008 */
/*        esrh = helicity(base, agl(i_hght(ptop, I_PRES)), stdir, stspd, &ix2, &ix3);
*/
          esrh = helicity(base, agl(i_hght(p_top, I_PRES)), stdir, stspd, &ix2, &ix3);

        define_parcel(4,100);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mlcape   = pcl.bplus;

        cape3km = pcl.cape3km;

        if (!qc(mlcape)) return -9999;

        lclh = agl(i_hght(pcl.lclpres, I_PRES));

        if (lclh > 2000) lclh = 0;
        else
        if (lclh <= 1000) lclh = 1;
        else
                lclh = ((2000-lclh)/1000);

        if (eshear < 25 ) eshear = 0;
        else
        if (eshear >= 60) eshear = 1.5;
        else
                eshear = (eshear/40);

        if (esrh <  -9998) esrh = 0;

        /* stp = (shr6/40) * (eh1km/100) * (cape/1000) * (cinh/125) * (lclh/1750); */
        stp = (eshear) * (esrh/150) * (mlcape/1500) * (lclh);
        if ( agl(i_hght(p_bot, I_PRES)) > 0.0) stp = 0;
        if (stp < 0) stp = 0;

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
       // printf("9 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);

        define_parcel(oldlplchoice, pres);

        return stp;
	}

float sigtorn_test(float stdir, float stspd)
/***************************************************************/
/*                                                             */
/*      Significant Tornado Parameter (effective layer)        */
/*      Rich Thompson SPC OUN                                  */
/*                                                             */
/***************************************************************/
        {
/* 24 Mar 2008 */
/*        float ix1, ix2, ix3, ix4, eshear, esrh, mlcape, lclh, stp, lpl, el, depth, pbot, ptop;*/

	float ix1, ix2, ix3, ix4, eshear, esrh, mlcape, lclh, stp, lpl, el, depth;
        float eshear40, eshear50, eshear60, pres, oldlplpres, base, cape, cape6km;
        int idxp;
        short oldlplchoice;
        struct _parcel pcl;

        oldlplchoice = lplvals.flag;

        idxp = getParmIndex("PRES");

        define_parcel(3,400);
        cape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        el = agl(i_hght(pcl.elpres, I_PRES));

/* 24 Mar 2008 */
/*        effective_inflow_layer(100, -250, &pbot, &ptop);

        base = agl(i_hght(pbot, I_PRES));
*/
	base = agl(i_hght(p_bot, I_PRES));

        depth = (el - base);

        wind_shear(p_bot, i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &ix4);
        eshear = ix4;

        if ((cape >= 100) && (el < 0)) {
                wind_shear( i_pres(msl(0)), i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
                eshear = ix4;
                }

/* 24 Mar 2008 */
/*        esrh = helicity(base, agl(i_hght(ptop, I_PRES)), stdir, stspd, &ix2, &ix3);
*/
          esrh = helicity(base, agl(i_hght(p_top, I_PRES)), stdir, stspd, &ix2, &ix3);

        define_parcel(4,100);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        cape6km = pcl.cape6km;

        if (!qc(mlcape)) return -9999;

        lclh = agl(i_hght(pcl.lclpres, I_PRES));

        if (lclh > 2000) lclh = 0;
        else
        if (lclh <= 1000) lclh = 1;
        else
                lclh = ((2000-lclh)/1000);

        if (eshear < 25 ) eshear = 0;
        else
        if (eshear >= 60) eshear = 1.5;
        else
                eshear = (eshear/40);

        if (esrh <  -9998) esrh = 0;

        stp = (eshear) * (esrh/150) * (cape6km/500) * (lclh);
        if ( agl(i_hght(p_bot, I_PRES)) > 0.0) stp = 0;
        if (stp < 0) stp = 0;

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
       // printf("10 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);

        define_parcel(oldlplchoice, pres);

        return stp;
        }

float sigtorn_cin(float stdir, float stspd)
/*****************************************************************/
/*                                                               */
/*      Significant Tornado Parameter (effective layer with CIN) */
/*      Rich Thompson SPC OUN                                    */
/*                                                               */
/*****************************************************************/
        {
/* 24 Mar 2008 */
/*        float ix1, ix2, ix3, ix4, eshear, esrh, mlcape, mlcinh, lclh, stp_cin, lpl, el, depth, pbot, ptop; */

	float ix1, ix2, ix3, ix4, eshear, esrh, mlcape, mlcinh, lclh, stp_cin, lpl, el, depth;
        float eshear40, eshear50, eshear60, oldlplpres, pres, base, cape;
        int idxp;

        short oldlplchoice;
        struct _parcel pcl;

        oldlplchoice = lplvals.flag;

        idxp = getParmIndex("PRES");

        define_parcel(3,400);
        cape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        el = agl(i_hght(pcl.elpres, I_PRES));
	
/* 24 Mar 2008 */
/*	effective_inflow_layer(100, -250, &pbot, &ptop);
	
        base = agl(i_hght(pbot, I_PRES));
*/
	base = agl(i_hght(p_bot, I_PRES));
        depth = (el - base);

        /*wind_shear(pbot, i_pres(msl(base + (depth * 0.4))), &ix1, &ix2, &ix3, &ix4);
        eshear40 = ix4;*/
        wind_shear(p_bot, i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &ix4);
        eshear = ix4;
        /*wind_shear(pbot, i_pres(msl(base + (depth * 0.6))), &ix1, &ix2, &ix3, &ix4);
        eshear60 = ix4;

        if ((eshear40 >= eshear50) && (eshear40 >= eshear60)) eshear=eshear40;
        else if ((eshear50 >= eshear40) && (eshear50 >= eshear60)) eshear=eshear50;
        else if ((eshear60 >= eshear40) && (eshear60 >= eshear50)) eshear=eshear60;*/

        if ((cape >= 100) && (el < 0)) {
                wind_shear( i_pres(msl(0)), i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
                eshear = ix4;
                }

/* 24 Mar 2008 */
/*        esrh = helicity( base, agl(i_hght(ptop, I_PRES)), stdir, stspd, &ix2, &ix3);
*/
	  esrh = helicity( base, agl(i_hght(p_top, I_PRES)), stdir, stspd, &ix2, &ix3);

        define_parcel(4,100);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mlcape   = pcl.bplus;

        mlcinh   = pcl.bminus;

        if (!qc(mlcape)) return -9999;

        if (mlcinh >= -50 ) mlcinh=1;
        else
        if (mlcinh < -200 ) mlcinh=0;
        else
                mlcinh = ((200 + mlcinh)/150);

        lclh = agl(i_hght(pcl.lclpres, I_PRES));

        if (lclh > 2000) lclh = 0;
        else
        if (lclh <= 1000) lclh = 1;
        else
                lclh = ((2000-lclh)/1000);

        if (eshear < 25 ) eshear = 0;
        else
        if (eshear >= 60) eshear = 1.5;
        else
                eshear = (eshear/40);

        if (esrh <  -9998) esrh = 0;

        /* stp = (shr6/40) * (eh1km/100) * (cape/1000) * (cinh/125) * (lclh/1750); */
        stp_cin = (eshear) * (esrh/150) * (mlcape/1500) * (lclh) * (mlcinh);

        if ( agl(i_hght(p_bot, I_PRES)) > 0.0) stp_cin = 0;
        if (stp_cin < 0) stp_cin = 0;

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
        //printf("11 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);

        define_parcel(oldlplchoice, pres);

        return stp_cin;

        }

float CB_sigtor(void)
/********************************************************************/
/* Craven&Brooks SigTorn                                            */
/********************************************************************/
        {
        float ix1, ix2, ix3, ix4, shr6, shr1, cape, lclh, cbt, dcapeval, oldlplpres, pres;
        short idxp, oldlplchoice;
        struct _parcel pcl;
        struct _lplvalues lpl2;

        /* How likely are significant tornadoes? */
        /* CBT = (6ksh * 1ksh * mlcp) / (mllcl hgt * dcape) */
        /* units are m/s2 */

        oldlplchoice = lplvals.flag;
/*      oldlplpres = lplvals.pres;
*/
        idxp = getParmIndex("PRES");

        lpl2 = lplvals;

        /* lift a mixed parcel */
        define_parcel(4, 100);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        lplvals = lpl2;

        wind_shear(sndg[sfc()][idxp], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        shr6 = ix4 *.514;

        wind_shear(sndg[sfc()][idxp], i_pres(msl(1000)), &ix1, &ix2, &ix3, &ix4);
        shr1 = ix4 *.514;

        cape   = pcl.bplus;

        dcapeval = dcape(&ix1, &ix2);

        lclh   = agl( i_hght(pcl.lclpres, I_PRES) );

        if (!qc(cape)) return -9999;

        cbt = (cape * shr6 * shr1) / (dcapeval * lclh);
        if (cbt > 5) cbt = -999;

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
       // printf("12 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);

        define_parcel(oldlplchoice, pres);

        return cbt;

        }

        /* NP */
        /* Enhanced Stretching Potential (J. Davies) */
float esp(void)
        /********************************************************************/
        /*      Enhanced Stretching Potential (Jon Davies)                  */
        /*      (uses 0-3 km mlcape and lapse rate)                         */
        /*                                                                  */
        /*      Rich Thompson SPC OUN                                       */
        /********************************************************************/
        {
        float ix1, ix2, cape3km, lllr, esp_1, pres; 
        int pIndex, zIndex;
        short oldlplchoice;
        struct _parcel pcl;

        oldlplchoice = lplvals.flag;

        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");

        lapse_rate(&ix2, sndg[sfc()][pIndex], i_pres(sndg[sfc()][zIndex]+3000));
        if (ix2 < 7.0) ix2 = 7.0;

        define_parcel(4, 100);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        cape3km = pcl.cape3km;

        esp_1 = ((ix2 - 7.0)/1.0) * (cape3km/50);
        if (pcl.bplus < 250.0) esp_1 = 0.0; 

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
       // printf("13 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);

        define_parcel(oldlplchoice, pres);

        return esp_1;
        }


float damaging_wind(void)
        /********************************************************************/
        /*      Damaging wind potential                                     */
        /*      (uses mlcape, 0-3 km lr, and 850-600 mb mean wind speed)    */
        /*                                                                  */
        /*      Rich Thompson SPC OUN                                       */
        /********************************************************************/

        {
        float mlcp, mlcn,  wndg, ix1, ix2, ix3, ix4, u_mw, v_mw, oldlplpres, pres;
        int pIndex, zIndex;
        short oldlplchoice;
        struct _parcel pcl;

        oldlplchoice = lplvals.flag;

        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");

        lapse_rate(&ix2, sndg[sfc()][pIndex], i_pres(sndg[sfc()][zIndex]+3000));
        if (ix2 < 7) ix2 = 0.0;

        define_parcel(4, mml_layer);
        ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mlcp = pcl.bplus;

        if (pcl.bminus > -10.0) mlcn = 1;
        if (pcl.bminus < -50.0) mlcn = 0;
        else
                mlcn = ((50 + pcl.bminus) / 40);

        mean_wind_npw(i_pres(msl(1000.0)), i_pres(msl(3500.0)), &u_mw, &v_mw, &ix3, &ix4);

        wndg = ((ix1/2000) * (ix2 / 9) * (ix4 / 30) * (mlcn));

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
       // printf("14 calling define_parcel  flag=%d-pres=%f------------------------->\n", oldlplchoice, pres);

        define_parcel(oldlplchoice, pres);

        return wndg;
        }

        /* NP */
/* 22 july 05 edit
short ww_type(void)
*/
        /********************************************************************/
        /*      Watch type guidance                                         */
        /*      A decision tree to help with ww issuance		    */
        /*                                                                  */
        /*      Rich Thompson SPC OUN                                       */
        /********************************************************************/
/* 22 july 07 edit
        {
        float ix1, ix2, ix3, ix4, lr75, shr6, t500, fzlh, mumixr, lowrh, midrh, low_mid_rh;
        float mucn, mlcn, mlcp, sbcp, lr1, shr6_dir, sr46_dir, sr46_spd, shr6_sr46_diff, mmp;
        float sig_tor, sig_tor_winter, sighail, wind_dmg, rm_scp, cbsig;
        float oldlplpres, pres, pbot, ptop;
        short oldlplchoice, ww_choice;
        short pIndex, tIndex, zIndex, tdIndex;
        struct _parcel pcl;

        oldlplchoice = lplvals.flag;

        tIndex = getParmIndex("TEMP");
        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");
        tdIndex = getParmIndex("DWPT");

	effective_inflow_layer(100, -250, &pbot, &ptop);

        define_parcel(1,0);
        ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        sbcp = pcl.bplus;
	sig_tor_winter = sigtorn_fixed(st_dir, st_spd);

        define_parcel(4,100);
        ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mlcn = pcl.bminus;
        mlcp = pcl.bplus;
        sig_tor = sigtorn_cin(st_dir, st_spd);
	
        define_parcel(3,400);
        ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mucn = pcl.bminus;

        lr75 = lapse_rate(&ix1, 700, 500);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        shr6 = ix4;
        shr6_dir = ix3;
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

        lapse_rate(&ix2, sndg[sfc()][pIndex], i_pres(sndg[sfc()][zIndex]+1000));
        lr1 = ix2;

        mean_relhum(&ix1, sndg[sfc()][pIndex]-150, sndg[sfc()][pIndex]-350);
        midrh = ix1;
        mean_relhum(&ix1, sndg[sfc()][pIndex], sndg[sfc()][pIndex]-150);
        lowrh = ix1;
        low_mid_rh = ((lowrh + midrh)/2);
        mmp = coniglio1();
        cbsig = (mlcp * kt_to_mps(shr6));
*/
/*
        printf("\n sig_tor = %.1f\n", sig_tor);
        printf("\n sr_shr_diff = %.1f\n", shr6_sr46_diff);
        printf("\n sr_spd = %.1f\n", sr46_spd);
        printf("\n low_mid_rh = %.1f\n", low_mid_rh);
        printf("\n sig_hail = %.1f\n", sighail);
        printf("\n rm_scp = %.1f\n", rm_scp);
        printf("\n mmp = %.1f\n", mmp);
        printf("\n cbsig = %.1f\n", cbsig);     
	printf("\n lr1 = %.1f\n", lr1);
*/
/*        if ((( sig_tor >= 3.0) && (shr6_sr46_diff <= 45.0)) && (((sr46_spd >= 15.0) && (low_mid_rh < 80.0)) ||
((sr46_spd <= 15.0) && (low_mid_rh >= 80.0)))) {
                ww_choice = 1;
                printf("\n*****************\n");                
                printf("\nww type = PDS TOR\n");
                printf("\n*****************\n\n");
                }
*/
/* 22 july 07 edit
        if ((sig_tor >= 3.0) && (sr46_spd >= 12.0) && (lr1 >= 5.0) && (agl(i_hght(pbot, I_PRES)) < 1)) {
                ww_choice = 1;
                printf("\n*****************\n");
                printf("\nww type = PDS TOR\n");
                printf("\n*****************\n\n");
                }

        else

        if ((sig_tor_winter >= 8.0) && (sr46_spd >= 12.0) && (agl(i_hght(pbot, I_PRES)) < 1)) {
                ww_choice = 1;
                printf("\n*****************\n");
                printf("\nww type = PDS TOR\n");
                printf("\n*****************\n\n");
                }
*/
/*        if (( sig_tor >= 1.0) && (rm_scp >= 4.0) && (shr6_sr46_diff <= 60.0) && 
(((sr46_spd >= 12.0) && (low_mid_rh < 80.0)) || ((sr46_spd <= 12.0) && (low_mid_rh >= 80.0)))) {
                ww_choice = 2;
                printf("\n*************\n");
                printf("\nww type = TOR\n");
                printf("\n*************\n\n");
                }

        if (( sig_tor >= 1.0) && (rm_scp >= 4.0) && (lr1 >= 5.0) && (((sr46_spd >= 12.0) && (low_mid_rh < 80.0)) ||
((sr46_spd <= 12.0) && (low_mid_rh >= 80.0)))) {
                ww_choice = 2;
                printf("\n*************\n");
                printf("\nww type = TOR\n");
                printf("\n*************\n\n");
                }
*/

/* 22 July 07 edit 
	else
        if (( sig_tor_winter >= 2.0) && (rm_scp >= 4.0) && (sr46_spd >= 10.0) && (agl(i_hght(pbot, I_PRES)) < 1)) {
                ww_choice = 2;
                printf("\n*************\n");
                printf("\nww type = TOR\n");
                printf("\n*************\n\n");
                }

        else
        if (( sig_tor >= 1.0) && (rm_scp >= 4.0) && (sr46_spd >= 10.0) && (agl(i_hght(pbot, I_PRES)) < 1)) {
                ww_choice = 2;
                printf("\n*************\n");
                printf("\nww type = TOR\n");
                printf("\n*************\n\n");
                }

	else
        if (( sig_tor_winter >= 2.0) && (rm_scp >= 4.0) && ((sr46_spd < 10.0) || (lr1 < 5.0))) {
                ww_choice = 3;
                printf("\n*************\n");
                printf("\nww type = SVR");
                printf("\n*************\n\n");
                }

        else
        if (( sig_tor >= 1.0) && (rm_scp >= 4.0) && ((sr46_spd < 10.0) || (lr1 < 5.0))) {
                ww_choice = 3;
                printf("\n*************\n");
                printf("\nww type = SVR\n");
                printf("\n*************\n\n");
                }

        else
        if (((rm_scp >= 2.0) && (sighail >= 0.5)) && (mucn >= -25.0)) {
                ww_choice = 3;
                printf("\n_____________\n");
                printf("\nww type = SVR\n");
                printf("\n_____________\n\n");
                }
        else
        if ((cbsig >= 40000) && (mmp >= 0.5) && (mucn >= -50.0)) {
                ww_choice = 3;
                printf("\n_____________\n");
                printf("\nww type = SVR\n");
                printf("\n_____________\n\n");
                }
        else
        if ((mucn >= -25.0) && ((wind_dmg >= 1.0) || (sighail >= 1.0)))  {
                ww_choice = 3;
                printf("\n__________________\n");
                printf("\nww type = mrgl SVR\n");
                printf("\n__________________\n\n");
                }
        else
                ww_choice = 0;

        if (ww_choice == 0) {
                printf("\nww type = none\n\n");
                }
*/

/*      define_parcel(oldlplchoice, oldlplpres);
        
        printf("\n ww type (1,2=TOR, 3=SVR, 4=mrgl SVR = %c\n", ww_choice);
*/

        /* set parcel back to user selection */
/* 27 july 07 edit 
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
*/
/*float convective_intiation(void)
*/
        /********************************************************************/
        /*      Convective Initiation                                       */
        /*                                                                  */
        /*      Calculates parcel residence time within 10 km of a boundary */
        /*      and number of minutes needed for a parcel to reach its LFC. */
        /*      Prompts user for motion of boundary perpendicular to the    */
        /*      boundary orientation, and assumes 1 m/s updraft             */
        /*                                                                  */
        /*      Rich Thompson SPC OUN                                       */
        /********************************************************************/
/*
        {
        float oldlplpres, lfc, ascent1, ascent5, p1, p2, u_mw, v_mw, ix1, ix2;
	float minutes_lift_1, minutes_lift_5, min_lift;
        short oldlplchoice, pIndex, zIndex;
        struct _parcel pcl;

        oldlplchoice = lplvals.flag;
        oldlplpres = lplvals.pres;

        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");

*/
        /* calculate lfc height and minutes of lift needed to reach lfc at 1 m/s  */
/*      define_parcel(4,100);
        ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        lfc = agl(i_hght(pcl.lfcpres, I_PRES));
	if( lfc < 0){ return min_lift = -9999;}

        ascent1 = 60.0;
	ascent5 = 300.0;
        minutes_lift_1 = lfc / ascent1;
	minutes_lift_5 = lfc / ascent5; 
*/
	/* input motion of boundary (normal to boundary orientation) */
	/* boundary motion speed assigned to bndry_spd, boundary motion direction assigned to bndry_dir */


        /* calculate parcel distance from boundary - assume ascent within 10 km zone */
        /* calculate trajectory allowing parcel to ascend in one minute time steps */
	/* convective initiation assumed when min_lift < minutes_lift (parcel reaches lfc before detraining) */


	/* iterative loop to calculate parcel residence time in zone of ascent */
	/* compare ascent along boundary to ml lfc for sounding */

/*	for (i = 0; i < minutes_lift_1; i = i + 1) {

*/

        /* vector difference between mean wind and boundary motion */
	/* avg of top and bottom of trajectory layer */
/*
	p1 = sndg[sfc][pIndex];
	z1 = sndg[sfc][zIndex];
	p2 = i_pres((z1 + ascent1), I_PRES);
        sr_wind( p1, p2, bndry_dir, bndry_spd, &u, &v, &ix1, &ix2);
*/
        /* calculate distance parcel has moved from boundary */

        /* parcel setback */
/*      define_parcel(oldlplchoice, oldlplpres);

        }
*/


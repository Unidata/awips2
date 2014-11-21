/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  DOS Video Graphics Routines (Part #3)                      */
/*  These routines are used to display the various indices and */
/*  parameters in the bottom left and middle boxes.            */
/*                                                             */
/*  John A. Hart and Richard L. Thompson                       */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  These routines are legacy code from multi-page NSHARP      */
/*  (roughly lines 65-2135)  				       */		
/*  SHOW_THERMOPARMS                                           */
/*  SHOW_MOISTURE                                              */
/*  SHOW_INSTABILITY                                           */
/*  SHOW_PARCEL                                                */
/*  SHOW_HODOPAGE2                                             */
/*  SHOW_SHEAR                                                 */
/*  SHOW_STORMPAGE3                                            */
/*  SHOW_PAGE                                                  */
/*  SHOW_STORMTYPE                                             */
/*  SHOW_SRDATA                                                */
/*  SHOW_INITIATION                                            */
/*  SHOW_SEVERE                                                */
/*  SHOW_HAILPOT                                               */
/*  SHOW_MAINPAGE					       */
/*  MAIN_THERMO						       */
/*  MAIN_WINDS						       */		 
/*                                                             */
/*  Routines below are used in current version of NSHARP       */
/*  CLEAR_PARAMAREA                                            */ 
/*  SHOW_SKEWTPAGE1                                            */ 
/*  SHOW_PARCEL_NEW					       */	
/*  SHOW_SHEAR_NEW					       */
/*  SHOW_WINTER_NEW					       */
/*  SHOW_HAIL_NEW					       */
/*  SHOW_SARS						       */
/*  SHOW_STP_STATS					       */
/*  PROB_SIGT_MLCAPE					       */
/*  PROB_SIGT_MLLCL					       */
/*  PROB_SIGT_ESRH					       */
/*  PROB_SIGT_EBS					       */
/*  PROB_SIGT_STP					       */
/*  PROB_SIGT_STPC					       */
/*  SHOW_SHIP_STATS					       */
/*  SHOW_EBS_STATS					       */
/*  SHOW_FIRE						       */	
/*  							       */	
/***************************************************************/

#define VIDEO
#define sars            sars_
#define spnsharp        spnsharp_
#define hailcast1       hailcast1_
#include "xwcmn.h"
#include "sharp95.h"
void sars(float *, float *, float * , float *, float *, float *, float *, float *, float *, int *, float *, float *, float *, float *, char *[15], float *[15], char *[80]);
void spnsharp(float *, float *, float * , float *, float *, float *, int *, float *, float *, char *[15], float *[15], char *[80],
float *, float *, float *);

char *hailwords( float num );

	/*NP*/
	void show_thermoparms( void )
	/*************************************************************/
	/*  SHOW_THERMOPARMS                                         */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays several thermodynamic parameters on the screen. */
	/*                                                           */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4;
	short txtlin, txtrow;
	char st[80];

	/* ----- Thermo Data Area ----- */
	setcolor(3);
	set_font(1);
	txtlin = 350;
	strcpy( st, "THERMODYNAMIC DATA" );
	outgtext ( st, 
		(((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2)),
		txtlin );
	setcolor(0);
	rectangle( 1, skv.brx + 22, txtlin+20, xwdth-7, txtlin+255 );
	setcolor(7);
	rectangle( 0, skv.brx + 22, txtlin+20, xwdth-7, txtlin+255 );

	show_moisture();
	show_instability();

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(5);
	strcpy( st, "------ MISC PARAMETERS ------" );
	txtrow = (((skv.brx + 20 + xwdth) / 2) - 
		   (getgtextextent( st ) / 2));
	txtlin += 8 * 21;
	outgtext ( st, txtrow, txtlin );
	setcolor(31);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "CB SigTor =", txtrow, txtlin );
	strcpy( st, qc2( CB_sigtor(), "", 2 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "K-Index =", txtrow, txtlin );
	strcpy( st, qc2( k_index( &ix1 ), "", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "SigTorn =", txtrow, txtlin );
	printf( "-------------------------------------> Calling sigtorn\n" );
	strcpy( st, qc2( sigtorn(st_dir, st_spd), "", 1 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "Max Temp =", txtrow, txtlin );
	strcpy( st, qc2( ctof(max_temp( &ix1, -1)), "F", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "Supercell =", txtrow, txtlin );
	strcpy( st, qc2( scp(st_dir, st_spd), "", 1 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "Conv Temp =", txtrow, txtlin );
	strcpy( st, qc2( ctof(cnvtv_temp( &ix1, -1)), "F", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "WBZ level =", txtrow, txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 ), I_PRES))), "ft", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "FGZ level =", txtrow, txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES))), "ft", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	/*bunkers_storm_motion(&ix1, &ix2, &ix3, &ix4);*/
	}

	/*NP*/
void show_moisture(void)
	/*************************************************************/
	/*  SHOW_MOISTURE                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays several moisture parameters on screen,          */
	/*  including:                                               */
	/*              1)  Precipitable Water                       */
	/*              2)  Mean Relative Humidity                   */
	/*              3)  Mean LL Relative Humidity (lowest 150mb) */
	/*              4)  Mean LL Mixing Ratio                     */
	/*              5)  Top of moist layer                       */
	/*************************************************************/
{
	float ix1;
	short txtlin, txtrow, pIndex;
	char st[40];

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(5);
	strcpy( st, "------ AVAILABLE MOISTURE ------" );
	txtrow = (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st )/2));
	txtlin = 375;
	outgtext ( st, txtrow, txtlin );

	txtrow = skv.brx + 25;
	txtlin += 18;
	setcolor(31);
	outgtext ( "P. Water =", txtrow, txtlin );
	strcpy(st, qc2( precip_water( &ix1, -1, -1), " in", 2 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext("Mean RH =", txtrow, txtlin );
	strcpy(st, qc2(mean_relhum(&ix1, -1, -1), " %" , 0));
	disp_param(st, txtrow + 165, txtlin);

	txtrow = skv.brx + 25;
	txtlin += 16;
	outgtext("Mean W =", txtrow, txtlin );
	strcpy(st, qc2(mean_mixratio(&ix1, -1, -1 ), " g/Kg", 1 ));
	disp_param(st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext("Mean LRH =", txtrow, txtlin );
	pIndex = getParmIndex("PRES");
	if (!sndg || pIndex == -1)
	  strcpy(st, "M");
	else
	  strcpy(st, qc2(mean_relhum(&ix1, -1, sndg[sfc()][pIndex] - 150), 
	    " %", 0));
	disp_param(st, txtrow + 165, txtlin);

	txtrow = skv.brx + 25;
	txtlin += 16;
	outgtext("Top of Moist Lyr =", txtrow, txtlin );
	strcpy(st, qc2( top_moistlyr( &ix1 ), " mb", 0 ));
	strcat(st, " / " );
	strcat(st, qc2(mtof(agl(i_hght(ix1, I_PRES))), " ft", 0 ));
	outgtext(st, txtrow+220, txtlin );
}

	/*NP*/
	void show_instability( void )
	/*************************************************************/
	/*  SHOW_INSTABILITY                                         */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays several instability indices on screen,          */
	/*  including:                                               */
	/*              1) 700-500mb Lapse Rates                     */
	/*              2) 850-500mb Lapse Rates                     */
	/*************************************************************/
	{
	float ix1;
	short txtlin, txtrow;
	char st[80];

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(5);
	strcpy( st, "------ CONDITIONAL INSTABILITY ------" );
	txtrow = (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st )/2));
	txtlin = 350 + ( 5 * 22);
	outgtext ( st, txtrow, txtlin );
	txtrow = skv.brx + 25;
	txtlin += 20;
	setcolor(31);
	outgtext ( "700-500mb Lapse Rate =", txtrow, txtlin );
	strcpy( st, qc2( delta_t( &ix1 ), " C", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( lapse_rate( &ix1, 700, 500 ), " C/km", 1 ));
	txtrow += 220;
	outgtext ( st, txtrow, txtlin );
	setcolor(31);
	txtlin = txtlin + 16;
	txtrow = skv.brx + 25;
	outgtext ( "850-500mb Lapse Rate =", txtrow, txtlin );
	strcpy( st, qc2( vert_tot( &ix1 ), " C", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( lapse_rate( &ix1, 850, 500 ), " C/km", 1 ));
	txtrow += 220;
	outgtext ( st, txtrow, txtlin );
	}

	/*NP*/
	void show_parcel( void )
	/*************************************************************/
	/*  SHOW_PARCEL                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Lifts a parcel, draws curve, and displays data on screen.*/
	/*************************************************************/
	{
	float ix1, sfctemp, sfcdwpt, sfcpres, j1, j2;
	float nv_cape, nv_cinh, nv_cap;
	short txtlin, txtrow;
	char st[40], st1[20];
	Parcel pcl;

	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	/* ----- Plot Parcel Trace on Skew-T ----- */
	setcolor(31);
	setlinestyle( 4, 1 );
/*      printf("\nshow_parcel temp=%.1f\n", sfctemp);
        printf("\nshow_parcel dwpt=%.1f\n", sfcdwpt);
        printf("\nshow_parcel pres=%.1f\n", sfcpres);

	trace_parcel( sfcpres, sfctemp, sfcdwpt);
*/	trace_dcape();

	setcliprgn ( skv.tlx, skv.bry+5, hov.brx, xhght - skv.bry - 10 );

	/* ----- Calculate Parcel Data ----- */
	ix1 = parcelx( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	nv_cape=pcl.bplus;
	nv_cinh=pcl.bminus;
	nv_cap =pcl.cap;

	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	j1 = pcl.bplus;
	j2 = i_hght(pcl.elpres, I_PRES) - i_hght(pcl.lfcpres, I_PRES);

	/* ----- Parcel Data Area ----- */
	setcolor(3);
	set_font(1);
	setlinestyle( 1, 1 );
	strcpy( st, "PARCEL DATA" );
	txtrow = (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st )/2));
	txtlin = 105;
	outgtext ( st, txtrow, txtlin );
	setcolor(0);
	rectangle( 1, skv.brx + 22, 125, xwdth-7, txtlin + 220);
	setcolor(7);
	rectangle( 0, skv.brx + 22, 125, xwdth-7, txtlin + 220);

	/* ----- Display Values on screen ----- */
	set_font(2);
	txtlin = 130;
	setcolor(5);
	strcpy( st, "*** ");
	strcat( st, lplvals.desc);
	strcat( st, " ***"  );
	txtrow = ((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2);
	outgtext ( st, txtrow, txtlin );
	setcolor(31);
	txtlin += 20;
	txtrow = skv.brx + 25;
	outgtext ( "LPL: ", txtrow, txtlin );
	itoa( (short)pcl.lplpres, st, 10);
	strcat( st, "mb   ");
	itoa( (short)pcl.lpltemp, st1, 10);
	strcat( st, st1 );
	strcat( st, "C / ");
	itoa( (short)pcl.lpldwpt, st1, 10);
	strcat( st, st1 );
	strcat( st, "C   ");
	itoa( (short)ctof(pcl.lpltemp), st1, 10);
	strcat( st, st1 );
	strcat( st, "F / ");
	itoa( (short)ctof(pcl.lpldwpt), st1, 10);
	strcat( st, st1 );
	strcat( st, "F");
	txtrow += 70;
	outgtext ( st, txtrow, txtlin );

	setcolor(31);
	txtlin += 30;
	txtrow = skv.brx + 25;
	outgtext ( "CAPE =", txtrow, txtlin );
	strcpy( st, qc2( pcl.bplus, " J/Kg" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 205;
	outgtext ( "LI (500mb) =", txtrow, txtlin );
	strcpy( st, qc2( pcl.li5, " C " , 0));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "Cape3km =", txtrow, txtlin );
	strcpy( st, qc2( pcl.cape3km, " J/Kg", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 205;
	outgtext ( "LImin =", txtrow, txtlin );
	strcpy( st, qc2( pcl.limax, "C / ", 0 ));
	disp_param( st, txtrow + 125, txtlin);
	strcpy( st, qc2( pcl.limaxpres, "mb", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "CINH =", txtrow, txtlin );
	strcpy( st, qc2( pcl.bminus, " J/Kg", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 205;
	outgtext ( "Cap =", txtrow, txtlin );
	strcpy( st, qc2( pcl.cap, "C / ", 0 ));
	disp_param( st, txtrow + 125, txtlin);
	strcpy( st, qc2( pcl.cappres, "mb", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31);
	txtlin += 30;
	txtrow = skv.brx + 25;
	outgtext ( "LEVEL     PRES     HGT(AGL)     TEMP", txtrow, txtlin );
	txtlin += 2;
	outgtext ( "_________________________________________", 
		    txtrow, txtlin );

	setcolor(31);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "LCL", txtrow, txtlin );
	strcpy( st, qc2( pcl.lclpres, "mb", 0 ));
	disp_param( st, txtrow + 130, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.lclpres, I_PRES ))), "ft", 0 ));
	disp_param( st, txtrow + 240, txtlin);

	setcolor(31);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "LFC", txtrow, txtlin );
	strcpy( st, qc2( pcl.lfcpres, "mb", 0 ));
	disp_param( st, txtrow + 130, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.lfcpres, I_PRES ))), "ft", 0 ));
	disp_param( st, txtrow + 240, txtlin);
	strcpy( st, qc2( i_temp(pcl.lfcpres, I_PRES ), "C", 0 ));
	disp_param( st, txtrow + 320, txtlin);

	setcolor(31);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "EL", txtrow, txtlin );
	strcpy( st, qc2( pcl.elpres, "mb", 0 ));
	disp_param( st, txtrow + 130, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.elpres, I_PRES ))), "ft", 0 ));
	disp_param( st, txtrow + 240, txtlin);
	strcpy( st, qc2( i_temp(pcl.elpres, I_PRES ), "C", 0 ));
	disp_param( st, txtrow + 320, txtlin);

	setcolor(31);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "MPL", txtrow, txtlin );
	strcpy( st, qc2( pcl.mplpres, "mb", 0 ));
	disp_param( st, txtrow + 130, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.mplpres, I_PRES ))), "ft", 0 ));
	disp_param( st, txtrow + 240, txtlin);
	}


/*NP*/
void clear_paramarea(void)
/*************************************************************/
/*  CLEAR_PARAMAREA                                          */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Clear right side of screen (parameter area).             */
/*                                                           */
/*************************************************************/
{
	setcliprgn(1, 1, xwdth, xhght);
       	setlinestyle(1, 2);
       	setcolor(24);
       	rectangle(1, skv.tlx, skv.bry + 15, hov.brx, xhght - 10);
       	setcolor(1);
       	rectangle(0, skv.tlx, skv.bry + 15, hov.brx, xhght - 10);
	
}

/*NP*/
void show_skewtpage1( void )
/*************************************************************/
/*  SHOW_SKEWTPAGE1                                          */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Displays the Initial Thermodynamic (default skewt)       */
/*  parameter list on the screen.                            */
/*************************************************************/
	{
        /*float hvars[20], h2[100];
        double T0, Td0;
        short tIndex, tdIndex, i;*/

        pid_t pid;
        float hvars[30], h2[100];
        float ix1, ix2, ix3, ix4, pres, mumixr,esicat;
        float T0, Td0, el, pbot, ptop, base, depth, effdep, ebs;
        short tIndex, tdIndex, pIndex, i;
        Parcel pcl;

        tIndex = getParmIndex("TEMP");
        tdIndex = getParmIndex("DWPT");
	pIndex = getParmIndex("PRES");

/* 	24 MAR 2008 */
/*	effective_inflow_layer(100, -250, &p_bot, &p_top);
*/
/*	printf("Calling CLEAR_PARAMAREA\n");*/
	clear_paramarea();
/*	printf("Calling SHOW_PARCEL_NEW\n");*/
	show_parcel_new();
/*	printf("Calling SHOW_SHEAR_NEW\n");*/
	show_shear_new();

	/* left inset display */
	if (display_mode_left == DISPLAY_WINTER_LEFT)
	   { show_winter_new();}
        if (display_mode_left == DISPLAY_SARS_LEFT)
           { show_sars(); }
	/* not using DISPLAY_HAIL - problems with hail model output in this mode */

        if (display_mode_left == DISPLAY_HAIL_LEFT)
       	   { 
	   write_hail_file("HAIL");

       	   for (i=0;i<100;i++) {
            	h2[i] = 0.0;}
           for (i=0;i<30;i++) {
          	hvars[i] = 0.0;}

           /* Call hail model */
/*           T0 = sndg[sfc()][tIndex];
           Td0 = sndg[sfc()][tdIndex];
           hailcast1(&T0, &Td0, &hvars);
*/

        /* Compute Effective Vertical Shear.  Default to 6km if not available */
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);

        pbot = sndg[sfc()][pIndex];
        el = 12000.0;
        if (pcl.bplus >= 100) {
                el = agl(i_hght(pcl.elpres, I_PRES));

/* 24 Mar 2008 */
/*                effective_inflow_layer(100, -250, &pbot, &ptop);*/

                }

/*        base = agl(i_hght(pbot, I_PRES));*/

	base = agl(i_hght(p_bot, I_PRES));
        depth = (el - base);
        effdep = base + (depth * 0.5);
        wind_shear(p_bot, i_pres(msl(effdep)), &ix1, &ix2, &ix3, &ix4);
        ebs = kt_to_mps(ix4)/effdep;

        printf("Shear = %.1f kt    %.1f mps\nEBS = %.6f\nDepth = %.1f m\n", ix4, kt_to_mps(ix4), ebs, effdep);

        T0 = sndg[sfc()][tIndex];
        Td0 = sndg[sfc()][tdIndex];

        mumixr = mixratio(lplvals.pres, lplvals.dwpt);
        hailcast1(&T0, &Td0, &ebs, &hvars, &mumixr, &esicat);


           /* Copy results to big array */
           h2[0]=1;        /* Hail model has been run */
           h2[1]=0;        /* New model has not yet been run */
           for (i=0;i<30;i++) //Chin was for (i=0;i<=30;i++), a bug as harvs[] size is only 30.
             {
               	printf( "HVARS[%d] = %f\n", i, hvars[i]); 
               	h2[i+2] = hvars[i];
             }
       	   show_hail_new(&h2);
	   }
		
       /* if (display_mode_left == DISPLAY_HAIL_LEFT)
           { show_sars(); }*/
        if (display_mode_left == DISPLAY_SHIP_LEFT)
           { show_ship_stats(); }
        if (display_mode_left == DISPLAY_STP_LEFT)
           { show_stp_stats(); }
        if (display_mode_left == DISPLAY_EBS_LEFT)
           { show_ebs_stats(); }	
	if (display_mode_left == DISPLAY_FIRE_LEFT)
	   { show_fire(); }	

	/* right inset display */
        if (display_mode_right == DISPLAY_WINTER_RIGHT) 
           { show_winter_new();}         
        if (display_mode_right == DISPLAY_SARS_RIGHT)
           { show_sars(); }
        if (display_mode_right == DISPLAY_HAIL_RIGHT)
           {
           write_hail_file("HAIL");

           for (i=0;i<100;i++) {
                h2[i] = 0.0;} 
           for (i=0;i<30;i++) {
                hvars[i] = 0.0;} 

/*           T0 = sndg[sfc()][tIndex];
           Td0 = sndg[sfc()][tdIndex];
           hailcast1(&T0, &Td0, &hvars);
*/

        /* Compute Effective Vertical Shear.  Default to 6km if not available */
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);

        pbot = sndg[sfc()][pIndex];
        el = 12000.0;
        if (pcl.bplus >= 100) {
                el = agl(i_hght(pcl.elpres, I_PRES));
/* 24 Mar 2008 */
/*                effective_inflow_layer(100, -250, &pbot, &ptop);*/
                }

/*        base = agl(i_hght(pbot, I_PRES));*/

	base = agl(i_hght(p_bot, I_PRES));
        depth = (el - base);
        effdep = base + (depth * 0.5);
        wind_shear(p_bot, i_pres(msl(effdep)), &ix1, &ix2, &ix3, &ix4);
        ebs = kt_to_mps(ix4)/effdep;

        printf("Shear = %.1f kt    %.1f mps\nEBS = %.6f\nDepth = %.1f m\n", ix4, kt_to_mps(ix4), ebs, effdep);


        T0 = sndg[sfc()][tIndex];
        Td0 = sndg[sfc()][tdIndex];
        hailcast1(&T0, &Td0, &ebs, &hvars, &mumixr, &esicat);

           h2[0]=1;        
           h2[1]=0;       
           for (i=0;i<30;i++) //Chin was for (i=0;i<=30;i++), a bug as harvs[] size is only 30.
             {
                printf( "HVARS[%d] = %f\n", i, hvars[i]); 
                h2[i+2] = hvars[i];
             }
           show_hail_new(&h2);
           }
     
        if (display_mode_right == DISPLAY_SHIP_RIGHT)
           { show_ship_stats(); }
        if (display_mode_right == DISPLAY_STP_RIGHT)
           { show_stp_stats(); }
        if (display_mode_right == DISPLAY_EBS_RIGHT)
           { show_ebs_stats(); }
        if (display_mode_right == DISPLAY_FIRE_RIGHT)
           { show_fire(); }

	
	plot_vis();
	plot_thetae();
	plot_windspeedprofile();
	plot_advectionprofile();
	plot_vertsrw();
	setcliprgn(1, 1, xwdth, xhght);

        XCopyArea(XtDisplay(draw_reg), canvas, XtWindow(draw_reg), gc, 0, 0, xwdth, xhght, 0, 0);
        XFlush(XtDisplay(draw_reg));

	}

/*NP*/
void show_hodopage2(void)
/*************************************************************/
/*  SHOW_HODOPAGE2                                           */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Displays the Initial Kinematic (default hodograph)       */
/*  parameter list on the screen.                            */
/*************************************************************/
{
	clear_paramarea();
	show_shear();
	show_meanwind();
	show_srdata();
}



/*NP*/
void show_heavypcpn(void)
/*************************************************************/
/*  SHOW_HEAVYPCPN                                           */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Displays heavy precipitation parameters.                 */
/*************************************************************/
{ 
        float ix1;
        short txtlin, txtrow, anc_x, anc_y;
        char st[40], st1[20];

	/* ----- Set anchor pixel ----- */
        anc_x = skv.brx + 20;
        anc_y = 240;

	/* ----- Heavy Precip Area ----- */
        setcolor(3);
        set_font(1);
        setlinestyle( 1, 1 );
        strcpy( st, "HEAVY RAINFALL" );
        txtrow = (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2));
        txtlin = anc_y;
        outgtext ( st, txtrow, txtlin );
        setcolor(0);
        rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
        setcolor(7);
        rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
        
	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31);
	txtlin = txtlin + 30;
	txtrow = skv.brx + 25;
	outgtext ( "Rogash QPF =", txtrow, txtlin );
	strcpy( st, qc2( Rogash_QPF(&ix1), " in", 2 ));
	disp_param( st, txtrow + 300, txtlin);
}




/*NP*/
void show_srlayers(void)
/*************************************************************/
/*  SHOW_SRLAYERS                                            */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*************************************************************/
{
        float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres, pct, pcttop, step;
	float hgttop, hgtbot, j2;
        short txtlin, txtrow, anc_x, anc_y, i;
        char st[40], st1[20];
	Parcel pcl;

        sfctemp = lplvals.temp;
        sfcdwpt = lplvals.dwpt;
        sfcpres = lplvals.pres;

	/* ----- Calculate Parcel Data ----- */
        ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	if (ix1 <= 50) {
		printf( "No CAPE - SR Layers not calculated.\n" );
		return;
	}

	printf( "Calculating SR Layers.\n");
	printf( "LPL Hgt (AGL):  %6.0f m.\n", agl(i_hght(pcl.lplpres, I_PRES)));
	printf( " EL Hgt (AGL):  %6.0f m.\n", agl(i_hght(pcl.elpres, I_PRES)));

        /* ----- Set anchor pixel ----- */
        anc_x = skv.brx + 20;
        anc_y = 375;
        
	/* ----- DATA Area ----- */
        setcolor(3);
        set_font(1);
        setlinestyle( 1, 1 );
        strcpy( st, "STORM RELATIVE LAYERS" );
        txtrow = (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2));
        txtlin = anc_y;
        outgtext ( st, txtrow, txtlin );
        setcolor(0);
        rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 200);
        setcolor(7);
        rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 200);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31);
	txtlin += 30;
	txtrow = skv.brx + 25;

	step=.1;
	pcttop= 1+step;
        j2 = i_hght(pcl.elpres, I_PRES) - i_hght(pcl.lplpres, I_PRES);
	for (pct = step; pct <= pcttop; pct += step)
		{
		hgttop = i_hght(pcl.lplpres, I_PRES) + (pct * j2);
		hgtbot = i_hght(pcl.lplpres, I_PRES) + ((pct-step) * j2);
		sr_wind( i_pres(hgtbot), i_pres(hgttop), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	        if(qc(ix3))
           		{
			sprintf( st, "%3.0f%%", (pct*100));
           		disp_param( st, txtrow + 30, txtlin);
			sprintf( st, "%5.0f-%5.0fm", agl(hgtbot), agl(hgttop));
           		disp_param( st, txtrow + 150, txtlin);
           		sprintf( st, "%.0f/%3.0f kt", ix3, ix4);
           		disp_param( st, txtrow + 265, txtlin);
           		sprintf( st, "(%.0f m/s)", kt_to_mps(ix4));
           		disp_param( st, txtrow + 360, txtlin);
           		}
        	else
           		{ disp_param( "M", txtrow + 100, txtlin); }

		txtlin += 16;
		}
}
	

	/*NP*/
void show_preciptype(void)
        /*************************************************************/
        /*  SHOW_PRECIPTYPE                                          */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Displays precipitation-type parameters.                  */
        /*************************************************************/
{
        float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres;
        short txtlin, txtrow, anc_x, anc_y;
        char st[40], st1[20], st2[20];

        /* ----- Set anchor pixel ----- */
        anc_x = skv.brx + 20;
        anc_y = 105;

        /* ----- Heavy Precip Area ----- */
        setcolor(3);
        set_font(1);
        setlinestyle( 1, 1 );
        strcpy( st, "PRECIPITATION TYPE" );
        txtrow = (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2));
        txtlin = anc_y;
        outgtext ( st, txtrow, txtlin );
        setcolor(0);
        rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
        setcolor(7);
        rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
	
	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31);
	txtlin = txtlin + 30;
	txtrow = skv.brx + 25;
	outgtext ( "Wet-Bulb Zero Level =", txtrow, txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 ), I_PRES))), "ft /", 0 ));
	strcat( st, qc2( wb_lvl(0, &ix1), " mb", 0));
	disp_param( st, txtrow + 360, txtlin);
	
	txtlin += 20;
	outgtext ( "850-700mb Thickness =", txtrow, txtlin );
	ix1 = i_hght(700, I_PRES) - i_hght(850, I_PRES);
	strcpy( st1, qc2( ix1, " m", 0 ));
	strcpy( st2, qc2(mean_temp(&ix2, 850, 700), " C", 1));
	sprintf( st, "%s / %s", st1, st2);
	disp_param( st, txtrow + 360, txtlin);
}


	/*NP*/
	void show_shear( void )
	/*************************************************************/
	/*  SHOW_SHEAR                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays the environmental shear values.                 */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres;
	short txtlin, txtrow, anc_x, anc_y, pIndex;
	char st[40], st1[20];

	/* ----- Set anchor pixel ----- */
	anc_x = skv.brx + 20;
	anc_y = 485;

	/* ----- Environmental Shear Area ----- */
	setcolor(3);
	set_font(1);
	setlinestyle( 1, 1 );
	strcpy( st, "ENVIRONMENTAL SHEAR" );
	txtrow = (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2));
	txtlin = anc_y;
	outgtext ( st, txtrow, txtlin );
	setcolor(0);
	rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
	setcolor(7);
	rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31);
	txtrow = anc_x + 10;
	txtlin = anc_y + 25;

	outgtext( "LAYER         DELTA V        TOT SHR", 
		  txtrow, txtlin );
	txtlin += 2;
	outgtext("________________________________________", 
		  txtrow, txtlin );

	txtlin += 17;
	pIndex = getParmIndex("PRES");
	if (!sndg || pIndex == -1)
	  ix4 = (int)RMISSD;
	else
	  wind_shear(sndg[sfc()][pIndex], i_pres(msl(1000)), &ix1, &ix2, &ix3, &ix4);
	outgtext( "Sfc - 1 km", txtrow, txtlin );
	strcpy( st, qc2( ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf( st, "(%s)", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param( st, txtrow + 245, txtlin);
	strcpy( st, qc2( kt_to_mps(ix4)/.3, "" , 2));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext( "Sfc - 2 km",txtrow, txtlin );

	if (!sndg || pIndex == -1)
	  ix4 = (int)RMISSD;
	else
	  wind_shear(sndg[sfc()][pIndex], i_pres(msl(2000)), &ix1, &ix2, &ix3, &ix4);
	strcpy(st, qc2(ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf(st, "(%s)", qc2(kt_to_mps(ix4), " m/s" , 0));
	disp_param(st, txtrow + 245, txtlin);
	strcpy(st, qc2(kt_to_mps(ix4)/.2, "" , 2));
	disp_param(st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext("Sfc - 6 km", txtrow, txtlin );

	if (!sndg || pIndex == -1)
	  ix4 = (int)RMISSD;
	else
	  wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, 
	    &ix3, &ix4);
	strcpy(st, qc2( ix4, " kt" , 0));
	disp_param(st, txtrow + 165, txtlin);
	sprintf(st, "(%s)", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param(st, txtrow + 245, txtlin);
	strcpy(st, qc2( kt_to_mps(ix4)/.6, "" , 2));
	disp_param(st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext( "Low - 6 km", txtrow, txtlin );
	wind_shear( -1, i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf( st, "(%s)", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param( st, txtrow + 245, txtlin);
	strcpy( st, qc2( kt_to_mps(ix4)/1.2, "" , 2));
	disp_param( st, txtrow + 315, txtlin);
	}


	/*NP*/
	void show_stormpage3( void )
	/*************************************************************/
	/*  SHOW_STORMPAGE3                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays the Storm-Type parameter list.                  */
	/*************************************************************/
	{
	clear_paramarea();
	show_initiation();
	show_stormtype();
	show_severe();
	}



	/*NP*/
        void show_precippage4( void )
	/*************************************************************/
        /*  SHOW_PRECIPPAGE4                                         */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Displays the Precip Type/Intensity Data                  */
        /*************************************************************/
        {
        clear_paramarea();
        show_preciptype();
        show_heavypcpn();
        show_srlayers();
	}

	void show_hailpage( float *h2 )
	/*************************************************************/
	/*************************************************************/
	{
        float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres;
	float pres, mumixr, lr75, t500, shr6, fzlh, ship, esi2;
	short oldlplchoice;
        short txtlin, txtrow, anc_x, anc_y, pIndex, tdIndex;
        char st[200], st1[20], *st2; 
	Parcel pcl;

	pIndex = getParmIndex("PRES");
	tdIndex = getParmIndex("DWPT");
	
	clear_paramarea();

        /* ----- Set anchor pixel ----- */
        anc_x = skv.brx + 20;
        anc_y = 125;

        /* ----- Draw Bounding Area ----- */
        setcolor(5);
        set_font(1);
        setlinestyle( 1, 1 );
	strcpy( st, "* * * HAILCAST HAIL MODEL - 4/21/10 * * *" );
        txtrow = (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2));
        txtlin = anc_y;
        outgtext ( st, txtrow, txtlin );
        setcolor(0);
        rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 110);
        setcolor(7);
        rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 110);

	/* ----- Hail Model Output ----- */
	set_font(3);
	setcolor(5);
        sprintf(st, "Hail Model --> (%.0f convecting members)  T/Td= %.0fF/%.0fF", h2[4], ctof(h2[2]), ctof(h2[3]));

        txtrow = anc_x + 10;
        txtlin = anc_y + 25;
        outgtext ( st, txtrow, txtlin );

        sprintf(st, "Avg: %.1f in.   Max: %.1f in.   Storm Cat: %.0f of 4", h2[5], h2[6], h2[17]);
        txtlin = txtlin + 20;
        outgtext ( st, txtrow, txtlin );

	if (h2[15] >= 0.95 && h2[4] >=1) setcolor(3);
	if (h2[15] >= 1.95) setcolor(2);
	sprintf(st, "Members >= 2 in.:  %d      Members >= 0.75 in.: %.0f", h2[8], h2[9]);
        txtlin = txtlin + 20;
        outgtext ( st, txtrow, txtlin );

	set_font(2);
	if (h2[4] == 0) {
	sprintf(st, "No Convecting Members");
        txtlin = txtlin + 25;
        outgtext( st, txtrow, txtlin );
        }else{
        sprintf(st, "Bias Corrected Size: %.1f", h2[15]);
        txtlin = txtlin + 25;
        outgtext( st, txtrow, txtlin );
        }
        /* ----- Set anchor pixel ----- */
        anc_x = skv.brx + 20;
        anc_y = 250;

        /* ----- Draw Bounding Area ----- */
        setcolor(5);
        set_font(1);
        setlinestyle( 1, 1 );
        strcpy( st, "Environmental" );
        txtrow = anc_x + 5;
        txtlin = anc_y;
        outgtext ( st, txtrow, txtlin );
        strcpy( st, "Parameters" );
        txtrow = anc_x + 5;
        txtlin = anc_y + 20;
        outgtext ( st, txtrow, txtlin );
        setcolor(0);
        rectangle( 1, anc_x + 2, anc_y + 40, anc_x + 175, anc_y + 220);
        setcolor(7);
        rectangle( 0, anc_x + 2, anc_y + 40, anc_x + 175, anc_y + 220);

	oldlplchoice = lplvals.flag;
	define_parcel(3, mu_layer);
	ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);

        /* ----- Parameters ----- */
        set_font(3);
        setcolor(1);
        txtrow = anc_x + 10;
        txtlin = anc_y + 40;
        strcpy(st, "MUCAPE:"); txtlin = txtlin + 15;
       	outgtext ( st, txtrow, txtlin );
	strcpy( st, qc2( pcl.bplus, " j/kg", 0)); disp_param( st, txtrow + 158, txtlin);
        strcpy(st, "700-500 LR:"); txtlin = txtlin + 15;
       	outgtext ( st, txtrow, txtlin );
	lr75 = lapse_rate(&ix1, 700, 500);
	strcpy( st, qc2( lr75, " C/km", 1)); disp_param( st, txtrow + 158, txtlin);
        strcpy(st, "0-6km Shear:"); txtlin = txtlin + 15;
       	outgtext ( st, txtrow, txtlin );
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
	shr6 = ix4;
	strcpy( st, qc2( shr6, " kt", 0)); disp_param( st, txtrow + 158, txtlin);
        strcpy(st, "MU MixRatio:"); txtlin = txtlin + 15;
       	outgtext ( st, txtrow, txtlin );
	mumixr = mixratio(lplvals.pres, lplvals.dwpt);
	strcpy( st, qc2( mumixr, " g/kg", 1)); disp_param( st, txtrow + 158, txtlin);
        strcpy(st, "FRZ LVL:"); txtlin = txtlin + 15;
       	outgtext ( st, txtrow, txtlin );
	fzlh = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));
	strcpy( st, qc2( fzlh, " ft", 0)); disp_param( st, txtrow + 158, txtlin);
        strcpy(st, "Supercell:"); txtlin = txtlin + 15;
       	outgtext ( st, txtrow, txtlin );
	strcpy( st, qc2( scp(st_dir, st_spd), "", 1)); disp_param( st, txtrow + 158, txtlin);
        strcpy(st, "CapeShear/6km:"); txtlin = txtlin + 15;
       	outgtext ( st, txtrow, txtlin );
	esi2 = kt_to_mps(shr6) * pcl.bplus / 6000;
	strcpy( st, qc2( esi2, "", 1)); disp_param( st, txtrow + 158, txtlin);
        strcpy(st, "500mb Temp:"); txtlin = txtlin + 15;
       	outgtext ( st, txtrow, txtlin );
	t500 =  i_temp(500, I_PRES);
	strcpy( st, qc2( t500, " C", 1)); disp_param( st, txtrow + 158, txtlin);
        strcpy(st, "H5T-SfcTd:"); txtlin = txtlin + 15;
       	outgtext ( st, txtrow, txtlin );
	strcpy( st, qc2( t500 - sndg[sfc()][tdIndex], " C", 1)); disp_param( st, txtrow + 158, txtlin);

        /* ----- Set anchor pixel ----- */
        anc_x = skv.brx + 200;
        anc_y = 300;
        setcolor(5);
        set_font(1);
        setlinestyle( 1, 1 );
/*
	if (h2[4] >= 5)
		{ strcpy(st, "SHIP + HailCast"); }
	else
		{ strcpy( st, "SHIP" ); }
*/
       /* strcpy( st, "SHIP" ); 
        txtrow = anc_x + 5;
        txtlin = anc_y;
        outgtext ( st, txtrow, txtlin );
        strcpy( st, "Sig. Hail Parameter" );
        txtrow = anc_x + 5;
        txtlin = anc_y + 20;
        outgtext ( st, txtrow, txtlin );

	ship = sig_hail(pcl.bplus, mumixr, lr75, t500, kt_to_mps(shr6), fzlh, pcl.bminus, h2[5], h2[12], h2[4]);
	sprintf( st, "%.1f", ship); 
        txtrow = anc_x + 5;
        txtlin = anc_y + 40;
        outgtext ( st, txtrow, txtlin );*/

        if (oldlplchoice == 3) pres = mu_layer;
        else if (oldlplchoice == 4) pres = mml_layer;
        else if (oldlplchoice == 5) pres = user_level;
        else pres = mml_layer;

        define_parcel(oldlplchoice, pres);

	}



char *hailwords(float num)
	{
	char st[200];
	strcpy(st, "Unknown code!");
	if (num == 1) strcpy(st, "No Hail Produced");
	if (num == 2) strcpy(st, "Dime - quarter most likely, isold golfball");
	if (num == 3) strcpy(st, "A few golfballs possible");
	if (num == 4) strcpy(st, "Tennis/baseballs possible");
	if (num == 5) strcpy(st, "Baseballs or larger!");

	return (char *)st;
	}

	/*NP*/
void show_page(short page)
	/*************************************************************/
	/*  SHOW_PAGE                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays the given parameter list on the screen.         */
	/*  (1=Skewt, 2=Hodo, 3=Storm, 4=Precip, 5=Winter).          */
	/*************************************************************/
{
	/* Rewritten by mkay. 6/8/00 */
	switch(page) {
	  case 1:
	   show_skewtpage1();
	  break;
	  case 2:
	   show_hodopage2();
	  break;
	  case 3:
	   show_stormpage3();
	  break;
	  case 4:
	   show_precippage4();
	  break;
	  case 5:
	   show_winterpage5();
	  break;
	  case 6:
	   show_mainpage();
	  break;
	  default:
	    fprintf(stderr, "Unknown page view request: (page=%d)\n", page);
	}
}

	/*NP*/
	void show_stormtype( void )
	/*************************************************************/
	/*  SHOW_STORMTYPE                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays stormtype information.                          */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres;
	short txtlin, txtrow, i,j;
	char st[40], st1[20];
	Parcel pcl;

	  txtlin = skv.tly +  18;
	  setcolor(1);

	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	/* ----- Calculate Parcel Data ----- */
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Storm Type Area ----- */
	setcolor(3);
	set_font(1);
	txtlin = 220;
	setlinestyle( 1, 1 );
	strcpy( st, "STORM TYPE" );
	outgtext( st, 
	(((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st )/2)), txtlin);
	setcolor(0);
	rectangle( 1, skv.brx + 22, txtlin+20, xwdth-7, txtlin+80);
	setcolor(7);
	rectangle( 0, skv.brx + 22, txtlin+20, xwdth-7, txtlin+80);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31);
	txtlin = txtlin + 30;
	txtrow = skv.brx + 25;
	outgtext( "CAPE =", txtrow, txtlin );
	strcpy( st, qc2( pcl.bplus, " J/Kg" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 200;
	mean_wind( -1, -1, &ix1, &ix2, &ix3, &ix4);
	ix1 = helicity( -1, -1, st_dir, st_spd, &ix5, &ix2);
	outgtext( "Eff. SREH=", txtrow, txtlin );
	sprintf( st1, " m%c/s%c", 178, 178 );
	strcpy( st, qc2( ix1, st1 , 0));
	disp_param( st, txtrow + 175, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "EHI =", txtrow, txtlin );
	strcpy( st, qc2( ehi(pcl.bplus, ix1), "" , 1));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 200;
	outgtext( "3km Shear =", txtrow, txtlin );
	wind_shear( -1, -1, &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( kt_to_mps( ix4 ), " m/s", 1));
	disp_param( st, txtrow + 175, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "BRN =", txtrow, txtlin );
	strcpy( st, qc2( pcl.brn, "" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 200;
	outgtext( "BRN Shear =", txtrow, txtlin );
	bulk_rich( pcl, &ix1 );
	sprintf( st1, " m%c/s%c", 178, 178 );
	strcpy( st, qc2( ix1, st1 , 0));
	disp_param( st, txtrow + 175, txtlin);
	}

	/*NP*/
	void show_meanwind( void )
	/*************************************************************/
	/*  SHOW_MEANWIND                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays the ground-relative mean wind values.           */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4, ix5;
	short txtlin, txtrow, anc_x, anc_y;
	char st[40], st1[20];

	/* ----- Set anchor pixel ----- */
	anc_x = skv.brx + 20;
	anc_y = 385;

	/* ----- Write Mean Wind Header ----- */
	setcolor(3);
	set_font(1);
	setlinestyle( 1, 1 );
	strcpy( st, "MEAN  WIND" );
	outgtext( st, 
	   (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2)), anc_y );
	setcolor(0);
	rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 85);
	setcolor(7);
	rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 85);

	set_font(2);
	setcolor(31);

	txtrow = anc_x + 10;
	txtlin = anc_y + 25;

	/* ----- 0-6 km Mean Wind ----- */
	outgtext( "BL - 6 km", txtrow, txtlin );
	mean_wind( -1, i_pres(agl(6000)), &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3) && qc(ix4))
	   { sprintf( st, "%d / %d kt", (short)ix3, (short)ix4); }
	else
	   { sprintf( st, "M / M"); }
	disp_param( st, txtrow + 245, txtlin);
	sprintf( st, " (%s)", qc2( kt_to_mps(ix4), " m/s", 0));
	disp_param( st, txtrow + 340, txtlin);

	/* ----- LFC-EL Mean Wind ----- */
	txtlin += 20;
	outgtext( "LFC - EL", txtrow, txtlin );
	mean_wind( -1, -1, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3) && qc(ix4))
	   { sprintf( st, "%d / %d kt", (short)ix3, (short)ix4); }
	else
	   { sprintf( st, "M / M"); }
	disp_param( st, txtrow + 245, txtlin);
	sprintf( st, " (%s)", qc2( kt_to_mps(ix4), " m/s", 0));
	disp_param( st, txtrow + 340, txtlin);

	/* ----- 850-200 mb Mean Wind ----- */
	txtlin += 20;
	outgtext( "850 - 200 mb", txtrow, txtlin );
	mean_wind( 850, 200, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3) && qc(ix4))
	   { sprintf( st, "%d / %d kt", (short)ix3, (short)ix4); }
	else
	   { sprintf( st, "M / M"); }
	disp_param( st, txtrow + 245, txtlin);
	sprintf( st, " (%s)", qc2( kt_to_mps(ix4), " m/s", 0));
	disp_param( st, txtrow + 340, txtlin);

	}


void show_srdata(void)
	/*************************************************************/
	/*  SHOW_SRDATA                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays storm-relative data.                            */
	/*************************************************************/
{
	float ix1, ix2, ix3, ix4, ix5;
	short i, txtlin, txtrow, anc_x, anc_y;
	int   tmp1, tmp2;
	char  st[40], st1[20], stval[20];
	char *srhlabels[] = {"Sfc - 1 km", "Sfc - 3 km","LPL - LFC"};
	char *srhlayers[] = {"0 1000", "0 3000", "-1 -1"};
	char *srwlabels[] = {"Sfc - 2 km", "4 - 6 km", "9 - 11 km"};
	char *srwlayers[] = {"0 2000", "4000 6000", "9000 11000"};

	/* ----- Set anchor pixel ----- */
	anc_x = skv.brx + 20;
	anc_y = 110;

	/* ----- Storm Relative Header ----- */
	setcolor(3);
	set_font(1);
	setlinestyle(1, 1);
	strcpy(st, "STORM  RELATIVE");
	outgtext(st, 
	  (((anc_x + xwdth) / 2) - (getgtextextent(st) / 2)), anc_y);
	setcolor(0);
	rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 250);
	setcolor(7);
	rectangle(0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 250);

	/* ----- Display Values on screen ----- */
	set_font(2);
	txtrow = anc_x + 10;
	txtlin = anc_y + 25;

	/* ----- Display Storm Motion ----- */
	setcolor(31);
	sprintf(st, "%3.0f%c / %3.0f kt    ( %.0f m/s )", st_dir, 176,
		 st_spd, kt_to_mps(st_spd));
	outgtext(st,
	  (((anc_x + xwdth) / 2) - (getgtextextent(st) / 2)), txtlin);

	/* ----- Storm Relative Helicity ----- */
	txtlin += 20;
	setcolor(5);
	strcpy(st, "------ SR HELICITY ------" );
	outgtext(st,
	  (((anc_x + xwdth) / 2) - (getgtextextent(st) / 2)), txtlin);

	setcolor(31);
	txtlin += 20;
	outgtext("LAYER        POS   NEG        TOT", txtrow, txtlin);
	txtlin += 2;
	outgtext("________________________________________", txtrow, txtlin);

	for (i=0; i<3; i++) {
	  txtlin += 20;
	  outgtext(srhlabels[i], txtrow, txtlin);
	  sscanf(srhlayers[i], "%d %d", &tmp1, &tmp2);
	  ix1 = helicity(tmp1, msl(tmp2), st_dir, st_spd, &ix2, &ix3);
	  strcpy(st, qc2(ix2, "", 0));
	  disp_param(st, txtrow + 145, txtlin);
	  strcpy(st, qc2(ix3, "", 0));
	  disp_param(st, txtrow + 195, txtlin);
	  sprintf(stval, " m%c/s%c",178, 178);
	  strcpy(st, qc2(ix1, stval, 0));
	  disp_param(st, txtrow + 315, txtlin);
	}

	txtlin += 20;
	setcolor(5);
	strcpy(st, "------ SR WINDS ------");
	outgtext(st,
	  (((anc_x + xwdth) / 2) - (getgtextextent(st) / 2)), txtlin);

	txtlin += 20;
	setcolor(31);
	outgtext("LAYER                VECTOR", txtrow, txtlin);
	txtlin += 2;
	outgtext("_______________________________________", txtrow, txtlin);

	for (i=0; i<3; i++) {
	  txtlin += 20;
	  outgtext(srwlabels[i], txtrow, txtlin);
	  sscanf(srwlayers[i], "%d %d", &tmp1, &tmp2);
	  sr_wind(i_pres(msl(tmp1)), i_pres(msl(tmp2)), st_dir, st_spd, 
  	    &ix1, &ix2, &ix3, &ix4);
	  if (qc(ix3)) {
   	    sprintf(st, "%.0f / %3.0f kt", ix3, ix4);
   	    disp_param(st, txtrow + 245, txtlin);
   	    sprintf(st, "(%.0f m/s)", kt_to_mps(ix4));
   	    disp_param(st, txtrow + 340, txtlin);
	  }
	  else { 
  	    disp_param("M", txtrow + 245, txtlin);
	  }
	}
}

	/*NP*/
	void show_initiation( void )
	/*************************************************************/
	/*  SHOW_INITIATION                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays thunderstorm initiation parameters.             */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres;
	short txtlin, txtrow;
	char st[40], st1[20];
	Parcel pcl;

	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	/* ----- Calculate Parcel Data ----- */
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Storm Type Area ----- */
	setcolor(3);
	set_font(1);
	txtlin = 105;
	setlinestyle( 1, 1 );
	strcpy( st, "CONVECTIVE  INITIATION" );
	outgtext( st, 
	  (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
	  txtlin);
	setcolor(0);
	rectangle( 1, skv.brx + 22, txtlin+20, xwdth-7, txtlin+110);
	setcolor(7);
	rectangle( 0, skv.brx + 22, txtlin+20, xwdth-7, txtlin+110);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31);
	txtlin = txtlin + 30;
	txtrow = skv.brx + 25;
	outgtext( "CINH =",  txtrow, txtlin );
	strcpy( st, qc2( pcl.bminus, " J/Kg" , 0));
	disp_param( st, txtrow + 160, txtlin);

	txtrow += 210;
	outgtext( "Cap =",  txtrow, txtlin );
	strcpy( st, qc2( pcl.cap, "C/ ", 0 ));
	disp_param( st, txtrow + 120, txtlin);
	strcpy( st, qc2( pcl.cappres, "mb", 0 ));
	disp_param( st, txtrow + 160, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "K-Index =",  txtrow, txtlin );
	strcpy( st, qc2( k_index( &ix1 ), "", 0 ));
	disp_param( st, txtrow + 160, txtlin);

	txtrow += 210;
	outgtext( "Mean RH =",  txtrow, txtlin );
	strcpy( st, qc2( mean_relhum( &ix1, -1, -1), " %" , 0));
	disp_param( st, txtrow + 160, txtlin);

	txtlin += 25;
	txtrow = skv.brx + 25;
	outgtext( "Top of Moist Lyr = ",  txtrow, txtlin );
	strcpy( st, qc2( top_moistlyr( &ix1 ), " mb", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( mtof(agl(i_hght(ix1, I_PRES))), " ft", 0 ));
	outgtext( st, txtrow + 210, txtlin );

	txtlin += 18;
	outgtext( "LFC Height =", txtrow, txtlin );
	strcpy( st, qc2( pcl.lfcpres, " mb", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( mtof(agl(i_hght(pcl.lfcpres, I_PRES ))), " ft", 0 ));
	outgtext( st, txtrow + 210, txtlin );
	}

	/*NP*/
	void show_hailpot( void )
	/*************************************************************/
	/*  SHOW_HAILPOT                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays hail forecasting parameters.                    */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres;
	short txtlin, txtrow;
	char st[40], st1[20];
	Parcel pcl;

	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	/* ----- Calculate Parcel Data ----- */
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Hail Area ----- */
	setcolor(5);
	set_font(2);
	txtlin = 305;
	txtlin += 25;
	setlinestyle( 1, 1 );
	strcpy( st, "----- HAIL POTENTIAL -----" );
	outgtext( st,
	  (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)), 
	  txtlin);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31);
	txtlin = txtlin + 20;
	txtrow = skv.brx + 25;
	outgtext( "CAPE =", txtrow, txtlin );
	strcpy( st, qc2( pcl.bplus, " J/Kg" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 190;
	outgtext( "WBZ level =", txtrow, txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 ), I_PRES))), " ft", 0 ));
	disp_param( st, txtrow + 185, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "Mid Lvl RH =", txtrow, txtlin );
	strcpy( st, qc2( mean_relhum( &ix1, 700, 500), " %" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 190;
	outgtext( "FZG level =", txtrow, txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES))), " ft", 0 ));
	disp_param( st, txtrow + 185, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "EL Storm Relative Wind Speed =", txtrow, txtlin );
	sr_wind( pcl.elpres+25, pcl.elpres-25, st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt", 0 ));
	disp_param( st, txtrow + 360, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "CHI1 =", txtrow, txtlin);
	bulk_rich( pcl, &ix1 );
	ix3 = (pcl.bplus * ix1) / agl(i_hght(wb_lvl( 0, &ix2 ), I_PRES)); 
	strcpy( st, qc2(ix3, "", 1));
	disp_param( st, txtrow + 165, txtlin);
	
 	txtrow += 190;	
	outgtext( "CHI2 =", txtrow, txtlin);
	bulk_rich( pcl, &ix1 );
	Mean_WBtemp( &ix2, -1, -1);
	strcpy( st, qc2(ix3/ix2, "", 1));
	disp_param( st, txtrow + 185, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "Avg BL Wetbulb Temp =", txtrow, txtlin);
	strcpy( st, qc2( Mean_WBtemp( &ix2, -1, -1), " C", 1));
	disp_param( st, txtrow + 260, txtlin);

	pinomoore();
	}


	/*NP*/
	void show_severe( void )
	/*************************************************************/
	/*  SHOW_SEVERE                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays severe weather forecasting parameters.          */
	/*************************************************************/
	{
	char st[80];
	short txtlin;

	/* ----- Severe Area ----- */
	setcolor(3);
	set_font(1);
	txtlin = 305;
	setlinestyle( 1, 1 );
	strcpy( st, "SEVERE POTENTIAL" );
	outgtext( st, 
	  (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)), 
	  txtlin);
	setcolor(0);
	rectangle( 1, skv.brx + 22, txtlin+20, xwdth-7, txtlin+300);
	setcolor(7);
	rectangle( 0, skv.brx + 22, txtlin+20, xwdth-7, txtlin+300);

	show_hailpot();
	show_torpot();
	show_windpot();
	}


        /*NP*/
        void show_torpot( void )
        /*************************************************************/
        /*  SHOW_TORPOT                                              */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Displays tornado forecasting parameters.                 */
        /*************************************************************/
        {
        float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres;
	float tlyr, blyr;
        short txtlin, txtrow;
        char st[40], st1[20], stval[80];
        Parcel pcl;

        sfctemp = lplvals.temp;
        sfcdwpt = lplvals.dwpt;
        sfcpres = lplvals.pres;

        /* ----- Calculate Parcel Data ----- */
        ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

        /* ----- Tornado Area ----- */
        setcolor(5);
        set_font(2);
        txtlin = 450;
        setlinestyle( 1, 1 );
        strcpy( st, "----- TORNADO POTENTIAL -----" );
        outgtext( st,
          (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
          txtlin);

        /* ----- Display Values on screen ----- */
        set_font(2);
        setcolor(31);
        txtlin = txtlin + 18;
        txtrow = skv.brx + 25;
        outgtext( "Low SRW (Sfc - LFC)  =", txtrow, txtlin );
	blyr = i_pres(msl(0));
	tlyr = pcl.lfcpres;
	sr_wind( blyr, tlyr, st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2(ix4, " kt", 0));
        disp_param( st, txtrow + 300, txtlin);

        txtlin = txtlin + 18;
        txtrow = skv.brx + 25;
        outgtext( "Mid SRW (LFC - LFC+4km)  =", txtrow, txtlin );
        blyr = pcl.lfcpres;
        tlyr = i_pres(i_hght(pcl.lfcpres, I_PRES) + 4000);
        sr_wind( blyr, tlyr, st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, " kt", 0));
        disp_param( st, txtrow + 300, txtlin);	

        txtlin = txtlin + 18;
        txtrow = skv.brx + 25;
        outgtext( "Low SRW (EL-4km - EL)  =", txtrow, txtlin );
        blyr = i_pres(i_hght(pcl.elpres, I_PRES) - 4000);
        tlyr = pcl.elpres;
        sr_wind( blyr, tlyr, st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, " kt", 0));
        disp_param( st, txtrow + 300, txtlin);
	
        txtlin = txtlin + 18;
        txtrow = skv.brx + 25;
	outgtext( "Sfc - 1 km Helicity:", txtrow, txtlin );
	ix1 = helicity( 0, 1000, st_dir, st_spd, &ix2, &ix3);
	sprintf(stval, " m%c/s%c",178, 178 );
	strcpy( st, qc2(ix1, stval, 0 ));
	disp_param( st, txtrow + 300, txtlin);
	}


        /*NP*/
        void show_windpot( void )
        /*************************************************************/
        /*  SHOW_WINDPOT                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Displays wind forecasting parameters.                    */
        /*************************************************************/
        {
        float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres;
	float tlyr, blyr;
        short txtlin, txtrow;
        char st[40], st1[20];
        Parcel pcl;

        sfctemp = lplvals.temp;
        sfcdwpt = lplvals.dwpt;
        sfcpres = lplvals.pres;

        /* ----- Calculate Parcel Data ----- */
        ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

        /* ----- Wind Area ----- */
        setcolor(5);
        set_font(2);
        txtlin = 540;
        setlinestyle( 1, 1 );
        strcpy( st, "----- DOWNBURST POTENTIAL -----" );
        outgtext( st,
          (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
          txtlin);

        /* ----- Display Values on screen ----- */
        set_font(2);
        setcolor(31);
        txtlin = txtlin + 20;
        txtrow = skv.brx + 25;
        outgtext( "DCAPE =", txtrow, txtlin );
	ix1 = dcape(&ix2, &ix3);
	if(qc(ix1))
	   {
	   strcpy( st1, qc2(ix1, " J/kg", 0));
	   sprintf( st, "%s descending from %s.", st1, qc2(ix2, "mb", 0));
           disp_param( st, txtrow + 350, txtlin);
	   sprintf( st, "Downdraft temperature at surface:  %s.\n", qc2(ctof(ix3), " F", 0));
	   outgtext( st, txtrow, txtlin + 20); 
	   }

	}


	void show_winterpage5()
	/*NP*/
        /*************************************************************/
        /*  SHOW_WINTERPAGE5                                         */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Displays winter forecasting parameters.                  */
        /*************************************************************/
	{
	clear_paramarea();
	show_partialthicknesses();
	show_posnegareas();
	show_initialphase();
	/* best_guess_ptype(); */
	}


	void show_partialthicknesses()
        /*************************************************************/
        /*  SHOW_PARTIALTHICKNESSES                                  */
        /*  John Hart  NSSFC KCMO                                    */
        /*************************************************************/
	{
	float ix1, ix2, ix3, ix4;
	float ptop, pbot, htop, hbot, mrh, mq, pw, mo, mopw;
	short txtlin, txtrow, pIndex;
	char st[80], st1[80];

        txtlin = 105;

	/* Do dendritic growth zone calcs */
	ptop = temp_lvl(-17, &ix1);
	pbot = temp_lvl(-12, &ix1);
	pIndex = getParmIndex("PRES");
	if (ptop < 0) ptop = sndg[sfc()][pIndex]; 
	if (pbot < 0) pbot = sndg[sfc()][pIndex]; 
	htop = i_hght(ptop, I_PRES);
	hbot = i_hght(pbot, I_PRES);
	mrh = mean_relhum(&ix1, pbot, ptop);
	mq = mean_mixratio(&ix1, pbot, ptop);
	mo = mean_omeg(&ix1, pbot, ptop) * 1000;
	pw = precip_water(&ix1, pbot, ptop);
	mopw = (mo * pw) * mrh / 100;

	/* ----- Data Area ----- */
        setcolor(3);
        set_font(1);
        strcpy( st, "Growth Zones" );
        outgtext ( st, (((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2)), txtlin );
        setcolor(0);
        rectangle( 1, skv.brx + 22, txtlin+20, xwdth-7, txtlin+160 );
        setcolor(7);
        rectangle( 0, skv.brx + 22, txtlin+20, xwdth-7, txtlin+160 );

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(5);

	txtrow = skv.brx + 25;
	setcolor(5);
	txtlin += 25;
	strcpy( st, "--- Dendrite Zone (-12 to -17) ---");
	outgtext ( st, (((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2)), txtlin );
	set_font(3);
	setcolor(31);
	txtlin += 17;
	outgtext( "Depth of layer = ", txtrow, txtlin); 
        strcpy( st1, qc2(mtof(htop - hbot), " ft", 0));
        disp_param( st1, txtrow + 250, txtlin);
	txtlin += 17;
	outgtext( "Mean RH of layer = ", txtrow, txtlin); 
        strcpy( st1, qc2(mrh, " %", 0));
        disp_param( st1, txtrow + 250, txtlin);
	txtlin += 17;
	outgtext( "Mean Q of layer = ", txtrow, txtlin); 
        strcpy( st1, qc2(mq , " g/kg", 1));
        disp_param( st1, txtrow + 250, txtlin);
	txtlin += 17;
	outgtext( "PW of layer = ", txtrow, txtlin); 
        strcpy( st1, qc2(pw , " in.", 2));
        disp_param( st1, txtrow + 250, txtlin);
	txtlin += 17;
	outgtext( "Mean Omega of layer = ", txtrow, txtlin); 
        strcpy( st1, qc2(mo , " ub/s", 0));
        disp_param( st1, txtrow + 250, txtlin);
	txtlin += 17;
	outgtext( "Omega*PW*RH = ", txtrow, txtlin); 
        strcpy( st1, qc2(mopw , " ", 2));
        disp_param( st1, txtrow + 250, txtlin);
	}	
	

void show_posnegareas(void)
	/*NP*/
        /*************************************************************/
        /*  SHOW_POSNEGAREAS                                         */
        /*  John Hart  NSSFC KCMO                                    */
        /*************************************************************/
{
	float h1000, ix1, ix2, ix3, ix4, pose, nege, ptop, pbot, sfc1;
	short txtlin, txtrow, pIndex;
	char st[80];

        txtlin = 360;

	h1000 = i_hght(1000, I_PRES);
	printf( "%.1f\n", h1000);

	/* ----- Display Temperature Values ----- */
	set_font(2);
	txtlin += 25;
	setcolor(5);
	strcpy( st, "--- Temperature Profile ---");
        outgtext ( st,
                (((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2)),
                txtlin );
	
	posneg_temperature(-1, &pose, &nege, &ptop, &pbot);

	txtlin += 18;
	txtrow = skv.brx + 25;
	setcolor(31);
	if ((pose > 0) && (nege < 0)) {
	  sprintf( st, "Pos= %5.0f J/kg         Neg=%5.0f J/kg", pose, nege);
	  outgtext(st, txtrow, txtlin);
	  txtlin += 18;
	  ix1 = mtof(i_hght(ptop, I_PRES)-i_hght(pbot, I_PRES));
	  sprintf(st, "Melting layer:  %3.0f - %3.0fmb (%.0f ft)\n", ptop, 
	    pbot, ix1);
	  outgtext(st, txtrow, txtlin);
	  txtlin += 18;
	  
	  pIndex = getParmIndex("PRES");
	  if (!sndg || pIndex == -1) {
	    sfc1 = pbot = RMISSD;
	    ix1 = (int)RMISSD;
	  }
	  else {
	    sfc1 = sndg[sfc()][pIndex];
	    ix1 = mtof(i_hght(pbot, I_PRES)-i_hght(sfc1, I_PRES));
	  }
	  sprintf(st, "Freezing layer: %3.0f - %4.0fmb (%.0f ft)\n", pbot, 
	    sfc1, ix1);
	  outgtext(st, txtrow, txtlin);
	}
	else {
	  outgtext("Warm/Cold layers not found.", txtrow, txtlin);
	}

	

	/* ----- Display Wetbulb Values ----- */
        set_font(2);
        txtlin += 25;
        setcolor(5);
        strcpy( st, "--- Wetbulb Profile ---");
        outgtext ( st,
                (((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2)),
                txtlin );

        posneg_wetbulb(-1, &pose, &nege, &ptop, &pbot);

        txtlin += 18;
        txtrow = skv.brx + 25;
        setcolor(31);
        if ((pose > 0) && (nege < 0)) {
          sprintf(st, "Pos= %5.0f J/kg         Neg=%5.0f J/kg", pose, nege);
          outgtext(st, txtrow, txtlin);
          txtlin += 18;
          ix1 = mtof(i_hght(ptop, I_PRES)-i_hght(pbot, I_PRES));
          sprintf(st, "Melting layer:  %3.0f - %3.0fmb (%.0f ft)\n", ptop, 
	    pbot, ix1);
          outgtext(st, txtrow, txtlin);
          txtlin += 18;

	  pIndex = getParmIndex("PRES");
	  if (!sndg || pIndex == -1) {
	    sfc1 = pbot = RMISSD;
	    ix1 = (int)RMISSD;
	  }
	  else {
            sfc1 = sndg[sfc()][pIndex];
            ix1 = mtof(i_hght(pbot, I_PRES)-i_hght(sfc1, I_PRES));
	  }
          sprintf(st, "Freezing layer: %3.0f - %4.0fmb (%.0f ft)\n", pbot, 
	    sfc1, ix1);
          outgtext(st, txtrow, txtlin);
        }
        else {
          outgtext("Warm/Cold layers not found.", txtrow, txtlin);
        }
}




	/*NP*/
void show_initialphase()
/*************************************************************/
/*  SHOW_INITIALPHASE                                        */
/*  John Hart  NSSFC KCMO                                    */
/*************************************************************/
	{
	float h1000, ix1, ix2, ix3, ix4, pose, nege, ptop, pbot, sfc1;
	short txtlin, txtrow, phase;
	char st[80], st1[80];

        txtlin = 280;

	/* ----- Data Area ----- */
        setcolor(3);
        set_font(4);
        strcpy( st, "Initial Phase" );
        outgtext ( st,
                (((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2)),
                txtlin );
        setcolor(0);
        rectangle( 1, skv.brx + 22, txtlin+20, xwdth-7, txtlin+65 );
        setcolor(7);
        rectangle( 0, skv.brx + 22, txtlin+20, xwdth-7, txtlin+65 );

	strcpy(st, init_phase(&ix1, &phase)); 

        set_font(4);
        setcolor(31);
	txtlin += 25;
	txtrow = skv.brx + 25;
	
	if (ix1>100)
		{
		sprintf( st1, "Precip. from:  %4.0fmb (%4.1f C)", ix1, i_temp(ix1, I_PRES));
		outgtext( st1, txtrow, txtlin);
		txtlin += 18;
		outgtext( st, txtrow, txtlin);
		}
	else
		{
		sprintf( st, "No Precipitation layers found.");
		outgtext( st, txtrow, txtlin);
		}
	}


/*NP*/
void best_guess_ptype(short txtlin, short txtrow)
/*************************************************************/
/*  BEST_GUESS_PTYPE                                         */
/*  John Hart  NSSFC KCMO                                    */
/*************************************************************/
{
	float h1000, ix1, ix2, ix3, ix4, pose, nege, ptop, pbot, sfc1;
	float itemp, ilvl;
	short phase, tIndex;
	char st[80], pt[80];
	struct _ptype ptype1;

	strcpy(st, init_phase(&ix1, &phase)); 
	ptype1.init_phase = phase;
	ptype1.init_temp = i_temp(ix1, I_PRES);
	ptype1.init_lvl = ix1;

	posneg_temperature(-1, &pose, &nege, &ptop, &pbot);
	ptype1.tpos = pose;
	ptype1.tneg = nege;

	tIndex = getParmIndex("TEMP");

	if (!sndg || tIndex == -1)
	  ptype1.tsfc = RMISSD;
	else
	  ptype1.tsfc = sndg[sfc()][tIndex];

	strcpy(pt, best_guess(ptype1));
        set_font(6);
        setcolor(31);
	sprintf(st, "%s", pt); 
        ix1 = (350 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);

	txtrow += 18;
        set_font(4);
	sprintf(st, "Based on sfc temperature of %.1f F.", ctof(ptype1.tsfc)); 
        ix1 = (350 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);
}


/*NP*/
void show_mainpage(void)
/*************************************************************/
/*  SHOW_MAINPAGE                                            */
/*  John Hart  SPC OUN                                       */
/*************************************************************/

{
	clear_paramarea();
	main_thermo();
	main_winds();
}


/*NP*/
void main_thermo(void)
/**************************************************************/
/*  MAIN_THERMO                                              */
/*  John Hart  SPC OUN                                       */
/*************************************************************/
{
	float ix1, ix2, ix3, pres;
        short txtlin, txtrow, oldlplchoice, pIndex, zIndex, tIndex;
        char st[100];
        Parcel pcl;
        Parcel pcl2;

        setcolor(3);
        set_font(1);
	setlinestyle(1, 1);
        txtlin = 125;
        strcpy(st, "THERMODYNAMIC OVERVIEW");
        outgtext(st, (((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2)), 
	  txtlin);
        setcolor(0);
        rectangle( 1, skv.brx + 25, txtlin+20, xwdth-10, txtlin+255 );
        setcolor(7);
        rectangle( 0, skv.brx + 25, txtlin+20, xwdth-10, txtlin+255 );

        set_font(3);

        txtlin = 160;
	txtrow = skv.brx + 30;
        setcolor(3);
	sprintf( st, "PARCEL    CAPE (j/kg)    LI (C)     CINH (j/kg)" );
	outgtext ( st, txtrow, txtlin );
	txtlin += 15;
	sprintf(st, "-----------------------------------------------" );
	outgtext(st, txtrow, txtlin);

	oldlplchoice = lplvals.flag;

        setcolor(31);
	txtlin += 16;
	define_parcel(1, 0);
	ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	ix1 = parcelx(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl2);
	sprintf(st, "SBCAPE    %5.0f        %5.0f        %5.0f (%.0f)", 
	  pcl.bplus, pcl.li5, pcl.bminus, pcl2.bminus);
	outgtext(st, txtrow, txtlin );
	
	txtlin += 16;
	define_parcel(3, mu_layer);
	ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	ix1 = parcelx( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl2);
	sprintf(st, "MUCAPE    %5.0f        %5.0f        %5.0f (%.0f)", 
	  pcl.bplus, pcl.li5, pcl.bminus, pcl2.bminus);
	outgtext(st, txtrow, txtlin);

	txtlin += 16;
	define_parcel(4, mml_layer);
	ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	ix1 = parcelx( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl2);
	sprintf(st, "MLCAPE    %5.0f        %5.0f        %5.0f (%.0f)", 
	  pcl.bplus, pcl.li5, pcl.bminus, pcl2.bminus);
	outgtext(st, txtrow, txtlin );

	txtlin += 16;
	define_parcel(2, 0);
	ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	ix1 = parcelx( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl2);
	sprintf( st, "Fcst Sfc  %5.0f        %5.0f        %5.0f (%.0f)", 
	  pcl.bplus, pcl.li5, pcl.bminus, pcl2.bminus);
	outgtext(st, txtrow, txtlin);

	if (oldlplchoice == 3)
	  pres = mu_layer;
        else if (oldlplchoice == 4)
	  pres = mml_layer;
        else if (oldlplchoice == 5)
	  pres = user_level;
	else
	  pres = mml_layer;
	define_parcel(oldlplchoice, pres);

	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");
	tIndex = getParmIndex("TEMP");

	txtlin += 30;
	precip_water(&ix1, -1, -1 );
	mean_relhum(&ix2, -1, -1 );
	if (sndg && pIndex != -1)
	  mean_relhum(&ix3, -1, sndg[sfc()][pIndex]-150);
	else
	  ix3 = RMISSD;
	sprintf( st, "PW = %4.2f       Mean RH = %3.0f       Low RH = %3.0f", 
	  ix1, ix2, ix3);
	outgtext(st, txtrow, txtlin);
	
	txtlin += 30;
	lapse_rate(&ix1, 850, 500);
	lapse_rate(&ix2, 700, 500);
	if (sndg && zIndex != -1)
	  lapse_rate(&ix3, sndg[sfc()][pIndex], 
	    i_pres(sndg[sfc()][zIndex]+3000));
	else
	  ix3 = RMISSD;
	sprintf(st, "850-500mb Lapse Rate =    %3.0f C / %3.1f C/km", 
	  i_temp(850, I_PRES)-i_temp(500, I_PRES), ix1);
	outgtext ( st, txtrow, txtlin );
	txtlin += 16;
	sprintf(st, "700-500mb Lapse Rate =    %3.0f C / %3.1f C/km", 
	  i_temp(700, I_PRES)-i_temp(500, I_PRES), ix2);
	outgtext(st, txtrow, txtlin );
	txtlin += 16;
	if (sndg && tIndex != -1 && zIndex != -1) {
	  sprintf(st, "Sfc-3km   Lapse Rate =    %3.0f C / %3.1f C/km", 
	    sndg[sfc()][tIndex]-i_temp(i_pres(sndg[sfc()][zIndex]+3000), 
	    I_PRES), ix3);
	}
	else {
	  sprintf(st, "Sfc-3km   Lapse Rate =    %3.0f C / %3.1f C/km", 
	    RMISSD, RMISSD);
	}
	outgtext(st, txtrow, txtlin);
	
	txtlin += 30;
	ix2 = mtof(agl(i_hght(wb_lvl(0, &ix1), I_PRES)));
	ix3 = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));
	sprintf(st, "WBZ = %5.0f ft            FZL = %5.0f ft", ix2, ix3);
	outgtext(st, txtrow, txtlin);
}


/*NP*/
void main_winds(void)
/*************************************************************/
/*  MAIN_WINDS                                               */
/*  John Hart  SPC OUN                                       */
/*************************************************************/
{
        float ix1, ix2, ix3, ix4, pres;
        short txtlin, txtrow, pIndex;
        char st[80];
        float sfctemp, sfcdwpt, sfcpres;
        Parcel pcl;

        setcolor(3);
        set_font(1);
        setlinestyle(1, 1);
        txtlin = 410;
        strcpy( st, "KINEMATIC OVERVIEW" );
        outgtext(st, (((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2)), 
	  txtlin);
        setcolor(0);
        rectangle( 1, skv.brx + 25, txtlin+20, xwdth-10, txtlin+255 );
        setcolor(7);
        rectangle(0, skv.brx + 25, txtlin+20, xwdth-10, txtlin+255);

        set_font(3);

        txtlin += 30;
        txtrow = skv.brx + 30;
        setcolor(3);
        sprintf(st, "Storm Motion  %3.0f%c / %.0f kt", st_dir, 176, st_spd);
        outgtext(st, txtrow, txtlin );
        
        setcolor(31);
	txtlin += 16;
	ix1 = helicity( 0, 1000, st_dir, st_spd, &ix2, &ix3);
        sprintf(st, "Sfc-1km Helicity:  %4.0f m%c/s%c", ix1, 178, 178);
        outgtext(st, txtrow, txtlin);
	
	txtlin += 16;
	ix1 = helicity( 0, 3000, st_dir, st_spd, &ix2, &ix3);
        sprintf(st, "Sfc-3km Helicity:  %4.0f m%c/s%c", ix1, 178, 178);
        outgtext(st, txtrow, txtlin);

	txtlin += 16;
	ix1 = helicity( -1, -1, st_dir, st_spd, &ix2, &ix3);
        sprintf(st, "Effective Helicity:%4.0f m%c/s%c", ix1, 178, 178);
        outgtext(st, txtrow, txtlin);

	txtlin += 30;

	pIndex = getParmIndex("PRES");
	if (sndg && pIndex != -1)
	  wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, 
	    &ix2, &ix3, &ix4);
	else
	  ix4 = RMISSD;
        sprintf(st, "Sfc-6km Shear: %4.0f kt (%.0f m/s)", ix4, kt_to_mps(ix4));
        outgtext(st, txtrow, txtlin);

	txtlin += 16;
	wind_shear(-1, i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        sprintf(st, "Low-6km Shear: %4.0f kt (%.0f m/s)", ix4, kt_to_mps(ix4));
        outgtext(st, txtrow, txtlin);

	txtlin += 16;
	ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	bulk_rich( pcl, &ix1 );
        sprintf(st, "BRN Shear: %4.0f m%c/s%c", ix1, 178, 178);
        outgtext(st, txtrow, txtlin);
}

        /*NP*/
        void show_parcel_new( void )
        /*************************************************************/
        /*  SHOW_PARCEL_NEW                                          */ 
        /*  Rich Thompson and  John Hart  SPC OUN                    */
        /*                                                           */
        /*  Lifts a parcel, draws curve, and displays data on screen.*/
	/*  Includes "clickable" parcel area inset                   */
        /*************************************************************/
        {
        float ix1, ix2, ix3, ix4, sfctemp, sfcdwpt, sfcpres, j1, j2, showalter;
/* 24 Mar 2008 */
/*        float nv_cape, nv_cinh, nv_cap, pres, ptop, pbot, mucape, mumixr, lcl;*/

	float nv_cape, nv_cinh, nv_cap, pres, mucape, mumixr, lcl, precip_efficiency, thte3;
        float lr75, shr6, fzlh, mucinh, ship, oldlplpres, sbcp, depth, el, mlcape;
        short txtlin, txtrow, oldlplchoice, pIndex, zIndex, tIndex, trow2, i, psigt_stpcin;
	short lcl_mark, lfc_mark, el_mark, type_ww, dcp, wwtype;
	short x, y, x2, y2;
        char st[100], st1[20];
	Parcel pcl;
	Parcel pcl2;

	bndry_ci(300.0, 10000.0);
	oldlplchoice = lplvals.flag;

        setcliprgn(1,1,xwdth, xhght);

        setlinestyle(1,1);
        txtlin = skv.tlx + 4;
        txtrow = skv.bry + 20;
        setcolor(0);
        rectangle(1, txtlin, txtrow, txtlin + 350, txtrow + 250);
        setcolor(31);
        rectangle(0, txtlin, txtrow, txtlin + 350, txtrow + 250);

        /* ----- Parcel Data Area ----- */
        setcolor(31);
        set_font(4);
        txtlin += 5;

	txtrow += 6;
        strcpy(st, "                                 CAPE      CINH       LCL         LI        LFC        EL");
        outgtext(st, txtlin, txtrow);
        txtrow += 12;
        moveto(txtlin-5, txtrow);
        lineto(txtlin+345, txtrow);

        /* SB PARCEL */
        txtrow += 6;
/*	setcolor(1);
	if (oldlplchoice == 1) { setcolor(18); rectangle(0, txtlin - 1, txtrow - 1, txtlin + 326, txtrow + 11);  setcolor(5);}
      	strcpy(st, "SBCAPE");
        outgtext(st, txtlin, txtrow);
*/      
/*	printf("\noldlplchoice before SB=%d\n", lplvals.flag);
*/	define_parcel(1,0);
        sbcp = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
/*      printf("\nsb temp=%.1f\n", lplvals.temp);
        printf("\nsb dwpt=%.1f\n", lplvals.dwpt);
        printf("\nsb pres=%.1f\n", lplvals.pres);
        ix1 = parcelx( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl2);
*/      if (oldlplchoice == 1)
        	{
/*        	printf("\nparcel choice AFTER SB button pushed =%d\n", lplvals.flag);
*/
        	sfctemp = lplvals.temp;
        	sfcdwpt = lplvals.dwpt;
        	sfcpres = lplvals.pres;
		setcolor(31);
        	setlinestyle( 4, 1 );
/*      printf("\nshow_parcel_new temp_sb=%.1f\n", lplvals.temp);
        printf("\nshow_parcel_new dwpt_sb=%.1f\n", lplvals.dwpt);
        printf("\nshow_parcel_new pres_sb=%.1f\n", lplvals.pres);
*/       	trace_parcel( sfcpres, sfctemp, sfcdwpt);
		trace_dcape();
		lcl_mark = pres_to_pix(pcl.lclpres);
        	setcolor(0);
/*
        	rectangle(1, skv.tlx + 413, lcl_mark, skv.tlx + 437, lcl_mark + 12);
		setcolor(22);
		set_font(4);
		setlinestyle(1, 2);
                moveto(skv.tlx + 415, lcl_mark);
		lineto(skv.tlx + 435, lcl_mark);
		sprintf( st, "LCL");
        	outgtext(st, skv.tlx + 415, lcl_mark + 1);
                lfc_mark = pres_to_pix(pcl.lfcpres);
                setcolor(0);
                rectangle(1, skv.tlx + 413, lfc_mark - 13, skv.tlx + 437, lfc_mark - 1);
                setcolor(20);
                moveto(skv.tlx + 415, lfc_mark);
                lineto(skv.tlx + 435, lfc_mark);
                sprintf( st, "LFC");
                outgtext(st, skv.tlx + 415, lfc_mark - 12);
		el_mark = pres_to_pix(pcl.elpres);
                setcolor(0);
                rectangle(1, skv.tlx + 413, el_mark - 13, skv.tlx + 437, el_mark - 1);
                setcolor(7);
                moveto(skv.tlx + 415, el_mark);
                lineto(skv.tlx + 437, el_mark);
                sprintf( st, "EL");
                outgtext(st, skv.tlx + 415, el_mark - 12);
*/		

		rectangle(1, skv.tlx + (xwdth*.38), lcl_mark, skv.tlx + (xwdth*.38) + 24, lcl_mark + 12);
                setcolor(22);
                set_font(4);
                setlinestyle(1, 2);
                moveto(skv.tlx + (xwdth*.38), lcl_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lcl_mark);
                sprintf( st, "LCL");
                outgtext(st, skv.tlx + (xwdth*.38), lcl_mark + 1);
                if (pcl.bplus > 0.0){
		lfc_mark = pres_to_pix(pcl.lfcpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lfc_mark - 13, skv.tlx + (xwdth*.38) + 24, lfc_mark - 1);
                setcolor(20);
                moveto(skv.tlx + (xwdth*.38), lfc_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lfc_mark);
		sprintf( st, "LFC");
                outgtext(st, skv.tlx + (xwdth*.38), lfc_mark - 12);
                el_mark = pres_to_pix(pcl.elpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), el_mark - 13, skv.tlx + (xwdth*.38) + 24, el_mark - 1);
                setcolor(7);
                moveto(skv.tlx + (xwdth*.38), el_mark);
                lineto(skv.tlx + (xwdth*.38) +20, el_mark);
                sprintf( st, "EL");
                outgtext(st, skv.tlx + (xwdth*.38), el_mark - 12);
		}
		setlinestyle(1,1);
                set_font(2);
        	setcolor(27);
		strcpy(st, "SB PARCEL");
        	outgtext(st, txtlin, txtrow);
/*		set_font(4); */
                setcolor(18);
                rectangle(0, txtlin - 1, txtrow - 1, txtlin + 341, txtrow + 11);
                setcolor(8);
                if (pcl.bplus >= 100) setcolor(18);
                if (pcl.bplus >= 500) setcolor(10);
                if (pcl.bplus >= 1000) setcolor(19);
                if (pcl.bplus >= 2500) setcolor(2);
                if (pcl.bplus >= 4000) setcolor(7);
                sprintf( st, "%5.0f", pcl.bplus);
                disp_param( st, txtlin+130, txtrow);
                if (pcl.bplus < 1) {setcolor(8); sprintf( st, "%5.0f", pcl.bminus);}
                else
                setcolor(21);
                if (pcl.bminus < -10) setcolor(22);
                if (pcl.bminus < -50) setcolor(18);
                if (pcl.bminus < -100) setcolor(8);
                sprintf( st, "%5.0f", pcl.bminus);
                disp_param( st, txtlin+168, txtrow);
                lcl = agl(i_hght(pcl.lclpres, I_PRES));
                setcolor(21);
                if (lcl > 500) setcolor(22);
                if (lcl > 1000) setcolor(23);
                if (lcl > 1500) setcolor(18);
                if (lcl > 2000) setcolor(8);
                sprintf( st, "%5.0fm", lcl);
        /*      strcpy( st, qc2(agl(i_hght(pcl.lclpres, I_PRES)), "m", 0));
        */      disp_param( st, txtlin+215, txtrow);
                setcolor(31);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
		set_font(4);
                strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+295, txtrow);
                strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
                disp_param( st, txtlin+340, txtrow);
                }
        else
                {
/*        	printf("\nparcel choice before SB button pushed =%d\n", lplvals.flag);
*/              setcolor(1);
		set_font(4);
                strcpy(st, "SB PARCEL");
                outgtext(st, txtlin, txtrow);
                sprintf( st, "%5.0f", pcl.bplus);
                disp_param( st, txtlin+130, txtrow);
                sprintf( st, "%5.0f", pcl.bminus);
                disp_param( st, txtlin+168, txtrow);
                lcl = agl(i_hght(pcl.lclpres, I_PRES));                
		sprintf( st, "%5.0fm", lcl);
                disp_param( st, txtlin+215, txtrow);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
                strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+295, txtrow);
                strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
                disp_param( st, txtlin+340, txtrow);
                }

	/* ML PARCEL */
        txtrow += 14;
        define_parcel(4, mml_layer);
        mlcape  = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        if (oldlplchoice == 4)
                {
        	sfctemp = lplvals.temp;
        	sfcdwpt = lplvals.dwpt;
        	sfcpres = lplvals.pres;
        	setcolor(31);
        	setlinestyle( 4, 1 );
        	trace_parcel( sfcpres, sfctemp, sfcdwpt);
                trace_dcape();
                lcl_mark = pres_to_pix(pcl.lclpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lcl_mark, skv.tlx + (xwdth*.38) + 24, lcl_mark + 12);
                setcolor(22);
                set_font(4);
                setlinestyle(1, 2);
                moveto(skv.tlx + (xwdth*.38), lcl_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lcl_mark);
                sprintf( st, "LCL");
                outgtext(st, skv.tlx + (xwdth*.38), lcl_mark + 1);
		if (pcl.bplus > 0.0){
                lfc_mark = pres_to_pix(pcl.lfcpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lfc_mark - 13, skv.tlx + (xwdth*.38) + 24, lfc_mark - 1);
                setcolor(20);
                moveto(skv.tlx + (xwdth*.38), lfc_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lfc_mark);
                sprintf( st, "LFC");
                outgtext(st, skv.tlx + (xwdth*.38), lfc_mark - 12);
                el_mark = pres_to_pix(pcl.elpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), el_mark - 13, skv.tlx + (xwdth*.38) + 24, el_mark - 1);
                setcolor(7);
                moveto(skv.tlx + (xwdth*.38), el_mark);
                lineto(skv.tlx + (xwdth*.38) +20, el_mark);
                sprintf( st, "EL");
                outgtext(st, skv.tlx + (xwdth*.38), el_mark - 12);
		}
                setlinestyle(1,1);
                setcolor(27);
                set_font(2);
                strcpy(st, "ML PARCEL");
                outgtext(st, txtlin, txtrow);
                setcolor(18);
                rectangle(0, txtlin - 1, txtrow - 1, txtlin + 341, txtrow + 11);
                setcolor(1);
                if (pcl.bplus >= 100) setcolor(18);
                if (pcl.bplus >= 500) setcolor(10);
                if (pcl.bplus >= 1000) setcolor(19);
                if (pcl.bplus >= 2500) setcolor(2);
                if (pcl.bplus >= 4000) setcolor(7);
                sprintf( st, "%5.0f", pcl.bplus);
                disp_param( st, txtlin+130, txtrow);
                if (pcl.bplus < 1) {setcolor(8); sprintf( st, "%5.0f", pcl.bminus);}
                else
                setcolor(22);
                if (pcl.bminus < -10) setcolor(22);
                if (pcl.bminus < -25) setcolor(23);
                if (pcl.bminus < -50) setcolor(18);
                if (pcl.bminus < -100) setcolor(8);
                sprintf( st, "%5.0f", pcl.bminus);
                disp_param( st, txtlin+168, txtrow);
                lcl = agl(i_hght(pcl.lclpres, I_PRES));
                setcolor(22);
                if (lcl > 500) setcolor(22);
                if (lcl > 1000) setcolor(23);
                if (lcl > 1500) setcolor(18);
                if (lcl > 2000) setcolor(8);
                sprintf( st, "%5.0fm", lcl);
                disp_param( st, txtlin+215, txtrow);
                setcolor(31);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
                set_font(4);
                strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+295, txtrow);
                strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
                disp_param( st, txtlin+340, txtrow);
                }
        else
                {
                setcolor(1);
                set_font(4);
                strcpy(st, "ML PARCEL");
                outgtext(st, txtlin, txtrow);
                sprintf( st, "%5.0f", pcl.bplus);
                disp_param( st, txtlin+130, txtrow);
                sprintf( st, "%5.0f", pcl.bminus);
                disp_param( st, txtlin+168, txtrow);
                lcl = agl(i_hght(pcl.lclpres, I_PRES));
                sprintf( st, "%5.0fm", lcl);
                disp_param( st, txtlin+215, txtrow);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
                strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+295, txtrow);
                strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
                disp_param( st, txtlin+340, txtrow);
                }

	/* FCST SFC PARCEL */		
        txtrow += 14;
	define_parcel(2, 0);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        if (oldlplchoice == 2)  
		{
        	sfctemp = lplvals.temp;
        	sfcdwpt = lplvals.dwpt;
        	sfcpres = lplvals.pres;
        	setcolor(31);
        	setlinestyle( 4, 1 );
        	trace_parcel( sfcpres, sfctemp, sfcdwpt);
                set_font(5);
		sprintf( st, "%.0f", ctof(sfctemp));
                x = temp_to_pix(sfctemp, sfcpres);
                y = pres_to_pix(sfcpres);
                x2 = x + getgtextextent(st)+2;
                y2 = y + 12;
                setcolor(0);
                rectangle(1, x+1, y+3, x2, y+13);
                setcolor(31);
                outgtext(st, x+2, y+2);

        	sprintf( st, "%.0f", ctof(sfcdwpt));
        	x = temp_to_pix(sfcdwpt, sfcpres);
        	y = pres_to_pix(sfcpres);
        	x2 = x + getgtextextent(st)-12;
        	y2 = y + 12;
        	setcolor(0);
        	rectangle(1, x-13, y+3, x2, y+13);
        	setcolor(31);
        	outgtext(st, x-12, y+2);

                trace_dcape();
                lcl_mark = pres_to_pix(pcl.lclpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lcl_mark, skv.tlx + (xwdth*.38) + 24, lcl_mark + 12);
                setcolor(22);
                set_font(4);
                setlinestyle(1, 2);
                moveto(skv.tlx + (xwdth*.38), lcl_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lcl_mark);
                sprintf( st, "LCL");
                outgtext(st, skv.tlx + (xwdth*.38), lcl_mark + 1);
		if (pcl.bplus > 0.0){
                lfc_mark = pres_to_pix(pcl.lfcpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lfc_mark - 13, skv.tlx + (xwdth*.38) + 24, lfc_mark - 1);
                setcolor(20);
                moveto(skv.tlx + (xwdth*.38), lfc_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lfc_mark);
                sprintf( st, "LFC");
                outgtext(st, skv.tlx + (xwdth*.38), lfc_mark - 12);
                el_mark = pres_to_pix(pcl.elpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), el_mark - 13, skv.tlx + (xwdth*.38) + 24, el_mark - 1);
                setcolor(7);
                moveto(skv.tlx + (xwdth*.38), el_mark);
                lineto(skv.tlx + (xwdth*.38) +20, el_mark);
                sprintf( st, "EL");
                outgtext(st, skv.tlx + (xwdth*.38), el_mark - 12);
		}	
		setlinestyle(1,1);
		setcolor(27);
		set_font(2);
		strcpy(st, "FCST SFC");
        	outgtext(st, txtlin, txtrow);
		setcolor(18);
		rectangle(0, txtlin - 1, txtrow - 1, txtlin + 341, txtrow + 11); 		
		setcolor(8);
        	if (pcl.bplus >= 100) setcolor(18);
        	if (pcl.bplus >= 500) setcolor(10);
        	if (pcl.bplus >= 1000) setcolor(19);
        	if (pcl.bplus >= 2500) setcolor(2);
        	if (pcl.bplus >= 4000) setcolor(7);
        	sprintf( st, "%5.0f", pcl.bplus);
        	disp_param( st, txtlin+130, txtrow);
		if (pcl.bplus < 1) {setcolor(8); sprintf( st, "%5.0f", pcl.bminus);}
		else
        	setcolor(21);
        	if (pcl.bminus < -10) setcolor(22);
        	if (pcl.bminus < -25) setcolor(23);
        	if (pcl.bminus < -50) setcolor(18);
        	if (pcl.bminus < -100) setcolor(8);
        	sprintf( st, "%5.0f", pcl.bminus);
        	disp_param( st, txtlin+168, txtrow);
        	lcl = agl(i_hght(pcl.lclpres, I_PRES));
        	setcolor(21);
        	if (lcl > 500) setcolor(22);
        	if (lcl > 1000) setcolor(23);
        	if (lcl > 1500) setcolor(18);
        	if (lcl > 2000) setcolor(8);
        	sprintf( st, "%5.0fm", lcl);
	        disp_param( st, txtlin+215, txtrow);
        	setcolor(31);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
		set_font(4);
        	strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
        	disp_param( st, txtlin+295, txtrow);
	       	strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
      		disp_param( st, txtlin+340, txtrow);
		}
	else 
		{
		setcolor(1);
		set_font(4);
		strcpy(st, "FCST SFC");
       		outgtext(st, txtlin, txtrow);
		sprintf( st, "%5.0f", pcl.bplus);
        	disp_param( st, txtlin+130, txtrow);
        	sprintf( st, "%5.0f", pcl.bminus);
        	disp_param( st, txtlin+168, txtrow);
                lcl = agl(i_hght(pcl.lclpres, I_PRES));
        	sprintf( st, "%5.0fm", lcl);
		disp_param( st, txtlin+215, txtrow);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
        	strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
        	disp_param( st, txtlin+295, txtrow);
        	strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
        	disp_param( st, txtlin+340, txtrow);
		}

        /* MU PARCEL */
        txtrow += 14;
	define_parcel(3, mu_layer);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	if (sbcp > ix1) {
		define_parcel(1,0);
		ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
		}
        if (oldlplchoice == 3)
                {
        	sfctemp = lplvals.temp;
        	sfcdwpt = lplvals.dwpt;
        	sfcpres = lplvals.pres;
        	setcolor(31);
        	setlinestyle( 4, 1 );
        	trace_parcel( sfcpres, sfctemp, sfcdwpt);
                trace_dcape();
                lcl_mark = pres_to_pix(pcl.lclpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lcl_mark, skv.tlx + (xwdth*.38) + 24, lcl_mark + 12);
                setcolor(22);
                set_font(4);
                setlinestyle(1, 2);
                moveto(skv.tlx + (xwdth*.38), lcl_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lcl_mark);
                sprintf( st, "LCL");
                outgtext(st, skv.tlx + (xwdth*.38), lcl_mark + 1);
		if (pcl.bplus > 0.0){
                lfc_mark = pres_to_pix(pcl.lfcpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lfc_mark - 13, skv.tlx + (xwdth*.38) + 24, lfc_mark - 1);
                setcolor(20);
                moveto(skv.tlx + (xwdth*.38), lfc_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lfc_mark);
                sprintf( st, "LFC");
                outgtext(st, skv.tlx + (xwdth*.38), lfc_mark - 12);
                el_mark = pres_to_pix(pcl.elpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), el_mark - 13, skv.tlx + (xwdth*.38) + 24, el_mark - 1);
                setcolor(7);
                moveto(skv.tlx + (xwdth*.38), el_mark);
                lineto(skv.tlx + (xwdth*.38) +20, el_mark);
                sprintf( st, "EL");
                outgtext(st, skv.tlx + (xwdth*.38), el_mark - 12);
		}	
		setlinestyle(1,1);
		setcolor(27);
		set_font(2);
		strcpy(st, "MU PARCEL");
        	outgtext(st, txtlin, txtrow);
	        setcolor(18);
                rectangle(0, txtlin - 1, txtrow - 1, txtlin + 341, txtrow + 11);
                setcolor(8);
                if (pcl.bplus >= 100) setcolor(18);
                if (pcl.bplus >= 500) setcolor(10);
                if (pcl.bplus >= 1000) setcolor(19);
                if (pcl.bplus >= 2500) setcolor(2);
                if (pcl.bplus >= 4000) setcolor(7);
                sprintf( st, "%5.0f", pcl.bplus);
                disp_param( st, txtlin+130, txtrow);
                if (pcl.bplus < 1) {setcolor(8); sprintf( st, "%5.0f", pcl.bminus);}
                else
                setcolor(21);
                if (pcl.bminus < -10) setcolor(22);
                if (pcl.bminus < -25) setcolor(23);
                if (pcl.bminus < -50) setcolor(18);
                if (pcl.bminus < -100) setcolor(8);
                sprintf( st, "%5.0f", pcl.bminus);
                disp_param( st, txtlin+168, txtrow);
                setcolor(31);
		set_font(4);
                strcpy( st, qc2(agl(i_hght(pcl.lclpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+215, txtrow);
		set_font(2);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
		set_font(4);                
		strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+295, txtrow);
                strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
                disp_param( st, txtlin+340, txtrow);
                mucape = pcl.bplus;
                mucinh = pcl.bminus;
                mumixr = mixratio(lplvals.pres, lplvals.dwpt);
                }
        else
                {
                setcolor(1);
		set_font(4);
                strcpy(st, "MU PARCEL");
                outgtext(st, txtlin, txtrow);
                sprintf( st, "%5.0f", pcl.bplus);
                disp_param( st, txtlin+130, txtrow);
                sprintf( st, "%5.0f", pcl.bminus);
                disp_param( st, txtlin+168, txtrow);
                lcl = agl(i_hght(pcl.lclpres, I_PRES));
                sprintf( st, "%5.0fm", lcl);
                disp_param( st, txtlin+215, txtrow);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
                strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+295, txtrow);
                strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
                disp_param( st, txtlin+340, txtrow);
                mucape = pcl.bplus;
                mucinh = pcl.bminus;
                mumixr = mixratio(lplvals.pres, lplvals.dwpt);
                }



        /* EFFECTIVE PARCEL */
        txtrow += 14;
	define_parcel(6, mu_layer);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	if (oldlplchoice == 6) 
		{ 
        	sfctemp = lplvals.temp;
        	sfcdwpt = lplvals.dwpt;
        	sfcpres = lplvals.pres;
        	setcolor(31);
        	setlinestyle( 4, 1 );
        	trace_parcel( sfcpres, sfctemp, sfcdwpt);
                trace_dcape();
                lcl_mark = pres_to_pix(pcl.lclpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lcl_mark, skv.tlx + (xwdth*.38) + 24, lcl_mark + 12);
                setcolor(22);
                set_font(4);
                setlinestyle(1, 2);
                moveto(skv.tlx + (xwdth*.38), lcl_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lcl_mark);
                sprintf( st, "LCL");
                outgtext(st, skv.tlx + (xwdth*.38), lcl_mark + 1);
		if (pcl.bplus > 0.0){
                lfc_mark = pres_to_pix(pcl.lfcpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lfc_mark - 13, skv.tlx + (xwdth*.38) + 24, lfc_mark - 1);
                setcolor(20);
                moveto(skv.tlx + (xwdth*.38), lfc_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lfc_mark);
                sprintf( st, "LFC");
                outgtext(st, skv.tlx + (xwdth*.38), lfc_mark - 12);
                el_mark = pres_to_pix(pcl.elpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), el_mark - 13, skv.tlx + (xwdth*.38) + 24, el_mark - 1);
                setcolor(7);
                moveto(skv.tlx + (xwdth*.38), el_mark);
                lineto(skv.tlx + (xwdth*.38) +20, el_mark);
                sprintf( st, "EL");
                outgtext(st, skv.tlx + (xwdth*.38), el_mark - 12);
		}	
		setlinestyle(1,1);
                setcolor(27);
		set_font(2);
                strcpy(st, "EFF PARCEL");
                outgtext(st, txtlin, txtrow);
		setcolor(18); 
		rectangle(0, txtlin - 1, txtrow - 1, txtlin + 341, txtrow + 11);  
		setcolor(8);
        	if (pcl.bplus >= 100) setcolor(18);
        	if (pcl.bplus >= 500) setcolor(10);
        	if (pcl.bplus >= 1000) setcolor(19);
        	if (pcl.bplus >= 2500) setcolor(2);
        	if (pcl.bplus >= 4000) setcolor(7);
        	sprintf( st, "%5.0f", pcl.bplus);
        	disp_param( st, txtlin+130, txtrow);
        	if (pcl.bplus < 1) {setcolor(8); sprintf( st, "%5.0f", pcl.bminus);}
        	else
        	setcolor(21);
        	if (pcl.bminus < -10) setcolor(22);
        	if (pcl.bminus < -25) setcolor(23);
        	if (pcl.bminus < -50) setcolor(18);
        	if (pcl.bminus < -100) setcolor(8);
        	sprintf( st, "%5.0f", pcl.bminus);
        	disp_param( st, txtlin+168, txtrow);
        	setcolor(31);
		set_font(4);
        	strcpy( st, qc2(agl(i_hght(pcl.lclpres, I_PRES)), "m", 0));
        	disp_param( st, txtlin+215, txtrow);
		set_font(2);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
		set_font(4);
        	strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
        	disp_param( st, txtlin+295, txtrow);
        	strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
        	disp_param( st, txtlin+340, txtrow);
                }
        else
                {
                setcolor(1);
		set_font(4);
                strcpy(st, "EFF PARCEL");
                outgtext(st, txtlin, txtrow);
                sprintf( st, "%5.0f", pcl.bplus);
                disp_param( st, txtlin+130, txtrow);
                sprintf( st, "%5.0f", pcl.bminus);
                disp_param( st, txtlin+168, txtrow);
                lcl = agl(i_hght(pcl.lclpres, I_PRES));
                sprintf( st, "%5.0fm", lcl);
                disp_param( st, txtlin+215, txtrow);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
                strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+295, txtrow);
                strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
                disp_param( st, txtlin+340, txtrow);
                }

        /* USER DEFINED PARCEL */
        txtrow += 14;
        define_parcel(5, user_level);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        if (oldlplchoice == 5)
                {
                sfctemp = lplvals.temp;
                sfcdwpt = lplvals.dwpt;
                sfcpres = lplvals.pres;
                setcolor(31);
                setlinestyle( 4, 1 );
                trace_parcel( sfcpres, sfctemp, sfcdwpt);
                trace_dcape();
                lcl_mark = pres_to_pix(pcl.lclpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lcl_mark, skv.tlx + (xwdth*.38) + 24, lcl_mark + 12);
                setcolor(22);
                set_font(4);
                setlinestyle(1, 2);
                moveto(skv.tlx + (xwdth*.38), lcl_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lcl_mark);
                sprintf( st, "LCL");
                outgtext(st, skv.tlx + (xwdth*.38), lcl_mark + 1);
		if (pcl.bplus > 0.0){
                lfc_mark = pres_to_pix(pcl.lfcpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), lfc_mark - 13, skv.tlx + (xwdth*.38) + 24, lfc_mark - 1);
                setcolor(20);
                moveto(skv.tlx + (xwdth*.38), lfc_mark);
                lineto(skv.tlx + (xwdth*.38) + 20, lfc_mark);
                sprintf( st, "LFC");
                outgtext(st, skv.tlx + (xwdth*.38), lfc_mark - 12);
                el_mark = pres_to_pix(pcl.elpres);
                setcolor(0);
                rectangle(1, skv.tlx + (xwdth*.38), el_mark - 13, skv.tlx + (xwdth*.38) + 24, el_mark - 1);
                setcolor(7);
                moveto(skv.tlx + (xwdth*.38), el_mark);
                lineto(skv.tlx + (xwdth*.38) +20, el_mark);
                sprintf( st, "EL");
                outgtext(st, skv.tlx + (xwdth*.38), el_mark - 12);
		}
                setlinestyle(1,1);
                setcolor(27);
                set_font(2);
                strcpy(st, "USER DEF");
                outgtext(st, txtlin, txtrow);
                setcolor(18);
                rectangle(0, txtlin - 1, txtrow - 1, txtlin + 341, txtrow + 11);
                setcolor(8);
                if (pcl.bplus >= 100) setcolor(18);
                if (pcl.bplus >= 500) setcolor(10);
                if (pcl.bplus >= 1000) setcolor(19);
                if (pcl.bplus >= 2500) setcolor(2);
                if (pcl.bplus >= 4000) setcolor(7);
                sprintf( st, "%5.0f", pcl.bplus);
                disp_param( st, txtlin+130, txtrow);
                if (pcl.bplus < 1) {setcolor(8); sprintf( st, "%5.0f", pcl.bminus);}
                else
                setcolor(21);
                if (pcl.bminus < -10) setcolor(22);
                if (pcl.bminus < -25) setcolor(23);
                if (pcl.bminus < -50) setcolor(18);
                if (pcl.bminus < -100) setcolor(8);
                sprintf( st, "%5.0f", pcl.bminus);
                disp_param( st, txtlin+168, txtrow);
                setcolor(31);
                set_font(4);
                strcpy( st, qc2(agl(i_hght(pcl.lclpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+215, txtrow);
                set_font(2);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
                set_font(4);
                strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+295, txtrow);
                strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
                disp_param( st, txtlin+340, txtrow);
                }
        else
                {
                setcolor(1);
                set_font(4);
                strcpy(st, "USER DEF");
                outgtext(st, txtlin, txtrow);
                sprintf( st, "%5.0f", pcl.bplus);
                disp_param( st, txtlin+130, txtrow);
                sprintf( st, "%5.0f", pcl.bminus);
                disp_param( st, txtlin+168, txtrow);
                lcl = agl(i_hght(pcl.lclpres, I_PRES));
                sprintf( st, "%5.0fm", lcl);
                disp_param( st, txtlin+215, txtrow);
                sprintf( st, "%5.0f", pcl.li5);
                disp_param( st, txtlin+250, txtrow);
                strcpy( st, qc2(agl(i_hght(pcl.lfcpres, I_PRES)), "m", 0));
                disp_param( st, txtlin+295, txtrow);
                strcpy( st, qc2(mtof(agl(i_hght(pcl.elpres, I_PRES))), "'", 0));
                disp_param( st, txtlin+340, txtrow);
                }

        /* set parcel back to user selection */
	/*if (oldlplchoice == 1)
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
        define_parcel(oldlplchoice, pres);*/

	/* Draw line */
	setcolor(31);
	txtrow += 18;
	moveto(skv.tlx + 5, txtrow );
	lineto(skv.tlx + 355, txtrow);

	tIndex = getParmIndex("TEMP");
	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");
	tIndex = getParmIndex("TEMP");

	/* parameter row 1 */
        txtrow += 5;
        setcolor(1);
/* 24 Mar 2008 */
/*        effective_inflow_layer(100,-250, &pbot, &ptop);*/
        sprintf( st, "PW = %s", qc2( precip_water(&ix1,-1,-1), " .in", 2 ));
        outgtext( st, txtlin, txtrow);
        sprintf( st, "WNDG = %s", qc2( damaging_wind(), "", 1));
        outgtext( st, txtlin + 275, txtrow);
        sprintf( st, "WBZ = %s", qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 ), I_PRES))), "'", 0 ));
        outgtext( st, txtlin + 195, txtrow);
        define_parcel(4, 100);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        sprintf( st, "3CAPE = %s", qc2( pcl.cape3km, " J/kg", 0));
        outgtext( st, txtlin + 85, txtrow);

      /* set parcel back to user selection */
       /* if (oldlplchoice == 1)
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
        define_parcel(oldlplchoice, pres);*/

	/* parameter row 2 */
	txtrow += 13;
        sprintf( st, "K = %s", qc2( k_index(&ix1), "", 0 ));
        outgtext( st, txtlin, txtrow);
	ix4 = dcape(&ix2, &ix3);
        sprintf( st, "DCAPE = %s", qc2( ix4, " J/kg", 0 ));
        outgtext( st, txtlin + 85, txtrow);
        sprintf( st, "FZL = %s", qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES))), "'", 0 ));
        outgtext( st, txtlin + 195, txtrow);
	fzlh = mtof(agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES)));
        sprintf( st, "ESP = %s", qc2( esp(), "", 1));
        outgtext( st, txtlin + 275, txtrow);

        /* set parcel back to user selection */
       /* if (oldlplchoice == 1)
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
        define_parcel(oldlplchoice, pres);*/

	/* parameter row 3 */
	txtrow += 13;
        sprintf( st, "MidRH = %s", qc2( mean_relhum(&ix1, sndg[sfc()][pIndex]-150, sndg[sfc()][pIndex]-350), "%", 0 ));
        outgtext( st, txtlin, txtrow);
	ix4 = dcape(&ix2, &ix3);
        sprintf( st, "DownT = %s", qc2( ctof(ix3), " F", 0 ));
        outgtext( st, txtlin + 85, txtrow);
        sprintf( st, "ConvT = %s", qc2( ctof(cnvtv_temp(&ix1, -1)), "F", 0 ));
        outgtext( st, txtlin + 195, txtrow);
        sprintf( st, "MMP = %s", qc2( coniglio1(), "", 2));
        outgtext( st, txtlin + 275, txtrow);

	/* parameter row 4 */
	txtrow += 13;
        sprintf( st, "LowRH = %s", qc2( mean_relhum(&ix3, -1, sndg[sfc()][pIndex]-150), "%", 0 ));
        outgtext( st, txtlin, txtrow);
	ix4 = dcape(&ix2, &ix3);
        sprintf( st, "MeanW = %s", qc2(mean_mixratio(&ix1, -1, -1 ), " g/kg", 1 ));
        outgtext( st, txtlin + 85, txtrow);
        sprintf( st, "MaxT = %s", qc2( ctof(max_temp( &ix1, -1)), "F", 0 ));
        outgtext( st, txtlin + 195, txtrow);
        define_parcel(3, 400);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        ix2 = agl(i_hght(pcl.elpres, I_PRES));
        ix3 = agl(i_hght(pcl.lfcpres, I_PRES));
	sprintf( st, "NCAPE = %s", qc2( ix1/(ix2 - ix3), "", 2));
        outgtext( st, txtlin + 275, txtrow);
        /*sprintf( st, "TEI = %s", qc2( ThetaE_diff(&ix1), "", 1));
        outgtext( st, txtlin + 275, txtrow);*/
	precip_efficiency =  ((mean_relhum(&ix3, -1, i_pres(sndg[sfc()][zIndex]+3000)/100) * precip_water(&ix1,-1,-1)));
/*	printf("\nmean_rh = %0.1f\n", (mean_relhum(&ix3, -1, i_pres(sndg[sfc()][zIndex]+3000))/100));
	printf("\npw = %0.1f\n", precip_water(&ix1, -1, -1));
	printf("\nprecip_efficiency = %0.1f\n", (ix1) * (mean_relhum(&ix3, -1, i_pres(sndg[sfc()][zIndex]+3000))/100));  
	printf("\n3km theta-e diff = %0.1f\n", ThetaE_diff2(&thte3)); 
*/
	/* Draw line */
	setcolor(31);
	txtrow += 15;
	moveto(skv.tlx + 5, txtrow );
	lineto(skv.tlx + 355, txtrow);
	moveto(skv.tlx + 220, txtrow );
	lineto(skv.tlx + 220, skv.bry+270);

	/* lapse rates and delta-T */
        setcolor(1);
	txtrow += 12;
	trow2 = txtrow;
        outgtext ( "Sfc-3km Agl Lapse Rate =", txtlin, txtrow );
        strcpy( st, qc2(sndg[sfc()][tIndex]-i_temp(i_pres(sndg[sfc()][zIndex]+3000),I_PRES), " C", 0 ));
        strcat( st, " / " );
        strcat( st, qc2( lapse_rate(&ix3, sndg[sfc()][pIndex], i_pres(sndg[sfc()][zIndex]+3000)), " C/km", 1 ));
        outgtext ( st, txtlin+125, txtrow );

	txtrow += 15;
        outgtext ( "3-6km Agl Lapse Rate =", txtlin, txtrow );
        strcpy( st, qc2(i_temp(i_pres(sndg[sfc()][zIndex]+3000),I_PRES)-i_temp(i_pres(sndg[sfc()][zIndex]+6000),I_PRES), " C", 0 ));
        strcat( st, " / " );
        strcat( st, qc2( lapse_rate(&ix3, i_pres(sndg[sfc()][zIndex]+3000), i_pres(sndg[sfc()][zIndex]+6000)), " C/km", 1 ));
        outgtext ( st, txtlin+125, txtrow );

	txtrow += 15;
        outgtext ( "850-500mb Lapse Rate =", txtlin, txtrow );
	strcpy( st, qc2( i_temp(850, I_PRES) - i_temp(500, I_PRES), " C", 0 ));
        strcat( st, " / " );
        strcat( st, qc2( lapse_rate( &ix1, 850, 500 ), " C/km", 1 ));
        outgtext ( st, txtlin+125, txtrow );

	txtrow += 15;
	lr75 = lapse_rate( &ix1, 700, 500 );
        outgtext ( "700-500mb Lapse Rate =", txtlin, txtrow );
        strcpy( st, qc2( i_temp(700, I_PRES) - i_temp(500, I_PRES), " C", 0 ));
        strcat( st, " / " );
        strcat( st, qc2( lr75, " C/km", 1 ));
        outgtext ( st, txtlin+125, txtrow );

	/* Composite Parameters */
	set_font(6);
	txtrow = trow2-2;
	txtlin = skv.tlx + 230;
	ix1 = scp(st_dir, st_spd);
	setcolor(8);
	if (ix1 < -0.45) setcolor(6);
	if (ix1 >= -0.45 && ix1 < 0.45) setcolor(8);
	if (ix1 >=  .45) setcolor(18);
	if (ix1 >= 1.95) setcolor(19);
	if (ix1 >= 11.95) setcolor(2);
	if (ix1 >= 19.95) setcolor(7);
        sprintf( st, "Supercell = %s", qc2( scp(st_dir, st_spd), "", 1 ));
        outgtext( st, txtlin, txtrow);

	txtrow += 16;
	txtlin = skv.tlx + 230;
	ix1 = sigtorn_cin(st_dir, st_spd);
	setcolor(8);
	/* original color scheme using T03 sample */
/*	if (ix1 < .45) setcolor(8);
	if (ix1 >= .45) setcolor(31);
	if (ix1 >= 1.95) setcolor(19);
	if (ix1 >= 3.95) setcolor(2);
	if (ix1 >= 5.95) setcolor(7);
*/
	/* updated color scheme using Thompson et al. (2012) sample */
        if (ix1 >= .5) setcolor(18);
        if (ix1 >= 1) setcolor(31);
        if (ix1 >= 2) setcolor(19);
        if (ix1 >= 4) setcolor(2);
        if (ix1 >= 8) setcolor(7);
        sprintf( st, "STP (CIN) = %s", qc2( ix1, "", 1 )); 
	outgtext( st, txtlin, txtrow);

        txtrow += 16;
        txtlin = skv.tlx + 230;
        ix4 = sigtorn_fixed(st_dir, st_spd);
        setcolor(8);
	/*original color scheme using T03 sample */
/*      if (ix4 < .95) setcolor(8);
        if (ix4 >= .95) setcolor(31);
        if (ix4 >= 3.95) setcolor(19);
        if (ix4 >= 7.95) setcolor(2);
        if (ix4 >= 11.95) setcolor(7);
*/
	/* updated color scheme using Thompson et al. (2012) sample */
        if (ix4 >= .5) setcolor(18);
        if (ix4 >= 1) setcolor(31);
        if (ix4 >= 2) setcolor(19);
        if (ix4 >= 5) setcolor(2);
        if (ix4 >= 7) setcolor(7);
        sprintf( st, "STP (fixed) = %s", qc2( ix4, "", 1 ));
        outgtext( st, txtlin, txtrow);

	/* requested output for D2D skew-T...RLT 3/30/11 */
	printf("\n***** additions for AWIPS2 D2D skew-T *****\n");
	showalter = i_temp(500.0,I_PRES) - (lifted(850.0, i_temp(850.0,I_PRES), i_dwpt(850.0,I_PRES), 500.0));
        printf("\n showalter index = %0.1f", showalter);
	sweat_index(&ix1);
	printf("\n sweat index = %0.1f", ix1);
	ix1 = bulk_rich(pcl, &ix2);
	printf("\n BRN = %0.1f", ix1);
	ix1 = mtof(agl(i_hght(pcl.mplpres, I_PRES)));
	printf("\n max parcel level (ft AGL) = %0.1f", ix1);
	ix1 = sqrt(pcl.bplus * 2);
	printf("\n max updraft (m/s) = %0.1f\n", ix1);
	printf("\n***** end AWIPS2 additions ******\n"); 
	/* end D2D skew-T additions */

	/* additions for sfcoa 9/21/11 RLT */
	/*printf("\n***** additions for sfcoa *****\n");
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(4000)), &ix1, &ix2, &ix3, &ix4);
	printf("\n 0-4 km bulk wind difference (kt) = %0.1f", ix4);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(5000)), &ix1, &ix2, &ix3, &ix4);
        printf("\n 0-5 km bulk wind difference (kt) = %0.1f", ix4);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(7000)), &ix1, &ix2, &ix3, &ix4);
        printf("\n 0-7 km bulk wind difference (kt) = %0.1f", ix4);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(9000)), &ix1, &ix2, &ix3, &ix4);
        printf("\n 0-9 km bulk wind difference (kt) = %0.1f", ix4);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(10000)), &ix1, &ix2, &ix3, &ix4);
        printf("\n 0-10 km bulk wind difference (kt) = %0.1f", ix4);
        wind_shear(i_pres(msl(3000)), i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        printf("\n 3-6 km AGL bulk shear U comp (kt) = %0.1f", ix1);
        printf("\n 3-6 km AGL bulk shear V comp (kt) = %0.1f", ix2);
        printf("\n 3-6 km AGL bulk wind difference (kt) = %0.1f", ix4);
        wind_shear(i_pres(msl(3000)), i_pres(msl(8000)), &ix1, &ix2, &ix3, &ix4);
        printf("\n 3-8 km AGL bulk shear U comp (kt) = %0.1f", ix1);
        printf("\n 3-8 km AGL bulk shear V comp (kt) = %0.1f", ix2);
        printf("\n 3-8 km AGL bulk wind difference (kt) = %0.1f", ix4);
        define_parcel(3, mu_layer);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        wind_shear(pcl.lclpres, pcl.elpres, &ix1, &ix2, &ix3, &ix4);
        printf("\n cloud layer shear U comp (kt) = %0.1f", ix1);
        printf("\n cloud layer shear V comp (kt) = %0.1f", ix2);
	ix1 = i_wndu(p_top, I_PRES);
	ix2 = i_wndv(p_top, I_PRES);
	printf("\n U comp at top of effective inflow layer (kt) = %0.1f", ix1);
        printf("\n V comp at top of effective inflow layer (kt) = %0.1f", ix2);	
	lapse_rate(&ix4, i_pres(sndg[sfc()][zIndex]+3000), i_pres(sndg[sfc()][zIndex]+6000));
        printf("\n 3-6 km lapse rate (C/km) = %0.1f", ix4);
        lapse_rate(&ix4, i_pres(sndg[sfc()][zIndex]+3000), i_pres(sndg[sfc()][zIndex]+8000));
        printf("\n 3-8 km lapse rate (C/km) = %0.1f", ix4);
	relh(800, &ix4);
	printf("\n 800 mb RH = %0.1f", ix4);
        relh(700, &ix4);
        printf("\n 700 mb RH = %0.1f", ix4);
        relh(600, &ix4);
        printf("\n 600 mb RH = %0.1f", ix4);
	mean_mixratio(&ix1, -1, sndg[sfc()][pIndex]-50.0 );
	mean_mixratio(&ix2, -1, sndg[sfc()][pIndex]-150.0 );
        printf("\n 50 mb mean mixing ratio (g/kg) = %0.1f", ix1);
        printf("\n 150 mb mean mixing ratio (g/kg) = %0.1f\n", ix2);
	printf("\n **** end of sfcoa additions ****\n\n");*/

	/* formatted for jh_calcs 2/10/12 RLT */
	/* bulk wind difference parameters - need to create variables and insert for ix4 */
/*      wind_shear(sndg[sfc()][1], i_pres(msl(4000)), &ix1, &ix2, &ix3, &ix4);
        wind_shear(sndg[sfc()][1], i_pres(msl(5000)), &ix1, &ix2, &ix3, &ix4);
        wind_shear(sndg[sfc()][1], i_pres(msl(7000)), &ix1, &ix2, &ix3, &ix4);
        wind_shear(sndg[sfc()][1], i_pres(msl(9000)), &ix1, &ix2, &ix3, &ix4);
        wind_shear(sndg[sfc()][1], i_pres(msl(10000)), &ix1, &ix2, &ix3, &ix4);
        wind_shear(i_pres(msl(3000)), i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        wind_shear(i_pres(msl(3000)), i_pres(msl(8000)), &ix1, &ix2, &ix3, &ix4);
        /*cloud layer bulk shear - need mu parcel LCL and EL, so find mu parcel part of jh_calcs */ 
/*      define_parcel(3, mu_layer);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        wind_shear(pcl.lclpres, pcl.elpres, &ix1, &ix2, &ix3, &ix4);
        ix1 = i_wndu(p_top, I_PRES);
        ix2 = i_wndv(p_top, I_PRES);
*/	/* lapse rates - need to create lapse rate variables and insert for ix4 */
/*      lapse_rate(&ix4, i_pres(sndg[sfc()][2]+3000), i_pres(sndg[sfc()][2]+6000));
        lapse_rate(&ix4, i_pres(sndg[sfc()][2]+3000), i_pres(sndg[sfc()][2]+8000));
*/	/* RH and mixing ratio - need to create variables and insert for ix4 */
/*      relh(800, &ix4);
        relh(700, &ix4);
        relh(600, &ix4);
        mean_mixratio(&ix4, -1, sndg[sfc()][1]-50.0 );
        mean_mixratio(&ix4, -1, sndg[sfc()][1]-150.0 );
*/
	/* end of sfcoa additions */

	/* parcel routine changes - LCL temp error check RLT 1/6/12 */
        /*define_parcel(3,400);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        printf("\n***** error checks for LCL temp in parcel routine *****\n");
        printf("\n CAPE below 0 C (J/kg) = %0.1f\n", pcl.bfzl);
        printf("\n CAPE below -10 C (J/kg) = %0.1f\n", pcl.wm10c);
        printf("\n CAPE below -20 C (J/kg) = %0.1f\n", pcl.wm20c);
        printf("\n CAPE below -30 C (J/kg) = %0.1f\n", pcl.wm30c); 
        printf("\n **** end of parcel routine error checks  ****\n\n"); */
	/*  end parcel routine error checks */

	txtrow += 16;
	txtlin = skv.tlx + 230;
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &shr6);

	define_parcel(3, 400);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);	
	el = agl(i_hght(pcl.elpres, I_PRES));
/* 24 Mar 2008 */
/*	if (agl(i_hght(pbot, I_PRES)) > 0.0){
		depth = el - agl(i_hght(pbot, I_PRES));
		wind_shear(pbot, i_pres(msl(depth*0.5)), &ix1, &ix2, &ix3, &shr6);
		}
*/
	if (agl(i_hght(p_bot, I_PRES)) > 0.0){
                depth = el - agl(i_hght(p_bot, I_PRES));
                wind_shear(p_bot, i_pres(msl(depth*0.5)), &ix1, &ix2, &ix3, &shr6);
                }

	ship = sig_hail(mucape, mumixr, lr75, i_temp(500, I_PRES), kt_to_mps(shr6), fzlh, mucinh, 0, 0, 25, mlcape);
	if (ship < .45) setcolor(8);
	if (ship >= .45) setcolor(31);
	if (ship >= .95) setcolor(19);
	if (ship >= 1.95) setcolor(2);
	if (ship >= 4.95) setcolor(7);
        sprintf( st, "SHIP = %s", qc2( ship, "", 1));
        outgtext( st, txtlin, txtrow);

        ww_type(&wwtype, &dcp);

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

	}

        /*NP*/
        void show_shear_new( void )
        /*************************************************************/
        /*  SHOW_SHEAR_NEW                                           */
        /*  John Hart and Rich Thompson SPC OUN                      */
        /*                                                           */
	/*  Displays shear parameters in lower middle inset area     */
        /*************************************************************/
        {
        float ix1, ix2, ix3, ix4, sfctemp, sfcdwpt, sfcpres, j1, j2;
	float ix5, ix6, ix7, ix8, lfc, mean_shear_dir, mean_shear_spd;
	float mean_u, mean_v, mean_dir, mean_spd, upper_u, upper_v, upper_dir, upper_spd;
        float nv_cape, nv_cinh, nv_cap, pres, ptop, pbot, mucape, mumixr;
        float lr75, shr6, fzlh, mucinh, ship, jh1, jh2, interp_spd, interp_dir;
	float el, lpl, depth, base, eshear, eshear40, eshear50, eshear60;
        short txtlin, txtrow, oldlplchoice, pIndex, zIndex, tIndex, trow2;
	int debugflag;
        char st[100], st1[20];
        Parcel pcl;
        Parcel pcl2;


        tIndex = getParmIndex("TEMP");
        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");
        tIndex = getParmIndex("TEMP");

        setcliprgn(1,1,xwdth, xhght);

        setlinestyle(1,1);
        /*txtlin = skv.tlx + 368;*/
	txtlin = skv.tlx + 358;
/*	txtlin = skv.tlx + (xwdth*.33);
*/      txtrow = skv.bry + 20;
        setcolor(0);
        rectangle(1, txtlin, txtrow, txtlin + 350, txtrow + 250);
        setcolor(31);
        rectangle(0, txtlin, txtrow, txtlin + 350, txtrow + 250);

        setcolor(31);
        set_font(4);
        txtlin += 5;

        txtrow += 11;
        strcpy(st, "                       SRH(m2/s2)    Shear(kt)       MnWind         SRW");
        outgtext(st, txtlin, txtrow);
        txtrow += 12;
        moveto(txtlin-5, txtrow);
        lineto(txtlin+345, txtrow);

        txtrow += 6;
        setcolor(1);
	jh1 = 0;
	jh2 = 1000;
        strcpy(st, "SFC - 1 km");
        outgtext(st, txtlin, txtrow);
	ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
        strcpy( st, qc2(ix1, "", 0));
        disp_param( st, txtlin+120, txtrow);
/*	wind_shear(i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4); */
        wind_shear( sndg[sfc()][pIndex], i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, "", 0));
        disp_param( st, txtlin+175, txtrow);
	mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        disp_param( st, txtlin+240, txtrow);
	sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        disp_param( st, txtlin+295, txtrow);

        txtrow += 11;
        setcolor(1);
	jh1 = 0;
	jh2 = 2000;
        strcpy(st, "SFC - 2 km");
        outgtext(st, txtlin, txtrow);
	ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
        strcpy( st, qc2(ix1, "", 0));
        disp_param( st, txtlin+120, txtrow);
/*	wind_shear(i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);*/
        wind_shear( sndg[sfc()][pIndex], i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, "", 0));
        disp_param( st, txtlin+175, txtrow);
	mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        disp_param( st, txtlin+240, txtrow);
	sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        disp_param( st, txtlin+295, txtrow);

        txtrow += 11;
        setcolor(1);
	jh1 = 0;
	jh2 = 3000;
        strcpy(st, "SFC - 3 km");
        outgtext(st, txtlin, txtrow);
	ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
        strcpy( st, qc2(ix1, "", 0));
        disp_param( st, txtlin+120, txtrow);
/*	wind_shear(i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4); */
        wind_shear( sndg[sfc()][pIndex], i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, "", 0));
        disp_param( st, txtlin+175, txtrow);
	mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        disp_param( st, txtlin+240, txtrow);
	sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        disp_param( st, txtlin+295, txtrow);

        txtrow += 11;
        setcolor(1);
/* 24 Mar 2008 */
/*        effective_inflow_layer(100,-250, &pbot, &ptop);
        if (pbot > 0) 
           {
           jh1 = agl(i_hght(pbot, I_PRES));
*/

        if (p_bot > 0) 
           {
           jh1 = agl(i_hght(p_bot, I_PRES));
           jh2 = agl(i_hght(p_top, I_PRES));

	   setcolor(18);
           rectangle(0, txtlin - 1, txtrow - 1, txtlin + 312, txtrow + 11);  setcolor(5);
           sprintf(st, "Eff Inflow Layer");
           outgtext(st, txtlin, txtrow);
           ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
           strcpy( st, qc2(ix1, "", 0));
           disp_param( st, txtlin+120, txtrow);
           wind_shear(i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           strcpy( st, qc2(ix4, "", 0));
           disp_param( st, txtlin+175, txtrow);
           mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
           disp_param( st, txtlin+240, txtrow);
           sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
           disp_param( st, txtlin+295, txtrow);
           }

        txtrow += 18;
        setcolor(1);
	jh1 = 0;
	jh2 = 6000;
        strcpy(st, "SFC - 6 km");
        outgtext(st, txtlin, txtrow);
	ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
        strcpy( st, qc2(ix1, "", 0));
/*	wind_shear(i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4); */
        wind_shear( sndg[sfc()][pIndex], i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, "", 0)); 
        disp_param( st, txtlin+175, txtrow);
        setcolor(1);
	mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        disp_param( st, txtlin+240, txtrow);
	sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        disp_param( st, txtlin+295, txtrow);

        txtrow += 11;
        setcolor(1);
        jh1 = 0;
        jh2 = 8000;
        strcpy(st, "SFC - 8 km");
        outgtext(st, txtlin, txtrow);
        ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
        strcpy( st, qc2(ix1, "", 0));
/*      wind_shear(i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4); */
        wind_shear( sndg[sfc()][pIndex], i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, "", 0));
        disp_param( st, txtlin+175, txtrow);
        setcolor(1);
        mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        disp_param( st, txtlin+240, txtrow);
        sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
        disp_param( st, txtlin+295, txtrow);

        txtrow += 11;
        setcolor(1);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        if (pcl.bplus > 0)
           {
           jh1 = agl(i_hght(pcl.lclpres, I_PRES));
           jh2 = agl(i_hght(pcl.elpres, I_PRES));
           sprintf(st, "LCL - EL (Cloud Layer)");
           outgtext(st, txtlin, txtrow);
           ix1 = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
           strcpy( st, qc2(ix1, "", 0));
           wind_shear(i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           strcpy( st, qc2(ix4, "", 0));
           disp_param( st, txtlin+175, txtrow);
           mean_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
           disp_param( st, txtlin+240, txtrow);
           sr_wind( i_pres(msl(jh1)), i_pres(msl(jh2)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                  sprintf( st, "%4.0f/%.0f", ix3, ix4);
           disp_param( st, txtlin+295, txtrow);
           }

        txtrow += 11;
        setcolor(1);

        oldlplchoice = lplvals.flag;
        define_parcel(3, 400);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        el = agl(i_hght(pcl.elpres, I_PRES));
        if (pcl.bplus >= 100.0)
           {
/* 24 Mar 2008 */
/*           effective_inflow_layer(100, -250, &pbot, &ptop);*/
/* all "pbot" below changed to "p_bot" global variable */
           base = agl(i_hght(p_bot, I_PRES));
           if (oldlplchoice == 3) { rectangle(0, txtlin - 1, txtrow - 1, txtlin + 312, txtrow + 11);  setcolor(5);}
           setcolor(18);
           rectangle(0, txtlin - 1, txtrow - 1, txtlin + 312, txtrow + 11);  setcolor(5);
           sprintf(st, "Lower Half Storm Depth", 37);
           outgtext(st, txtlin, txtrow);
           ix1 = helicity(p_bot, p_top, st_dir, st_spd, &ix2, &ix3);
           strcpy( st, qc2(ix1, "", 0));
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

           strcpy( st, qc2(eshear, "", 0));
           setcolor(5);
           disp_param( st, txtlin+175, txtrow);
           mean_wind( i_pres(msl(base)), i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
           disp_param( st, txtlin+240, txtrow);
           sr_wind( i_pres(msl(base)), i_pres(msl(base + (depth * 0.5))), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "%4.0f/%.0f", ix3, ix4);
           disp_param( st, txtlin+295, txtrow);

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


           }

        txtrow += 18;
        setcolor(1);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	bulk_rich( pcl, &ix1 );
        sprintf( st, "BRN Shear =  %.0f m%c/s%c", ix1, 178, 178 );
        outgtext(st, txtlin, txtrow);

        txtrow += 11;
        setcolor(1);
	sr_wind( i_pres(msl(4000)), i_pres(msl(6000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	strcpy( st, "4-6km SR Wind =");
        outgtext(st, txtlin, txtrow);
        sprintf( st, "%4.0f/%.0f kt", ix3, ix4 );
        disp_param(st, txtlin + 150, txtrow);

	setcolor(2);
        /*if (i_wspd(i_pres(msl(1000)), I_PRES) > 0)*/
	interp_spd = speed(i_wndu(i_pres(msl(1000)), I_PRES), i_wndv(i_pres(msl(1000)), I_PRES));
	interp_dir = angle(i_wndu(i_pres(msl(1000)), I_PRES), i_wndv(i_pres(msl(1000)), I_PRES));
	if ((interp_spd > 0) && (interp_dir > 0)){
		wind_barb(interp_dir, interp_spd, txtlin + 280, txtrow + 40, 7);
		}
	   /*wind_barb(i_wdir(i_pres(msl(1000)), I_PRES), i_wspd(i_pres(msl(1000)), I_PRES), txtlin + 280, txtrow + 40, 7);*/
	setcolor(25);
	/*if (i_wspd(i_pres(msl(6000)), I_PRES) > 0)*/ 
        interp_spd = speed(i_wndu(i_pres(msl(6000)), I_PRES), i_wndv(i_pres(msl(6000)), I_PRES));
        interp_dir = angle(i_wndu(i_pres(msl(6000)), I_PRES), i_wndv(i_pres(msl(6000)), I_PRES));
        if ((interp_spd > 0) && (interp_dir > 0)){	
        	wind_barb(interp_dir, interp_spd, txtlin + 280, txtrow + 40, 7);
		}
	   /*wind_barb(i_wdir(i_pres(msl(6000)), I_PRES), i_wspd(i_pres(msl(6000)), I_PRES), txtlin + 280, txtrow + 40, 7);*/
	strcpy( st, "1km & 6km AGL Wind Barbs");
	setcolor(1);
        outgtext(st, txtlin+200, txtrow+95);

        txtrow += 18;
        setcolor(1);
	corfidi_MCS_motion(&ix1, &ix2, &ix3, &ix4, &ix5, &ix6, &ix7, &ix8);
	strcpy( st, "Corfidi Downshear =");
        outgtext(st, txtlin, txtrow);
        sprintf( st, "%4.0f/%.0f kt", ix3, ix4 );
        disp_param(st, txtlin + 150, txtrow);

        txtrow += 11;
        setcolor(1);
	strcpy( st, "Corfidi Upshear =");
        outgtext(st, txtlin, txtrow);
        sprintf( st, "%4.0f/%.0f kt", ix7, ix8 );
        disp_param(st, txtlin + 150, txtrow);

        txtrow += 11;
        setcolor(12);
	bunkers_storm_motion(&ix1, &ix2, &ix3, &ix4); 
	strcpy( st, "Bunkers Right =");
        outgtext(st, txtlin, txtrow);
        sprintf( st, "%4.0f/%.0f kt", st_dir, st_spd);
        disp_param(st, txtlin + 150, txtrow);

        txtrow += 11;
        setcolor(26);
        bunkers_left_motion(&ix1, &ix2, &ix3, &ix4);
        strcpy( st, "Bunkers Left =");
        outgtext(st, txtlin, txtrow); 
        sprintf( st, "%4.0f/%.0f kt", ix3, ix4);
        disp_param(st, txtlin + 150, txtrow);

	/* GLD and JPR flow orientation below lfc */
	/*{	
	define_parcel(4, 100);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	lfc = agl(i_hght(pcl.lfcpres, I_PRES));
	wind_shear(pcl.lplpres, pcl.lfcpres, &ix1, &ix2, &ix3, &ix4);
	mean_shear_dir = ix3;
	mean_shear_spd = ix4;
	mean_wind(pcl.lplpres, pcl.lfcpres, &ix1, &ix2, &ix3, &ix4);
	mean_u = ix1;
	mean_v = ix2;
	mean_dir = ix3;
	mean_spd = ix4;
	mean_wind(i_pres(msl(lfc/2)), pcl.lfcpres, &ix1, &ix2, &ix3, &ix4);
	upper_u = ix1;
	upper_v = ix2;
	upper_dir = ix3;
	upper_spd = ix4;
	printf("\n ****** GLD and JPR boundary stuff ******");
	printf("\n 100 mb mean parcel LFC (m AGL) = %0.1f", lfc);
	printf("\n mean DIR sfc to lfc shear = %0.1f", mean_shear_dir);
        printf("\n mean SPD sfc to lfc shear = %0.1f", mean_shear_spd); 
        printf("\n DIR (mean) sfc to lfc = %0.1f", mean_dir);
        printf("\n SPD (mean) sfc to lfc = %0.1f", mean_spd);
        printf("\n DIR (upper half) sfc to lfc = %0.1f", upper_dir);
        printf("\n SPD (upper half) sfc to lfc = %0.1f", upper_spd);
	printf("\n ****** end boundary stuff ******\n\n");
	}*/

	/* BLEP technique calculations */
/*        txtrow += 22;
        setcolor(26);
	debugflag = 1;
	blep_technique(&ix1, &ix2);
        strcpy( st, "BLEP Straightline =");
        outgtext(st, txtlin, txtrow); 
        sprintf( st, "%4.0f kt", ix1 );
        disp_param(st, txtlin + 150, txtrow);
        txtrow += 11;
        strcpy( st, "BLEP Rotational =");
        outgtext(st, txtlin, txtrow); 
        sprintf( st, "%4.0f kt", ix2 );
        disp_param(st, txtlin + 150, txtrow);
*/	
	
	/* test STP using cape6km */
        txtrow += 44;
        ix1 = sigtorn_test(st_dir, st_spd);
        setcolor(8);
        if (ix1 < .45) setcolor(8);
        if (ix1 >= .45) setcolor(31);
        if (ix1 >= 1.95) setcolor(19);
        if (ix1 >= 3.95) setcolor(2);
        if (ix1 >= 5.95) setcolor(7);
        sprintf( st, "STPC (test) = %s", qc2( ix1, "", 1 ));
        outgtext( st, txtlin, txtrow);

	/* Tropical Cyclone Tornado Parameter test */
/*	ix1 = sigtorn_tc(st_dir, st_spd);
        setcolor(8);
        if (ix1 < .45) setcolor(8);
        if (ix1 >= .45) setcolor(31);
        if (ix1 >= 1.95) setcolor(19);
        if (ix1 >= 3.95) setcolor(2);
        if (ix1 >= 5.95) setcolor(7);
        sprintf( st, "TCTP = %s", qc2( ix1, "", 1 ));
        outgtext( st, txtlin + 100, txtrow);
*/
	}

        /*NP*/
        void show_winter_new( void )
        /*************************************************************/
        /*  SHOW_WINTER_NEW                                          */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*************************************************************/
        {
        float ix1, ix2, ix3, ix4, sfctemp, sfcdwpt, sfcpres, j1, j2;
        float ix5, ix6, ix7, ix8, pose, nege; 
	float ptop, pbot, htop, hbot, mrh, mq, mo, pw, mopw;
        float lr75, shr6, fzlh, mucinh, ship, jh1, jh2, pres;
        short txtlin, txtrow, oldlplchoice, pIndex, zIndex, tIndex, trow2;
	short phase;
        char st[100], st1[100];
        Parcel pcl;
        Parcel pcl2;

        tIndex = getParmIndex("TEMP");
        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");
        tIndex = getParmIndex("TEMP");

        /* Do dendritic growth zone calcs */
        ptop = temp_lvl(-17, &ix1);
        pbot = temp_lvl(-12, &ix1);
        if (ptop < 0) ptop = sndg[sfc()][pIndex];
        if (pbot < 0) pbot = sndg[sfc()][pIndex];
        htop = i_hght(ptop, I_PRES);
        hbot = i_hght(pbot, I_PRES);
        mrh = mean_relhum(&ix1, pbot, ptop);
        mq = mean_mixratio(&ix1, pbot, ptop);
        mo = mean_omeg(&ix1, pbot, ptop) * 1000;
        pw = precip_water(&ix1, pbot, ptop);
        mopw = (mo * pw) * mrh / 100;


        setcliprgn(1,1,xwdth, xhght);

	/* ----- Draw box around parameter area ----- */
        setlinestyle(1,1);
        /*txtlin = skv.tlx + 731;*/
	if (display_mode_right == DISPLAY_WINTER_RIGHT)
		{txtlin = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_WINTER_LEFT)
                {txtlin = skv.tlx + 712;}
        if (display_mode_left == DISPLAY_WINTER_LEFT && display_mode_right == DISPLAY_WINTER_RIGHT)
                {
                setcolor(5);
                set_font(6);
                sprintf( st, "Please choose another inset");
                outgtext ( st, txtlin +394, txtrow + 115);
		}	
        txtrow = skv.bry + 20;
        setcolor(0);
        rectangle(1, txtlin, txtrow, txtlin + 350, txtrow + 250);
        setcolor(31);
        rectangle(0, txtlin, txtrow, txtlin + 350, txtrow + 250);

        /* ----- Dendritic Zone Data ----- */
        setcolor(5);
        set_font(4);
        txtlin += 5;
        txtrow += 11;
        strcpy(st, " * * * DENDRITIC GROWTH ZONE (-12 to -17C) * * *");
	ix1 = (350 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);

        txtrow += 15;
	if (mopw < -.1) setcolor(13); else setcolor(31);
	sprintf(st, "OPRH (Omega*PW*RH):  %s", qc2(mopw, "", 2));
	ix1 = (350 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);

        set_font(4);
	setcolor(31);
        txtrow += 20;

	strcpy(st, "Layer Depth:");
        outgtext(st, txtlin, txtrow);
	sprintf( st, "%.0f ft (%.0f - %.0f ft msl)", mtof(htop-hbot), mtof(hbot), mtof(htop));
        outgtext(st, txtlin+100, txtrow);

        txtrow += 14;
	strcpy(st, "Mean Layer RH:");
        outgtext(st, txtlin, txtrow);
        strcpy( st, qc2(mrh, " %", 0));
        disp_param( st, txtlin + 150, txtrow);

	strcpy(st, "Mean Layer MixRat:");
        outgtext(st, txtlin+165, txtrow);
        strcpy( st, qc2(mq, " g/kg", 1));
        disp_param( st, txtlin + 315, txtrow);

        txtrow += 14;
	strcpy(st, "Mean Layer PW:");
        outgtext(st, txtlin, txtrow);
        strcpy( st, qc2(pw, " in.", 2));
        disp_param( st, txtlin + 150, txtrow);

	strcpy(st, "Mean Layer Omega:");
        outgtext(st, txtlin+165, txtrow);
        strcpy( st, qc2(mo, " ub/s", 0));
        disp_param( st, txtlin + 315, txtrow);

        txtrow += 14;
        moveto(txtlin, txtrow);
        lineto(txtlin+320, txtrow);
        moveto(txtlin+160, txtrow);
        lineto(txtlin+160, txtrow-28);

	/* ----- Initial Phase of Precip ----- */
        txtrow += 8;
        set_font(4);
        setcolor(31);
        strcpy(st, init_phase(&ix1, &phase));

/*	printf( "Writing Phase information\n"); */

        if (ix1>100)
                {
                sprintf( st1, "Initial Phase:    %s from: %.0fmb (%.0f ft msl ; %.1f C)", st, ix1, mtof(i_hght(ix1, I_PRES)), i_temp(ix1, I_PRES));
                outgtext( st1, txtlin, txtrow);
		printf( "%s\n", st1);
                } 
        else
                {
                sprintf( st, "Initial Phase:  No Precipitation layers found.");
                outgtext( st, txtlin, txtrow);
                }

        txtrow += 15;
        moveto(txtlin, txtrow);
        lineto(txtlin+320, txtrow);

	/* ----- Temperature Pos/Neg Areas ----- */
	posneg_temperature(-1, &pose, &nege, &ptop, &pbot);
        txtrow += 8;
        set_font(4);
        setcolor(31);
	strcpy(st, "TEMPERATURE PROFILE");
	outgtext(st, txtlin, txtrow);
        txtrow += 14;

        if ((pose > 0) && (nege < 0)) {
          sprintf( st, "Pos= %.0f J/kg    Neg=%.0f J/kg", pose, nege);
          outgtext(st, txtlin, txtrow);
          txtrow += 14;
          ix1 = mtof(i_hght(ptop, I_PRES)-i_hght(pbot, I_PRES));
          sprintf(st, "Melt Lyr:  %3.0f - %3.0fmb (%.0f ft)", ptop, pbot, ix1);
          outgtext(st, txtlin, txtrow);
          txtrow += 14;
          ix2 = sndg[sfc()][pIndex];
          ix1 = mtof(i_hght(pbot, I_PRES)-i_hght(ix2, I_PRES));
          sprintf(st, "Frz Lyr: %3.0f - %4.0fmb (%.0f ft)", pbot, ix2, ix1);
          outgtext(st, txtlin, txtrow);
        }
        else { 
          txtrow += 14;
	  outgtext("Warm/Cold layers not found.", txtlin, txtrow); 
          txtrow += 14;
	}

        /* ----- WetBulb Pos/Neg Areas ----- */
	posneg_wetbulb(-1, &pose, &nege, &ptop, &pbot);
        txtrow -= 42;
	txtlin += 165;
        set_font(4);
        setcolor(31);
        strcpy(st, "WETBULB PROFILE");
        outgtext(st, txtlin, txtrow);
        txtrow += 14;

        if ((pose > 0) && (nege < 0)) {
          sprintf( st, "Pos= %.0f J/kg    Neg=%.0f J/kg", pose, nege);
          outgtext(st, txtlin, txtrow);
          txtrow += 14;
          ix1 = mtof(i_hght(ptop, I_PRES)-i_hght(pbot, I_PRES));
          sprintf(st, "Melt Lyr:  %3.0f - %3.0fmb (%.0f ft)", ptop, pbot, ix1);
          outgtext(st, txtlin, txtrow);
          txtrow += 14;
          ix2 = sndg[sfc()][pIndex];
          ix1 = mtof(i_hght(pbot, I_PRES)-i_hght(ix2, I_PRES));
          sprintf(st, "Frz Lyr: %3.0f - %4.0fmb (%.0f ft)", pbot, ix2, ix1);
          outgtext(st, txtlin, txtrow);
        }
        else {
          txtrow += 14;
          outgtext("Warm/Cold layers not found.", txtlin, txtrow);
          txtrow += 14;
        }

        txtrow += 14;
	txtlin -= 165;
        moveto(txtlin, txtrow);
        lineto(txtlin+320, txtrow);
        moveto(txtlin+160, txtrow);
        lineto(txtlin+160, txtrow-56);

        /* ----- Best Guess Precip Type ----- */
        txtrow += 8;
        set_font(4);
        setcolor(31);
        strcpy(st, "* * * BEST GUESS PRECIP TYPE * * *");
        ix1 = (350 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);

        txtrow += 18;
	best_guess_ptype(txtlin, txtrow);

	}

        /*NP*/
        void show_hail_new(float *h2)
        /*************************************************************/
        /*  SHOW_TIMELINE                                            */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*************************************************************/
        {
        float ix1, ix2, ix3, ix4, sfctemp, sfcdwpt, sfcpres, j1, j2;
        float nv_cape, nv_cinh, nv_cap, pres, ptop, pbot, mucape, mumixr, mlcape;
        float lr75, shr3, shr6, shr9, fzlh, mucinh, ship, esi2, t500, el, depth;
        float srh3, matches, matches2, avsize, p1, haillist[15];
        short txtlin, txtrow, oldlplchoice, pIndex, zIndex, tIndex, trow2, i, j;
        short tdIndex, nsndgs, trx, temp_mark, y;
        char st[100], st1[20], sndglist[15][15];
        Parcel pcl;
        Parcel pcl2;

        /* added 25OCT06 by RLT */

        tIndex = getParmIndex("TEMP");
        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");
        tdIndex = getParmIndex("DWPT");

	oldlplchoice = lplvals.flag;

        /* plot height of freezing level, -20C, and -30C for Donavon hail technique */
        set_font(4);
        setlinestyle(1, 2);
        temp_mark = pres_to_pix(temp_lvl(0, &ix1 ));
        setcolor(0);
        rectangle(1, skv.tlx + (xwdth*.38), temp_mark - 13, skv.tlx + (xwdth*.38) + 72, temp_mark - 1);
        setcolor(26);
        moveto(skv.tlx + (xwdth*.38), temp_mark);
        lineto(skv.tlx + (xwdth*.38) + 25, temp_mark);
        sprintf( st, "FZL = %s", qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES))), "'", 0 ));
        outgtext(st, skv.tlx + (xwdth*.38), temp_mark - 12);

        temp_mark = pres_to_pix(temp_lvl(-20, &ix1 ));
        setcolor(0);
        rectangle(1, skv.tlx + (xwdth*.38), temp_mark - 13, skv.tlx + (xwdth*.38) + 72, temp_mark - 1);
        setcolor(26);
        moveto(skv.tlx + (xwdth*.38), temp_mark);
        lineto(skv.tlx + (xwdth*.38) + 25, temp_mark);
        sprintf( st, "-20C = %s", qc2( mtof(agl(i_hght(temp_lvl( -20, &ix1 ), I_PRES))), "'", 0 ));
        outgtext(st, skv.tlx + (xwdth*.38), temp_mark - 12);

        temp_mark = pres_to_pix(temp_lvl(-30, &ix1 ));
        setcolor(0);
        rectangle(1, skv.tlx + (xwdth*.38), temp_mark - 13, skv.tlx + (xwdth*.38) + 72, temp_mark - 1);
        setcolor(26);
        moveto(skv.tlx + (xwdth*.38), temp_mark);
        lineto(skv.tlx + (xwdth*.38) + 25, temp_mark);
        sprintf( st, "-30C = %s", qc2( mtof(agl(i_hght(temp_lvl( -30, &ix1 ), I_PRES))), "'", 0 ));
        outgtext(st, skv.tlx + (xwdth*.38), temp_mark - 12);


	define_parcel(4, 100);
	mlcape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);

        define_parcel(3, mu_layer);
        sfctemp = lplvals.temp;
        sfcdwpt = lplvals.dwpt;
        sfcpres = lplvals.pres;

        /* ----- Calculate Parcel Data ----- */ 
        ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);


        /*printf( "SHOW_HAIL:Enter\n" );*/

        setcliprgn(1,1,xwdth, xhght);

	/* ----- Draw Bounding Box ----- */
        setlinestyle(1,1);
        /*txtlin = skv.tlx + 731;*/
	if (display_mode_right == DISPLAY_HAIL_RIGHT)
		{txtlin = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_HAIL_LEFT)
                {txtlin = skv.tlx + 712;}
        txtrow = skv.bry + 20;
        setcolor(0);
        rectangle(1, txtlin, txtrow, txtlin + 350, txtrow + 250);
        setcolor(31);
        rectangle(0, txtlin, txtrow, txtlin + 350, txtrow + 250);

	txtlin += 5;
        txtrow += 5;

        /* ----- Hail Model Output ----- */
        setcolor(5);
        set_font(4);
	strcpy( st, "* * * HAILCAST HAIL MODEL - 4/21/10 * * *" );
        ix1 = (350 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);

        txtrow += 15;

        set_font(4);
        setcolor(31);
        sprintf(st, "Hailcast1 --> (%.0f convecting)    T/Td= %.0fF/%.0fF    Storm Cat: %.0f of 4", h2[18], ctof(h2[2]), ctof(h2[3]),h2[25]);
        outgtext ( st, txtlin, txtrow );

        if (h2[24] >= 1.00 && h2[18] >= 1) setcolor(3);
        if (h2[24] >= 1.95) setcolor(2);

        txtrow += 15;
        sprintf(st, "Avg: %.1f in.     Max: %.1f in.     Min: %.1f in.     SIG =  %.0f     SVR =  %.0f      ", h2[19], h2[20],h2[21],h2[22], h2[23]);
        outgtext ( st, txtlin, txtrow );

        txtrow +=20;
        set_font(4);
        setcolor(31);
        if(h2[4] == 0) setcolor(31); 
        sprintf(st, "Hailcast2 --> (%.0f convecting)    T/Td= %.0fF/%.0fF    Storm Cat: %.0f of 4", h2[4], ctof(h2[2]), ctof(h2[3]),h2[17]);
        outgtext ( st, txtlin, txtrow );

        if (h2[15] >= 1.00 && h2[4] >= 1) setcolor(3);
        if (h2[15] >= 1.95) setcolor(2);

        if(h2[4] == 0) h2[15] = 0;
        sprintf(st, "Avg: %.1f in.     Max: %.1f in.     Min: %.1f in.     SIG =  %.0f     SVR =  %.0f      ", h2[5], h2[6],h2[7],h2[8], h2[9]);
        txtrow += 14;
        outgtext ( st, txtlin, txtrow );



        txtrow += 15;
        setcolor(31);
        moveto(txtlin, txtrow);
        lineto(txtlin+340, txtrow);



        setcolor(31);
        set_font(6);
        if (h2[4] == 0 && h2[18] == 0) {
        sprintf(st, "No Convecting Members");
        txtrow += 6;
        ix1 = (350 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);
        }else{
/* If convecting members then...........*/
        txtrow +=4;
        if (h2[24] < 1.00) setcolor(31);
        if (h2[24] >= 1.00 && h2[18] >= 1) setcolor(3);
        if (h2[24] >= 1.95) setcolor(2);
        sprintf(st, "Hailcast1--->   %.1f", h2[24]);
        ix1 = (350 - getgtextextent(st))/2; 
        outgtext(st, txtlin + ix1 - 85, txtrow);

        if (h2[15] < 1.00) setcolor(31);
        if (h2[15] >= 1.00 && h2[4] >= 1) setcolor(3);
        if (h2[15] >= 1.95) setcolor(2);
        sprintf(st, "Hailcast2--->   %.1f",h2[15]);
        ix1 = (350 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 + 70, txtrow);

        }  

        txtrow += 18;
	setcolor(31);
        moveto(txtlin, txtrow);
        lineto(txtlin+340, txtrow);

        /* ----- Parameters ----- */
        /* removing parameter plot as part of SARS + hail model revamp 1/28/10 */
/*	set_font(4);
        setcolor(31);
        txtrow += 19;
	trx = txtrow;
        strcpy(st, "MUCAPE:");
        outgtext ( st, txtlin, txtrow );
        strcpy( st, qc2( pcl.bplus, " j/kg", 0)); disp_param( st, txtlin + 158, txtrow);

        txtrow += 14;
        strcpy(st, "700-500 LR:");
        outgtext ( st, txtlin, txtrow );
        lr75 = lapse_rate(&ix1, 700, 500);
        strcpy( st, qc2( lr75, " C/km", 1)); disp_param( st, txtlin + 158, txtrow);

        txtrow += 14;
        strcpy(st, "0-6km Shear:");
        outgtext ( st, txtlin, txtrow );
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        shr6 = ix4;
        strcpy( st, qc2( shr6, " kt", 0)); disp_param( st, txtlin + 158, txtrow);

        wind_shear(sndg[sfc()][pIndex], i_pres(msl(9000)), &ix1, &ix2, &ix3, &ix4);
        shr9 = ix4;
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(3000)), &ix1, &ix2, &ix3, &ix4);
        shr3 = ix4;

        txtrow += 14;
        strcpy(st, "MU MixRatio:");
        outgtext ( st, txtlin, txtrow );
        mumixr = mixratio(lplvals.pres, lplvals.dwpt);
        strcpy( st, qc2( mumixr, " g/kg", 1)); disp_param( st, txtlin + 158, txtrow);

        txtrow += 14;
        strcpy(st, "FRZ LVL:");
        outgtext ( st, txtlin, txtrow );
        fzlh = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));
        strcpy( st, qc2( fzlh, " ft", 0)); disp_param( st, txtlin + 158, txtrow);

        txtrow += 14;
        strcpy(st, "Supercell:");
        outgtext ( st, txtlin, txtrow );
        strcpy( st, qc2( scp(st_dir, st_spd), "", 1)); disp_param( st, txtlin + 158, txtrow);

        txtrow += 14;
        strcpy(st, "CapeShear/6km:");
        outgtext ( st, txtlin, txtrow );
        esi2 = kt_to_mps(shr6) * pcl.bplus / 6000;
        strcpy( st, qc2( esi2, "", 1)); disp_param( st, txtlin + 158, txtrow);

        txtrow += 14;
        strcpy(st, "500mb Temp:");
        outgtext ( st, txtlin, txtrow );
        t500 =  i_temp(500, I_PRES);
        strcpy( st, qc2( t500, " C", 1)); disp_param( st, txtlin + 158, txtrow);

        txtrow += 14;
        strcpy(st, "H5T-SfcTd:");
        outgtext ( st, txtlin, txtrow );
        strcpy( st, qc2( t500 - sndg[sfc()][tdIndex], " C", 1)); disp_param( st, txtlin + 158, txtrow);

        txtrow += 14;
        moveto(txtlin+160, txtrow);
        lineto(txtlin+160, txtrow-140);
*/

        /* Compute SARS Data */
        define_parcel(4, 100);
        mlcape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        define_parcel(3, 400);
        mucape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mumixr = mixratio(lplvals.pres, lplvals.dwpt);	
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4); shr6 = kt_to_mps(ix4);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(9000)), &ix1, &ix2, &ix3, &ix4); shr9 = kt_to_mps(ix4);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(3000)), &ix1, &ix2, &ix3, &ix4); shr3 = kt_to_mps(ix4);
 
          if (agl(i_hght(p_bot, I_PRES)) > 0){
                wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.25)), &ix1, &ix2, &ix3, &ix4);  shr3 = kt_to_mps(ix4);
                wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.5)), &ix1, &ix2, &ix3, &ix4);  shr6 = kt_to_mps(ix4);
                wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.75)), &ix1, &ix2, &ix3, &ix4);  shr9 = kt_to_mps(ix4);
                }
 
        t500 =  i_temp(500, I_PRES);
        lr75 = lapse_rate(&ix1, 700, 500);	
        fzlh = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));
        ship = sig_hail(pcl.bplus, mumixr, lr75, t500, kt_to_mps(shr6), fzlh, pcl.bminus, 0, 0, 25, mlcape);
        srh3 = helicity(0, 3000, st_dir, st_spd, &ix2, &ix3);
           if (agl(i_hght(p_bot, I_PRES)) > 0){
                srh3 = helicity(p_bot, p_top, st_dir, st_spd, &ix2, &ix3);
                }

/*        printf("Beginning SARS\n");
        printf("mumixr = %f\n", mumixr);
        printf("mucape = %f\n", pcl.bplus);
        printf("500mb T= %f\n", t500);
        printf("7-5LR  = %f\n", lr75);
        printf("6km Shr= %f\n", kt_to_mps(shr6));
        printf("9km Shr= %f\n", kt_to_mps(shr9));
        printf("3km Shr= %f\n", kt_to_mps(shr3));
        printf("SHIP   = %f\n", ship);
        printf("3km SRH= %f\n", srh3);
*/

        sars(&mumixr, &mucape, &t500, &lr75, &shr6, &shr9, &shr3, &ship, &srh3, &nsndgs, &matches, &p1, &avsize, &matches2, sndglist, &haillist, &sars_filename);
 
	for (i=0; i < 15; i++) sndglist[i][14] = '\0';
        printf( "%d High Quality HAIL Matches were found.\n", nsndgs);
        for (i=0;i<nsndgs;i++) { printf( "HAIL match = %s  %.2f\n", sndglist[i], haillist[i]); }
        printf( "%.0f Total matches were found.\n", matches);
        printf( "%.0f Percent were SIG HAIL.\n", p1);

        /* ----- SARS matches ----- */
/*        set_font(4);
        setcolor(31);
        txtrow += 18;
        if (nsndgs>0)
           { txtrow += 3; }
        else
           {
           sprintf( st, "No Quality HAIL Matches");
           outgtext ( st, txtlin+195, txtrow + 70);
           txtrow += 3;
           }
 
        for (i=0;i<nsndgs;i++)
           {
           if (nsndgs > 10) nsndgs = 10;
           setcolor(5);
           txtrow += 14;
           j = haillist[i];
           if (j<2) {setcolor(18);}
           if (j<1) {setcolor(8);}
           sprintf( st, "%s", sndglist[i]);
           outgtext ( st, txtlin+180, txtrow-20 );
           sprintf( st, "%.2f", haillist[i]);
           outgtext ( st, txtlin+280, txtrow-20 );
           }
*/


        /* ----- SARS matches ----- */
        /*  SARS hail size */
        txtrow += 6;
        set_font(4);
        setcolor(5);
        strcpy( st, "* * * SARS HAIL SIZE * * *" );
        ix1 = (350 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);

        txtrow += 15;
        set_font(6);
        if (matches2 == 0) {
                setcolor(31);
                sprintf(st, "No Matches");
		ix1 = (350 - getgtextextent(st))/2;	
                outgtext ( st, txtlin + ix1 - 5, txtrow );
                }
        if (matches2 == 1 || avsize <= 1.49) setcolor(31);
        if (matches2 >= 2 && (avsize < 2.06 && avsize > 1.49)) setcolor(3);
        if (matches2 >= 2 && avsize >= 2.06) setcolor(2);
	if (matches2 >= 1) {
		set_font(6);	
		if (avsize <= 1.49) {
	                sprintf(st, "Best guess from SARS = < 1 inch");
        	        ix1 = (350 - getgtextextent(st))/2;
                	outgtext ( st, txtlin + ix1 - 5, txtrow );
			}
                if ((avsize > 1.49) && (avsize <= 1.68)) {
                        sprintf(st, "Best guess from SARS = 1 - 1.5 inch");
                        ix1 = (350 - getgtextextent(st))/2;
                        outgtext ( st, txtlin + ix1 - 5, txtrow );
                        } 
                if ((avsize > 1.68) && (avsize <= 2.06)) {
                        sprintf(st, "Best guess from SARS = 1.75 inch");
                        ix1 = (350 - getgtextextent(st))/2;
                        outgtext ( st, txtlin + ix1 - 5, txtrow );
                        } 
                if ((avsize > 2.06) && (avsize <= 2.39)) {
                        sprintf(st, "Best guess from SARS = 2 inch");
                        ix1 = (350 - getgtextextent(st))/2;
                        outgtext ( st, txtlin + ix1 - 5, txtrow );
                        } 
                if ((avsize > 2.39) && (avsize <= 2.52)) {
                        sprintf(st, "Best guess from SARS = 2.5 inch");
                        ix1 = (350 - getgtextextent(st))/2;
                        outgtext ( st, txtlin + ix1 - 5, txtrow );
                        } 
                if ((avsize > 2.52) && (avsize <= 2.56)) {
                        sprintf(st, "Best guess from SARS = 2.75 inch");
                        ix1 = (350 - getgtextextent(st))/2;
                        outgtext ( st, txtlin + ix1 - 5, txtrow );
                        } 
                if ((avsize > 2.56) && (avsize <= 2.64)) {
                        sprintf(st, "Best guess from SARS = 3 - 4 inch");
                        ix1 = (350 - getgtextextent(st))/2;
                        outgtext ( st, txtlin + ix1 - 5, txtrow );
                        } 
                if (avsize > 2.64) {
                        sprintf(st, "Best guess from SARS = > 4 inch");
                        ix1 = (350 - getgtextextent(st))/2;
                        outgtext ( st, txtlin + ix1 - 5, txtrow );
                        } 
		txtrow += 18;
		set_font(4);
        	sprintf(st, "AVG size = %.2f (based on %.0f matches)", avsize, matches2);
        	ix1 = (350 - getgtextextent(st))/2;
        	outgtext(st, txtlin + ix1 - 5, txtrow);
		/*txtrow += 20;
		set_font(4);
		setcolor(31);
		sprintf(st, "(based on %.0f matches)", matches2);
        	ix1 = (350 - getgtextextent(st))/2;
        	outgtext(st, txtlin + ix1 - 5, txtrow);*/
	      }	

        txtrow += 18;
        setcolor(31);
        moveto(txtlin, txtrow);
        lineto(txtlin+340, txtrow);
	if (matches2 > 0) {
        	txtrow += 7;
        	set_font(4);
        	setcolor(31);
        	strcpy( st, "SARS output ranges for reported sizes (white)");
        	ix1 = (350 - getgtextextent(st))/2;
        	outgtext(st, txtlin + ix1 - 5, txtrow);

        	txtrow += 18;
/*        	setcolor(31);
		set_font(4);
        	strcpy(st, "<1");
        	outgtext(st, txtlin + 60, txtrow);
        	strcpy(st, "1-1.5");
        	outgtext(st, txtlin + 95, txtrow);
        	strcpy(st, "1.75");
        	outgtext(st, txtlin + 130, txtrow);
        	strcpy(st, "2");
        	outgtext(st, txtlin + 165, txtrow);
        	strcpy(st, "2.5");
        	outgtext(st, txtlin + 200, txtrow);
        	strcpy(st, "2.75");
        	outgtext(st, txtlin + 235, txtrow);
        	strcpy(st, "3-4");
        	outgtext(st, txtlin + 270, txtrow);
        	strcpy(st, ">4");
        	outgtext(st, txtlin + 305, txtrow);
*/		
		/* SARS for reported < 1" hail */
                if (avsize <= 1.49) {
			setcolor(31);
			set_font(6);
			strcpy(st, "<1");
               		outgtext(st, txtlin + 60, txtrow);
                        set_font(4);
                        strcpy(st, "1-1.5");
                        outgtext(st, txtlin + 95, txtrow);
                        strcpy(st, "1.75");
                        outgtext(st, txtlin + 130, txtrow);
                        strcpy(st, "2");
                        outgtext(st, txtlin + 165, txtrow);
                        strcpy(st, "2.5");
                        outgtext(st, txtlin + 200, txtrow);
                        strcpy(st, "2.75");
                        outgtext(st, txtlin + 235, txtrow);
                        strcpy(st, "3-4");
                        outgtext(st, txtlin + 270, txtrow);
                        strcpy(st, ">4");
                        outgtext(st, txtlin + 305, txtrow);
			setcolor(27);
                        rectangle(0, txtlin + 56, txtrow - 5, txtlin + 91, txtrow + 60);
			}
                /* SARS for reported 1-1.5" hail */
                if ((avsize > 1.49) && (avsize <= 1.68)) {
                        setcolor(31);
                        set_font(4); 
                        strcpy(st, "<1");
                        outgtext(st, txtlin + 60, txtrow);
                        set_font(6);
                        strcpy(st, "1-1.5");
                        outgtext(st, txtlin + 95, txtrow);
			set_font(4);
                        strcpy(st, "1.75");
                        outgtext(st, txtlin + 130, txtrow);
                        strcpy(st, "2");
                        outgtext(st, txtlin + 165, txtrow);
                        strcpy(st, "2.5");
                        outgtext(st, txtlin + 200, txtrow);
                        strcpy(st, "2.75");
                        outgtext(st, txtlin + 235, txtrow);
                        strcpy(st, "3-4");
                        outgtext(st, txtlin + 270, txtrow);
                        strcpy(st, ">4");
                        outgtext(st, txtlin + 305, txtrow);
                        setcolor(27);
                        rectangle(0, txtlin + 91, txtrow - 5, txtlin + 126, txtrow + 60); 
                        }
                /* SARS for reported 1.75" hail */
                if ((avsize > 1.68) && (avsize <= 2.06)) {
                        setcolor(31);
                        set_font(4); 
                        strcpy(st, "<1");
                        outgtext(st, txtlin + 60, txtrow);
                        strcpy(st, "1-1.5");
                        outgtext(st, txtlin + 95, txtrow);
			set_font(6);
                        strcpy(st, "1.75");
                        outgtext(st, txtlin + 130, txtrow);
			set_font(4);
                        strcpy(st, "2");
                        outgtext(st, txtlin + 165, txtrow);
                        strcpy(st, "2.5");
                        outgtext(st, txtlin + 200, txtrow);
                        strcpy(st, "2.75");
                        outgtext(st, txtlin + 235, txtrow);
                        strcpy(st, "3-4");
                        outgtext(st, txtlin + 270, txtrow);
                        strcpy(st, ">4");
                        outgtext(st, txtlin + 305, txtrow);
                        setcolor(27);
                        rectangle(0, txtlin + 126, txtrow - 5, txtlin + 161, txtrow + 60); 
                        }
                /* SARS for reported 2" hail */
                if ((avsize > 2.06) && (avsize <= 2.39)) {
                        setcolor(31);
                        set_font(4); 
                        strcpy(st, "<1");
                        outgtext(st, txtlin + 60, txtrow);
                        strcpy(st, "1-1.5");
                        outgtext(st, txtlin + 95, txtrow);
                        strcpy(st, "1.75");
                        outgtext(st, txtlin + 130, txtrow);
			set_font(6);
                        strcpy(st, "2");
                        outgtext(st, txtlin + 165, txtrow);
			set_font(4);
                        strcpy(st, "2.5");
                        outgtext(st, txtlin + 200, txtrow);
                        strcpy(st, "2.75");
                        outgtext(st, txtlin + 235, txtrow);
                        strcpy(st, "3-4");
                        outgtext(st, txtlin + 270, txtrow);
                        strcpy(st, ">4");
                        outgtext(st, txtlin + 305, txtrow);
                        setcolor(27);
                        rectangle(0, txtlin + 161, txtrow - 5, txtlin + 196, txtrow + 60); 
                        }
                /* SARS for reported 2.5" hail */
                if ((avsize > 2.39) && (avsize <= 2.52)) {
                        setcolor(31);
                        set_font(4);
                        strcpy(st, "<1");
                        outgtext(st, txtlin + 60, txtrow); 
                        strcpy(st, "1-1.5");
                        outgtext(st, txtlin + 95, txtrow);
                        strcpy(st, "1.75");
                        outgtext(st, txtlin + 130, txtrow);
                        strcpy(st, "2");
                        outgtext(st, txtlin + 165, txtrow);
                        set_font(6);
                        strcpy(st, "2.5");
                        outgtext(st, txtlin + 200, txtrow);
			set_font(4);	
                        strcpy(st, "2.75");
                        outgtext(st, txtlin + 235, txtrow);
                        strcpy(st, "3-4");
                        outgtext(st, txtlin + 270, txtrow);
                        strcpy(st, ">4");
                        outgtext(st, txtlin + 305, txtrow);
                        setcolor(27);
                        rectangle(0, txtlin + 196, txtrow - 5, txtlin + 231, txtrow + 60);
                        }
                /* SARS for reported 2.75" hail */
		if ((avsize > 2.52) && (avsize <= 2.56)) {
                        setcolor(31);
                        set_font(4);
                        strcpy(st, "<1");
                        outgtext(st, txtlin + 60, txtrow); 
                        strcpy(st, "1-1.5");
                        outgtext(st, txtlin + 95, txtrow);
                        strcpy(st, "1.75");
                        outgtext(st, txtlin + 130, txtrow);
                        strcpy(st, "2");
                        outgtext(st, txtlin + 165, txtrow);
                        strcpy(st, "2.5");
                        outgtext(st, txtlin + 200, txtrow);
			set_font(6);	
                        strcpy(st, "2.75");
                        outgtext(st, txtlin + 235, txtrow);
			set_font(4);
                        strcpy(st, "3-4");
                        outgtext(st, txtlin + 270, txtrow);
                        strcpy(st, ">4");
                        outgtext(st, txtlin + 305, txtrow);
                        setcolor(27);
                        rectangle(0, txtlin + 231, txtrow - 5, txtlin + 266, txtrow + 60);
                        }
                /* SARS for reported 3-4" hail */
                if ((avsize > 2.56) && (avsize <= 2.64)) {
                        setcolor(31);
                        set_font(4);
                        strcpy(st, "<1");
                        outgtext(st, txtlin + 60, txtrow); 
                        strcpy(st, "1-1.5");
                        outgtext(st, txtlin + 95, txtrow);
                        strcpy(st, "1.75");
                        outgtext(st, txtlin + 130, txtrow);
                        strcpy(st, "2");
                        outgtext(st, txtlin + 165, txtrow);
                        strcpy(st, "2.5");
                        outgtext(st, txtlin + 200, txtrow);
                        strcpy(st, "2.75");
                        outgtext(st, txtlin + 235, txtrow);
			set_font(6);
                        strcpy(st, "3-4");
                        outgtext(st, txtlin + 270, txtrow);
			set_font(4);
                        strcpy(st, ">4");
                        outgtext(st, txtlin + 305, txtrow);
                        setcolor(27);
                        rectangle(0, txtlin + 266, txtrow - 5, txtlin + 301, txtrow + 60);
                        }
                /* SARS for reported >4" hail */
                if (avsize > 2.64) {
                        setcolor(31);
                        set_font(4);
                        strcpy(st, "<1");
                        outgtext(st, txtlin + 60, txtrow); 
                        strcpy(st, "1-1.5");
                        outgtext(st, txtlin + 95, txtrow);
                        strcpy(st, "1.75");
                        outgtext(st, txtlin + 130, txtrow);
                        strcpy(st, "2");
                        outgtext(st, txtlin + 165, txtrow);
                        strcpy(st, "2.5");
                        outgtext(st, txtlin + 200, txtrow);
                        strcpy(st, "2.75");
                        outgtext(st, txtlin + 235, txtrow);
                        strcpy(st, "3-4");
                        outgtext(st, txtlin + 270, txtrow);
			set_font(6);
                        strcpy(st, ">4");
                        outgtext(st, txtlin + 305, txtrow);
                        setcolor(27);
                        rectangle(0, txtlin + 301, txtrow - 5, txtlin + 336, txtrow + 60);
                        }

		txtrow += 15;
		setcolor(31);
		set_font(4);
		strcpy(st, "+1 STD");
        	outgtext(st, txtlin, txtrow);
		setcolor(27);
        	strcpy(st, "1.9");
       		outgtext(st, txtlin + 60, txtrow);
        	strcpy(st, "2.0");
        	outgtext(st, txtlin + 95, txtrow);
        	strcpy(st, "2.3");
        	outgtext(st, txtlin + 130, txtrow);
        	strcpy(st, "2.8");
        	outgtext(st, txtlin + 165, txtrow);
        	strcpy(st, "2.9");
        	outgtext(st, txtlin + 200, txtrow);
        	strcpy(st, "3.0");
        	outgtext(st, txtlin + 235, txtrow);
        	strcpy(st, "3.0");
        	outgtext(st, txtlin + 270, txtrow);
        	strcpy(st, "3.0");
        	outgtext(st, txtlin + 305, txtrow);

                txtrow += 15;
                setcolor(31);
                strcpy(st, "AVG");
                outgtext(st, txtlin, txtrow);
                setcolor(27);
                strcpy(st, "1.5");
                outgtext(st, txtlin + 60, txtrow);
                strcpy(st, "1.5");
                outgtext(st, txtlin + 95, txtrow);
                strcpy(st, "1.8");
                outgtext(st, txtlin + 130, txtrow);
                strcpy(st, "2.3");
                outgtext(st, txtlin + 165, txtrow);
                strcpy(st, "2.5");
                outgtext(st, txtlin + 200, txtrow);
                strcpy(st, "2.5");
                outgtext(st, txtlin + 235, txtrow);
                strcpy(st, "2.6");
                outgtext(st, txtlin + 270, txtrow);
                strcpy(st, "2.7");
                outgtext(st, txtlin + 305, txtrow);

        	txtrow += 15;
        	setcolor(31);
        	strcpy(st, "-1 STD");
        	outgtext(st, txtlin, txtrow);
		setcolor(27);
        	strcpy(st, "1.1");
        	outgtext(st, txtlin + 60, txtrow);
        	strcpy(st, "1.1");
        	outgtext(st, txtlin + 95, txtrow);
        	strcpy(st, "1.3");
        	outgtext(st, txtlin + 130, txtrow);
        	strcpy(st, "1.7");
        	outgtext(st, txtlin + 165, txtrow);
        	strcpy(st, "2.1");
        	outgtext(st, txtlin + 200, txtrow);
        	strcpy(st, "2.1");
        	outgtext(st, txtlin + 235, txtrow);
        	strcpy(st, "2.2");
        	outgtext(st, txtlin + 270, txtrow);
        	strcpy(st, "2.4");
        	outgtext(st, txtlin + 305, txtrow);
		
/*		setcolor(27);
		if (avsize <= 1.49) rectangle(0, txtlin + 56, txtrow - 50, txtlin + 91, txtrow + 12);
                if ((avsize > 1.49) && (avsize <= 1.68)) rectangle(0, txtlin + 91, txtrow - 50, txtlin + 126, txtrow + 12);
                if ((avsize > 1.68) && (avsize <= 2.06)) rectangle(0, txtlin + 126, txtrow - 50, txtlin + 161, txtrow + 12);
                if ((avsize > 2.06) && (avsize <= 2.39)) rectangle(0, txtlin + 161, txtrow - 50, txtlin + 196, txtrow + 12);
                if ((avsize > 2.39) && (avsize <= 2.52)) rectangle(0, txtlin + 196, txtrow - 50, txtlin + 231, txtrow + 12);
                if ((avsize > 2.52) && (avsize <= 2.56)) rectangle(0, txtlin + 231, txtrow - 50, txtlin + 266, txtrow + 12);
                if ((avsize > 2.56) && (avsize <= 2.64)) rectangle(0, txtlin + 266, txtrow - 50, txtlin + 301, txtrow + 12);
		if ((avsize >= 2.65)) rectangle(0, txtlin + 301, txtrow - 50, txtlin + 336, txtrow + 12);
*/		
	       }

/*
        set_font(4);
        setcolor(31);
	txtrow = trx - 14;
	if (nsndgs>0) 
	   {
	   sprintf( st, "Largest SARS Matches");
           outgtext ( st, txtlin+165, txtrow );
           txtrow += 3;
	   }
	else
	   {
	   sprintf( st, "No Quality SARS Matches");
           outgtext ( st, txtlin+180, txtrow+70 );
           txtrow += 3;
	   }

	for (i=0;i<nsndgs;i++)
	   {
           if (nsndgs > 9) nsndgs = 9;
           txtrow += 14;
	   sprintf( st, "%s  %.2f", sndglist[i], haillist[i]);
           outgtext ( st, txtlin+165, txtrow );
	   }
*/
	/* ----- Set Parcel Back ----- */
       /* if (oldlplchoice == 3) pres = mu_layer;
        else if (oldlplchoice == 4) pres = mml_layer;
        else if (oldlplchoice == 5) pres = user_level;
        else pres = mml_layer; 
        define_parcel(oldlplchoice, pres);*/

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

	}


/*NP*/
void show_sars(void)
        /*************************************************************/
        /*  SHOW_SARS                                                */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*************************************************************/
        {
        float ix1, ix2, ix3, ix4, sfctemp, sfcdwpt, sfcpres, j1, j2;
        float nv_cape, nv_cinh, nv_cap, pres, ptop, pbot, mucape, mumixr, avsize, matches2;
        float lr75, shr3, shr6, shr9, fzlh, mucinh, ship, esi2, t500;
        float srh3, matches, p1, p2, haillist[15], suplist[15], oldlplpres;
	float mucp, mlcp, mllcl, srh1, shr3k, shr6k, shr9k;
        short txtlin, txtrow, oldlplchoice, pIndex, zIndex, tIndex, trow2, i;
        short tdIndex, nsndgs, trx, j, temp_mark, y;
        char st[100], st1[20], st2[20], sndglist[15][15];
	char tortags[3][10] = { "NONTOR", "WEAKTOR", "SIGTOR" };
        Parcel pcl;   
        Parcel pcl2;

        /* added 25OCT06 by RLT */

        tIndex = getParmIndex("TEMP");
        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");
        tdIndex = getParmIndex("DWPT");

        oldlplchoice = lplvals.flag;
	oldlplpres = lplvals.pres;

        /* plot height of freezing level, -20C, and -30C for Donavon hail technique */
        set_font(4);
        setlinestyle(1, 2);
        temp_mark = pres_to_pix(temp_lvl(0, &ix1 ));
        setcolor(0);
        rectangle(1, skv.tlx + (xwdth*.38), temp_mark - 13, skv.tlx + (xwdth*.38) + 72, temp_mark - 1);
        setcolor(26);
        moveto(skv.tlx + (xwdth*.38), temp_mark);
        lineto(skv.tlx + (xwdth*.38) + 25, temp_mark);
        sprintf( st, "FZL = %s", qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES))), "'", 0 ));
        outgtext(st, skv.tlx + (xwdth*.38), temp_mark - 12);

        temp_mark = pres_to_pix(temp_lvl(-20, &ix1 ));
        setcolor(0);
        rectangle(1, skv.tlx + (xwdth*.38), temp_mark - 13, skv.tlx + (xwdth*.38) + 72, temp_mark - 1);
        setcolor(26);
        moveto(skv.tlx + (xwdth*.38), temp_mark);
        lineto(skv.tlx + (xwdth*.38) + 25, temp_mark);
        sprintf( st, "-20C = %s", qc2( mtof(agl(i_hght(temp_lvl( -20, &ix1 ), I_PRES))), "'", 0 ));
        outgtext(st, skv.tlx + (xwdth*.38), temp_mark - 12);

        temp_mark = pres_to_pix(temp_lvl(-30, &ix1 ));
        setcolor(0);
        rectangle(1, skv.tlx + (xwdth*.38), temp_mark - 13, skv.tlx + (xwdth*.38) + 72, temp_mark - 1);
        setcolor(26);
        moveto(skv.tlx + (xwdth*.38), temp_mark);
        lineto(skv.tlx + (xwdth*.38) + 25, temp_mark);
        sprintf( st, "-30C = %s", qc2( mtof(agl(i_hght(temp_lvl( -30, &ix1 ), I_PRES))), "'", 0 ));
        outgtext(st, skv.tlx + (xwdth*.38), temp_mark - 12);


        define_parcel(4,100);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mlcp = pcl.bplus;
        mllcl = agl(i_hght(pcl.lclpres, I_PRES));

        define_parcel(3, mu_layer);
        sfctemp = lplvals.temp;
        sfcdwpt = lplvals.dwpt;
        sfcpres = lplvals.pres;

        /* ----- Calculate Parcel Data ----- */
        ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	mucp = pcl.bplus;
        mumixr = mixratio(lplvals.pres, lplvals.dwpt);

/*        printf( "SHOW_SARS:Enter\n" );*/
        setcliprgn(1,1,xwdth, xhght);

        /* ----- Draw Bounding Box ----- */
        setlinestyle(1,1);
        /*txtlin = skv.tlx + 731;*/
        if (display_mode_right== DISPLAY_SARS_RIGHT)
                {txtlin = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_SARS_LEFT)
                {txtlin = skv.tlx + 712;}
/*	txtlin = skv.tlx + (xwdth*.65);
*/      txtrow = skv.bry + 20;
        setcolor(0);
        rectangle(1, txtlin, txtrow, txtlin + 350, txtrow + 250);
        setcolor(31);
        rectangle(0, txtlin, txtrow, txtlin + 350, txtrow + 250);

        txtlin += 5;
        txtrow += 5;

        /* ----- Titles ----- */
        setcolor(31);
        set_font(6);
        strcpy( st, "SARS - Sounding Analog System" );
        ix1 = (350 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);
        txtrow += 16;
        moveto(txtlin-5, txtrow);
        lineto(txtlin+345, txtrow);
        moveto(txtlin+170, txtrow);
        lineto(txtlin+170, txtrow+230);
        txtrow += 5;
        set_font(4);
        strcpy( st, "SUPERCELL" );
        ix1 = (175 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);
        strcpy( st, "SGFNT HAIL" );
        ix1 = (175 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 + 170, txtrow);
        txtrow += 15;
        strcpy( st, "browse to ~thompson / sup / snd / *" );
        ix1 = (175 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 - 5, txtrow);
        strcpy( st, "browse to ~jewell / hail / snd / *" );
        ix1 = (175 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 + 170, txtrow);
        txtrow += 15;
        moveto(txtlin+5, txtrow);
        lineto(txtlin+160, txtrow);
        moveto(txtlin+180, txtrow);
        lineto(txtlin+335, txtrow);

	/* Compute Hail Sars Data */
/*	mumixr = mixratio(lplvals.pres, lplvals.dwpt);
*/	t500 =  i_temp(500, I_PRES);
	lr75 = lapse_rate(&ix1, 700, 500);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4); shr6 = kt_to_mps(ix4);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(9000)), &ix1, &ix2, &ix3, &ix4); shr9 = kt_to_mps(ix4);
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(3000)), &ix1, &ix2, &ix3, &ix4); shr3 = kt_to_mps(ix4);
/* 24 Mar 2008 */
/*        if (agl(i_hght(pbot, I_PRES)) > 0){
                wind_shear(pbot, i_pres(msl(agl(i_hght(ptop, I_PRES))*0.25)), &ix1, &ix2, &ix3, &ix4);  shr3 = kt_to_mps(ix4);
                wind_shear(pbot, i_pres(msl(agl(i_hght(ptop, I_PRES))*0.5)), &ix1, &ix2, &ix3, &ix4);  shr6 = kt_to_mps(ix4);
                wind_shear(pbot, i_pres(msl(agl(i_hght(ptop, I_PRES))*0.75)), &ix1, &ix2, &ix3, &ix4);  shr9 = kt_to_mps(ix4);
                }*/

	  if (agl(i_hght(p_bot, I_PRES)) > 0){
                wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.25)), &ix1, &ix2, &ix3, &ix4);  shr3 = kt_to_mps(ix4);
                wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.5)), &ix1, &ix2, &ix3, &ix4);  shr6 = kt_to_mps(ix4);
                wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.75)), &ix1, &ix2, &ix3, &ix4);  shr9 = kt_to_mps(ix4);
                }
	fzlh = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));
	ship = sig_hail(pcl.bplus, mumixr, lr75, t500, kt_to_mps(shr6), fzlh, pcl.bminus, 0, 0, 25, mlcp);
	srh3 = helicity(0, 3000, st_dir, st_spd, &ix2, &ix3);
	srh1 = helicity(0, 1000, st_dir, st_spd, &ix2, &ix3);
/* 24 Mar 2008 */
/*        if (agl(i_hght(pbot, I_PRES)) > 0){
        	srh3 = helicity(pbot, ptop, st_dir, st_spd, &ix2, &ix3);
        	srh1 = helicity(pbot, ptop, st_dir, st_spd, &ix2, &ix3);
		}*/

	  if (agl(i_hght(p_bot, I_PRES)) > 0){
                srh3 = helicity(p_bot, p_top, st_dir, st_spd, &ix2, &ix3);
                srh1 = helicity(p_bot, p_top, st_dir, st_spd, &ix2, &ix3);
                }

/*      printf("Beginning SARS\n");
        printf("mumixr = %f\n", mumixr);
        printf("mucape = %f\n", pcl.bplus);
        printf("500mb T= %f\n", t500);
        printf("7-5LR  = %f\n", lr75);
        printf("6km Shr= %f\n", kt_to_mps(shr6));
        printf("9km Shr= %f\n", kt_to_mps(shr9));
        printf("3km Shr= %f\n", kt_to_mps(shr3));
        printf("SHIP   = %f\n", ship);
        printf("3km SRH= %f\n", srh3);
*/
	sars(&mumixr, &mucp, &t500, &lr75, &shr6, &shr9, &shr3, &ship, &srh3, &nsndgs, &matches, &p1, &avsize, &matches2, sndglist, &haillist, &sars_filename);
/*        printf("SHOW_SARS: --------> avg SARS hail size = %f\n", avsize);
	printf("matches2 = %f\n", matches2);
        printf("Finished SARS\n");
*/
        for (i=0; i < 15; i++) sndglist[i][14] = '\0';
	printf( "%d High Quality HAIL Matches were found.\n", nsndgs);
	for (i=0;i<nsndgs;i++) { printf( "HAIL match = %s  %.2f\n", sndglist[i], haillist[i]); }
	printf( "%.0f Total matches were found.\n", matches);
	printf( "%.0f Percent were SIG HAIL.\n", p1);

        /* ----- SARS matches ----- */
        set_font(4);
        setcolor(31);
	txtrow += 18;
	if (nsndgs>0) 
	   { txtrow += 3; }
	else
	   {
	   sprintf( st2, "No Quality HAIL Matches");
           outgtext ( st2, txtlin+195, txtrow + 70);
           txtrow += 3;
	   }

	for (i=0;i<nsndgs;i++)
	   {
	   if (nsndgs > 10) nsndgs = 10;
           setcolor(5);
           txtrow += 14;
	   j = haillist[i];
	   if (j<2) {setcolor(18);}
	   if (j<1) {setcolor(8);}
	   sprintf( st, "%s", sndglist[i]);
           outgtext ( st, txtlin+180, txtrow-20 );
	   sprintf( st, "%.2f", haillist[i]);
           outgtext ( st, txtlin+280, txtrow-20 );
	   }

	/* ----- Plot Hail SARS Result ----- */
	p2 = 100.0 - p1;
	setcolor(31);
	strcpy(st1, "No Matches");
	if (matches>0){ setcolor(31); strcpy(st1, "Non-sig Hail");}
	if (p1>=50){ setcolor(12); strcpy(st1, "**SIG HAIL!**");}
        if (p1>=50){sprintf( st, "(%.0f matches out of 1148 sndgs)", matches);
                ix1 = (175 - getgtextextent(st))/2;
                outgtext ( st, txtlin + ix1 + 170, skv.bry + 245 );}
	if (p1<50 && matches>0){sprintf( st, "(%.0f matches out of 1148 sndgs)", matches);
		ix1 = (175 - getgtextextent(st))/2;
        	outgtext ( st, txtlin + ix1 + 170, skv.bry + 245 );}
        sprintf( st, "SARS:  %s  (%.0f%s SIG)", st1, p1, "%");
	ix1 = (175 - getgtextextent(st))/2;
        outgtext ( st, txtlin + ix1 + 170, skv.bry + 259 );
/*
        sprintf(st, "AVG size = %.1f    (based on %.0f matches)", avsize, matches2);
        ix1 = (175 - getgtextextent(st))/2;
        outgtext(st, txtlin + ix1 + 150, skv.bry + 230);
*/

/* REJ/RLT hack attempt at supercell SARS 12/17/05...because John was too lazy */
/* I am hurt by the statement above.  But then again, it rings of truth! :-) */
/* This is a formal retraction of the previous REJ/RLT statement...assuming this routine works :^) */

        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4); shr6k = ix4;
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(9000)), &ix1, &ix2, &ix3, &ix4); shr9k = ix4;
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(3000)), &ix1, &ix2, &ix3, &ix4); shr3k = ix4;

        spnsharp(&mlcp, &mllcl, &t500, &lr75, &shr6k, &srh1, &nsndgs, &matches, &p1, sndglist, &suplist, &sup_filename, &shr3k, &shr9k, &srh3);
/*        printf("Finished supercell SARS\n");*/

/*	variables passed to supercell SARS fortran routine */
/*	printf("\n mlcape = %.1f\n", mlcp);
        printf("\n mllcl = %.1f\n", mllcl);
        printf("\n t500 = %.1f\n", t500);
        printf("\n lr75 = %.1f\n", lr75);
        printf("\n shr6 = %.1f\n", shr6k);
        printf("\n srh1 = %.1f\n", srh1);
        printf("\n shr3k = %.1f\n", shr3k);
        printf("\n shr9k = %.1f\n", shr9k);
*/
	 
        for (i=0; i < 15; i++) sndglist[i][14] = '\0';
        printf( "%d High Quality SUPERCELL Matches were found.\n", nsndgs);
	/* modified loop and printf statement to properly display supercell match type - RLT 4/24/12 */
        for (i=0;i<nsndgs;i++) {
		j = suplist[i]; 
		printf( "SUPERCELL match = %s %s\n", sndglist[i], tortags[j]);
		}
        printf( "%.0f Total matches were found.\n", matches);
        printf( "%.0f Percent were TOR.\n", p1);

        /* ----- Supercell SARS matches ----- */
        set_font(4);
        setcolor(31);
	txtrow = skv.bry + 75;
        txtrow += 18;
        if (nsndgs>0)
           { txtrow += 3; }
        else
           {
           sprintf( st2, "No Quality SUPERCELL Matches");
           outgtext ( st2, txtlin+5, txtrow + 70);
           txtrow += 3;
           }

        for (i=0;i<nsndgs;i++)
           {
           if (nsndgs > 10) nsndgs = 10;
           setcolor(2);
           txtrow += 14;
/* I am now passing values in suplist (0-3).  I assigned an array of strings called tortags to plot the right stuff on the screen. */
	   j = suplist[i];
	   if (j<2) {setcolor(6);}
	   if (j<1) {setcolor(18);}
           sprintf( st, "%s", sndglist[i]);
           outgtext ( st, txtlin+10, txtrow-20 );
           sprintf( st, "%s", tortags[j]);
           outgtext ( st, txtlin+110, txtrow-20 );
           }

        /* ----- Plot Supercell SARS Result ----- */
        p2 = 100.0 - p1;
        setcolor(31);
        strcpy(st1, "No Matches");
	define_parcel(1,0);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);	
	if (pcl.bplus >= 100){
        	if (matches>0){ setcolor(31); strcpy(st1, "NONTOR");}
        	if (p1>50){ setcolor(12); strcpy(st1, "**TOR!**");}
        	if (p1>50){sprintf( st, "(%.0f matches out of 856 sndgs)", matches);
                	ix1 = (175 - getgtextextent(st))/2;
                	outgtext ( st, txtlin + ix1, skv.bry + 245 );}
        	if (p1<=50 && matches>0){sprintf( st, "(%.0f matches out of 856 sndgs)", matches);
                	ix1 = (175 - getgtextextent(st))/2;
                	outgtext ( st, txtlin + ix1, skv.bry + 245 );}
        	sprintf( st, "SARS:  %s  (%.0f%s TOR)", st1, p1, "%");
        	ix1 = (175 - getgtextextent(st))/2;
        	outgtext ( st, txtlin + ix1, skv.bry + 259 );
		}

	/* ----- Set Parcel Back ----- */
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


        }


        /*NP*/
        void show_stp_stats(void)
        /*************************************************************/
        /*  SHOW_STP_STATS                                           */
        /*  Rich Thompson SPC OUN				     */	
        /*                                                           */
        /*************************************************************/
        {
	float s10th, s25th, s50th, s75th, s90th;
	float stpc, maxval, ix1, pres;
        short tlx, tly, oldlplchoice, pIndex, zIndex, tIndex, trow2, i, y, hash;
        char st[100];	
        Parcel pcl;

	/* added 25OCT06 by RLT */
	/*display_mode_left = DISPLAY_STP_LEFT;
	if (inset_mode == DISPLAY_LEFT)
		{display_mode_left = DISPLAY_STP_LEFT;}
	if (inset_mode == DISPLAY_RIGHT)
		{display_mode_right = DISPLAY_STP_RIGHT;}
	*/
        setcliprgn(1,1,xwdth, xhght);

        /* ----- Draw Bounding Box ----- */
        setlinestyle(1,1);
        /*tlx = skv.tlx + 731;*/
        if (display_mode_right == DISPLAY_STP_RIGHT)
                {tlx = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_STP_LEFT)
                {tlx = skv.tlx + 712;}
        tly = skv.bry + 20;

        setcolor(0);
        rectangle(1, tlx, tly, tlx + 350, tly + 250);
        setcolor(31);
        rectangle(0, tlx, tly, tlx + 350, tly + 250);
	moveto(tlx, tly + 250);
	lineto(tlx + 350, tly + 250);

	/* graph title */
       	setcolor(31);
	set_font(6); 
	sprintf( st, "Effective-Layer STP (with CIN)");
        outgtext ( st, tlx+75, tly+1);	

        /* ----- Plot Y-Coord hash marks ----- */
	maxval = 12.0;
	/* 1 STP = 20 pixels in y */
	setlinestyle(2, 1);
        for (i = 0; i <= maxval - 1; i = i + 1) {
	
           hash = i * (short)(240 / maxval);	   
           setcolor(24);
	   set_font(4);	
	   moveto(tlx+12, tly + 240 - hash);
           lineto(tlx+350, tly + 240 - hash);
           itoa(i, st, 10);
	   setcolor(1);
           outgtext(st, (tlx + 7) - (getgtextextent(st) / 2), tly + 235 - hash);
        }

	/* plot labels for each box */
	set_font(5);
	setcolor(31);
        sprintf( st, "EF4+");
        outgtext ( st, tlx+34, tly+240);
	sprintf( st, "EF3");
        outgtext ( st, tlx+91, tly+240);
        sprintf( st, "EF2");
        outgtext ( st, tlx+146, tly+240);
        sprintf( st, "EF1");
        outgtext ( st, tlx+201, tly+240);
        sprintf( st, "EF0");
        outgtext ( st, tlx+256, tly+240);
        sprintf( st, "NONTOR");
        outgtext ( st, tlx+303, tly+240);

	/* percentile rank values from Fig. 12 in Thompson et al. (2012) WAF */
        /* f4+ box and whiskers values */
        s90th = 11.0;
        s75th = 8.3;
        s50th = 5.3;
        s25th = 2.8;
        s10th = 1.2;
        /* plot f4+ box and whiskers */
        setcolor(21);
        setlinestyle(1, 3);
        /* draw box */
        rectangle(0, tlx+30, tly+(short)(240-(s75th*20)), tlx+60, tly+(short)(240-(s25th*20)));
        /* draw upper whisker */
        moveto(tlx+45, tly+(short)(240-(s75th*20)));
        lineto(tlx+45, tly+(short)(240-(s90th*20)));
        /* draw lower whisker */
        moveto(tlx+45, tly+(short)(240-(s25th*20)));
        lineto(tlx+45, tly+(short)(240-(s10th*20)));
        /* draw median */
        moveto(tlx+30, tly+(short)(240-(s50th*20)));
        lineto(tlx+60, tly+(short)(240-(s50th*20)));

        /* f3 box and whiskers values */
        s90th = 8.4;
        s75th = 4.5;
        s50th = 2.4;
        s25th = 1.0;
        s10th = 0.2;
        /* plot f3 box and whiskers */
        setcolor(21);
        setlinestyle(1, 3);
        /* draw box */
        rectangle(0, tlx+85, tly+(short)(240-(s75th*20)), tlx+115, tly+(short)(240-(s25th*20)));
        /* draw upper whisker */
        moveto(tlx+100, tly+(short)(240-(s75th*20)));
        lineto(tlx+100, tly+(short)(240-(s90th*20)));
        /* draw lower whisker */
        moveto(tlx+100, tly+(short)(240-(s25th*20)));
        lineto(tlx+100, tly+(short)(240-(s10th*20)));
        /* draw median */
        moveto(tlx+85, tly+(short)(240-(s50th*20)));
        lineto(tlx+115, tly+(short)(240-(s50th*20)));

        /* f2 box and whiskers values */
        s90th = 5.6;
        s75th = 3.7;
        s50th = 1.7;
        s25th = 0.6;
        s10th = 0.0;
        /* plot f2 box and whiskers */
        setcolor(21);
        setlinestyle(1, 3);
        /* draw box */
        rectangle(0, tlx+140, tly+(short)(240-(s75th*20)), tlx+170, tly+(short)(240-(s25th*20)));
        /* draw upper whisker */
        moveto(tlx+155, tly+(short)(240-(s75th*20)));
        lineto(tlx+155, tly+(short)(240-(s90th*20)));
        /* draw lower whisker */
        moveto(tlx+155, tly+(short)(240-(s25th*20)));
        lineto(tlx+155, tly+(short)(240-(s10th*20)));
        /* draw median */
        moveto(tlx+140, tly+(short)(240-(s50th*20)));
        lineto(tlx+170, tly+(short)(240-(s50th*20)));

        /* f1 box and whiskers values */
        s90th = 4.5;
        s75th = 2.6;
        s50th = 1.2;
        s25th = 0.3;
        s10th = 0.0;
        /* plot f1 box and whiskers */
        setcolor(22);
        setlinestyle(1, 3);
        /* draw box */
        rectangle(0, tlx+195, tly+(short)(240-(s75th*20)), tlx+225, tly+(short)(240-(s25th*20)));
        /* draw upper whisker */
        moveto(tlx+210, tly+(short)(240-(s75th*20)));
        lineto(tlx+210, tly+(short)(240-(s90th*20)));
        /* draw lower whisker */
        moveto(tlx+210, tly+(short)(240-(s25th*20)));
        lineto(tlx+210, tly+(short)(240-(s10th*20)));
        /* draw median */
        moveto(tlx+195, tly+(short)(240-(s50th*20)));
        lineto(tlx+225, tly+(short)(240-(s50th*20)));

        /* f0 box and whiskers values */
        s90th = 3.7;
        s75th = 2.0;
        s50th = 0.8;
        s25th = 0.1;
        s10th = 0.0;
        /* plot f0 box and whiskers */
        setcolor(22);
        setlinestyle(1, 3);
        /* draw box */
        rectangle(0, tlx+250, tly+(short)(240-(s75th*20)), tlx+280, tly+(short)(240-(s25th*20)));
        /* draw upper whisker */
        moveto(tlx+265, tly+(short)(240-(s75th*20)));
        lineto(tlx+265, tly+(short)(240-(s90th*20)));
        /* draw lower whisker */
        moveto(tlx+265, tly+(short)(240-(s25th*20)));
        lineto(tlx+265, tly+(short)(240-(s10th*20)));
        /* draw median */
        moveto(tlx+250, tly+(short)(240-(s50th*20)));
        lineto(tlx+280, tly+(short)(240-(s50th*20)));

	/* NONtor percentile ranks are a combination sighail and sigwind values from Fig. 12 in Thompson et al. (2012) WAF */
        /* nontor box and whiskers values */
        s90th = 1.5;
        s75th = 0.7;
        s50th = 0.2;
        s25th = 0.0;
        s10th = 0.0;
        /* plot nontor box and whiskers */
        setcolor(23);
        setlinestyle(1, 3);
        /* draw box */
        rectangle(0, tlx+305, tly+(short)(240-(s75th*20)), tlx+335, tly+(short)(240-(s25th*20)));
        /* draw upper whisker */
        moveto(tlx+320, tly+(short)(240-(s75th*20)));
        lineto(tlx+320, tly+(short)(240-(s90th*20)));
        /* draw lower whisker */
        moveto(tlx+320, tly+(short)(240-(s25th*20)));
        lineto(tlx+320, tly+(short)(240-(s10th*20)));
        /* draw median */
        moveto(tlx+305, tly+(short)(240-(s50th*20)));
        lineto(tlx+335, tly+(short)(240-(s50th*20)));

	/* original sigtor, weaktor, and nontor plot code below */
        /* plot labels for each box */
/*      set_font(5);
        setcolor(31);
        sprintf( st, "SIGTOR");
        outgtext ( st, tlx+60, tly+240);
        sprintf( st, "WEAKTOR");
        outgtext ( st, tlx+155, tly+240);
        sprintf( st, "NONTOR");
        outgtext ( st, tlx+258, tly+240);
*/
	/* sigtor box and whiskers values */
/*	s90th = 6.3;
	s75th = 4.5;
	s50th =	2.2;
	s25th = 1.1;
	s10th = 0.3;
        setcolor(23);
        setlinestyle(1, 3);
	rectangle(0, tlx+50, tly+(short)(240-(s75th*20)), tlx+100, tly+(short)(240-(s25th*20)));
	moveto(tlx+75, tly+(short)(240-(s75th*20)));
	lineto(tlx+75, tly+(short)(240-(s90th*20)));
	moveto(tlx+75, tly+(short)(240-(s25th*20)));
	lineto(tlx+75, tly+(short)(240-(s10th*20)));
	moveto(tlx+50, tly+(short)(240-(s50th*20)));
	lineto(tlx+100, tly+(short)(240-(s50th*20)));	
*/
        /* weaktor box and whiskers values */
/*      s90th = 3.4;
        s75th = 1.9;
        s50th = 0.8;
        s25th = 0.2;
        s10th = 0.0;
        setcolor(23);
        setlinestyle(1, 3);
        rectangle(0, tlx+150, tly+(short)(240-(s75th*20)), tlx+200, tly+(short)(240-(s25th*20)));
        moveto(tlx+175, tly+(short)(240-(s75th*20)));
        lineto(tlx+175, tly+(short)(240-(s90th*20)));
        moveto(tlx+175, tly+(short)(240-(s25th*20)));
        lineto(tlx+175, tly+(short)(240-(s10th*20)));
        moveto(tlx+150, tly+(short)(240-(s50th*20)));
        lineto(tlx+200, tly+(short)(240-(s50th*20)));
*/
        /* nontor box and whiskers values */
/*      s90th = 2.2;
        s75th = 1.0;
        s50th = 0.3;
        s25th = 0.1;
        s10th = 0.0;
        setcolor(23);
        setlinestyle(1, 3);
        rectangle(0, tlx+250, tly+(short)(240-(s75th*20)), tlx+300, tly+(short)(240-(s25th*20)));
        moveto(tlx+275, tly+(short)(240-(s75th*20)));
        lineto(tlx+275, tly+(short)(240-(s90th*20)));
        moveto(tlx+275, tly+(short)(240-(s25th*20)));
        lineto(tlx+275, tly+(short)(240-(s10th*20)));
        moveto(tlx+250, tly+(short)(240-(s50th*20)));
        lineto(tlx+300, tly+(short)(240-(s50th*20)));
*/
	/* plot sounding value of STPC */
	/* max plotted STPC value will be 11 */
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	stpc = sigtorn_cin(st_dir, st_spd);
	if (stpc > 11) stpc = 11;
	y = (short)(240 - (stpc * 20));
	/* set line color to match color coding of composite parameter inset */
	/* original color scheme from T03 sample */
/*        if (stpc < .45) setcolor(8);
        if (stpc >= .45) setcolor(31);
        if (stpc >= 1.95) setcolor(19);
        if (stpc >= 3.95) setcolor(2);
        if (stpc >= 5.95) setcolor(7);
*/
	/* updated color scheme using Thompson et al. (2012) sample */
        if (stpc < .5) setcolor(8);
        if (stpc >= .5) setcolor(18);
        if (stpc >= 1) setcolor(31);
        if (stpc >= 2) setcolor(19);
        if (stpc >= 4) setcolor(2);
        if (stpc >= 8) setcolor(7);
	setlinestyle(1, 2);
	moveto(tlx, tly + y);
	lineto(tlx + 350, tly + y);

	prob_sigt_mlcape();
	prob_sigt_mllcl();
	prob_sigt_eshear();
	prob_sigt_esrh();
	prob_sigt_stpc();
	prob_sigt_stp();
	}
	
	/* NP */
void prob_sigt_mlcape()
        /***************************************************************/
        /*  PROB_SIGT_MLCAPE                                           */
        /*  Rich Thompson SPC OUN                                      */
        /*                                                             */
	/* Calculates and plots the probability of an EF2+ tornado     */
	/* (given a supercell) based on MLCAPE.  Probabilities are     */
	/* derived from Thompson et al. (2012) convective mode sample. */
        /***************************************************************/
        {
        float psigt_mlcape;
        float mlcape;
	float ix1, tlx, tly, pres;
	short oldlplchoice;
	char st[100];
        Parcel pcl;

        /* draw probability inset box */

        setcliprgn(1,1,xwdth, xhght);

        /*tlx = skv.tlx + 731;*/
        if (display_mode_right == DISPLAY_STP_RIGHT)
                {tlx = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_STP_LEFT)
                {tlx = skv.tlx + 712;}
        tly = skv.bry + 20;

        setcolor(0);
        setlinestyle(1, 1);
        rectangle(1, tlx + 200, tly + 20, tlx + 350, tly + 112);
        setcolor(31);
        rectangle(0, tlx + 200, tly + 20, tlx + 350, tly + 112);
        set_font(4);
        sprintf( st, "Prob EF2+ torn with supercell");
        outgtext ( st, tlx+205, tly+21);
	sprintf( st, "Sample CLIMO = .15 sigtor");
	outgtext ( st, tlx+205, tly+31);
	moveto(tlx+200, tly+43);
	lineto(tlx+350, tly+43);
	moveto(tlx+200, tly+87);
	setlinestyle(2, 1);
	lineto(tlx+350, tly+87);	

        oldlplchoice = lplvals.flag;

	/* lift ML parcel */
	define_parcel(4, 100);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mlcape = pcl.bplus;

	/* logic statements for Thompson et al. (2012) WAF sample */
	/* 24 sigtor, 125 non-sigtor supercells */
        if (mlcape >= 4000){ psigt_mlcape=.16;
                setcolor(31);
                }
        else
		/* 86 sigtor, 347 non-sigtor supercells */
                if (mlcape >= 3000 && mlcape < 4000){ psigt_mlcape=.20;
                        setcolor(19);
                        }
        else
		/* 77 sigtor, 357 non-sigtor supercells */
                if (mlcape >= 2500 && mlcape < 3000){ psigt_mlcape=.18;
                        setcolor(19);
                        }
        else
		/* 87 sigtor, 535 non-sigtor supercells */
                if (mlcape >= 2000 && mlcape < 2500){ psigt_mlcape=.14;
                        setcolor(31);
                        }
        else
                /* 125 sigtor, 845 non-sigtor supercells */
                if (mlcape >= 1500 && mlcape < 2000){ psigt_mlcape=.13;
                        setcolor(31);
                        }
        else
		/* 187 sigtor, 1086 non-sigtor supercells */
                if (mlcape >= 1000 && mlcape < 1500){ psigt_mlcape=.15;
                        setcolor(31);
                        }
        else
                /* 225 sigtor, 1199 non-sigtor supercells */
                if (mlcape >= 500 && mlcape < 1000){ psigt_mlcape=.16;
                        setcolor(31);
                        }
        else
                /* 103 sigtor, 545 non-sigtor supercells */
                if (mlcape >= 250 && mlcape < 500){ psigt_mlcape=.14;
                        setcolor(31);
                        }
        else
                /* 88 sigtor, 663 non-sigtor supercells */
                if (mlcape > 0 && mlcape < 250){ psigt_mlcape=.12;
                        setcolor(18);
                        }
	else
		if (mlcape == 0.0) { psigt_mlcape=0.00;
			setcolor(8);
			}


	/* original logic statements based on T03 sample */
/*        if (mlcape >= 4500){ psigt_mlcape=.31;
		setcolor(19);
		}
        else
                if (mlcape >= 3500 && mlcape < 4500){ psigt_mlcape=.23;
			setcolor(19);
			}
        else
                if (mlcape >= 2500 && mlcape < 3500){ psigt_mlcape=.25;
			setcolor(19);
			}
        else
                if (mlcape >= 1500 && mlcape < 2500){ psigt_mlcape=.14;
			setcolor(31);
			}
        else
                if (mlcape < 1500 && mlcape >= 50){ psigt_mlcape=.08;
			setcolor(18);
			}
	else
		if (mlcape < 50){ psigt_mlcape=0.0;
			setcolor(8);
			}
*/
        sprintf( st, "%.2f", psigt_mlcape);
        disp_param( st, tlx+335, tly+45);
        setcolor(31);
	set_font(4);
	sprintf( st, "based on CAPE: ");
        outgtext( st, tlx+208, tly+45);

        /* ----- Set Parcel Back ----- */
        /*if (oldlplchoice == 3) pres = mu_layer;
        else if (oldlplchoice == 4) pres = mml_layer;
        else if (oldlplchoice == 5) pres = user_level;
        else pres = mml_layer;
        define_parcel(oldlplchoice, pres);*/

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

        }

	/* NP */
void prob_sigt_mllcl()
        /***************************************************************/
        /*  PROB_SIGT_MLLCL                                            */
        /*  Rich Thompson SPC OUN                                      */
        /*                                                             */
        /* Calculates and plots the probability of an EF2+ tornado     */
        /* (given a supercell) based on MLLCL.  Probabilities are      */
        /* derived from Thompson et al. (2012) convective mode sample. */
        /***************************************************************/
        {
        float psigt_mllcl;
        float mllcl;
	float ix1, tlx, tly, pres;
	short oldlplchoice;
        char st[100];
	Parcel pcl;

        setcliprgn(1,1,xwdth, xhght);

        /*tlx = skv.tlx + 731;*/
        if (display_mode_right == DISPLAY_STP_RIGHT)
                {tlx = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_STP_LEFT)
                {tlx = skv.tlx + 712;}
        tly = skv.bry + 20;

        oldlplchoice = lplvals.flag;

	/* lift current parcel */
	define_parcel(4, 100);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mllcl = agl(i_hght(pcl.lclpres, I_PRES));	

	/* logic statements for Thompson et al. (2012) WAF sample */ 
	/* 370 sigtor, 1532 non-sigtor supercells */
        if (mllcl < 750 ){ psigt_mllcl=.19;
                setcolor(19);
                }
	/* 309 sigtor, 1304 non-sigtor supercells */
        else
                if (mllcl >= 750 && mllcl < 1000 ){ psigt_mllcl=.19;
                        setcolor(19);
                        }
	/* 182 sigtor, 1065 non-sigtor supercells */
        else
                if (mllcl >= 1000 && mllcl < 1250){ psigt_mllcl=.15;
                        setcolor(31);
                        }
	/* 86 sigtor, 735 non-sigtor supercells */
        else
                if (mllcl >= 1250 && mllcl < 1500){ psigt_mllcl=.10;
                        setcolor(18);
                        }
	/* 32 sigtor, 461 non-sigtor supercells */
        else
                if (mllcl >= 1500 && mllcl < 1750){ psigt_mllcl=.06;
                        setcolor(8);
                        }
	/* 17 sigtor, 279 non-sigtor supercells */
        else
                if (mllcl >= 1750 && mllcl < 2000){ psigt_mllcl=.06;
                        setcolor(8);
                        }
	/* 6 sigtor, 326 non-sigtor supercells */
        else
                if (mllcl >= 2000 && mllcl < 2500){ psigt_mllcl=.02;
                        setcolor(8);
                        }
        else
                if (mllcl >= 2500){ psigt_mllcl=0.0;
                        setcolor(8);
                        }


	/* original logic statements based on T03 sample */
/*
        if (mllcl < 500 ){ psigt_mllcl=.20;
		setcolor(31);
		}
        else
                if (mllcl > 500 && mllcl <= 750){ psigt_mllcl=.18;
			setcolor(31);
			}
        else
                if (mllcl > 750 && mllcl <=1000){ psigt_mllcl=.23;
			setcolor(19);
			}
        else
                if (mllcl > 1000 && mllcl <= 1250){ psigt_mllcl=.17;
			setcolor(31);
			}
        else
                if (mllcl > 1250 && mllcl <= 1500){ psigt_mllcl=.07;
			setcolor(18);
			}
        else
                if (mllcl > 1500 && mllcl <= 1750){ psigt_mllcl=.07;
			setcolor(18);
			}
        else
                if (mllcl > 1750 && mllcl <= 2000){ psigt_mllcl=.03;
			setcolor(8);
			}
	else
		if (mllcl > 2000){ psigt_mllcl=0.0;
			setcolor(8);
			}
*/
        sprintf( st, "%.2f", psigt_mllcl);
        disp_param( st, tlx+335, tly+55);
        setcolor(31);
        set_font(4);
        sprintf( st, "based on LCL: ");
        outgtext( st, tlx+208, tly+55);

        /* ----- Set Parcel Back ----- */
        /*if (oldlplchoice == 3) pres = mu_layer;
        else if (oldlplchoice == 4) pres = mml_layer;
        else if (oldlplchoice == 5) pres = user_level;
        else pres = mml_layer;
        define_parcel(oldlplchoice, pres);*/

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

        }

	/* NP */
void prob_sigt_esrh()
        /**************************************************************/
        /*  PROB_SIGT_ESRH                                            */
        /*  Rich Thompson SPC OUN                                     */
        /*                                                            */
        /* Calculates and plots the probability of an EF2+ tornado    */
        /* (given a supercell) based on effective SRH.                */
	/* Probabilities are derived from Thompson et al. (2012)      */
	/* convective mode sample        			      */
        /**************************************************************/
        {
        float psigt_esrh;
        float esrh, jh1, jh2, pbot, ptop, ix1, ix2, ix3, tlx, tly;
        char st[100];
	Parcel pcl;

        setcliprgn(1,1,xwdth, xhght);

        /*tlx = skv.tlx + 731;*/
        if (display_mode_right == DISPLAY_STP_RIGHT)
                {tlx = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_STP_LEFT)
                {tlx = skv.tlx + 712;}
	tly = skv.bry + 20;

	/* calculate ESRH */
/* 24 Mar 2008 */
/*        effective_inflow_layer(100,-250, &pbot, &ptop);
	if (pbot > 0)
	   {
	   jh1 = agl(i_hght(pbot, I_PRES));
	   jh2 = agl(i_hght(ptop, I_PRES));
	   }*/

        if (p_bot > 0)
           {
           jh1 = agl(i_hght(p_bot, I_PRES));
           jh2 = agl(i_hght(p_top, I_PRES));
           }
        esrh = helicity(jh1, jh2, st_dir, st_spd, &ix2, &ix3);
/* 24 Mar 2008 */
/*	if (pbot < 0)*/

	if (p_bot < 0)
	   {
	   esrh = 0.0;
	   }		

/* logic statement for Thompson et al. (2012) WAF sample */
	/* 68 sigtor, 94 non-sigtor supercells */ 
        if (esrh > 700) { psigt_esrh=.42;
                setcolor(2);
                }
	/* 74 sigtor, 128 non-sigtor supercells */
        else
                if (esrh >= 600 && esrh < 700){ psigt_esrh=.37;
                        setcolor(2);
                        }
	/* 130 sigtor, 213 non-sigtor supercells */
        else
                if (esrh >= 500 && esrh < 600){ psigt_esrh=.38;
                        setcolor(2);
                        } 
	/* 146 sigtor, 391 non-sigtor supercells */
        else
                if (esrh >= 400 && esrh < 500){ psigt_esrh=.27;
                        setcolor(19);
                        }
	/* 180 sigtor, 710 non-sigtor supercells */
        else
                if (esrh >= 300 && esrh < 400){ psigt_esrh=.20;
                        setcolor(19);
                        }
	/* 170 sigtor, 1074 non-sigtor supercells */
        else
                if (esrh >= 200 && esrh < 300){ psigt_esrh=.14;
                        setcolor(31);
                        }
	/* 126 sigtor, 1440 non-sigtor supercells */
        else
                if (esrh >= 100 && esrh < 200){ psigt_esrh=.08;
                        setcolor(18);
                        }
	/* 44 sigtor, 711 non-sigtor supercells */ 
        else
                if (esrh >= 50 && esrh < 100){ psigt_esrh=.06;
                        setcolor(8);
                        }
	/* 64 sigtor, 941 non-sigtor supercells */
        else
                if (esrh < 50){ psigt_esrh=.06;
                        setcolor(8);
                        }

/* original logic statements from T03 sample */ 		
/*        if (esrh > 450) { psigt_esrh=.37;
		setcolor(2);
		}
        else
                if (esrh >= 350 && esrh < 450){ psigt_esrh=.42;
			setcolor(2);
			}
        else
                if (esrh >= 250 && esrh < 350){ psigt_esrh=.21;
			setcolor(19);
			}
        else
                if (esrh >= 150 && esrh < 250){ psigt_esrh=.17;
			setcolor(31);
			}
        else
                if (esrh >= 100 && esrh < 150){ psigt_esrh=.11;
			setcolor(18);
			}
        else
                if (esrh >= 50 && esrh < 100){ psigt_esrh=.06;
			setcolor(8);
			}
        else
                if (esrh < 50 && esrh > .01){ psigt_esrh=.01;
			setcolor(8);
			}
	else
		if (esrh < .01){ psigt_esrh=0.0;
			setcolor(8);
			}
*/
        sprintf( st, "%.2f", psigt_esrh);
        disp_param( st, tlx+335, tly+65);
        setcolor(31);
        set_font(4);
        sprintf( st, "based on ESRH:");
        outgtext( st, tlx+208, tly+65);

        }

	/* NP */
void prob_sigt_eshear()
        /****************************************************************/
        /*  PROB_SIGT_ESHEAR                                            */
        /*  Rich Thompson SPC OUN                                       */
        /*                                                              */
        /* Calculates and plots the probability of an EF2+ tornado      */
        /* (given a supercell) based on effective bulk wind difference. */
	/* Probabilities are derived from Thompson et al. (2012)        */
	/* convective mode sample                                       */
        /****************************************************************/
        {
        float psigt_eshear;
/* 24 Mar 2008 */
/*        float base, depth, el, ptop, pbot, eshear;*/
	float base, depth, el, eshear;
	float ix1, ix2, ix3, ix4, tlx, tly, pres;
	short oldlplchoice;
        char st[100];
	Parcel pcl;

        setcliprgn(1,1,xwdth, xhght);

        /*tlx = skv.tlx + 731;*/
        if (display_mode_right == DISPLAY_STP_RIGHT)
                {tlx = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_STP_LEFT)
                {tlx = skv.tlx + 712;}
        tly = skv.bry + 20;

        oldlplchoice = lplvals.flag;

	/* lift current parcel */
	define_parcel(3, 400);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);

        el = agl(i_hght(pcl.elpres, I_PRES));
/* 24 Mar 2008 */
/*	effective_inflow_layer(100, -250, &pbot, &ptop);
	base = agl(i_hght(pbot, I_PRES));*/

	base = agl(i_hght(p_bot, I_PRES));
	depth = (el - base); 

/* 24 Mar 2008 */
/*        wind_shear(pbot, i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &eshear);*/

	wind_shear(p_bot, i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &eshear);

	/* case of missing EL but effective inflow base exists - default to 0-6 km bulk shear */
       	if (eshear < -99) {
              	eshear = 0.0;
               	}
        if (el < 0) {
	        wind_shear( i_pres(msl(0)), i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        	eshear = ix4;
        	}
/* logic statements for Thompson et al. (2012) WAF sample */
	/* 17 sigtor, 49 non-sigtor supercells */
        if (eshear >=80){ psigt_eshear=.26;
                setcolor(19);
                }
        /* 111 sigtor, 300 non-sigtor supercells */
        else
                if (eshear >=70 && eshear < 80){ psigt_eshear=.36;
                        setcolor(2);
                        }
        /* 251 sigtor, 676 non-sigtor supercells */
        else
                if (eshear >=60 && eshear < 70){ psigt_eshear=.27;
                        setcolor(19);
                        }
        /* 303 sigtor, 1260 non-sigtor supercells */
        else
                if (eshear >=50 && eshear < 60){ psigt_eshear=.19;
                        setcolor(19);
                        }
        /* 218 sigtor, 1647 non-sigtor supercells */
        else
                if (eshear >=40 && eshear < 50){ psigt_eshear=.12;
                        setcolor(18);
                        }
	/* 74 sigtor, 1264 non-sigtor supercells */
        else
                if (eshear >=30 && eshear < 40){ psigt_eshear=.06;
                        setcolor(8);
                        }
        /* 23 sigtor, 437 non-sigtor supercells */
        else
                if (eshear >=20 && eshear < 30){ psigt_eshear=.05;
                        setcolor(8);
                        }
        /* 5 sigtor, 169 non-sigtor supercells */
        else    
                if (eshear > 0.0 && eshear < 20){ psigt_eshear=.03;
                        setcolor(8);
                        }

        else     if (eshear == 0.0){psigt_eshear = 0.00;
			setcolor(8);
			} 

/* original logic statements based on T03 sample */		
/*        if (eshear >=70){ psigt_eshear=.20;
		setcolor(19);
		}
        else
                if (eshear >=60 && eshear < 70){ psigt_eshear=.24;
			setcolor(31);
			}
        else
                if (eshear >=50 && eshear < 60){ psigt_eshear=.19;
			setcolor(31);
			}
        else
                if (eshear >=40 && eshear < 50){ psigt_eshear=.12;
			setcolor(31);
			}
        else
                if (eshear >=30 && eshear < 40){ psigt_eshear=.10;
			setcolor(18);
			}
        else
                if (eshear < 30 && eshear >= 20){ psigt_eshear=.06;
			setcolor(8);
			}
	else	
		if (eshear < 20){ psigt_eshear=0.0;
			setcolor(8);
			}
*/
        sprintf( st, "%.2f", psigt_eshear);
        disp_param( st, tlx+335, tly+75);
        setcolor(31);
        set_font(4);
        sprintf( st, "based on EBWD: ");
        outgtext( st, tlx+208, tly+75);

        /* ----- Set Parcel Back ----- */
        /*if (oldlplchoice == 3) pres = mu_layer;
        else if (oldlplchoice == 4) pres = mml_layer;
        else if (oldlplchoice == 5) pres = user_level;
        else pres = mml_layer;
        define_parcel(oldlplchoice, pres);*/

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

        }

	/* NP */
void prob_sigt_stp()
        /*************************************************************/
        /*  PROB_SIGT_STP                                            */
        /*  Rich Thompson SPC OUN                                    */
        /*                                                           */
        /* Calculates and plots the probability of an EF2+ tornado   */
        /* (given a supercell) based on the fixed-layer STP.         */
	/* Probabilities are derived from Thompson et al. (2012)     */
	/* convective mode sample.                 		     */
        /*************************************************************/
        {
        float psigt_stp;
        float stp_nocin, ix1, tlx, tly;
        char st[100];
	Parcel pcl;

        setcliprgn(1,1,xwdth, xhght);

        /*tlx = skv.tlx + 731;*/
        if (display_mode_right == DISPLAY_STP_RIGHT)
                {tlx = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_STP_LEFT)
                {tlx = skv.tlx + 712;}
        tly = skv.bry + 20;

	/* calculate STP  */
        stp_nocin = sigtorn_fixed(st_dir, st_spd);

/* logic statements for Thompson et al. (2012) WAF sample */
        /* 32 sigtor, 23 non-sigtor supercells */
        if (stp_nocin >= 9){ psigt_stp=.58;
                setcolor(7);
                }
        /* 46 sigtor, 37 non-sigtor supercells */
        else
                if (stp_nocin >= 7 && stp_nocin < 9){ psigt_stp=.55;
                        setcolor(7);
                        }
        /* 112 sigtor, 172 non-sigtor supercells */
        else
                if (stp_nocin >= 5 && stp_nocin < 7){ psigt_stp=.39;
                        setcolor(2);
                        }
        /* 172 sigtor, 515 non-sigtor supercells */
        else
                if (stp_nocin >= 3 && stp_nocin < 5){ psigt_stp=.25;
                        setcolor(19);
                        }
        /* 177 sigtor, 541 non-sigtor supercells */
        else
                if (stp_nocin >= 2 && stp_nocin < 3){ psigt_stp=.25;
                        setcolor(19);
                        }
        /* 218 sigtor, 1091 non-sigtor supercells */
        else
                if (stp_nocin >= 1 && stp_nocin < 2){ psigt_stp=.17;
                        setcolor(31);
                        }
        /* 114 sigtor, 950 non-sigtor supercells */
        else
                if (stp_nocin >= .5 && stp_nocin < 1){ psigt_stp=.11;
                        setcolor(18);
                        }
        /* 71 sigtor, 1107 non-sigtor supercells */
        else
                if (stp_nocin >= .1 && stp_nocin < .5){ psigt_stp=.06;
                        setcolor(8);
                        }
        /* 60 sigtor, 1266 non-sigtor supercells */
        else    
                if (stp_nocin < .1){ psigt_stp=.05;
                        setcolor(8);
                        }

/* original logic statements based on T03 sample */
/*        if (stp_nocin >= 8){ psigt_stp=.67;
		setcolor(7);
		}
        else
                if (stp_nocin >= 6 && stp_nocin < 8){ psigt_stp=.48;
			setcolor(2);
			}
        else
                if (stp_nocin >= 4 && stp_nocin < 6){ psigt_stp=.39;
			setcolor(2);
			}
        else
                if (stp_nocin >= 2 && stp_nocin < 4){ psigt_stp=.32;
			setcolor(19);
			}
        else
                if (stp_nocin >= 1 && stp_nocin < 2){ psigt_stp=.20;
			setcolor(31);
			}
        else
                if (stp_nocin >= .5 && stp_nocin < 1){ psigt_stp=.12;
			setcolor(18);
			}
        else
                if (stp_nocin < .5 && stp_nocin > .01){ psigt_stp=.04;
			setcolor(8);
			}
	else	
		if (stp_nocin < .01){ psigt_stp=0.0;
			setcolor(8);
			}
*/

        sprintf( st, "%.2f", psigt_stp);
        disp_param( st, tlx+335, tly+99);
        setcolor(31);
        set_font(4);
        sprintf( st, "based on STP_fixed: ");
        outgtext( st, tlx+208, tly+99);


/*        sprintf( st, "%.2f", psigt_stp);
        disp_param( st, tlx+335, tly+89);
        setcolor(31);
        set_font(4);
        sprintf( st, "based on STP: ");
        outgtext( st, tlx+208, tly+89);
*/
        }

	/* NP */
void prob_sigt_stpc()
        /*************************************************************/
        /*  PROB_SIGT_STPC                                           */
        /*  Rich Thompson SPC OUN                                    */
        /*                                                           */
        /* Calculates and plots the probability of an EF2+ tornado   */
        /* (given a supercell) based on the effective-layer STP      */
	/* including CIN.  Probabilities are derived from            */
	/* Thompson et al. (2012) convective mode sample             */
        /*************************************************************/
        {
        float psigt_stpcin;
        float stp_cin, ix1, tlx, tly;
        char st[100];
	Parcel pcl;

        setcliprgn(1,1,xwdth, xhght);

        /*tlx = skv.tlx + 731;*/
        if (display_mode_right == DISPLAY_STP_RIGHT)
                {tlx = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_STP_LEFT)
                {tlx = skv.tlx + 712;}
        tly = skv.bry + 20;

	/* calculate STPC */
        stp_cin = sigtorn_cin(st_dir, st_spd);

/* logic statements for Thompson et al. (2012) WAF sample */
        /* 39 sigtor, 28 non-sigtor supercells */
        if (stp_cin >= 10){ psigt_stpcin=.58;
		setcolor(7);
		}
        /* 39 sigtor, 32 non-sigtor supercells */
        else
                if (stp_cin >= 8 && stp_cin < 10){ psigt_stpcin=.55;
			setcolor(7);
			}			
        /* 54 sigtor, 104 non-sigtor supercells */
        else
                if (stp_cin >= 6 && stp_cin < 8){ psigt_stpcin=.34;
			setcolor(2);
			}
        /* 146 sigtor, 305 non-sigtor supercells */
        else
                if (stp_cin >= 4 && stp_cin < 6){ psigt_stpcin=.32;
			setcolor(2);
			}
        /* 219 sigtor, 842 non-sigtor supercells */
        else
                if (stp_cin >= 2 && stp_cin < 4){ psigt_stpcin=.21;
			setcolor(19);
			}
        /* 200 sigtor, 963 non-sigtor supercells */
        else
                if (stp_cin >= 1 && stp_cin < 2){ psigt_stpcin=.17;
                        setcolor(31);
                        }
        /* 112 sigtor, 823 non-sigtor supercells */
        else
                if (stp_cin >= .5 && stp_cin < 1){ psigt_stpcin=.12;
			setcolor(18);
			}
        /* 84 sigtor, 1026 non-sigtor supercells */
        else
                if (stp_cin >= .1 && stp_cin < .5){ psigt_stpcin=.08;
                        setcolor(18);
                        }
        /* 109 sigtor, 1571 non-sigtor supercells */
	else	
		if (stp_cin < .1){ psigt_stpcin=.06;
			setcolor(8);	
			}

/* original logic statements based on T03 sample */
/*      if (stp_cin >= 8){ psigt_stpcin=.78;
                setcolor(7);
                }
        else
                if (stp_cin >= 6 && stp_cin < 8){ psigt_stpcin=.53;
                        setcolor(7);
                        }
        else
                if (stp_cin >= 4 && stp_cin < 6){ psigt_stpcin=.41;
                        setcolor(2);
                        }
        else
                if (stp_cin >= 2 && stp_cin < 4){ psigt_stpcin=.27;
                        setcolor(19);
                        }
        else
                if (stp_cin >= 1 && stp_cin < 2){ psigt_stpcin=.17;
                        setcolor(31);
                        }
        else
                if (stp_cin >= .5 && stp_cin < 1){ psigt_stpcin=.11;
                        setcolor(18);
                        }
        else
                if (stp_cin < .5 && stp_cin > .01){ psigt_stpcin=.03;
                        setcolor(8);
                        }
        else
                if (stp_cin < .01){ psigt_stpcin=.00;
                        setcolor(8);
                        }
*/
        sprintf( st, "%.2f", psigt_stpcin);
        disp_param( st, tlx+335, tly+89);
        setcolor(31);
        set_font(4);
        sprintf( st, "based on STPC: ");
        outgtext( st, tlx+208, tly+89);

/*        sprintf( st, "%.2f", psigt_stpcin);
        disp_param( st, tlx+335, tly+99);
        setcolor(31);
        set_font(4);
        sprintf( st, "based on STPC: ");
        outgtext( st, tlx+208, tly+99);
*/
        }

        /*NP*/
        void show_ship_stats(void)
        /*************************************************************/
        /*  SHOW_SHIP_STATS	                                     */
        /*  Rich Thompson SPC OUN                                    */
        /*                                                           */
        /*************************************************************/
        {
        float s10th, s25th, s50th, s75th, s90th, w10th, w25th, w50th, w75th, w90th;
        float n10th, n25th, n50th, n75th, n90th, stp, maxval, ix1, ix2, ix3;
        float shr6, fzlh, ship, pres, mlcape, fzl;
        short tlx, tly, oldlplchoice, pIndex, zIndex, tIndex, trow2, i, y, hash;
	short temp_mark;
        char st[100];
        Parcel pcl;

	pIndex = getParmIndex("PRES");



        /* added 25OCT06 by RLT */
	/*display_mode = DISPLAY_SHIP;*/

        setcliprgn(1,1,xwdth, xhght);
        /* ----- Draw Bounding Box ----- */
        setlinestyle(1,1);
        /*tlx = skv.tlx + 731;*/
        if (display_mode_right == DISPLAY_SHIP_RIGHT)
                {tlx = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_SHIP_LEFT)
                {tlx = skv.tlx + 712;}
        tly = skv.bry + 20;
        setcolor(0);
        rectangle(1, tlx, tly, tlx + 350, tly + 250);
        setcolor(31);
        rectangle(0, tlx, tly, tlx + 350, tly + 250);
        moveto(tlx, tly + 250);
        lineto(tlx + 350, tly + 250);

        /* graph title */
        setcolor(31);
        set_font(6);
        sprintf( st, "Significant Hail Parameter (SHIP)");
        outgtext ( st, tlx+60, tly+1);

        /* ----- Plot Y-Coord hash marks ----- */
        maxval = 8.0;
        /* 1 STP = 20 pixels in y */
        setlinestyle(2, 1);
        for (i = 0; i <= maxval - 1; i = i + 1) {

           hash = i * (short)(240 / maxval);
           setcolor(24);
           set_font(4);
           moveto(tlx+12, tly + 240 - hash);
           lineto(tlx+350, tly + 240 - hash);
           itoa(i, st, 10);
           setcolor(1);
           outgtext(st, (tlx + 7) - (getgtextextent(st) / 2), tly + 235 - hash);
        }
        /* plot labels for each box */
        set_font(5);
        setcolor(31);
        sprintf( st, "< 2 in");
        outgtext ( st, tlx+103, tly+241);
        sprintf( st, ">= 2 in");
        outgtext ( st, tlx+244, tly+241);

        /* nonsig hail box and whiskers values */
        s90th = 1.6;
        s75th = 1.0;
/*      s50th = 2.7;  */
        s25th = 0.4;
        s10th = 0.3;
        /* plot nonsig hail box and whiskers */
        setcolor(23);
        setlinestyle(1, 3);
        /* draw box */
        rectangle(0, tlx+66, tly+(short)(240-(s75th*30)), tlx+141, tly+(short)(240-(s25th*30)));
        /* draw upper whisker */
        moveto(tlx+103, tly+(short)(240-(s75th*30)));
        lineto(tlx+103, tly+(short)(240-(s90th*30)));
        /* draw lower whisker */
        moveto(tlx+103, tly+(short)(240-(s25th*30)));
        lineto(tlx+103, tly+(short)(240-(s10th*30)));
        /* draw median */
/*      moveto(tlx+50, tly+(short)(240-(s50th*20)));
        lineto(tlx+100, tly+(short)(240-(s50th*20)));
*/
        /* sig hail box and whiskers values */
        s90th = 3.2;
        s75th = 2.5;
/*      s50th = 0.3; */
        s25th = 1.3;
        s10th = 1.0;
        /* plot sig hail box and whiskers */
        setcolor(23);
        setlinestyle(1, 3);
        /* draw box */
        rectangle(0, tlx+207, tly+(short)(240-(s75th*30)), tlx+282, tly+(short)(240-(s25th*30)));
        /* draw upper whisker */
        moveto(tlx+244, tly+(short)(240-(s75th*30)));
        lineto(tlx+244, tly+(short)(240-(s90th*30)));
        /* draw lower whisker */
        moveto(tlx+244, tly+(short)(240-(s25th*30)));
        lineto(tlx+244, tly+(short)(240-(s10th*30)));
        /* draw median */
/*      moveto(tlx+150, tly+(short)(240-(s50th*20)));
        lineto(tlx+200, tly+(short)(240-(s50th*20)));
*/

        oldlplchoice = lplvals.flag;

	/* plot height of freezing level, -20C, and -30C for Donavon hail technique */
	set_font(4);
        setlinestyle(1, 2);
        temp_mark = pres_to_pix(temp_lvl(0, &ix1 ));
        setcolor(0);
        rectangle(1, skv.tlx + (xwdth*.38), temp_mark - 13, skv.tlx + (xwdth*.38) + 72, temp_mark - 1);
        setcolor(26);
        moveto(skv.tlx + (xwdth*.38), temp_mark);
        lineto(skv.tlx + (xwdth*.38) + 25, temp_mark);
        sprintf( st, "FZL = %s", qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES))), "'", 0 ));
        outgtext(st, skv.tlx + (xwdth*.38), temp_mark - 12);

        temp_mark = pres_to_pix(temp_lvl(-20, &ix1 ));
        setcolor(0);
        rectangle(1, skv.tlx + (xwdth*.38), temp_mark - 13, skv.tlx + (xwdth*.38) + 72, temp_mark - 1);
        setcolor(26);
        moveto(skv.tlx + (xwdth*.38), temp_mark);
        lineto(skv.tlx + (xwdth*.38) + 25, temp_mark);
        sprintf( st, "-20C = %s", qc2( mtof(agl(i_hght(temp_lvl( -20, &ix1 ), I_PRES))), "'", 0 ));
        outgtext(st, skv.tlx + (xwdth*.38), temp_mark - 12);

        temp_mark = pres_to_pix(temp_lvl(-30, &ix1 ));
        setcolor(0);
        rectangle(1, skv.tlx + (xwdth*.38), temp_mark - 13, skv.tlx + (xwdth*.38) + 72, temp_mark - 1);
        setcolor(26);
        moveto(skv.tlx + (xwdth*.38), temp_mark);
        lineto(skv.tlx + (xwdth*.38) + 25, temp_mark);
        sprintf( st, "-30C = %s", qc2( mtof(agl(i_hght(temp_lvl( -30, &ix1 ), I_PRES))), "'", 0 ));
        outgtext(st, skv.tlx + (xwdth*.38), temp_mark - 12);


        /* plot sounding value of SHIP */
        /* max plotted SHIP value will be 10 */
        
        wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &shr6);
	define_parcel(4, 100);
	mlcape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	define_parcel(3, 400);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        fzlh = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));
        ship = sig_hail(pcl.bplus, mixratio(lplvals.pres, lplvals.dwpt), lapse_rate( &ix1, 700, 500 ), i_temp(500, I_PRES), 
		kt_to_mps(shr6), fzlh, pcl.bminus, 0, 0, 25, mlcape);
        if (ship > 7) ship = 7;
        y = (short)(240 - (ship * 30));
        /* set line color to match color coding of composite parameter inset */
        if (ship < .5) setcolor(8);
        if (ship >= .5) setcolor(31);
        if (ship >= 1) setcolor(19);
        if (ship >= 2) setcolor(2);
        if (ship >= 5) setcolor(7);
        setlinestyle(1, 2);
        moveto(tlx, tly + y);
        lineto(tlx + 350, tly + y);

        /* ----- Set Parcel Back ----- */
        /*if (oldlplchoice == 3) pres = mu_layer;
        else if (oldlplchoice == 4) pres = mml_layer;
        else if (oldlplchoice == 5) pres = user_level;
        else pres = mml_layer;
        define_parcel(oldlplchoice, pres);*/

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


        }

        /*NP*/
	float show_ebs_stats(void)

        /*************************************************************/
        /*  SHOW_EBS_STATS                                          */
        /*  Rich Thompson SPC OUN                                    */
        /*                                                           */
        /*************************************************************/
        {
        float s10, s20, s30, s40, s50, s60, s70, s80, s90, s100;
        float m10, m20, m30, m40, m50, m60, m70, m80, m90, m100;
        float n10, n20, n30, n40, n50, n60, n70, n80, n90, n100;
	float ebs10, ebs20, ebs30, ebs40, ebs50, ebs60, ebs70, ebs80, ebs90, ebs100;
        float maxval, ix1, ix2, ix3, ix4;
        float base, el, depth, pres, pbot, ptop;
	short x10, x20, x30, x40, x50, x60, x70, x80, x90, x100;
	short y10, y20, y30, y40, y50, y60, y70, y80, y90, y100;
	short my10, my20, my30, my40, my50, my60, my70, my80, my90, my100;
	short ny10, ny20, ny30, ny40, ny50, ny60, ny70, ny80, ny90, ny100;
        short tlx, tly, oldlplchoice, pIndex, zIndex, tIndex, trow2, i, y, hash, maxmark, mark;
        char st[100];
        Parcel pcl;

        /* added 25OCT06 by RLT */
	/*display_mode = DISPLAY_EBS;*/
	
        setcliprgn(1,1,xwdth, xhght);

        oldlplchoice = lplvals.flag;

        /* ----- Draw Bounding Box ----- */
        setlinestyle(1,1);
        /*tlx = skv.tlx + 731;*/
        if (display_mode_right == DISPLAY_EBS_RIGHT)
                {tlx = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_EBS_LEFT)
                {tlx = skv.tlx + 712;}
        tly = skv.bry + 20;
        setcolor(0);
        rectangle(1, tlx, tly, tlx + 350, tly + 250);
        setcolor(31);
        rectangle(0, tlx, tly, tlx + 350, tly + 250);
        moveto(tlx, tly + 250);
        lineto(tlx + 350, tly + 250);

        /* graph title */
        setcolor(31);
        set_font(6);
        sprintf( st, "Effective Bulk Wind Difference (kt, y axis)");
        outgtext ( st, tlx+35, tly+1);
	set_font(4);
	setcolor(23);
	sprintf( st, "supercell");
	outgtext ( st, tlx+50, tly+14);
	sprintf( st, "mrgl supercell (dashed)");
	outgtext ( st, tlx+110, tly+14);
	setcolor(18);
	sprintf( st, "non-supercell");
	outgtext ( st, tlx+235, tly+14);

        /* ----- Plot Y-Coord hash marks ----- */
        /* 1 m/s EBS = 6 pixels in y */
/*        maxval = 40.0;
        setlinestyle(2, 1);
        for (i = 0; i <= maxval - 1; i = i + 5) {

           hash = i * (short)(240 / maxval);
           setcolor(24);
           set_font(4);
           moveto(tlx+12, tly + 240 - hash);
           lineto(tlx+350, tly + 240 - hash);
           itoa(i, st, 10);
           setcolor(1);
           outgtext(st, (tlx + 7) - (getgtextextent(st) / 2), tly + 235 - hash);
        }
*/
        /* ----- Plot Y-Coord hash marks ----- */
        /* 1 kt EBS = 3 pixels in y */
        maxval = 80.0;
        setlinestyle(2, 1);
        for (i = 0; i <= maxval - 1; i = i + 10) {

           hash = i * (short)(240 / maxval);
           setcolor(24);
           set_font(4);
           moveto(tlx+12, tly + 240 - hash);
           lineto(tlx+350, tly + 240 - hash);
           itoa(i, st, 10);
           setcolor(1);
           outgtext(st, (tlx + 7) - (getgtextextent(st) / 2), tly + 235 - hash);
        }

	for (i = 10; i <= 100; i=i+10) {
		mark = (i*3.3)-12;
		setcolor(31);
		itoa(i, st, 1);
		outgtext(st, (tlx + 12 + mark - (getgtextextent(st) / 2)), tly + 240);
	}
					

        /* plot labels for each box */
/*        set_font(5);
        setcolor(31);
        sprintf( st, "< 2 in");
        outgtext ( st, tlx+103, tly+241);
        sprintf( st, ">= 2 in");
        outgtext ( st, tlx+244, tly+241);
*/	/* sb supercell mean ebs values by percentage of storm depth */
	/* values in m/s */
/*	s10 = 9.0;
	s20 = 13.8;	
	s30 = 17.2;
	s40 = 20.0;
	s50 = 22.2;
	s60 = 24.4;
	s70 = 27.0;
	s80 = 29.6;
	s90 = 31.3;
	s100 = 31.1;
*/
	/* values in kt */
        s10 = 18.0;
        s20 = 27.6;
        s30 = 34.4;
        s40 = 40.0;
        s50 = 44.4;
        s60 = 48.8;
        s70 = 54.0;
        s80 = 59.2; 
        s90 = 62.6;
        s100 = 62.2;

        /* mrgl supercell mean ebs values by percentage of storm depth */
	/* values in m/s */
/*	m10 = 6.2;
        m20 = 10.2;
        m30 = 12.8;
        m40 = 14.3;
        m50 = 16.1;
        m60 = 18.0;
        m70 = 19.9;
        m80 = 21.8;
        m90 = 23.8;
        m100 = 24.2;
*/
	/* values in kt */
        m10 = 12.4;
        m20 = 20.4;
        m30 = 25.6;
        m40 = 28.6;
        m50 = 32.2;
        m60 = 36.0;
        m70 = 39.8;
        m80 = 43.6;
        m90 = 47.6;
        m100 = 48.4;

        /* nonsupercell mean ebs values by percentage of storm depth */
/*      n10 = 4.1;
        n20 = 6.1;
        n30 = 7.2;
        n40 = 7.9;
        n50 = 8.5;
        n60 = 10.0;
        n70 = 11.6;
        n80 = 13.5;
        n90 = 15.2;
        n100 = 16.0;
*/
	/* values in kt */
        n10 = 8.2;
        n20 = 12.2;
        n30 = 14.4;
        n40 = 15.8;
        n50 = 17.0;
        n60 = 20.0;
        n70 = 23.2;
        n80 = 27.0;
        n90 = 30.4;
        n100 = 32.0;

	/* plot sb supercell ebs values by percentage of storm depth */
	setcolor(23);
	setlinestyle(1,2);
        y10 = tly+(short)(240 - (s10 * 3));
	x10 = tlx + 33;
	moveto(x10,y10);
	y20 = tly+(short)(240 - (s20 * 3));
	x20 = tlx + 66;
	lineto(x20,y20);
	moveto(x20,y20);
        y30 = tly+(short)(240 - (s30 * 3));
        x30 = tlx + 99;
        lineto(x30,y30);
        moveto(x30,y30);
        y40 = tly+(short)(240 - (s40 * 3));
        x40 = tlx + 131;
        lineto(x40,y40);
        moveto(x40,y40);
        y50 = tly+(short)(240 - (s50 * 3));
        x50 = tlx + 165;
        lineto(x50,y50);
        moveto(x50,y50);
        y60 = tly+(short)(240 - (s60 * 3));
        x60 = tlx + 198;
        lineto(x60,y60);
        moveto(x60,y60);
        y70 = tly+(short)(240 - (s70 * 3));
        x70 = tlx + 231;
        lineto(x70,y70);
        moveto(x70,y70);
        y80 = tly+(short)(240 - (s80 * 3));
        x80 = tlx + 264;
        lineto(x80,y80);
        moveto(x80,y80);
        y90 = tly+(short)(240 - (s90 * 3));
        x90 = tlx + 297;
        lineto(x90,y90);
        moveto(x90,y90);	
        y100 = tly+(short)(240 - (s100 * 3));
        x100 = tlx + 330;
        lineto(x100,y100);

        /* plot mrgl supercell ebs values by percentage of storm depth */
        setcolor(23);
        setlinestyle(2,2);
        my10 = tly+(short)(240 - (m10 * 3));
        moveto(x10,my10);
        my20 = tly+(short)(240 - (m20 * 3));
        lineto(x20,my20);
        moveto(x20,my20);
        my30 = tly+(short)(240 - (m30 * 3));
        lineto(x30,my30);
        moveto(x30,my30);
        my40 = tly+(short)(240 - (m40 * 3));
        lineto(x40,my40);
        moveto(x40,my40);
        my50 = tly+(short)(240 - (m50 * 3));
        lineto(x50,my50);
        moveto(x50,my50);
        my60 = tly+(short)(240 - (m60 * 3));
        lineto(x60,my60);
        moveto(x60,my60);
        my70 = tly+(short)(240 - (m70 * 3));
        lineto(x70,my70);
        moveto(x70,my70);
        my80 = tly+(short)(240 - (m80 * 3));
        lineto(x80,my80);
        moveto(x80,my80);
        my90 = tly+(short)(240 - (m90 * 3));
        lineto(x90,my90);
        moveto(x90,my90);
        my100 = tly+(short)(240 - (m100 * 3));
        lineto(x100,my100);


        /* plot nonsupercell ebs values by percentage of storm depth */
        setcolor(8);
        setlinestyle(1,2);
        ny10 = tly+(short)(240 - (n10 * 3));
        moveto(x10,ny10);
        ny20 = tly+(short)(240 - (n20 * 3));
        lineto(x20,ny20);
        moveto(x20,ny20);
        ny30 = tly+(short)(240 - (n30 * 3));
        lineto(x30,ny30);
        moveto(x30,ny30);
        ny40 = tly+(short)(240 - (n40 * 3));
        lineto(x40,ny40);
        moveto(x40,ny40);
        ny50 = tly+(short)(240 - (n50 * 3));
        lineto(x50,ny50);
        moveto(x50,ny50);
        ny60 = tly+(short)(240 - (n60 * 3));
        lineto(x60,ny60);
        moveto(x60,ny60);
        ny70 = tly+(short)(240 - (n70 * 3));
        lineto(x70,ny70);
        moveto(x70,ny70);
        ny80 = tly+(short)(240 - (n80 * 3));
        lineto(x80,ny80);
        moveto(x80,ny80);
        ny90 = tly+(short)(240 - (n90 * 3));
        lineto(x90,ny90);
        moveto(x90,ny90);
        ny100 = tly+(short)(240 - (n100 * 3));
        lineto(x100,ny100);


        /* nonsig hail box and whiskers values */
/*      s90th = 2.3;
        s75th = 1.0;
        s50th = 2.7;  
        s25th = 0.3;
        s10th = 0.2;
*/      /* plot nonsig hail box and whiskers */
/*      setcolor(23);
        setlinestyle(1, 3);
*/      /* draw box */
/*      rectangle(0, tlx+66, tly+(short)(240-(s75th*30)), tlx+141, tly+(short)(240-(s25th*30)));
*/      /* draw upper whisker */
/*        moveto(tlx+103, tly+(short)(240-(s75th*30)));
        lineto(tlx+103, tly+(short)(240-(s90th*30)));
*/        /* draw lower whisker */
/*        moveto(tlx+103, tly+(short)(240-(s25th*30)));
        lineto(tlx+103, tly+(short)(240-(s10th*30)));
*/        /* draw median */
/*      moveto(tlx+50, tly+(short)(240-(s50th*20)));
        lineto(tlx+100, tly+(short)(240-(s50th*20)));
*/

        /* plot sounding value of EBS every 10% of storm depth */
        /* max plotted EBS value will be 40 m/s */

/* 24 Mar 2008 */
/*	effective_inflow_layer(100, -250, &pbot, &ptop);
	base = agl(i_hght(pbot, I_PRES));
	if (pbot < 0.0){*/

        base = agl(i_hght(p_bot, I_PRES));
        if (p_bot < 0.0){
		setcolor(5);
        	set_font(6);
        	sprintf( st, "No Effective Inflow Layer");
        	outgtext ( st, tlx+90, tly+215);
		}

        define_parcel(3,400);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);

        el = agl(i_hght(pcl.elpres, I_PRES));
        depth = (el - base);
	if (pcl.bplus < 100.0) return -999.0;

	/* plot sounding values in kt */
        setcolor(5);
        setlinestyle(1,3);
/* 24 Mar 2008 */
/* all "p_bot" below were changed from "pbot" */
        wind_shear(p_bot, i_pres(msl(base + (depth * 0.1))), &ix1, &ix2, &ix3, &ix4);
        ebs10 = ix4;
	if (ebs10 > 70.0) ebs10 = 70.0;
        moveto(x10, tly+(short)(240 - (ebs10 * 3)));
/*	if (ebs10 > 70.0) ebs10 = 70.0;
        lineto(x10, tly+(short)(240 - (ebs10 * 3)));
        if (ebs10 == 70.0) setlinestyle(2,3);
*/      wind_shear(p_bot, i_pres(msl(base + (depth * 0.2))), &ix1, &ix2, &ix3, &ix4);
        ebs20 = ix4;
        if (ebs20 > 70.0) ebs20 = 70.0;
        if ((ebs10 == 70.0) && (ebs20 == 70.0)) setlinestyle(2,3);
        if ((ebs10 == 70.0) && (ebs20 < 70.0)) setlinestyle(1,3);
        lineto(x20, tly+(short)(240 - (ebs20 * 3)));
        moveto(x20, tly+(short)(240 - (ebs20 * 3)));
        wind_shear(p_bot, i_pres(msl(base + (depth * 0.3))), &ix1, &ix2, &ix3, &ix4);
        ebs30 = ix4;
        if (ebs30 > 70.0) ebs30 = 70.0;
        if ((ebs20 == 70.0) && (ebs30 == 70.0)) setlinestyle(2,3);
        if ((ebs20 == 70.0) && (ebs30 < 70.0)) setlinestyle(1,3);
        lineto(x30, tly+(short)(240 - (ebs30 * 3)));
        moveto(x30, tly+(short)(240 - (ebs30 * 3)));
        wind_shear(p_bot, i_pres(msl(base + (depth * 0.4))), &ix1, &ix2, &ix3, &ix4);
        ebs40 = ix4;
        if (ebs40 > 70.0) ebs40 = 70.0;
        if ((ebs30 == 70.0) && (ebs40 == 70.0)) setlinestyle(2,3);
        if ((ebs30 == 70.0) && (ebs40 < 70.0)) setlinestyle(1,3);
        lineto(x40, tly+(short)(240 - (ebs40 * 3)));
        moveto(x40, tly+(short)(240 - (ebs40 * 3)));
        wind_shear(p_bot, i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &ix4);
        ebs50 = ix4;
        if (ebs50 > 70.0) ebs50 = 70.0;
        if ((ebs40 == 70.0) && (ebs50 == 70.0)) setlinestyle(2,3);
        if ((ebs40 == 70.0) && (ebs50 < 70.0)) setlinestyle(1,3);
        lineto(x50, tly+(short)(240 - (ebs50 * 3)));
        moveto(x50, tly+(short)(240 - (ebs50 * 3)));
        wind_shear(p_bot, i_pres(msl(base + (depth * 0.6))), &ix1, &ix2, &ix3, &ix4);
        ebs60 = ix4;
        if (ebs60 > 70.0) ebs60 = 70.0;
        if ((ebs50 == 70.0) && (ebs60 == 70.0)) setlinestyle(2,3);
        if ((ebs50 == 70.0) && (ebs60 < 70.0)) setlinestyle(1,3);
        lineto(x60, tly+(short)(240 - (ebs60 * 3)));
        moveto(x60, tly+(short)(240 - (ebs60 * 3)));
        wind_shear(p_bot, i_pres(msl(base + (depth * 0.7))), &ix1, &ix2, &ix3, &ix4);
        ebs70 = ix4;
        if (ebs70 > 70.0) ebs70 = 70.0;
        if ((ebs60 == 70.0) && (ebs70 == 70.0)) setlinestyle(2,3);
        if ((ebs60 == 70.0) && (ebs70 < 70.0)) setlinestyle(1,3);
        lineto(x70, tly+(short)(240 - (ebs70 * 3)));
        moveto(x70, tly+(short)(240 - (ebs70 * 3)));
        wind_shear(p_bot, i_pres(msl(base + (depth * 0.8))), &ix1, &ix2, &ix3, &ix4);
        ebs80 = ix4;
        if (ebs80 > 70.0) ebs80 = 70.0;
        if ((ebs70 == 70.0) && (ebs80 == 70.0)) setlinestyle(2,3);
        if ((ebs70 == 70.0) && (ebs80 < 70.0)) setlinestyle(1,3);
        lineto(x80, tly+(short)(240 - (ebs80 * 3)));
        moveto(x80, tly+(short)(240 - (ebs80 * 3)));
        wind_shear(p_bot, i_pres(msl(base + (depth * 0.9))), &ix1, &ix2, &ix3, &ix4);
        ebs90 = ix4;
        if (ebs90 > 70.0) ebs90 = 70.0;
        if ((ebs80 == 70.0) && (ebs90 == 70.0)) setlinestyle(2,3);
        if ((ebs80 == 70.0) && (ebs90 < 70.0)) setlinestyle(1,3);
        lineto(x90, tly+(short)(240 - (ebs90 * 3)));
        moveto(x90, tly+(short)(240 - (ebs90 * 3)));
        wind_shear(p_bot, i_pres(msl(base + (depth * 1.0))), &ix1, &ix2, &ix3, &ix4);
        ebs100 = ix4;
        if (ebs100 > 70.0) ebs100 = 70.0;
        if ((ebs90 == 70.0) && (ebs100 == 70.0)) setlinestyle(2,3);
        if ((ebs90 == 70.0) && (ebs100 < 70.0)) setlinestyle(1,3);
        lineto(x100, tly+(short)(240 - (ebs100 * 3)));

        /* plot sounding values in m/s */
/*      setcolor(5);
        setlinestyle(1,3);
        wind_shear(pbot, i_pres(msl(base + (depth * 0.1))), &ix1, &ix2, &ix3, &ix4);
        ebs10 = kt_to_mps(ix4);
        moveto(x10, tly+(short)(240 - (ebs10 * 6)));
        wind_shear(pbot, i_pres(msl(base + (depth * 0.2))), &ix1, &ix2, &ix3, &ix4);
        ebs20 = kt_to_mps(ix4);
        lineto(x20, tly+(short)(240 - (ebs20 * 6)));
        moveto(x20, tly+(short)(240 - (ebs20 * 6)));
        wind_shear(pbot, i_pres(msl(base + (depth * 0.3))), &ix1, &ix2, &ix3, &ix4);
        ebs30 = kt_to_mps(ix4);
        lineto(x30, tly+(short)(240 - (ebs30 * 6)));
        moveto(x30, tly+(short)(240 - (ebs30 * 6)));
        wind_shear(pbot, i_pres(msl(base + (depth * 0.4))), &ix1, &ix2, &ix3, &ix4);
        ebs40 = kt_to_mps(ix4);
        lineto(x40, tly+(short)(240 - (ebs40 * 6)));
        moveto(x40, tly+(short)(240 - (ebs40 * 6)));
        wind_shear(pbot, i_pres(msl(base + (depth * 0.5))), &ix1, &ix2, &ix3, &ix4);
        ebs50 = kt_to_mps(ix4);
        lineto(x50, tly+(short)(240 - (ebs50 * 6)));
        moveto(x50, tly+(short)(240 - (ebs50 * 6)));
        wind_shear(pbot, i_pres(msl(base + (depth * 0.6))), &ix1, &ix2, &ix3, &ix4);
        ebs60 = kt_to_mps(ix4);
        lineto(x60, tly+(short)(240 - (ebs60 * 6)));
        moveto(x60, tly+(short)(240 - (ebs60 * 6)));
        wind_shear(pbot, i_pres(msl(base + (depth * 0.7))), &ix1, &ix2, &ix3, &ix4);
        ebs70 = kt_to_mps(ix4);
        lineto(x70, tly+(short)(240 - (ebs70 * 6)));
        moveto(x70, tly+(short)(240 - (ebs70 * 6)));
        wind_shear(pbot, i_pres(msl(base + (depth * 0.8))), &ix1, &ix2, &ix3, &ix4);
        ebs80 = kt_to_mps(ix4);
        lineto(x80, tly+(short)(240 - (ebs80 * 6)));
        moveto(x80, tly+(short)(240 - (ebs80 * 6)));
        wind_shear(pbot, i_pres(msl(base + (depth * 0.9))), &ix1, &ix2, &ix3, &ix4);
        ebs90 = kt_to_mps(ix4);
        lineto(x90, tly+(short)(240 - (ebs90 * 6)));
        moveto(x90, tly+(short)(240 - (ebs90 * 6)));
        wind_shear(pbot, i_pres(msl(base + (depth * 1.0))), &ix1, &ix2, &ix3, &ix4);
        ebs100 = kt_to_mps(ix4);
        lineto(x100, tly+(short)(240 - (ebs100 * 6)));
*/

        /* ----- Set Parcel Back ----- */
        /*if (oldlplchoice == 3) pres = mu_layer;
        else if (oldlplchoice == 4) pres = mml_layer;
        else if (oldlplchoice == 5) pres = user_level;
        else pres = mml_layer;
        define_parcel(oldlplchoice, pres);*/

        }

        /*NP*/
        void show_fire(void)
        /*************************************************************/
        /*  SHOW_FIRE                                                */ 
        /*  Rich Thompson SPC OUN                                    */ 
        /*                                                           */
        /*************************************************************/
        {
        float ix1, ix2, ix3, ix4, pres, h1, h2, p1, p2, sfctemp, sfcdwpt, sfcpres, sfcrh;
        short tlx, tly, oldlplchoice, pIndex, zIndex, tIndex, trow2, i, y, hash;
	short txtlin, txtrow;
        char st[100];   
        Parcel pcl;

	oldlplchoice = lplvals.flag;

        /* added 25OCT06 by RLT */
        /*display_mode = DISPLAY_FIRE;*/

        tIndex = getParmIndex("TEMP");
        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");

        setcliprgn(1,1,xwdth, xhght);

        /* ----- Draw Bounding Box ----- */
        setlinestyle(1,1);
        /*tlx = skv.tlx + 731;*/
        if (display_mode_right == DISPLAY_FIRE_RIGHT)
                {tlx = skv.tlx + 1066;}
        if (display_mode_left == DISPLAY_FIRE_LEFT)
                {tlx = skv.tlx + 712;}
        tly = skv.bry + 20;

        setcolor(0);
        rectangle(1, tlx, tly, tlx + 350, tly + 250);
        setcolor(31);
        rectangle(0, tlx, tly, tlx + 350, tly + 250);
        moveto(tlx, tly + 250); 
        lineto(tlx + 350, tly + 250);

        /* graph title */
        setcolor(31);
        set_font(6); 
        sprintf( st, "Fire Weather Parameters");
        outgtext ( st, tlx+85, tly+5);  

	/* graph subtitles */
	tly += 30;
	setcolor(22);
	set_font(6);
        sprintf( st, "Moisture");
        outgtext ( st, tlx+35, tly+1);
	setcolor(25);
        sprintf( st, "Low-Level Wind");
        outgtext ( st, tlx+200, tly+1);
	setcolor(31);
        moveto(tlx, tly + 20);
        lineto(tlx + 350, tly + 20);
	setcolor(17);
        sprintf( st, "Derived Indices");
        outgtext ( st, tlx+120, tly+140);
	moveto(tlx, tly + 160);
        lineto(tlx + 350, tly + 160);

	define_parcel(1, 0);
        ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        sfctemp = lplvals.temp;
        sfcdwpt = lplvals.dwpt;
        sfcpres = lplvals.pres;

        tlx += 35;
        tly += 30;
	p1 = sndg[sfc()][pIndex];
	p2 = sndg[sfc()][pIndex];
	set_font(6);
        relh(-1, &ix3);
	sfcrh = ix3;
        if (ix3 >=  0) setcolor(7);
        if (ix3 > 10) setcolor(2);
        if (ix3 > 15) setcolor(19);
        if (ix3 > 20) setcolor(31);
        if (ix3 > 30) setcolor(18);
        if (ix3 > 35) setcolor(8);
        sprintf( st, "SFC RH  = %s", qc2(ix3, "%", 0));
        outgtext( st, tlx, tly);
	setcolor(31);
	set_font(4);
        mean_wind( p1, p2, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "SFC = %4.0f/%.0f", ix3, ix4);
        disp_param( st, tlx+270, tly);

	h2 = 1000;	
        tly += 20;
	sprintf( st, "0-1 km RH  = %s", qc2( mean_relhum(&ix3, p1, i_pres(msl(h2))), "%", 0 ));
        outgtext( st, tlx, tly);
        mean_wind( p1, i_pres(msl(h2)), &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "0-1 km mean = %4.0f/%.0f", ix3, ix4);
        disp_param( st, tlx+270, tly);

	tly += 20;
/*	p2 = pcl.lclpres;
        sprintf( st, "Sfc-LCL RH  = %s", qc2( mean_relhum(&ix3, p1, p2), "%", 0 ));
        outgtext( st, tlx, tly);
        mean_wind( p1, p2, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "Sfc-LCL mean = %4.0f/%.0f", ix3, ix4);
        disp_param( st, tlx+270, tly);
*/

	pbl_top(&p2);
        sprintf( st, "BL mean RH  = %s", qc2( mean_relhum(&ix3, p1, p2), "%", 0 ));
        outgtext( st, tlx, tly);
        mean_wind( p1, p2, &ix1, &ix2, &ix3, &ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else
                sprintf( st, "BL mean = %4.0f/%.0f", ix3, ix4);
        disp_param( st, tlx+270, tly);

	tly += 20;
	define_parcel(3, 500);
	parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        precip_water(&ix1, -1, -1);
	if ((ix1 < 0.5) && (pcl.bplus > 50) && (sfcrh < 35)){ 
		set_font(6);
		setcolor(2);
		}
        sprintf( st, "PW  = %s", qc2( ix1, "in", 2 ));
        outgtext( st, tlx, tly);
	setcolor(31);
	set_font(6);
 	max_wind(&ix1, &ix2, &ix3, -1, p2);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else{
                if (ix3 >=  0) setcolor(8);
                if (ix3 > 10) setcolor(18);
                if (ix3 > 20) setcolor(31);
                if (ix3 > 30) setcolor(19);
                if (ix3 > 40) setcolor(2);
                if (ix3 > 50) setcolor(7);
                sprintf( st, "BL max = %4.0f/%.0f", ix2, ix3);
		}
        disp_param( st, tlx+270, tly);	  	


        tly += 85;
	ix3 = fosberg(&ix4);
           if (ix3 < 0)
                  strcpy( st, qc2(ix3, "", 0));
           else {
		if (ix3 < 30) setcolor(8);
        	if (ix3 >= 30) setcolor(18);
		if (ix3 >= 40) setcolor(31);
        	if (ix3 >= 50) setcolor(19);
        	if (ix3 >= 60) setcolor(2);
        	if (ix3 >= 70) setcolor(7);

                sprintf( st, "Fosberg FWI = %4.0f", ix3);
		}
        disp_param( st, tlx+205, tly);
	

	/* parcel setback */
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

	}

void AdvanceFrame(void)
	{
	int fcount, maxf, ier;

	/* Interrogate currently displayed frame */
/*	printf( "\n\n\n\n\n\n\n\nCurrently viewing Time #%d of %d (%s).\n", curdatatype_ptr->timeptr, curdatatype_ptr->ntimes, curdatatype_ptr->time);
*/
	fcount = curdatatype_ptr->timeptr + 1;
	maxf   = curdatatype_ptr->ntimes;
	if (fcount > maxf) fcount=1;

	/* Set frame to be the very next item in the list */
	curdatatype_ptr->timeptr = fcount;
	strcpy(curdatatype_ptr->time, curdatatype_ptr->timelist[fcount-1]);
/*	printf( "Changing to Time #%d of %d (%s).\n", curdatatype_ptr->timeptr, curdatatype_ptr->ntimes, curdatatype_ptr->time);
*/
	/* Load the sounding */
	ier = load_sounding(curdatatype_ptr->stype);
	if (ier != 0) AdvanceFrame();
	}

void BackFrame(void)
	{
        int fcount, maxf, ier;

        /* Interrogate currently displayed frame */
/*      printf( "\n\n\n\n\n\n\n\nCurrently viewing Time #%d of %d (%s).\n", curdatatype_ptr->timeptr, curdatatype_ptr->ntimes, curdatatype_ptr->time);
*/
        fcount = curdatatype_ptr->timeptr - 1;
        maxf   = curdatatype_ptr->ntimes;
        if (fcount < 1) fcount=maxf;

        /* Set frame to be the very next item in the list */
        curdatatype_ptr->timeptr = fcount;
        strcpy(curdatatype_ptr->time, curdatatype_ptr->timelist[fcount-1]);
/*      printf( "Changing to Time #%d of %d (%s).\n", curdatatype_ptr->timeptr, curdatatype_ptr->ntimes, curdatatype_ptr->time);
*/
        /* Load the sounding */
        ier = load_sounding(curdatatype_ptr->stype);
	if (ier != 0) BackFrame();

	}

void autoload(void)
/*************************************************************/
/* AUTOLOAD                                                  */
/*************************************************************/
        {
        int ret;
        float sfcw, mucape;
        short tIndex, tdIndex, pIndex, i;
        Parcel pcl;

        printf( "**********\nAutoLoading Sounding Now!\n**********\n" );
        printf( "   Filename:   %s\n", autostart.filename);
        printf( "   Date/Time:  %s\n", autostart.dattim);
        printf( "   Station:    %s\n", autostart.station);
        printf( "   GIF    :    %s\n", autostart.giffile);
        printf( "**********\n");

        setconfigdatapointer(0);
        printf( "    CURDATA filename:  %s\n", curdatatype_ptr->filename);
        curdatatype_ptr->station[0] = '\0';
        curdatatype_ptr->filename[0] = '\0';
        curdatatype_ptr->time[0] = '\0';

        strcpy(curdatatype_ptr->filename, autostart.filename);
        strcpy(curdatatype_ptr->time, autostart.dattim);
        strcpy(curdatatype_ptr->station, autostart.station);
        curdatatype_ptr->stype = NSHARP_OBS;

        ret = load_sounding(curdatatype_ptr->stype);
        /* ret = load_sounding2(curdatatype_ptr->filename, 
                curdatatype_ptr->time, 
                curdatatype_ptr->station, 
                curdatatype_ptr->stype); */
/*      printf( "   LOAD_SOUNDING return value = %d\n", ret);
        printf( "   Preparing to create GIF image.\n");
*/
        pIndex = getParmIndex("PRES");
        tIndex = getParmIndex("TEMP");
        tdIndex = getParmIndex("DWPT");

        /* ----- Check whether this meets the winter criteria.  If so, plot in that mode ----- */
        define_parcel(3, 300);
        ret = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
        mucape = pcl.bplus;
        sfcw = ctof(wetbulb(sndg[sfc()][pIndex], sndg[sfc()][tIndex], sndg[sfc()][tdIndex]));
/*     	printf( "Sfc Wetbulb Temperature = %.1f\n", sfcw);
       	printf( "MUCAPE                  = %.0f\n", mucape);
*/
	if ((sfcw < 35) && (mucape < 300)) 
		{
		/* printf( "Switching to Winter Mode!!!!!! \n"); */
		display_mode_left = DISPLAY_WINTER_LEFT;
        	draw_skewt();
        	draw_hodo();
        	show_skewtpage1();
		}
       	ret = save_gif(autostart.giffile);
        exit(0);
        }

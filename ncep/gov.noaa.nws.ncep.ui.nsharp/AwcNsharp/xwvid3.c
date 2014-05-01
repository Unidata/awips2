/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  DOS Video Graphics Routines (Part #3)                      */
/*  These routines are used to display the various indices and */
/*  parameters in the right-side area.                         */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  SHOW_THERMOPARMS                                           */
/*  SHOW_MOISTURE                                              */
/*  SHOW_INSTABILITY                                           */
/*  SHOW_PARCEL                                                */
/*  CLEAR_PARAMAREA                                            */
/*  SHOW_SKEWTPAGE1                                            */
/*  SHOW_HODOPAGE2                                             */
/*  SHOW_SHEAR                                                 */
/*  SHOW_STORMPAGE3                                            */
/*  SHOW_PAGE                                                  */
/*  SHOW_STORMTYPE                                             */
/*  SHOW_SRDATA                                                */
/*  SHOW_INITIATION                                            */
/*  SHOW_SEVERE                                                */
/*  SHOW_HAILPOT                                               */
/*                                                             */
/*Log:                                                         */
/* L.Hinson   3/03  Initialized ix1                            */
/***************************************************************/

#define VIDEO
#include <sharp95.h>
#include <xwcmn.h>

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
	float ix1, ix2, ix3;
	short txtlin, txtrow;
	char st[80];
	int fzgstatus,nparams;
	float params[2];

	/* ----- Thermo Data Area ----- */
	ix1=0.0;
	setcolor(3);
	set_font(1);
	txtlin = 350;
	txtlin = 335;
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
	txtlin += 7 * 21+5;
	outgtext ( st, txtrow, txtlin );
	setcolor(31);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "Total Totals =", txtrow, txtlin );
	strcpy( st, qc2( t_totals( &ix1, &ix2, &ix3 ), "", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "K-Index =", txtrow, txtlin );
	strcpy( st, qc2( k_index( &ix1 ), "", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "SWEAT Index =", txtrow, txtlin );
	strcpy( st, qc2( sweat_index( &ix1 ), "", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "Max Temp =", txtrow, txtlin );
	strcpy( st, qc2( ctof(max_temp( &ix1, -1)), "F", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "ThetaE Diff =", txtrow, txtlin );
	strcpy( st, qc2( ThetaE_diff( &ix1 ), "C", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "ConvTemp(s)=", txtrow, txtlin );
	/* strcpy( st, qc2( ctof(cnvtv_temp( &ix1, -50 )), "F", 0 )); */
        strcpy( st, qc2( ctof(old_cnvtv_temp( &ix1 )), "F/", 0 ));
	strcat( st, qc2(ctof(cnvtv_temp( & ix1, -50)), "F", 0 ));
	disp_param( st, txtrow + 173, txtlin);

	setcolor(31);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "WBZ level =", txtrow, txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 )))), "ft", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "FZG LVL(AGL)=", txtrow, txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 )))), "ft", 0 ));
	disp_param( st, txtrow + 175, txtlin);
	
	txtrow = skv.brx + 25;
	txtlin += 18;
	/*outgtext ( "FZG LVL(MSL)=", txtrow, txtlin);*/
	fzgstatus=mult_temp_lvl(0.0,params,&nparams);
	if (fzgstatus<0) {
	  strcpy( st, "FZG LVL(MSL) = SNDG BLO FZG");
	  outgtext(st, txtrow, txtlin);
	} else {
	  strcpy( st, "FZG LVL(MSL) = ");
	  strcat( st, qc2(mtof(i_hght(params[0])),"ft",0));
	  if( nparams==2) {
	    strcat(st,",");
	    strcat(st, qc2(mtof(i_hght(params[1])),"ft",0));
	  }
	  outgtext(st, txtrow,txtlin);
	}
	
	/* txtrow += 210;
	outgtext ( "FZG LVL =", txtrow, txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 )))), "ft", 0 ));
	disp_param( st, txtrow + 165, txtlin); */

	
	}

	/*NP*/
	void show_moisture( void )
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
	float ix1=-999.;
	short txtlin, txtrow;
	char st[40];

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(5);
	strcpy( st, "------ AVAILABLE MOISTURE ------" );
	txtrow = (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st )/2));
	txtlin = 375;
    txtlin = 360;
	outgtext ( st, txtrow, txtlin );

	txtrow = skv.brx + 25;
	txtlin += 18;
	setcolor(31);
	outgtext ( "P. Water =", txtrow, txtlin );
	strcpy( st, qc2( precip_water( &ix1, -1, -1), " in", 2 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "Mean RH =", txtrow, txtlin );
	strcpy( st, qc2( mean_relhum( &ix1, -1, -1), " %" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow = skv.brx + 25;
	txtlin += 16;
	outgtext ( "Mean W =", txtrow, txtlin );
	strcpy( st, qc2( mean_mixratio( &ix1, -1, -1 ), " g/Kg", 1 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "Mean LRH =", txtrow, txtlin );
	strcpy( st, qc2( mean_relhum( &ix1, -1, sndg[sfc()][1] - 150 ), " %", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow = skv.brx + 25;
	txtlin += 16;
	outgtext ( "Top of Moist Lyr =", txtrow, txtlin );
	strcpy( st, qc2( top_moistlyr( &ix1 ), " mb", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( mtof(agl(i_hght(ix1))), " ft", 0 ));
	outgtext ( st, txtrow+220, txtlin );
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
	txtlin = 320 + ( 5 * 22);
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
	float ix1, sfctemp, sfcdwpt, sfcpres;
	short txtlin, txtrow;
	char st[40], st1[20];
	struct _parcel pcl;

	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	setcliprgn ( skv.tlx, skv.tly, xwdth, xhght );

	/* ----- Plot Parcel Trace on Skew-T ----- */
	setcolor(31);
	setlinestyle( 4, 1 );
	trace_parcel( sfcpres, sfctemp, sfcdwpt);

	/* ----- Calculate Parcel Data ----- */
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

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
	strcat( st, "mb  ");
	itoa( (short)pcl.lpltemp, st1, 10);
	strcat( st, st1 );
	strcat( st, "C/");
	itoa( (short)pcl.lpldwpt, st1, 10);
	strcat( st, st1 );
	strcat( st, "C  ");
	itoa( (short)ctof(pcl.lpltemp), st1, 10);
	strcat( st, st1 );
	strcat( st, "F/");
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
	outgtext ( "BFZL =", txtrow, txtlin );
	strcpy( st, qc2( pcl.bfzl, " J/Kg", 0 ));
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
	outgtext ( "LEVEL  PRES  HGT(AGL)  HGT(MSL)  TEMP", txtrow, txtlin );
	txtlin += 2;
	outgtext ( "_________________________________________", 
		    txtrow, txtlin );

	setcolor(31);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "LCL", txtrow, txtlin );
	strcpy( st, qc2( pcl.lclpres, "mb", 0 ));
	disp_param(st, txtrow+104,txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.lclpres ))), "ft", 0 ));
	disp_param(st, txtrow + 186, txtlin);
	strcpy( st, qc2( mtof(i_hght(pcl.lclpres )), "ft", 0 ));
	disp_param(st, txtrow+278, txtlin);
	setcolor(31);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "LFC", txtrow, txtlin );
	strcpy( st, qc2( pcl.lfcpres, "mb", 0 ));
	disp_param( st, txtrow + 104, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.lfcpres ))), "ft", 0 ));
	disp_param( st, txtrow + 186, txtlin);
	strcpy( st, qc2( mtof(i_hght(pcl.lfcpres )), "ft", 0 ));
	disp_param( st, txtrow + 278, txtlin);
	strcpy( st, qc2( i_temp(pcl.lfcpres ), "C", 0 ));
	disp_param( st, txtrow + 331, txtlin);

	setcolor(31);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "EL", txtrow, txtlin );
	strcpy( st, qc2( pcl.elpres, "mb", 0 ));
	disp_param( st, txtrow + 104, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.elpres ))), "ft", 0 ));
	disp_param( st, txtrow + 186, txtlin);
        strcpy( st, qc2( mtof(i_hght(pcl.elpres )), "ft", 0 ));
	disp_param( st, txtrow + 278, txtlin);
	strcpy( st, qc2( i_temp(pcl.elpres ), "C", 0 ));
	disp_param( st, txtrow + 331, txtlin);

	setcolor(31);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "MPL", txtrow, txtlin );
	strcpy( st, qc2( pcl.mplpres, "mb", 0 ));
	disp_param( st, txtrow + 104, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.mplpres ))), "ft", 0 ));
	disp_param( st, txtrow + 186, txtlin);
	strcpy( st, qc2( mtof(i_hght(pcl.mplpres )), "ft", 0 ));
	disp_param( st, txtrow + 278, txtlin);
	}


	/*NP*/
	void clear_paramarea( void )
	/*************************************************************/
	/*  CLEAR_PARAMAREA                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Clear right side of screen (parameter area).             */
	/*                                                           */
	/*************************************************************/
	{
	setcolor(4);
	setcliprgn(1, 1, xwdth, xhght);
	rectangle( 1, skv.brx + 21, skv.tly + 77, xwdth-5, xhght-5);
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
	clear_paramarea();
	show_parcel();
	show_thermoparms();
	}

	/*NP*/
	void show_hodopage2( void )
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
        void show_heavysnow(void)
        /*************************************************************/
        /*  SHOW_HEAVYSNOW                                           */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Displays heavy snowfall parameters.                      */
        /*************************************************************/
        {
        float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres;
        short txtlin, txtrow, anc_x, anc_y;
        char st[40], st1[20];
        int notyet=1;

        if(notyet > 0) return;

        /* ----- Set anchor pixel ----- */
        anc_x = skv.brx + 20;
        anc_y = 375;

        /* ----- Heavy Precip Area ----- */
        setcolor(3);
        set_font(1);
        setlinestyle( 1, 1 );
        strcpy( st, "HEAVY SNOWFALL" );
        txtrow = (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2));
        txtlin = anc_y;
        outgtext ( st, txtrow, txtlin );
        setcolor(0);
        rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
        setcolor(7);
        rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
        
        set_font(2);
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
        char st[40], st1[20];

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
        outgtext ( "Melting Level =", txtrow, txtlin );
        strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 )))), "ft /", 0 ));
        strcat( st, qc2( wb_lvl(0, &ix1), " mb", 0));
        strcat( st, "\0");
        disp_param( st, txtrow + 300, txtlin);
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
	short txtlin, txtrow, anc_x, anc_y;
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
	outgtext( "Low - 3 km", txtrow, txtlin );
	/* wind_shear( -1, i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4); */
	wind_shear( -1, i_pres(msl(3000)), &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf( st, "(%s)", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param( st, txtrow + 245, txtlin);
	/* strcpy( st, qc2( kt_to_mps(ix4)/.6, "" , 2)); */
	strcpy( st, qc2( kt_to_mps(ix4)/.3, "" , 2));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext( "Sfc - 2 km",txtrow, txtlin );
	wind_shear( -1, i_pres(msl(2000)), &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf( st, "(%s)\0", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param( st, txtrow + 245, txtlin);
	strcpy( st, qc2( kt_to_mps(ix4)/.2, "" , 2));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext( "Sfc - 6 km", txtrow, txtlin );
	wind_shear( -1, i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf( st, "(%s)", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param( st, txtrow + 245, txtlin);
	strcpy( st, qc2( kt_to_mps(ix4)/.6, "" , 2));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext( "Sfc - 12 km", txtrow, txtlin );
	wind_shear( -1, i_pres(msl(12000)), &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf( st, "(%s)", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param( st, txtrow + 245, txtlin);
        if(qc(ix4))
	   strcpy( st, qc2( kt_to_mps(ix4)/1.2, "" , 2));
        else
           strcpy( st, qc2( ix4, "" , 2));
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
        show_heavysnow();
        }



	/*NP*/
	void show_page( short page )
	/*************************************************************/
	/*  SHOW_PAGE                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays the given parameter list on the screen.         */
	/*  (1=Skewt, 2=Hodo, 3=Storm, 4=Precip).                    */
	/*************************************************************/
	{
	if( page == 1 )
	   {
	   show_skewtpage1();
	   return;
	   }
	if( page == 2 )
	   {
	   show_hodopage2();
	   return;
	   }
	if( page == 3 )
	   {
	   show_stormpage3();
	   return;
	   }
        if( page == 4 )
           {
           show_precippage4();
           return;
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
	struct _parcel pcl;

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
	outgtext( "Sfc - 6 km", txtrow, txtlin );
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


	/*NP*/
	void show_srdata( void )
	/*************************************************************/
	/*  SHOW_SRDATA                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays storm-relative data.                            */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4, ix5;
	short txtlin, txtrow, anc_x, anc_y;
	char st[40], st1[20], stval[20];

	/* ----- Set anchor pixel ----- */
	anc_x = skv.brx + 20;
	anc_y = 110;

	/* ----- Storm Relative Header ----- */
	setcolor(3);
	set_font(1);
	setlinestyle( 1, 1 );
	strcpy( st, "STORM  RELATIVE" );
	outgtext( st, 
	  (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2)), anc_y );
	setcolor(0);
	rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 250);
	setcolor(7);
	rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 250);

	/* ----- Display Values on screen ----- */
	set_font(2);
	txtrow = anc_x + 10;
	txtlin = anc_y + 25;

	/* ----- Display Storm Motion ----- */
	setcolor(31);
	sprintf( st, "%3.0f%c / %3.0f kt    ( %.0f m/s )", st_dir, 176,
		 st_spd, kt_to_mps(st_spd));
	outgtext( st,
	  (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2)), txtlin);

	/* ----- Storm Relative Helicity ----- */
	txtlin += 20;
	setcolor(5);
	strcpy( st, "------ SR HELICITY ------" );
	outgtext( st,
	  (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2)), txtlin);

	setcolor(31);
	txtlin += 20;
	outgtext( "LAYER        POS   NEG        TOT",
		txtrow, txtlin );
	txtlin += 2;
	outgtext( "________________________________________", txtrow, txtlin );

	txtlin += 15;
	outgtext( "Sfc - 1 km", txtrow, txtlin );
	ix1 = helicity( 0, 1000, st_dir, st_spd, &ix2, &ix3);
	strcpy( st, qc2(ix2, "", 0 ));
	disp_param( st, txtrow + 145, txtlin);
	strcpy( st, qc2(ix3, "", 0 ));
	disp_param( st, txtrow + 195, txtlin);
	sprintf(stval, " m%c/s%c",178, 178 );
	strcpy( st, qc2(ix1, stval, 0 ));
	disp_param( st, txtrow + 315, txtlin);


	txtlin += 15;
	outgtext( "Sfc - 2 km", txtrow, txtlin );
	ix1 = helicity( 0, 2000, st_dir, st_spd, &ix2, &ix3);
	strcpy( st, qc2(ix2, "", 0 ));
	disp_param( st, txtrow + 145, txtlin);
	strcpy( st, qc2(ix3, "", 0 ));
	disp_param( st, txtrow + 195, txtlin);
	sprintf(stval, " m%c/s%c",178, 178 );
	strcpy( st, qc2(ix1, stval, 0 ));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 15;
	outgtext( "Sfc - 3 km", txtrow, txtlin );
	ix1 = helicity( 0, 3000, st_dir, st_spd, &ix2, &ix3);
	strcpy( st, qc2(ix2, "", 0 ));
	disp_param( st, txtrow + 145, txtlin);
	strcpy( st, qc2(ix3, "", 0 ));
	disp_param( st, txtrow + 195, txtlin);
	strcpy( st, qc2(ix1, stval, 0 ));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 15;
	outgtext( "LPL - LFC", txtrow, txtlin );
	ix1 = helicity( -1, -1, st_dir, st_spd, &ix2, &ix3);
	strcpy( st, qc2(ix2, "", 0 ));
	disp_param( st, txtrow + 145, txtlin);
	strcpy( st, qc2(ix3, "", 0 ));
	disp_param( st, txtrow + 195, txtlin);
	strcpy( st, qc2(ix1, stval, 0 ));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	setcolor(5);
	strcpy( st, "------ SR WINDS ------" );
	outgtext( st,
	  (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2)), txtlin);

	txtlin += 20;
	setcolor(31);
	outgtext( "LAYER                VECTOR", txtrow, txtlin );
	txtlin += 2;
	outgtext("_______________________________________",
		  txtrow, txtlin );

	txtlin += 20;
	outgtext( "Sfc - 2 km", txtrow, txtlin );
	sr_wind( i_pres(msl(0)), i_pres(msl(2000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3))
	   {
	   sprintf( st, "%.0f / %3.0f kt", ix3, ix4);
	   disp_param( st, txtrow + 245, txtlin);
	   sprintf( st, "(%.0f m/s)", kt_to_mps(ix4));
	   disp_param( st, txtrow + 340, txtlin);
	   }
	else
	   { disp_param( "M", txtrow + 245, txtlin); }

	txtlin += 20;
	outgtext( "4 - 6 km", txtrow, txtlin );
	sr_wind( i_pres(msl(4000)), i_pres(msl(6000)), st_dir, st_spd, 
		 &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3))
	   {
	   sprintf( st, "%.0f / %3.0f kt", ix3, ix4);
	   disp_param( st, txtrow + 245, txtlin);
	   sprintf( st, "(%.0f m/s)", kt_to_mps(ix4));
	   disp_param( st, txtrow + 340, txtlin);
	   }
	else
	   { disp_param( "M", txtrow + 245, txtlin); }

	txtlin += 20;
	outgtext( "9 - 11 km", txtrow, txtlin );
	sr_wind( i_pres(msl(9000)), i_pres(msl(11000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	if(qc(ix3))
	   {
	   sprintf( st, "%.0f / %3.0f kt", ix3, ix4);
	   disp_param( st, txtrow + 245, txtlin);
	   sprintf( st, "(%.0f m/s)", kt_to_mps(ix4));
	   disp_param( st, txtrow + 340, txtlin);
	   }
	else
	   { disp_param( "M", txtrow + 245, txtlin); }
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
	struct _parcel pcl;

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
	strcpy( st, qc2( pcl.cap, "C / ", 0 ));
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
	strcat( st, qc2( mtof(agl(i_hght(ix1))), " ft", 0 ));
	outgtext( st, txtrow + 210, txtlin );

	txtlin += 18;
	outgtext( "LFC Height =", txtrow, txtlin );
	strcpy( st, qc2( pcl.lfcpres, " mb", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( mtof(agl(i_hght(pcl.lfcpres ))), " ft", 0 ));
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
	float params[2];
	int nparams;
	short txtlin, txtrow;
	int fzgstatus;
	char st[40], st1[20];
	struct _parcel pcl;

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
	strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 )))), " ft", 0 ));
	disp_param( st, txtrow + 185, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "Mid Lvl RH =", txtrow, txtlin );
	strcpy( st, qc2( mean_relhum( &ix1, 700, 500), " %" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 190;
	outgtext( "FZG level =", txtrow, txtlin );
/*	strcpy( st, qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 )))), " ft", 0 ));
	disp_param( st, txtrow + 185, txtlin); */
	fzgstatus=mult_temp_lvl(0.0,params,&nparams);
/*	printf("Fzg heights=%f %f\n",mtof(agl(i_hght(params[0]))),
	  mtof(agl(i_hght(params[1])))); */
	
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
        ix3 = (pcl.bplus * ix1) / agl(i_hght(wb_lvl( 0, &ix2 ))); 
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
        char st[40], st1[20], stval[20];
        struct _parcel pcl;

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
        txtlin = txtlin + 15;
        txtrow = skv.brx + 25;
        outgtext( "Low SRW (Sfc - LFC)  =", txtrow, txtlin );
        blyr = i_pres(msl(0));
        tlyr = pcl.lfcpres;
        if(tlyr > 0)
        {
        sr_wind( blyr, tlyr, st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, " kt", 0));
        disp_param( st, txtrow + 300, txtlin);
        }

        txtlin = txtlin + 15;
        txtrow = skv.brx + 25;
        outgtext( "Mid SRW (LFC - LFC+4km)  =", txtrow, txtlin );
        blyr = pcl.lfcpres;
        tlyr = i_pres(i_hght(pcl.lfcpres) + 4000);
        if((tlyr > 0)&&(blyr > 0))
        {
        sr_wind( blyr, tlyr, st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, " kt", 0));
        disp_param( st, txtrow + 300, txtlin);  
        }

        txtlin = txtlin + 15;
        txtrow = skv.brx + 25;
        outgtext( "Low SRW (EL-4km - EL)  =", txtrow, txtlin );
        blyr = i_pres(i_hght(pcl.elpres) - 4000);
        tlyr = pcl.elpres;
        if(tlyr > 0)
        {
        sr_wind( blyr, tlyr, st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, " kt", 0));
        disp_param( st, txtrow + 300, txtlin);
        }
        /* ----- Additional Param's ----- */
        setcolor(5);
        set_font(2);
        txtlin = txtlin+20;
        setlinestyle( 1, 1 );
        strcpy( st, "----- Additional Parameters -----" );
        outgtext( st,
          (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
          txtlin);

	set_font(2);
	setcolor(31);
	txtlin = txtlin + 15;
	txtrow = skv.brx + 25;
	outgtext( "CAPE =", txtrow, txtlin );
	strcpy( st, qc2( pcl.bplus, " J/Kg" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 190;
	outgtext( "0-3km CAPE =", txtrow, txtlin );
	strcpy( st, qc2( pcl.bplus3000, " J/Kg", 0 ));
	disp_param( st, txtrow + 185, txtlin);
	
	txtlin = txtlin + 15;
	txtrow = skv.brx + 25;
	outgtext( "BRN Shear ", txtrow, txtlin );
	bulk_rich( pcl, &ix1 );
	sprintf( st1, " m%c/s%c", 178, 178 );
	strcpy( st, qc2( ix1, st1 , 0));
	disp_param( st, txtrow + 165, txtlin);
	
	txtrow +=190;
        outgtext ( "CINH =", txtrow, txtlin );
	strcpy( st, qc2( pcl.bminus, " J/Kg", 0 ));
	disp_param( st, txtrow + 185, txtlin);
	
	txtlin = txtlin + 15;
	txtrow = skv.brx + 25;
	outgtext( "BRN =", txtrow, txtlin );
	strcpy( st, qc2( pcl.brn, "" , 0));
	disp_param( st, txtrow + 165, txtlin);
	txtrow +=190;
	
	outgtext( "LFC Height =", txtrow, txtlin );
	strcpy( st, qc2(agl(i_hght(pcl.lfcpres)), " m", 0 ));
	outgtext( st, txtrow + 135, txtlin );
	
	txtlin = txtlin + 15;
	txtrow = skv.brx + 25;
	outgtext ("0-3kmSREH",txtrow, txtlin);
	ix1 = helicity( 0, 3000, st_dir, st_spd, &ix2, &ix3);
	sprintf(stval, " m%c/s%c",178, 178 );
	strcpy( st, qc2(ix1, stval, 0 ));
	disp_param( st, txtrow + 165, txtlin);
	
	txtrow +=190;
	outgtext ("0-1km SREH",txtrow, txtlin);
	ix1 = helicity( 0, 1000, st_dir, st_spd, &ix2, &ix3);
	sprintf(stval, " m%c/s%c",178, 178 );
	strcpy( st, qc2(ix1, stval, 0 ));
	disp_param( st, txtrow + 190, txtlin);
	
	ix1 = helicity( -1, -1, st_dir, st_spd, &ix5, &ix2);
	txtlin = txtlin + 15;
	txtrow = skv.brx + 25;
	outgtext( "EHI =", txtrow, txtlin );
	strcpy( st, qc2( ehi(pcl.bplus, ix1), "" , 1));
	disp_param( st, txtrow + 165, txtlin);
	
	txtrow += 190;
	outgtext ( "LCL =", txtrow, txtlin );
	strcpy( st, qc2(agl(i_hght(pcl.lclpres)), " m", 0 ));
	disp_param(st, txtrow+190,txtlin);
		
        }

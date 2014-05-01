/************************************************************************
 *  SHARP-95                                                   		*
 *  Advanced Interactive Sounding Analysis Program             		*
 *                                                             		*
 *  XW Video Graphics Routines (Part #3)				*
 *  These routines are used to display the various indices and 		*
 *  parameters in the right-side area.                         		*
 *                                                             		*
 *  John A. Hart                                               		*
 *  National Severe Storms Forecast Center                     		*
 *  Kansas City, Missouri                                      		*
 *      --------------------------------------------------     		*
 *  List of Routines in this module:                          		*
 *                                                             		*
 *  SHOW_THERMOPARMS                    	                       	*
 *  SHOW_MOISTURE                               	               	*
 *  SHOW_INSTABILITY                                    		*
 *  SHOW_PARCEL                                                		*
 *  CLEAR_PARAMAREA           	                                 	*
 *  SHOW_SKEWTPAGE1             	                               	*
 *  SHOW_HODOPAGE2                      	                       	*
 *  SHOW_SHEAR                                  	               	*
 *  SHOW_STORMPAGE3                                     	       	*
 *  SHOW_PAGE              	                                    	*
 *  SHOW_STORMTYPE              	                               	*
 *  SHOW_SRDATA                         	                       	*
 *  SHOW_INITIATION                             	               	*
 *  SHOW_SEVERE                                         	       	*
 *  SHOW_HAILPOT                                               		*
 *                                                             		*
 ************************************************************************
 *  OPC MODIFICATION - J. Morgan 4/27/05                       		*
 *  header - Added show_opcpage5					*
 *  header - Added show_inversion, show_lifted, show_surfmxd   		*
 *  show_page() - Added new page (5=OPC).                     		*
 *  SHOW_OPCPAGE5 - New function        				*
 *  SHOW_GRADIENT - New function					*
 *  SHOW_INVERSION - New function          				*
 *  SHOW_MIXHEIGHT - New function          				*
 ***********************************************************************/
#define VIDEO
#include "gui.h"
#include "sharp95.h"

extern int cursor_xwdth, cursor_xhght;
/*
 *	Private functions
 */
void show_hailpot ( void );
void show_heavypcpn ( void );
void show_heavysnow ( void );
void show_hodopage2 ( void );
void show_initiation ( void );
void show_instability ( void );
void show_meanwind ( void );
void show_moisture ( void );
void show_precippage4 ( void );
void show_preciptype ( void );
void show_severe ( void );
void show_shear ( void );
void show_skewtpage1 ( void );
void show_srdata ( void );
void show_stormpage3 ( void );
void show_stormtype ( void );
void show_thermoparms ( void );
void show_torpot ( void );
void show_gradient ( void );
void show_inversion ( void );
void show_mixheight ( void );
void show_opcpage5 ( void );

/*=====================================================================*/

void show_thermoparms ( void )
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

	/* ----- Thermo Data Area ----- */
	setcolor(3, draw_reg, gc);
	set_font(1);
	txtlin = 350;
	strcpy( st, "THERMODYNAMIC DATA" );
	outgtext ( st,
		(((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2)),
		(int)txtlin );
	setcolor(0, draw_reg, gc);
	rectangle( 1, (short)(skv.brx+22), (short)(txtlin+20),
			(short)(xwdth-7), (short)(txtlin+255) );
	setcolor(7, draw_reg, gc);
	rectangle( 0, (short)(skv.brx+22), (short)(txtlin+20),
			(short)(xwdth-7), (short)(txtlin+255) );

	show_moisture();
	show_instability();

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(5, draw_reg, gc);
	strcpy( st, "------ MISC PARAMETERS ------" );
	txtrow = (((skv.brx + 20 + xwdth) / 2) -
		   (getgtextextent( st ) / 2));
	txtlin += 8 * 21;
	outgtext ( st, (int)txtrow, (int)txtlin );
	setcolor(31, draw_reg, gc);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "Total Totals =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( t_totals( &ix1, &ix2, &ix3 ), "", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "K-Index =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( k_index( &ix1 ), "", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31, draw_reg, gc);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "SWEAT Index =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( sweat_index( &ix1 ), "", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "Max Temp =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ctof(max_temp( &ix1, -1)), "F", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31, draw_reg, gc);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "ThetaE Diff =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ThetaE_diff( &ix1 ), "C", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "Conv Temp =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ctof(cnvtv_temp( &ix1, -50 )), "F", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31, draw_reg, gc);
	txtrow = skv.brx + 25;
	txtlin += 18;
	outgtext ( "WBZ level =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 )))), "ft", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "FGZ level =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 )))), "ft", 0 ));
	disp_param( st, txtrow + 165, txtlin);
	}

	/*NP*/
	void show_moisture ( void )
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
	float ix1=-999.0F;
	short txtlin, txtrow;
	char st[40];

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(5, draw_reg, gc);
	strcpy( st, "------ AVAILABLE MOISTURE ------" );
	txtrow = (((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2));
	txtlin = 375;
	outgtext ( st, (int)txtrow, (int)txtlin );

	txtrow = skv.brx + 25;
	txtlin += 18;
	setcolor(31, draw_reg, gc);
	outgtext ( "P. Water =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( precip_water( &ix1, -1.0F, -1.0F), " in", 2 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "Mean RH =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( mean_relhum( &ix1, -1.0F, -1.0F), " %" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow = skv.brx + 25;
	txtlin += 16;
	outgtext ( "Mean W =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( mean_mixratio( &ix1, -1.0F, -1.0F ), " g/Kg", 1 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 210;
	outgtext ( "Mean LRH =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( mean_relhum( &ix1, -1.0F, sndgp->sndg[sfc()].pres - 150 ), " %", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow = skv.brx + 25;
	txtlin += 16;
	outgtext ( "Top of Moist Lyr =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( top_moistlyr( &ix1 ), " mb", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( mtof(agl(i_hght(ix1))), " ft", 0 ));
	outgtext ( st, txtrow+220, (int)txtlin );
	}

	/*NP*/
	void show_instability ( void )
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
	setcolor(5, draw_reg, gc);
	strcpy( st, "------ CONDITIONAL INSTABILITY ------" );
	txtrow = (((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2));
	txtlin = 350 + (5 * 22);
	outgtext ( st, (int)txtrow, (int)txtlin );
	txtrow = skv.brx + 25;
	txtlin += 20;
	setcolor(31, draw_reg, gc);
	outgtext ( "700-500mb Lapse Rate =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( delta_t( &ix1 ), " C", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( lapse_rate( &ix1, 700.0F, 500.0F ), " C/km", 1 ));
	txtrow += 220;
	outgtext ( st, (int)txtrow, (int)txtlin );
	setcolor(31, draw_reg, gc);
	txtlin = txtlin + (short)16;
	txtrow = skv.brx + (short)25;
	outgtext ( "850-500mb Lapse Rate =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( vert_tot( &ix1 ), " C", 0.0F ));
	strcat( st, " / " );
	strcat( st, qc2( lapse_rate( &ix1, 850.0F, 500.0F ), " C/km", 1 ));
	txtrow += 220;
	outgtext ( st, (int)txtrow, (int)txtlin );
	}

/*=======================================================================*/

void show_parcel ( void )
	/*************************************************************/
	/*  SHOW_PARCEL                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Lifts a parcel, draws curve, and displays data on screen.	*
	 * Log:								*
	 * T. Piper/SAIC	02/05	Added qc check			*
	 ***************************************************************/
	{
	float sfctemp, sfcdwpt, sfcpres;
	short txtlin, txtrow;
	char st[40], st1[20];
	struct _parcel pcl;
/*----------------------------------------------------------------------------*/
	if (!qc (i_temp (700.0F)))
	     return;

	sfctemp = sndgp->lplvals.temp;
	sfcdwpt = sndgp->lplvals.dwpt;
	sfcpres = sndgp->lplvals.pres;

	setcliprgn ( skv.tlx, skv.tly, xwdth, xhght, draw_reg, gc );

	/* ----- Plot Parcel Trace on Skew-T ----- */
	setcolor(31, draw_reg, gc);
	setlinestyle( 4, 1 );
	trace_parcel( sfcpres, sfctemp, sfcdwpt);

	/* ----- Calculate Parcel Data ----- */
	parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Parcel Data Area ----- */
	setcolor(3, draw_reg, gc);
	set_font(1);
	setlinestyle( 1, 1 );
	strcpy( st, "PARCEL DATA" );
	txtrow = (((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st)/2));
	txtlin = 105;
	outgtext ( st, (int)txtrow, (int)txtlin );
	setcolor(0, draw_reg, gc);
	rectangle( 1, (short)(skv.brx+22), 125, (short)(xwdth-7), (short)(txtlin+220));
	setcolor(7, draw_reg, gc);
	rectangle( 0, (short)(skv.brx+22), 125, (short)(xwdth-7), (short)(txtlin+220));

	/* ----- Display Values on screen ----- */
	set_font(2);
	txtlin = 130;
	setcolor(5, draw_reg, gc);
	strcpy( st, "*** ");
	strcat( st, sndgp->lplvals.desc);
	strcat( st, " ***"  );
	txtrow = ((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st) / 2);
	outgtext ( st, (int)txtrow, (int)txtlin );
	setcolor(31, draw_reg, gc);
	txtlin += 20;
	txtrow = skv.brx + (short)25;
	outgtext ( "LPL: ", (int)txtrow, (int)txtlin );
	itoa( pcl.lplpres, st, 10);
	strcat( st, "mb  ");
	itoa( pcl.lpltemp, st1, 10);
	strcat( st, st1 );
	strcat( st, "C/");
	itoa( pcl.lpldwpt, st1, 10);
	strcat( st, st1 );
	strcat( st, "C  ");
	itoa( ctof(pcl.lpltemp), st1, 10);
	strcat( st, st1 );
	strcat( st, "F/");
	itoa( ctof(pcl.lpldwpt), st1, 10);
	strcat( st, st1 );
	strcat( st, "F");
	txtrow += 70;
	outgtext ( st, (int)txtrow, (int)txtlin );

	setcolor(31, draw_reg, gc);
	txtlin += 30;
	txtrow = skv.brx + 25;
	outgtext ( "CAPE =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.bplus, " J/Kg" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 205;
	outgtext ( "LI (500mb) =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.li5, " C " , 0));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31, draw_reg, gc);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "BFZL =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.bfzl, " J/Kg", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 205;
	outgtext ( "LImin =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.limax, "C / ", 0 ));
	disp_param( st, txtrow + 125, txtlin);
	strcpy( st, qc2( pcl.limaxpres, "mb", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31, draw_reg, gc);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "CINH =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.bminus, " J/Kg", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 205;
	outgtext ( "Cap =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.cap, "C / ", 0 ));
	disp_param( st, txtrow + 125, txtlin);
	strcpy( st, qc2( pcl.cappres, "mb", 0 ));
	disp_param( st, txtrow + 165, txtlin);

	setcolor(31, draw_reg, gc);
	txtlin += 30;
	txtrow = skv.brx + 25;
	outgtext ( "LEVEL     PRES     HGT(AGL)     TEMP", (int)txtrow, (int)txtlin );
	txtlin += 2;
	outgtext ( "_________________________________________",
		    (int)txtrow, (int)txtlin );

	setcolor(31, draw_reg, gc);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "LCL", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.lclpres, "mb", 0 ));
	disp_param( st, txtrow + 130, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.lclpres ))), "ft", 0 ));
	disp_param( st, txtrow + 240, txtlin);

	setcolor(31, draw_reg, gc);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "LFC", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.lfcpres, "mb", 0 ));
	disp_param( st, txtrow + 130, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.lfcpres ))), "ft", 0 ));
	disp_param( st, txtrow + 240, txtlin);
	strcpy( st, qc2( i_temp(pcl.lfcpres ), "C", 0 ));
	disp_param( st, txtrow + 320, txtlin);

	setcolor(31, draw_reg, gc);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "EL", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.elpres, "mb", 0 ));
	disp_param( st, txtrow + 130, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.elpres ))), "ft", 0 ));
	disp_param( st, txtrow + 240, txtlin);
	strcpy( st, qc2( i_temp(pcl.elpres ), "C", 0 ));
	disp_param( st, txtrow + 320, txtlin);

	setcolor(31, draw_reg, gc);
	txtlin += 16;
	txtrow = skv.brx + 25;
	outgtext ( "MPL", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.mplpres, "mb", 0 ));
	disp_param( st, txtrow + 130, txtlin);
	strcpy( st, qc2( mtof(agl(i_hght(pcl.mplpres ))), "ft", 0 ));
	disp_param( st, txtrow + 240, txtlin);
	setcliprgn ( 1, 1, xwdth, xhght, draw_reg, gc );
	}


	/*NP*/
	void clear_paramarea ( void )
	/*************************************************************/
	/*  CLEAR_PARAMAREA                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Clear right side of screen (parameter area).             */
	/*                                                           */
	/*************************************************************/
	{

	setcliprgn(1, 1, xwdth, xhght, draw_reg, gc);
	setlinestyle (1,1);

	setcolor(4, draw_reg, gc);
	/*rectangle( 1, skv.brx + 21, skv.tly + 77, xwdth-5, xhght-5);*/
	rectangle( 1, skv.brx + 20, skv.tly, xwdth-5, xhght-5);

        setcolor(1, draw_reg, gc);
	rectangle( 0, skv.brx + 20, skv.tly, xwdth-5, xhght-5);

	setcolor(0, cursor_out, gc_cursor);
	setcliprgn (1 , 1, cursor_xwdth, cursor_xhght, cursor_out, gc_cursor );

	rectangle_cursor ( 1, 1, 1, cursor_xwdth, cursor_xhght);
	}

	/*NP*/
	void show_skewtpage1 ( void )
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
	void show_hodopage2 ( void )
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
	void show_heavypcpn ( void )
        /*************************************************************/
        /*  SHOW_HEAVYPCPN                                           */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Displays heavy precipitation parameters.                 */
        /*************************************************************/
        {
        float ix1;
        short txtlin, txtrow, anc_x, anc_y;
        char st[40];

	/* ----- Set anchor pixel ----- */
        anc_x = skv.brx + 20;
        anc_y = 240;

	/* ----- Heavy Precip Area ----- */
        setcolor(3, draw_reg, gc);
        set_font(1);
        setlinestyle( 1, 1 );
        strcpy( st, "HEAVY RAINFALL" );
        txtrow = (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2));
        txtlin = anc_y;
        outgtext ( st, (int)txtrow, (int)txtlin );
        setcolor(0, draw_reg, gc);
        rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
        setcolor(7, draw_reg, gc);
        rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31, draw_reg, gc);
	txtlin = txtlin + 30;
	txtrow = skv.brx + 25;
	outgtext ( "Rogash QPF =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( Rogash_QPF(&ix1), " in", 2 ));
	disp_param( st, txtrow + 300, txtlin);
        }

/*============================================================================*/

void show_heavysnow ( void )
        /*************************************************************/
        /*  SHOW_HEAVYSNOW                                           */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Displays heavy snowfall parameters.                      */
        /*************************************************************/
        {
        short txtlin, txtrow, anc_x, anc_y;
        char st[40];
	int notyet=1;

        if(notyet > 0) return;

        /* ----- Set anchor pixel ----- */
        anc_x = skv.brx + 20;
        anc_y = 375;

        /* ----- Heavy Precip Area ----- */
        setcolor(3, draw_reg, gc);
        set_font(1);
        setlinestyle( 1, 1 );
        strcpy( st, "HEAVY SNOWFALL" );
        txtrow = (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2));
        txtlin = anc_y;
        outgtext ( st, (int)txtrow, (int)txtlin );
        setcolor(0, draw_reg, gc);
        rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
        setcolor(7, draw_reg, gc);
        rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);

	set_font(2);
	}

/*=========================================================================*/

void show_preciptype ( void )
        /*************************************************************/
        /*  SHOW_PRECIPTYPE                                          */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Displays precipitation-type parameters.                  */
        /*************************************************************/
        {
        float ix1;
        short txtlin, txtrow, anc_x, anc_y;
        char st[40];

        /* ----- Set anchor pixel ----- */
        anc_x = skv.brx + 20;
        anc_y = 105;

        /* ----- Heavy Precip Area ----- */
        setcolor(3, draw_reg, gc);
        set_font(1);
        setlinestyle( 1, 1 );
        strcpy( st, "PRECIPITATION TYPE" );
        txtrow = (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2));
        txtlin = anc_y;
        outgtext ( st, (int)txtrow, (int)txtlin );
        setcolor(0, draw_reg, gc);
        rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
        setcolor(7, draw_reg, gc);
        rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31, draw_reg, gc);
	txtlin = txtlin + 30;
	txtrow = skv.brx + 25;
	outgtext ( "Melting Level =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 )))), "ft /", 0 ));
	strcat( st, qc2( wb_lvl(0, &ix1), " mb", 0));
	strcat( st, "\0");
	disp_param( st, txtrow + 300, txtlin);
	}


	/*NP*/
	void show_shear ( void )
	/*************************************************************/
	/*  SHOW_SHEAR                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays the environmental shear values.                 */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4;
	short txtlin, txtrow, anc_x, anc_y;
	char st[40];

	/* ----- Set anchor pixel ----- */
	anc_x = skv.brx + 20;
	anc_y = 485;

	/* ----- Environmental Shear Area ----- */
	setcolor(3, draw_reg, gc);
	set_font(1);
	setlinestyle( 1, 1 );
	strcpy( st, "ENVIRONMENTAL SHEAR" );
	txtrow = (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2));
	txtlin = anc_y;
	outgtext ( st, (int)txtrow, (int)txtlin );
	setcolor(0, draw_reg, gc);
	rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);
	setcolor(7, draw_reg, gc);
	rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 125);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31, draw_reg, gc);
	txtrow = anc_x + 10;
	txtlin = anc_y + 25;

	outgtext( "LAYER         DELTA V        TOT SHR",
		  (int)txtrow, (int)txtlin );
	txtlin += 2;
	outgtext("________________________________________",
		  (int)txtrow, (int)txtlin );

	txtlin += 17;
	outgtext( "Low - 3 km", (int)txtrow, (int)txtlin );
	wind_shear( -1, i_pres(msl(3000)), &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf( st, "(%s)", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param( st, txtrow + 245, txtlin);
	strcpy( st, qc2( kt_to_mps(ix4)/.3F, "" , 2));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext( "Sfc - 2 km",(int)txtrow, (int)txtlin );
	wind_shear( -1, i_pres(msl(2000)), &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf( st, "(%s)", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param( st, txtrow + 245, txtlin);
	strcpy( st, qc2( kt_to_mps(ix4)/.2F, "" , 2));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext( "Sfc - 6 km", (int)txtrow, (int)txtlin );
	wind_shear( -1, i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf( st, "(%s)", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param( st, txtrow + 245, txtlin);
	strcpy( st, qc2( kt_to_mps(ix4)/.6F, "" , 2));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext( "Sfc - 12 km", (int)txtrow, (int)txtlin );
	wind_shear( -1, i_pres(msl(12000)), &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt" , 0));
	disp_param( st, txtrow + 165, txtlin);
	sprintf( st, "(%s)", qc2( kt_to_mps(ix4), " m/s" , 0));
	disp_param( st, txtrow + 245, txtlin);
        if(qc(ix4))
	   strcpy( st, qc2( kt_to_mps(ix4)/1.2F, "" , 2));
        else
	   strcpy( st, qc2( ix4, "" , 2));
	disp_param( st, txtrow + 315, txtlin);
	}


	/*NP*/
	void show_stormpage3 ( void )
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
        void show_precippage4 ( void )
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
	void show_page ( short page )
	/*************************************************************/
	/*  SHOW_PAGE                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays the given parameter list on the screen.         */
	/*  (1=Skewt, 2=Hodo, 3=Storm, 4=Precip).                    */
	/*                                                           */
	/*  OPC MODIFICATION - J. Morgan 4/27/05                     */
	/*  show_page() - Added new page (5=OPC).                    */
	/*              - Reordered pages                            */
	/*************************************************************/
	{
	if( page == 1 )
	   {
	   show_skewtpage1();
	   return;
	   }
	if( page == 2 )
	   {
	   show_opcpage5();
	   return;
	   }
	if( page == 3 )
	   {
	   show_hodopage2();
	   return;
	   }
	if( page == 4 )
	   {
	   show_stormpage3();
	   return;
	   }
	if( page == 5 )
     	   {
	   show_precippage4();
	   return;
	   }
	}

	/*NP*/
	void show_stormtype ( void )
	/*************************************************************/
	/*  SHOW_STORMTYPE                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays stormtype information.                          */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4, ix5, sfctemp, sfcdwpt, sfcpres;
	short txtlin, txtrow;
	char st[40], st1[20];
	struct _parcel pcl;

	  txtlin = skv.tly +  18;
	  setcolor(1, draw_reg, gc);

	sfctemp = sndgp->lplvals.temp;
	sfcdwpt = sndgp->lplvals.dwpt;
	sfcpres = sndgp->lplvals.pres;

	/* ----- Calculate Parcel Data ----- */
	ix1 = parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Storm Type Area ----- */
	setcolor(3, draw_reg, gc);
	set_font(1);
	txtlin = 220;
	setlinestyle( 1, 1 );
	strcpy( st, "STORM TYPE" );
	outgtext( st,
	(((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st )/2)), txtlin);
	setcolor(0, draw_reg, gc);
	rectangle( 1, skv.brx + 22, txtlin+20, xwdth-7, txtlin+80);
	setcolor(7, draw_reg, gc);
	rectangle( 0, skv.brx + 22, txtlin+20, xwdth-7, txtlin+80);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31, draw_reg, gc);
	txtlin = txtlin + 30;
	txtrow = skv.brx + 25;
	outgtext( "CAPE =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.bplus, " J/Kg" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 200;
	/*mean_wind( -1.0F, -1.0F, &ix1, &ix2, &ix3, &ix4);*/
	printf("wind dir %f speed %f\n",sndgp->st_dir, sndgp->st_spd);
	ix1 = helicity( -1.0F, -1.0F, sndgp->st_dir, sndgp->st_spd, &ix5, &ix2);
	outgtext( "Eff. SREH=", (int)txtrow, (int)txtlin );
	sprintf( st1, " m%c/s%c", 178, 178 );
	strcpy( st, qc2( ix1, st1 , 0.0F));
	disp_param( st, txtrow + 175, txtlin);

	txtlin += 18;
	txtrow = skv.brx + (short)25;
	outgtext( "EHI =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ehi(pcl.bplus, ix1), "" , 1));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 200;
	outgtext( "3km Shear =", (int)txtrow, (int)txtlin );
	wind_shear( -1, -1, &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( kt_to_mps( ix4 ), " m/s", 1));
	disp_param( st, txtrow + 175, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "BRN =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.brn, "" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 200;
	outgtext( "BRN Shear =", (int)txtrow, (int)txtlin );
	bulk_rich( pcl, &ix1 );
	sprintf( st1, " m%c/s%c", 178, 178 );
	strcpy( st, qc2( ix1, st1 , 0));
	disp_param( st, txtrow + 175, txtlin);
	}

	/*NP*/
	void show_meanwind ( void )
	/*************************************************************/
	/*  SHOW_MEANWIND                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays the ground-relative mean wind values.           */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4;
	short txtlin, txtrow, anc_x, anc_y;
	char st[40];

	/* ----- Set anchor pixel ----- */
	anc_x = skv.brx + 20;
	anc_y = 385;

	/* ----- Write Mean Wind Header ----- */
	setcolor(3, draw_reg, gc);
	set_font(1);
	setlinestyle( 1, 1 );
	strcpy( st, "MEAN  WIND" );
	outgtext( st,
	   (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2)), anc_y );
	setcolor(0, draw_reg, gc);
	rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 85);
	setcolor(7, draw_reg, gc);
	rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 85);

	set_font(2);
	setcolor(31, draw_reg, gc);

	txtrow = anc_x + 10;
	txtlin = anc_y + 25;

	/* ----- 0-6 km Mean Wind ----- */
	outgtext( "Sfc - 6 km", (int)txtrow, (int)txtlin );
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
	outgtext( "LFC - EL", (int)txtrow, (int)txtlin );
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
	outgtext( "850 - 200 mb", (int)txtrow, (int)txtlin );
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
	void show_srdata ( void )
	/*************************************************************/
	/*  SHOW_SRDATA                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays storm-relative data.                            */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4;
	short txtlin, txtrow, anc_x, anc_y;
	char st[40], stval[20];

	/* ----- Set anchor pixel ----- */
	anc_x = skv.brx + 20;
	anc_y = 110;

	/* ----- Storm Relative Header ----- */
	setcolor(3, draw_reg, gc);
	set_font(1);
	setlinestyle( 1, 1 );
	strcpy( st, "STORM  RELATIVE" );
	outgtext( st,
	  (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2)), anc_y );
	setcolor(0, draw_reg, gc);
	rectangle( 1, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 250);
	setcolor(7, draw_reg, gc);
	rectangle( 0, anc_x + 2, anc_y + 20, xwdth-7, anc_y + 250);

	/* ----- Display Values on screen ----- */
	set_font(2);
	txtrow = anc_x + 10;
	txtlin = anc_y + 25;

	/* ----- Display Storm Motion ----- */
	setcolor(31, draw_reg, gc);
	sprintf( st, "%3.0f%c / %3.0f kt    ( %.0f m/s )", sndgp->st_dir, 176,
		 sndgp->st_spd, kt_to_mps(sndgp->st_spd));
	outgtext( st,
	  (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2)), (int)txtlin);

	/* ----- Storm Relative Helicity ----- */
	txtlin += 20;
	setcolor(5, draw_reg, gc);
	strcpy( st, "------ SR HELICITY ------" );
	outgtext( st,
	  (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2)), (int)txtlin);

	setcolor(31, draw_reg, gc);
	txtlin += 20;
	outgtext( "LAYER        POS   NEG        TOT",
		(int)txtrow, (int)txtlin );
	txtlin += 2;
	outgtext( "________________________________________", (int)txtrow, (int)txtlin );

	txtlin += 20;
	outgtext( "Sfc - 2 km", (int)txtrow, (int)txtlin );
	ix1 = helicity( 0, 2000, sndgp->st_dir, sndgp->st_spd, &ix2, &ix3);
	strcpy( st, qc2(ix2, "", 0 ));
	disp_param( st, txtrow + 145, txtlin);
	strcpy( st, qc2(ix3, "", 0 ));
	disp_param( st, txtrow + 195, txtlin);
	sprintf(stval, " m%c/s%c",178, 178 );
	strcpy( st, qc2(ix1, stval, 0 ));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext( "Sfc - 3 km", (int)txtrow, (int)txtlin );
	ix1 = helicity( 0, 3000, sndgp->st_dir, sndgp->st_spd, &ix2, &ix3);
	strcpy( st, qc2(ix2, "", 0 ));
	disp_param( st, txtrow + 145, txtlin);
	strcpy( st, qc2(ix3, "", 0 ));
	disp_param( st, txtrow + 195, txtlin);
	strcpy( st, qc2(ix1, stval, 0 ));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	outgtext( "LPL - LFC", (int)txtrow, (int)txtlin );
	ix1 = helicity( -1, -1, sndgp->st_dir, sndgp->st_spd, &ix2, &ix3);
	strcpy( st, qc2(ix2, "", 0 ));
	disp_param( st, txtrow + 145, txtlin);
	strcpy( st, qc2(ix3, "", 0 ));
	disp_param( st, txtrow + 195, txtlin);
	strcpy( st, qc2(ix1, stval, 0 ));
	disp_param( st, txtrow + 315, txtlin);

	txtlin += 20;
	setcolor(5, draw_reg, gc);
	strcpy( st, "------ SR WINDS ------" );
	outgtext( st,
	  (((anc_x + xwdth) / 2) - (getgtextextent( st ) / 2)), (int)txtlin);

	txtlin += 20;
	setcolor(31, draw_reg, gc);
	outgtext( "LAYER                VECTOR", (int)txtrow, (int)txtlin );
	txtlin += 2;
	outgtext("_______________________________________",
		  (int)txtrow, (int)txtlin );

	txtlin += 20;
	outgtext( "Sfc - 2 km", (int)txtrow, (int)txtlin );
	sr_wind( i_pres(msl(0)), i_pres(msl(2000)), sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
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
	outgtext( "4 - 6 km", (int)txtrow, (int)txtlin );
	sr_wind( i_pres(msl(4000)), i_pres(msl(6000)), sndgp->st_dir, sndgp->st_spd,
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
	outgtext( "9 - 11 km", (int)txtrow, (int)txtlin );
	sr_wind( i_pres(msl(9000)), i_pres(msl(11000)), sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
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
	void show_initiation ( void )
	/*************************************************************/
	/*  SHOW_INITIATION                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays thunderstorm initiation parameters.             */
	/*************************************************************/
	{
	float ix1, sfctemp, sfcdwpt, sfcpres;
	short txtlin, txtrow;
	char st[40];
	struct _parcel pcl;

	sfctemp = sndgp->lplvals.temp;
	sfcdwpt = sndgp->lplvals.dwpt;
	sfcpres = sndgp->lplvals.pres;

	/* ----- Calculate Parcel Data ----- */
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Storm Type Area ----- */
	setcolor(3, draw_reg, gc);
	set_font(1);
	txtlin = 105;
	setlinestyle( 1, 1 );
	strcpy( st, "CONVECTIVE  INITIATION" );
	outgtext( st,
	  (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
	  (int)txtlin);
	setcolor(0, draw_reg, gc);
	rectangle( 1, skv.brx + 22, txtlin+20, xwdth-7, txtlin+110);
	setcolor(7, draw_reg, gc);
	rectangle( 0, skv.brx + 22, txtlin+20, xwdth-7, txtlin+110);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31, draw_reg, gc);
	txtlin = txtlin + 30;
	txtrow = skv.brx + 25;
	outgtext( "CINH =",  (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.bminus, " J/Kg" , 0));
	disp_param( st, txtrow + 160, txtlin);

	txtrow += 210;
	outgtext( "Cap =",  (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.cap, "C/ ", 0 ));
	disp_param( st, txtrow + 120, txtlin);
	strcpy( st, qc2( pcl.cappres, "mb", 0 ));
	disp_param( st, txtrow + 160, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "K-Index =",  (int)txtrow, (int)txtlin );
	strcpy( st, qc2( k_index( &ix1 ), "", 0 ));
	disp_param( st, txtrow + 160, txtlin);

	txtrow += 210;
	outgtext( "Mean RH =",  (int)txtrow, (int)txtlin );
	strcpy( st, qc2( mean_relhum( &ix1, -1, -1), " %" , 0));
	disp_param( st, txtrow + 160, txtlin);

	txtlin += 25;
	txtrow = skv.brx + 25;
	outgtext( "Top of Moist Lyr = ", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( top_moistlyr( &ix1 ), " mb", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( mtof(agl(i_hght(ix1))), " ft", 0 ));
	outgtext( st, txtrow + 210, (int)txtlin );

	txtlin += 18;
	outgtext( "LFC Height =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.lfcpres, " mb", 0 ));
	strcat( st, " / " );
	strcat( st, qc2( mtof(agl(i_hght(pcl.lfcpres ))), " ft", 0 ));
	outgtext( st, txtrow + 210, (int)txtlin );
	}

	/*NP*/
	void show_hailpot ( void )
	/*************************************************************/
	/*  SHOW_HAILPOT                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Displays hail forecasting parameters.                    */
	/*************************************************************/
	{
	float ix1, ix2, ix3, ix4, sfctemp, sfcdwpt, sfcpres;
	short txtlin, txtrow;
	char st[40];
	struct _parcel pcl;

	sfctemp = sndgp->lplvals.temp;
	sfcdwpt = sndgp->lplvals.dwpt;
	sfcpres = sndgp->lplvals.pres;

	printf("sfc temp %f dew %f pre %f\n", sfctemp, sfcdwpt, sfcpres);
	/* ----- Calculate Parcel Data ----- */
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	/* ----- Hail Area ----- */
	setcolor(5, draw_reg, gc);
	set_font(2);
	txtlin = 305;
	txtlin += 25;
	setlinestyle( 1, 1 );
	strcpy( st, "----- HAIL POTENTIAL -----" );
	outgtext( st,
	  (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
	  (int)txtlin);

	/* ----- Display Values on screen ----- */
	set_font(2);
	setcolor(31, draw_reg, gc);
	txtlin = txtlin + 20;
	txtrow = skv.brx + 25;
	outgtext( "CAPE =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( pcl.bplus, " J/Kg" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 190;
	outgtext( "WBZ level =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(wb_lvl( 0, &ix1 )))), " ft", 0 ));
	disp_param( st, txtrow + 185, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "Mid Lvl RH =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( mean_relhum( &ix1, 700, 500), " %" , 0));
	disp_param( st, txtrow + 165, txtlin);

	txtrow += 190;
	outgtext( "FZG level =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( mtof(agl(i_hght(temp_lvl( 0, &ix1 )))), " ft", 0 ));
	disp_param( st, txtrow + 185, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "EL Storm Relative Wind Speed =", (int)txtrow, (int)txtlin );
	sr_wind( pcl.elpres+25, pcl.elpres-25, sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2( ix4, " kt", 0 ));
	disp_param( st, txtrow + 360, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "CHI1 =", (int)txtrow, (int)txtlin);
	bulk_rich( pcl, &ix1 );
	ix3 = (pcl.bplus * ix1) / agl(i_hght(wb_lvl( 0, &ix2 )));
	printf("CHI = %f devide by %f pcl.bplus= %f, brnshear=%f\n", ((pcl.bplus * ix1)) , agl(i_hght(wb_lvl( 0, &ix2 ))),pcl.bplus,ix1 );
	strcpy( st, qc2(ix3, "", 1));
	disp_param( st, txtrow + 165, txtlin);

 	txtrow += 190;
	outgtext( "CHI2 =", (int)txtrow, (int)txtlin);
	bulk_rich( pcl, &ix1 );
	Mean_WBtemp( &ix2, -1, -1);
	strcpy( st, qc2(ix3/ix2, "", 1));
	disp_param( st, txtrow + 185, txtlin);

	txtlin += 18;
	txtrow = skv.brx + 25;
	outgtext( "Avg BL Wetbulb Temp =", (int)txtrow, (int)txtlin);
	strcpy( st, qc2( Mean_WBtemp( &ix2, -1, -1), " C", 1));
	disp_param( st, txtrow + 260, txtlin);
	}


	/*NP*/
	void show_severe ( void )
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
	setcolor(3, draw_reg, gc);
	set_font(1);
	txtlin = 305;
	setlinestyle( 1, 1 );
	strcpy( st, "SEVERE POTENTIAL" );
	outgtext( st,
	  (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
	  (int)txtlin);
	setcolor(0, draw_reg, gc);
	rectangle( 1, skv.brx + 22, txtlin+20, xwdth-7, txtlin+300);
	setcolor(7, draw_reg, gc);
	rectangle( 0, skv.brx + 22, txtlin+20, xwdth-7, txtlin+300);

	show_hailpot();
	show_torpot();
	}


        /*NP*/
        void show_torpot ( void )
        /*************************************************************/
        /*  SHOW_TORPOT                                              */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Displays tornado forecasting parameters.                 */
        /*************************************************************/
        {
        float ix1, ix2, ix3, ix4, sfctemp, sfcdwpt, sfcpres;
	float tlyr, blyr;
        short txtlin, txtrow;
        char st[40];
        struct _parcel pcl;

        sfctemp = sndgp->lplvals.temp;
        sfcdwpt = sndgp->lplvals.dwpt;
        sfcpres = sndgp->lplvals.pres;

        /* ----- Calculate Parcel Data ----- */
        ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

        /* ----- Tornado Area ----- */
        setcolor(5, draw_reg, gc);
        set_font(2);
        txtlin = 450;
        setlinestyle( 1, 1 );
        strcpy( st, "----- TORNADO POTENTIAL -----" );
        outgtext( st,
          (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
          (int)txtlin);

        /* ----- Display Values on screen ----- */
        set_font(2);
        setcolor(31, draw_reg, gc);
        txtlin = txtlin + 20;
        txtrow = skv.brx + 25;
        outgtext( "Low SRW (Sfc - LFC)  =", (int)txtrow, (int)txtlin );
	blyr = i_pres(msl(0));
	tlyr = pcl.lfcpres;
	if(tlyr > 0)
        {
	sr_wind( blyr, tlyr, sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
	strcpy( st, qc2(ix4, " kt", 0));
        disp_param( st, txtrow + 300, txtlin);
        }

        txtlin = txtlin + 20;
        txtrow = skv.brx + 25;
        outgtext( "Mid SRW (LFC - LFC+4km)  =", (int)txtrow, (int)txtlin );
        blyr = pcl.lfcpres;
        tlyr = i_pres(i_hght(pcl.lfcpres) + 4000);
	if((tlyr > 0)&&(blyr > 0))
        {
        sr_wind( blyr, tlyr, sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, " kt", 0));
        disp_param( st, txtrow + 300, txtlin);
        }

        txtlin = txtlin + 20;
        txtrow = skv.brx + 25;
        outgtext( "Low SRW (EL-4km - EL)  =", (int)txtrow, (int)txtlin );
        blyr = i_pres(i_hght(pcl.elpres) - 4000);
        tlyr = pcl.elpres;
        if(tlyr > 0)
        {
        sr_wind( blyr, tlyr, sndgp->st_dir, sndgp->st_spd, &ix1, &ix2, &ix3, &ix4);
        strcpy( st, qc2(ix4, " kt", 0));
        disp_param( st, txtrow + 300, txtlin);
        }
	}


/*NP*/
void show_opcpage5 ( void )
/*************************************************************/
/*  OPC MODIFICATION - J. Morgan 4/27/05                     */
/*  SHOW_OPCPAGE5 - New function        				 */
/*                                                           */
/*  SHOW_OPCPAGE5                                            */
/*  J. Morgan OPC                                            */
/*                                                           */
/*  Displays the OPC Lower Level Stability                   */
/*  parameter list on the screen.                            */
/*************************************************************/
/*  Called by 	xwvid3.c: show_page()                         */
/*************************************************************/

	{
		clear_paramarea();
		show_gradient();
		show_inversion();
		show_mixheight();
	}

/*======================================================================*/

void show_gradient ( void )
/*************************************************************/
/*  OPC MODIFICATION - J. Morgan 4/27/05                     */
/*  SHOW_GRADIENT - New function				*/
/*                                                           */
/*  SHOW_GRADIENT                                            */
/*  J. Morgan OPC                                            */
/*                                                           */
/*  Displays Surface-975 hPa Temperature Gradient data.      */
/*************************************************************/
/*  Called by 	xwvid3.c: show_opcpage5()				*
 * Log:									*
 * S. Chiswell/UCAR	06/06	Increased st to 44			*
 ***********************************************************************/
/*
 *	nsharp_proto.h, xwvid6.c:
 *		void setcliprgn ( short tlx, short tly, short brx,
 *				short bry, Widget _canvas, GC _gc );
 *		void setcolor ( int color, Widget canvas, GC _gc );
 *		void set_font ( short siz );
 *		void setlinestyle ( short style, short width );
 *		int getgtextextent ( char *st );
 *		void outgtext ( char *st, int x, int y );
 *		void rectangle ( int type, short x, short y,
 *					short width, short height );
 *
 *	nsharp_proto.h, xwvid2.c:
 *		void disp_param ( char *value, short rcol, short rlin )
 *
 *	basics.c:
 *		char	*qc2 ( float value, char *label, short prec )
 *		short qc ( float value )
 *		float i_hght ( float pres )
 *		float i_temp ( float pres )
 *
 *************************************************************/
{
	float ix975_mb, ix975_m, ix975_C;
	float ixSfc_mb, ixSfc_m, ixSfc_C;
	short txtlin, txtrow;
	char st[44];

	/* ----- OPC Stability Header ----- */
	setcliprgn ( skv.tlx, skv.tly, xwdth, xhght, draw_reg, gc );
	setcolor(3, draw_reg, gc);
	set_font(1);
	setlinestyle( 1, 1 );
	strcpy( st, "OPC LOW LEVEL STABILITY" );
	txtrow = (((skv.brx + 20 + xwdth) / 2) -
					(getgtextextent(st)/2));
	txtlin = 105;	/* y=105 */
	outgtext ( st, (int)txtrow, (int)txtlin );

	/* ----- OPC Stability Data Area ----- */
	setcolor(0, draw_reg, gc);
	rectangle( 1, (short)(skv.brx+22), 125, (short)(xwdth-7),
			(short)(txtlin+540));	/* y=125..665 */
	setcolor(7, draw_reg, gc);
	rectangle( 0, (short)(skv.brx+22), 125, (short)(xwdth-7),
			(short)(txtlin+540));	/* y=125..665 */

	/* ----- Sfc-975 Grad Header ----- */
	set_font(2);
	txtlin = 130;	/* y=130 */
	setcolor(5, draw_reg, gc);
	strcpy( st, "----- SURFACE-975 hPa TEMP GRADIENT -----" );
	txtrow = ((skv.brx + 20 + xwdth) / 2) -
				(getgtextextent(st) / 2);
	outgtext( st, (int)txtrow, (int)txtlin);

	/* ----- Sfc-975 Grad Data ----- */
	ix975_mb = 975;
	ix975_m	= i_hght( ix975_mb );
	ix975_C	= i_temp( ix975_mb );

	ixSfc_mb = sndgp->sndg[sfc()].pres;
	ixSfc_m	= i_hght( ixSfc_mb );
	ixSfc_C	= sndgp->sndg[sfc()].temp;

	/* ----- Sfc-975 Grad Data Display----- */
	setcolor(31, draw_reg, gc);
	txtlin += 25;	/* y=155 */
	txtrow = skv.brx + 25;
	outgtext ( "LEVEL       PRES        HEIGHT      TEMP", (int)txtrow, (int)txtlin );
	txtlin += 2;	/* y=157 */
	outgtext ( "__________________________________________",
				(int)txtrow, (int)txtlin );

	txtlin += 16;	/* y=173 */
	txtrow = skv.brx + 25;
	outgtext ( "975 hPa", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ix975_mb, " mb", 0 ));
	disp_param( st, txtrow + 170, txtlin);
	strcpy( st, qc2( ix975_m, " m", 0 ));
	disp_param( st, txtrow + 270, txtlin);
	strcpy( st, qc2( ix975_C, " C", 2 ));
	disp_param( st, txtrow + 370, txtlin);

	txtlin += 16;	/* y=189 */
	txtrow = skv.brx + 25;
	outgtext ( "Surface", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ixSfc_mb, " mb", 0 ));
	disp_param( st, txtrow + 170, txtlin);
	strcpy( st, qc2( ixSfc_m, " m", 0 ));
	disp_param( st, txtrow + 270, txtlin);
	strcpy( st, qc2( ixSfc_C, " C", 2 ));
	disp_param( st, txtrow + 370, txtlin);

	/* ----- Sfc-975 Grad ----- */
	txtlin += 25;	/* y=214 */
	txtrow = skv.brx + 25;
	outgtext ( "975-Sfc Gradient =", (int)txtrow, (int)txtlin );
	if  (qc(ix975_C) && qc(ixSfc_C)) {
	    strcpy( st, qc2( ix975_C-ixSfc_C, " C", 2 ));
	}
	else {
	    sprintf( st, "M");
	}
	disp_param( st, txtrow + 240, txtlin);

	/* ----- Next ----- */
	setcliprgn ( 1, 1, xwdth, xhght, draw_reg, gc );
	/*show_inversion();
	show_mixheight();*/
}

/*=====================================================================*/

void show_inversion ( void )
/************************************************************************
 *  OPC MODIFICATION - J. Morgan 5/2/05                      		*
 *  SHOW_INVERSION - New function					*
 *                                                           		*
 *  SHOW_INVERSION                                           		*
 *  J. Morgan OPC                                            		*
 *                                                            		*
 *  Displays base height of lowest inversion data.           		*
 ************************************************************************
 *  Called by 	xwvid3.c: show_opcpage5()                     		*
 * Log:									*
 * S. Chiswell/UCAR	06/06	Increased st to 44			*
 ************************************************************************
 *
 *	nsharp_proto.h, xwvid6.c:
 *		void setcolor ( int color, Widget canvas, GC _gc );
 *		void set_font ( short siz );
 *		int getgtextextent ( char *st );
 *		void outgtext ( char *st, int x, int y );
 *
 *	nsharp_proto.h, xwvid2.c:
 *		void disp_param ( char *value, short rcol, short rlin )
 *
 *	basics.c:
 *		char	*qc2 ( float value, char *label, short prec )
 *		float i_hght ( float pres )
 *
 *	skparams.c:
 *		void low_inv ( float *inv_mb, float *inv_dC )
 *
 ***********************************************************************/
{
	float ix1, ix2;
	short txtlin, txtrow;
	char st[44];

	/* ----- Inversion Height Header ----- */
	set_font(2);
	txtlin = 244;	/* y=244 */
	setcolor(5, draw_reg, gc);
	strcpy( st, "-------- LOWEST INVERSION HEIGHT --------" );
	txtrow = ((skv.brx + 20 + xwdth) / 2) -
				(getgtextextent(st) / 2);
	outgtext( st, (int)txtrow, (int)txtlin);

	/* ----- Inversion Height Data ----- */
	/* void low_inv ( float *inv_mb, float *inv_dC ) */
	low_inv( &ix1, &ix2 );

	/* ----- Inversion Height Data Display----- */
	setcolor(31, draw_reg, gc);
	txtlin += 20;	/* y=264 */
	txtrow = skv.brx + (short)25;
	outgtext ( "Base Height    =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( i_hght( ix1 ), " m ", 0 ));
	disp_param( st, txtrow + 235, txtlin);

	txtlin += 20;	/* y=284 */
	txtrow = skv.brx + 25;
	outgtext ( "Base Pressure  =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ix1, " mb" , 0 ));
	disp_param( st, txtrow + 235, txtlin);

	txtlin += 20;	/* y=304 */
	txtrow = skv.brx + 25;
	outgtext ( "Change in Temp =", (int)txtrow, (int)txtlin );
	sprintf( st, "%01.2f C ", ix2);
	disp_param( st, txtrow + 235, txtlin);
}

/*=====================================================================*/

void show_mixheight ( void )
/************************************************************************
 *  OPC MODIFICATION - J. Morgan 5/3/05                      		*
 *  SHOW_MIXHEIGHT - New function					*
 *                                                           		*
 *  SHOW_MIXHEIGHT                                           		*
 *  J. Morgan OPC                                            		*
 *                                                           		*
 *  Displays surface-based mixing depth data                 		*
 ************************************************************************
 *  Called by 	xwvid3.c: show_opcpage5()                     		*
 * Log:									*
 * S. Chiswll/UCAR	06/06	Increased st to 44			*
 ************************************************************************
 *
 * 	nsharp_proto.h, xwvid6.c:
 *		void setcolor ( int color, Widget canvas, GC _gc );
 *		void set_font ( short siz );
 *		int getgtextextent ( char *st );
 *		void outgtext ( char *st, int x, int y );
 *
 *	nsharp_proto.h, xwvid2.c:
 *		void disp_param ( char *value, short rcol, short rlin )
 *
 *	basics.c:
 *		short qc ( float value )
 *		char	*qc2 ( float value, char *label, short prec )
 *		float i_hght ( float pres )
 *
 *	skparams.c:
 *        void mix_height ( float *mh_mb, *mh_drct, *mh_sped, *mh_dC,
 * 				*mh_lr, *mh_drct_max, *mh_sped_max, flag )
 *
 ***********************************************************************/
{
	float ix1, ix2, ix3, ix4, ix5, ix6, ix7;
	short txtlin, txtrow;
	char st[44];

	/* ----- Mixing Height Header ----- */
	set_font(2);
	txtlin = 334;	/* y=334 */
	setcolor(5, draw_reg, gc);
	strcpy( st, "------------- MIXING HEIGHT -------------" );
	txtrow = ((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st) / 2);
	outgtext( st, (int)txtrow, (int)txtlin);

	setcolor(31, draw_reg, gc);
	txtlin += 20;	/* y=354 */
	txtrow = skv.brx + 25;
	outgtext ( "Dry Ad Lapse Rate =", (int)txtrow, (int)txtlin );
	sprintf( st, "9.8 C/km");
	disp_param( st, txtrow + 255, txtlin);

	txtlin += 20;	/* y=374 */
	txtrow = skv.brx + 25;
	outgtext ( "Thresh Lapse Rate =", (int)txtrow, (int)txtlin );
	sprintf( st, "8.3 C/km");
	disp_param( st, txtrow + 255, txtlin);

	/* ----- Layer Based Header ----- */
	set_font(2);
	txtlin += 25;	/* y=399 */
	setcolor(6, draw_reg, gc);
	strcpy( st, "*** Layer Based ***" );
	txtrow = ((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st) / 2);
	outgtext( st, (int)txtrow, (int)txtlin);

	/* ----- Layer Based Data ----- */
	/* void mix_height ( float *mh_mb, *mh_drct, *mh_sped, *mh_dC,
			*mh_lr, *mh_drct_max, *mh_sped_max, flag ) */
	mix_height( &ix1, &ix2, &ix3, &ix4, &ix5, &ix6, &ix7, 1);

	/* ----- Layer Based Data Display----- */
	setcolor(31, draw_reg, gc);
	txtlin += 20;	/* y=419 */
	txtrow = skv.brx + (short)25;
	outgtext ( "Mixing Height      =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( i_hght( ix1 ), " m   ", 0 ));
	disp_param( st, txtrow + 375, txtlin);

	txtlin += 20;	/* y=439 */
	txtrow = skv.brx + 25;
	outgtext ( "Mixing Pressure    =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ix1, " mb  " , 0 ));
	disp_param( st, txtrow + 375, txtlin);

	txtlin += 20;	/* y=459 */
	txtrow = skv.brx + 25;
	outgtext ( "Top Mix Layer Wind =", (int)txtrow, (int)txtlin );
	if  (qc(ix2) && qc(ix3)) {
	    sprintf( st, "%d%c / %d kt  ", (short)ix2, 176, (short)ix3);
	}
	else {
	    sprintf( st, "M / M");
	}
	disp_param( st, txtrow + 375, txtlin);

	txtlin += 20;	/* y=479 */
	txtrow = skv.brx + 25;
	outgtext ( "Mix Layer Max Wind =", (int)txtrow, (int)txtlin );
	if  (qc(ix6) && qc(ix7)) {
	    sprintf( st, "%d%c / %d kt  ", (short)ix6, 176, (short)ix7);
	}
	else {
	    sprintf( st, "M / M");
	}
	disp_param( st, txtrow + 375, txtlin);

	txtlin += 20;	/* y=499 */
	txtrow = skv.brx + 25;
	outgtext ( "Layer Lapse Rate   =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ix4, " C", 2 ));
	strcat( st, " / " );
	strcat( st, qc2( ix5, " C/km", 1 ));
	disp_param( st, txtrow + 375, txtlin);

	/* ----- Surface Based Header ----- */
	set_font(2);
	txtlin += 25;	/* y=524 */
	setcolor(7, draw_reg, gc);
	strcpy( st, "*** Surface Based ***" );
	txtrow = ((skv.brx + 20 + xwdth) / 2) - (getgtextextent(st) / 2);
	outgtext( st, (int)txtrow, (int)txtlin);

	/* ----- Surface Based Data ----- */
	/* void mix_height ( float *mh_mb, *mh_drct, *mh_sped, *mh_dC,
			*mh_lr, *mh_drct_max, *mh_sped_max, flag ) */
	mix_height( &ix1, &ix2, &ix3, &ix4, &ix5, &ix6, &ix7, 0);

	/* ----- Surface Based Data Display----- */
	setcolor(31, draw_reg, gc);
	txtlin += 20;	/* y=544 */
	txtrow = skv.brx + (short)25;
	outgtext ( "Mixing Height      =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( i_hght( ix1 ), " m   ", 0 ));
	disp_param( st, txtrow + 375, txtlin);

	txtlin += 20;	/* y=564 */
	txtrow = skv.brx + 25;
	outgtext ( "Mixing Pressure    =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ix1, " mb  " , 0 ));
	disp_param( st, txtrow + 375, txtlin);

	txtlin += 20;	/* y=584 */
	txtrow = skv.brx + 25;
	outgtext ( "Top Mix Layer Wind =", (int)txtrow, (int)txtlin );
	if  (qc(ix2) && qc(ix3)) {
	    sprintf( st, "%d%c / %d kt  ", (short)ix2, 176, (short)ix3);
	}
	else {
	    sprintf( st, "M / M");
	}
	disp_param( st, txtrow + 375, txtlin);

	txtlin += 20;	/* y=604 */
	txtrow = skv.brx + 25;
	outgtext ( "Mix Layer Max Wind =", (int)txtrow, (int)txtlin );
	if  (qc(ix6) && qc(ix7)) {
	    sprintf( st, "%d%c / %d kt  ", (short)ix6, 176, (short)ix7);
	}
	else {
	    sprintf( st, "M / M");
	}
	disp_param( st, txtrow + 375, txtlin);

	txtlin += 20;	/* y=624 */
	txtrow = skv.brx + 25;
	outgtext ( "Layer Lapse Rate   =", (int)txtrow, (int)txtlin );
	strcpy( st, qc2( ix4, " C", 2 ));
	strcat( st, " / " );
	strcat( st, qc2( ix5, " C/km", 1 ));
	disp_param( st, txtrow + 375, txtlin);

}


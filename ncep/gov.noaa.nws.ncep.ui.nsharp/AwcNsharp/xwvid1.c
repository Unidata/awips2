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
#define GLOBAL
#define VIDEO
#include <xwcmn.h>
#include <sharp95.h>
#include <math.h>
#include <gui.h>
#include "winline.h"


typedef struct {       /* for drawing moving crosshairs or zoom boxes */
  int start_x, start_y,
      last_x,  last_y;
  GC  gc;
  XPoint   points[4];
  int itype; /* 0 = temp 1 = dwpt */
  int ilev; /* 0 = bottom 1 = top 2 = inbetween */
  short yy, i;
} rubber_band_data;

rubber_band_data rbdata;
int g_Parcel=-1;
int g_TvParcelCor=-1;

       /*NP*/
       void make_screen( int argc, char *argv[])
       /*************************************************************/
       /*  MAKE_SCREEN                                              */
       /*  John Hart  NSSFC KCMO                                    */
       /*                                                           */
       /*  Draws basic SHARP graphic template on screen, including  */
       /*  areas, buttons, and tables.                              */
       /*                                                           */
       /*************************************************************/
       {
       char st[80], *t;
       int i;
       float ix1, ix2;

       pagenum = 1;


       X_Init(argc, argv);

       /* ----- RAOB Title Line ----- */
       setcolor(1);
       set_font( 1 );

       /* ----- Parameter Area ----- */
       setcolor(4);
       rectangle( 1, skv.brx + 20, skv.tly, xwdth-5, xhght-5);
       setcolor(1);
       rectangle( 0, skv.brx + 20, skv.tly, xwdth-5, xhght-5);

       /* ----- Cursor Data Area ----- */
       setcolor(3);
       strcpy( st, "CURSOR DATA" );
       outgtext ( st,
              (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
       skv.tly + 5 );
       setcolor(0);
       rectangle( 1, skv.brx + 30, skv.tly + 22, xwdth-15, 100);
       setcolor(1);
       rectangle( 0, skv.brx + 30, skv.tly + 22, xwdth-15, 100);

       mode = 1;
       pagenum = 1;

       /* ----- Begin Main Processing Loop ----- */
       StartLoop();
       }


	/*NP*/
	void draw_skewt( void )
	/*************************************************************/
	/*  DRAW_SKEWT                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws a standard Skew-T/LogP diagram.                    */
	/*************************************************************/
	{
	short x1, y1, x2, y2, y, i, j;
	float a, thta, thtae, ix1;
	float lvl[] = {1050,1000,850,700,500,300,200,100};
	float mixratios[] = {0.5,1,2,4,8,16,32};
	int mixratiocount=7;
    char rtitle[200];      	
	 setcliprgn(1, 1, xwdth, xhght );
	 setcolor(0);
	 rectangle( 1, 1, 1, skv.brx+14, skv.bry+skv.tly+14);
	 setcolor(1);
	 set_font( 2 );
	 setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
	 setlinestyle( 1, 1 );
	 rectangle( 0, skv.tlx, skv.tly, skv.brx, skv.bry);

	/* ----- Draw Skewed Temperature Lines ----- */
	 setcolor(24);
         setcolor(21);
	 setlinestyle( 1, 1 );
	for( i=-160; i<=50; i=i+10 ) { isotherm( i ); }

	/* ----- Draw Dry Adiabats ----- */
	 setcolor(24);
	 setlinestyle( 1, 1 );
	for( thta=-70; thta<=350; thta=thta+20) { dry_adiabat(thta); };
	
	/* ----- Draw Moist Adiabats ---- */
	setcolor(14);
	setlinestyle (4, 1);
	for (thtae=-2+273.16;thtae<=38+273.16;thtae+=4) 
	  {moist_adiabat(thtae); };
	  
	/* ----- Draw Mixing ratio Lines ---- */
	setcolor(23);
	setlinestyle (2,1);
	for (i=0;i<mixratiocount;i++) {mixingratio(mixratios[i]);};

	/* ----- Draw Horizontal Pressure Lines ----- */
	 setcolor(1);
	 setlinestyle( 1, 1 );
	
	for(i=1; i<8; i++) { isobar( (float)lvl[i], 0 );  }
	for(i=100; i<=1050; i=i+50) { isobar( (float)i, 1 ); }

        /* ----- Plot old sounding if exists ----- */
        setcolor(28);
        if ((sndg2[0][0] > 0) && (overlay_previous == 1))
           {
           trace_temp2(3);
           trace_dwpt2(3);
           }

	/* ----- Plot Environmental Temperature Data ----- */
	setcolor(2);
	if ( numlvl > 0 ) trace_temp( 3 );

	/* ----- Plot Environmental Dew Point Data ----- */
	setcolor(3);
	if ( numlvl > 0 ) trace_dwpt( 3 );

	/* ----- Plot Environmental Virtual Temperature Data ----- */
	 setcolor(2);
	if ( numlvl > 0 ) trace_vtmp( 1 );

	/* ----- Plot Environmental Wetbulb Temperature Data ----- */
	 setcolor(6);
	 setlinestyle( 1, 1 );
	if ( numlvl > 0 ) trace_wetbulb( 1 );

	 setcolor(1);
	 rectangle( 0, skv.tlx, skv.tly, skv.brx, skv.bry);

	/* ----- Plot Wind Barbs ----- */
	 setcolor(5);
	 setlinestyle( 1, 1 );
	if ( numlvl > 0 ) plot_barbs();

        /* ----- If Available, plot VVEL profile ----- */
        if (numlvl > 0) vvel_profile();

	/* ----- Display Skew-T Inset ----- */
	draw_skinset();

	setcliprgn ( 1, 1, xwdth, xhght );
	setcolor(1);
	set_font( 1 );
        sprintf( rtitle, "%s (%s)", raobtitle, raob_type);
        outgtext ( rtitle, skv.tlx, 1 );

        update_text_values();
          
	 /*XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg), 
		    gc, 0, 0, xwdth, xhght, 0, 0 ); */

	}
	
	/*NP*/
	void drawWindbarbs(int rwx) 
	/****************************************************************/
	/* DRAWWINDBARBS                                                */
	/****************************************************************/
	{
	int lastpres, i, x, y;
      	/*Draw Wind Barbs*/
      	setcolor(8);
      	lastpres = 1100;
	for( i = 0; i < numlvl && sndg[i][1]>=100; i++)
	  {
	  if( qc( sndg[i][5] ) )
	    {
	    y = pmap(fnP(sndg[i][1]),1);
	    x = rwx+35;
            if((sndg[i][2]-i_hght(lastpres)) > 400)
       	      {
	      wind_barb( sndg[i][5], sndg[i][6], x, y, 4);
	      lastpres = sndg[i][1];
	      }
	    }
	  }  
	}
	
	
	/*NP*/
	void draw_ICG() 
	/*****************************************************************/
	/* DRAW_ICG                                                      */
	/* LARRY J. HINSON AWC/KCMO                                      */
	/*****************************************************************/
	
	{
	  char buf[80];
	  register i,j;
	  int X0=-30,Y0=260,XM=30,YM=1200;
	  int count=0;
	  int width,height;
	  setcliprgn(1,1,xwdth,xhght);
	  setcolor(0);
	  rectangle(1,1,1,skv.brx+14,skv.bry+skv.tly+14);
	  setcolor(1);
	  set_font(2);
	  setlinestyle(1,1);
          sprintf( buf, "%s (%s)", raobtitle, raob_type);
          outgtext ( buf, skv.tlx, 1 );
          update_text_values();
	  /*Get font height*/
	  height=getfontheight();
	  /*Establish Coordinate System*/
	  view(1,1,skv.brx+14,skv.bry+skv.tly+14);
	  setwindow(X0,fnP(Y0),XM,fnP(YM));
	  /*Draw and label Pressure lines*/
	  for (j=1000;j>=300;j-=100) {
	    line_w(-25,fnP(j),25,fnP(j));
	    itoa((short)j,buf,80);
	    outgtext(buf,pmap(-25,0)-getgtextextent(buf)-2,pmap(fnP(j),1));
	  }
	  /*Draw and label Temperature lines and RH*/
	  count=0;
	  for (i=-25;i<=25;i+=5) {
	    setcolor(1);
	    line_w(i,fnP(1000),i,fnP(300));
	    itoa((short)i,buf,80);
	    setcolor(2);
	    outgtext(buf,pmap(i,0)-getgtextextent(buf)/2,pmap(fnP(1000),1)+height);
	    itoa((short)count,buf,80);
	    count+=10;
	    setcolor(3);
	    outgtext(buf,pmap(i,0)-getgtextextent(buf)/2,pmap(fnP(300),1)-height);
	  }
	  setcolor(0);
	  setlinestyle(2,1);
	  line_w(0,fnP(1000),0,fnP(300));
	  setcolor(2);
	  sprintf(buf,"TEMPERATURE (C)");
	  outgtext(buf,pmap(0,0)-getgtextextent(buf)/2,pmap(fnP(1000),1)+2*height+3);
	  setcolor(3);
	  sprintf(buf,"RELATIVE HUMIDITY"); 
	  outgtext(buf,pmap(0,0)-getgtextextent(buf)/2,pmap(fnP(300),1)-2*height-3);
	  setcolor(7);
	  sprintf(buf,"EQUIVALENT POTENTIAL INSTABILITY x 1E-3 K/m");
	  outgtext(buf,pmap(0,0)-getgtextextent(buf)/2,pmap(fnP(1000),1)+3*height+3);
	  /*Plot Temperature/RH Trace*/
	  if (numlvl>0) {
	    int x1,x2,nx1w,nx2w,x,y,first;
	    float RH;
    	    float const1,theta1,thetase1,theta2,thetase2,dthetasedz,midpres;
       	    drawWindbarbs(pmap(25,0));
	    /*setcliprgn(pmap(-25,0),pmap(fnP(300),1),pmap(25,0),pmap(fnP(1000),1));*/
	    /* Draw Temperature */
	    setcolor(2);
	    first=-1;
	    for (i=0;i<numlvl && sndg[i][1]>=300;i++) {
	      if (sndg[i][3]>-900.0)
	        if (first) {
	          moveto_w(sndg[i][3],fnP(sndg[i][1]));
	          first=0;
	        }
	        else
	          lineto_w(sndg[i][3],fnP(sndg[i][1]));
	      ;
	    }
	    

	    /*RH plot in x-direction requires re-scale of window*/
	    /*Grab x/y coordinate @ T,p=-25,300 & T,p=25,300 */
	    x1=pmap(-25,0);x2=pmap(25,0);
	
	    /*Extrapolate new x1 and x2 external window coords*/
	    nx1w=100.0/(x2-x1)*(-x1);
            nx2w=100.0/(x2-x1)*(skv.brx+14-x1);
	
	    setwindow(nx1w,fnP(Y0),nx2w,fnP(YM));
	    setcolor(3);
	    first=-1;
	    for(i=0;i<numlvl && sndg[i][1]>=300;i++) {
	      if (sndg[i][3]>-900.0) {
        	RH=100*mixratio(sndg[i][1],sndg[i][4])/mixratio(sndg[i][1],sndg[i][3]);
	        if (first) {
		  moveto_w(RH,fnP(sndg[i][1]));
		  first=0;
		  }
		else
		  lineto_w(RH,fnP(sndg[i][1]));
              }
	    }	  

	    /*Plot Equivalent Potential Instability*/
	    /*dTheta-E/dz plot set to re-scale to temperature x 1E-3*/
	   
            setwindow(X0,fnP(Y0),XM,fnP(YM));
            setcliprgn(pmap(-25,0),pmap(fnP(300),1),pmap(25,0),pmap(fnP(1000),1));
            setcolor(7);
	    const1=2500000.0/1004.0;
	    first=-1;
	    for (i=0;i<numlvl-1 && sndg[i][1]>=100;i++) {
	      if (sndg[i][3] > -900.0 && sndg[i+1][3]>-900) {
	        theta1=theta(sndg[i][1],sndg[i][3],1000)+273.15;
	        thetase1=theta1*exp(const1*mixratio(sndg[i][1],sndg[i][3])*.001/(sndg[i][3]+273.15));
	        theta2=theta(sndg[i+1][1],sndg[i+1][3],1000)+273.15;
	        thetase2=theta2*exp(const1*mixratio(sndg[i+1][1],sndg[i+1][3])*.001/(sndg[i][3]+273.15));
	        /*Do D-Theta-se/dz*/
	        dthetasedz=(thetase2-thetase1)/(sndg[i+1][2]-sndg[i][2]);
	        midpres=(sndg[i][1]+sndg[i+1][1])/2;
	        if (first) {
	          moveto_w(dthetasedz*1E3,fnP(midpres));
	          first=0;
	        }
	        else
	          lineto_w(dthetasedz*1E3,fnP(midpres));
	      }
	    }
	  setcliprgn(1,1,xwdth,xhght); 
	    
	  }
	    
        }
        
        /*NP*/
        void draw_TURB()
        /*****************************************************************/
        /* DRAW_TURB                                                     */
        /* LARRY J. HINSON AWC/KCMO                                      */
        /*****************************************************************/
        {
	  char buf[80];
	  register i,j;
	  int X0=9,Y0=80,XM=-3.0,YM=1300;
	  int count=0;
	  int width,height;
	  float x1,x2,nx1w,nx2w;
	  setcliprgn(1,1,xwdth,xhght);
	  setcolor(0);
	  rectangle(1,1,1,skv.brx+14,skv.bry+skv.tly+14);
	  setcolor(1);
	  set_font(2);
	  setlinestyle(1,1);
          sprintf( buf, "%s (%s)", raobtitle, raob_type);
          outgtext ( buf, skv.tlx, 1 );
          update_text_values();
	  /*Get font height*/
	  height=getfontheight();
	  /*Establish Coordinate System*/
	  view(1,1,skv.brx+14,skv.bry+skv.tly+14);
	  setwindow(X0,fnP(Y0),XM,fnP(YM));
	  /*Draw and label Pressure lines*/
	  for (j=1000;j>=100;j-=100) {
	    line_w(8,fnP(j),-2,fnP(j));
	    itoa((short)j,buf,80);
	    outgtext(buf,pmap(8,0)-getgtextextent(buf)-2,pmap(fnP(j),1));
	  }
	  /*Draw and label Richardson Number Lines from Top*/
	  for (i=8;i>=-2;i--) {
	    setcolor(1);
	    if (i % 2 == 0) 
	      line_w(i,fnP(100),i,fnP(1000));
	    setcolor(7);
	    itoa((short)i,buf,80);
	    outgtext(buf,pmap(i,0)-getgtextextent(buf)/2,pmap(fnP(100),1)-height-2);
	  }
	  setcolor(0);
	  setlinestyle(2,1);
	  line_w(0,fnP(1000),0,fnP(100));
	  /*SHEAR plot in x-direction requires re-scale of window*/
	  /*Grab x/y coordinate @ Ri,p=8,300 & Ri,p=-2,300 */
	  x1=pmap(8,0);x2=pmap(-2,0);
	  /*Extrapolate new x1 and x2 external window coords*/
	  nx1w=50.0/(x2-x1)*(-x1);
          nx2w=50.0/(x2-x1)*(skv.brx+14-x1);
	  setwindow(nx1w,fnP(Y0),nx2w,fnP(YM));
          setcolor(11);
	  /* Label Wind Shear Lines from Bottom*/
	  for (i=0;i<=50;i+=5) {
	    itoa((short)i,buf,80);
	    outgtext(buf,pmap(i,0)-getgtextextent(buf)/2,pmap(fnP(1000),1)+height+2);
	  }
	  
	  /* Labels */
	  sprintf(buf,"%s","LN(RICHARDSON NUMBER)");
	  setcolor(7);
	  outgtext(buf,pmap(25,0)-getgtextextent(buf)/2,pmap(fnP(100),1)-2*height-2);
	  setcolor(11);
	  sprintf(buf,"%s","WIND SHEAR TKE PRODUCTION x 1E3 joules/sec");
	  outgtext(buf,pmap(25,0)-getgtextextent(buf)/2,pmap(fnP(1000),1)+2*height+2);
	  if (numlvl>0) {
	    float g,gammadry,meanT,meanTd,meanVirtualT,gamma,windshear,Ri,midpres;
	    float dthetadz,theta1,theta2,meanTheta;
            float u1,v1,u2,v2,u,v,windshearsqrd,midPres;
            float dz,tke_windshear_prod;
            float lastptx,lastpty,lastpts;
            int s1,s2;
            FILE *fil1;
            int first=-1;
            drawWindbarbs(pmap(50,0));
	    /*Plot Graph of Richardson Number */
	    setcolor(4);
	    setcliprgn(pmap(0,0),pmap(fnP(100),1),pmap(50,0),pmap(fnP(1000),1)); 
	    g=9.8; /* m/s**2 */
	    for (i=0;i<numlvl-2 && sndg[i][1]>=100;i++) {
	      s1=i;
	      s2=i+1;
	      if (sndg[s1][3] > -900 & sndg[s2][3]> -900.0) {
	        u1=-sndg[s1][6]*sin(sndg[s1][5]*PI/180);
	        v1=-sndg[s1][6]*cos(sndg[s1][5]*PI/180);
	        u2=-sndg[s2][6]*sin(sndg[s2][5]*PI/180);
	        v2=-sndg[s2][6]*cos(sndg[s2][5]*PI/180);
	        u=u2-u1;v=v2-v1;
	        windshear=sqrt(u*u+v*v)*.51479/(sndg[s2][2]-sndg[s1][2]);
	        midPres=(sndg[s1][1]+sndg[s2][1])/2;
	        theta1=theta(sndg[s1][1],sndg[s1][3],1000)+273.16;
	        theta2=theta(sndg[s2][1],sndg[s2][3],1000)+273.16;
	        meanTheta=(theta1+theta2)/2.0;
	        dz=sndg[s2][2]-sndg[s1][2];
	        dthetadz=(theta2-theta1)/dz;
	        if (windshear != 0.0) {
	      	  windshearsqrd=(windshear*windshear);
	      	  Ri=(g/meanTheta)*(dthetadz/windshearsqrd);
	          if (! first) {
	            setcolor(7);
	            setwindow(X0,fnP(Y0),XM,fnP(YM));
	            line_w(lastptx,lastpty,log(Ri),fnP(midPres));
	            setcolor(11);
	            setwindow(nx1w,fnP(Y0),nx2w,fnP(YM));
	            tke_windshear_prod=0.54*dz*windshearsqrd;
	            /*line_w(lastpts,lastpty,windshearsqrd*10000,fnP(midPres));*/
	            line_w(lastpts,lastpty,tke_windshear_prod*100,fnP(midPres));
	          }
	          lastptx=log(Ri);
	          lastpty=fnP(midPres);
	          lastpts=0.54*dz*windshearsqrd*100;
	          first=0;
	        }
	      }
	    }
	  }
	  setcliprgn(1,1,xwdth,xhght); 
	}
	
	/*NP*/
	void draw_Clouds( void )
	/*****************************************************************/
	/* DRAW_CLOUDS                                                   */
	/* LARRY J. HINSON AWC/KCMO                                      */
	/*****************************************************************/
	{
		int startflag,endflag,s1,s2,s3,i,spsub,epsub;
		float T1,T2,T3,dz,d2T,R1,R2,R3,d2R,startpres,endpres,x1,x2,y,t2,p2;
		float Tavg,DDavg,DD;
		float dd1,dd2,dd3,d2x;
		int cloudAmt,top,basefound;
		float d2xparam=0.0000;
		float Taccum=0.0;
		float DDaccum=0.0;
		float TCount=0.0;
		char buf[80];
		startflag=0;
		endflag=0;
		/*******/
		pagenum = 1;
		clear_paramarea();
		draw_skewt();
		/*show_page( pagenum );*/
		mode=6;

		/*******/
		for (i=1;i<numlvl-1;i++) {
			s1=i-1;
			s2=i;
			s3=i+1;
			if (sndg[s1][3]>-900.0 && sndg[s2][3] > -900.0 && sndg[s3][3]>-900) {
				T1=sndg[s1][3];
				T2=sndg[s2][3];
				T3=sndg[s3][3];
				dz=sndg[s3][2]-sndg[s1][2];
				if (dz==0) dz=1;
				d2T=(T3-2*T2+T1)/(dz*dz);
				R1=100*mixratio(sndg[s1][1],sndg[s1][4])/mixratio(sndg[s1][1],sndg[s1][3]);
				R2=100*mixratio(sndg[s2][1],sndg[s2][4])/mixratio(sndg[s2][1],sndg[s2][3]);
				R3=100*mixratio(sndg[s3][1],sndg[s3][4])/mixratio(sndg[s3][1],sndg[s3][3]);
				d2R=(R3-2*R2+R1)/(dz*dz);
				if (d2T>=0 && d2R<=0 && !startflag) {
					startflag=-1;
					startpres=sndg[s2][1];
					spsub=s2;

				}
				else if ( !(d2T>=0 && d2R<=0) && startflag) {
					startflag=0;
					endpres=sndg[s2][1];
					epsub=s2;
					Tavg=Taccum/TCount;
					DDavg=DDaccum/TCount;
					cloudAmt=getCloudAmount(Tavg,DDavg);
					Taccum=0.0;
					DDaccum=0.0;
					TCount=0;
					switch (cloudAmt) {
					case 1:
						strcpy(buf,"OVC");
						break;
					case 2:
						strcpy(buf,"BKN");
						break;
					case 3:
						strcpy(buf,"SCT");
						break;
					case 4:
						strcpy(buf,"FEW");
						break;
					}
					if (cloudAmt != 4) {
						setcolor(5);
						XFillRectangle(XtDisplay(draw_reg), canvas, gc,90,pres_to_pix(endpres),
								140-90,pres_to_pix(startpres)-pres_to_pix(endpres));
						setcolor(8);
						set_font(2);
						setlinestyle(1,1);
						outgtext(buf,100,pres_to_pix(endpres));
					}
				}
				;
				if ((d2T>=0 && d2R<=0) && startflag) {
					Taccum+=sndg[s2][3];
					DD=sndg[s2][3]-sndg[s2][4];
					DDaccum+=DD;
					TCount++;
				}
			}
		}
		top=0;
		for (i=numlvl-1;i>0; i--) {
			basefound=0;
			s1=i-1;
			s2=i;
			s3=i+1;
			if (sndg[s1][3]>-900.0 && sndg[s2][3] > -900.0 && sndg[s3][3]>-900) {

				if (! top) {
					dd1=sndg[s1][3]-sndg[s1][4];
					dd2=sndg[s2][3]-sndg[s2][4];
					dd3=sndg[s3][3]-sndg[s3][4];
					dz=(sndg[s3][2]-sndg[s1][2])/2.0;
					if (dz==0) dz=1;
					d2x=(dd3-2*dd2+dd1)/(dz*dz);
					if (d2x>0 && dd2<4.5) {
						top=-1;
						endpres=sndg[s2][1];
						epsub=s2;
						/* Now work downward till you get moistening with height */
						/* Do this until you reach lowest level of this condition */
						;
						while(i>1 && ! basefound) {
							i--;
							s1=i-1;
							s2=i;
							s3=i+1;
							if (sndg[s1][3]>-900.0 && sndg[s2][3] > -900.0 && sndg[s3][3]>-900) {
								dd1=sndg[s1][3]-sndg[s1][4];
								dd2=sndg[s2][3]-sndg[s2][4];
								dd3=sndg[s3][3]-sndg[s3][4];
								dz=(sndg[s3][2]-sndg[s1][2])/2.0;
								if (dz==0) dz=1;
								d2x=(dd3-2*dd2+dd1)/(dz*dz);
								if (d2x < -d2xparam) {
									while ((d2x < -d2xparam  && dd2<4.5) && i>1) {
										i--;
										s1=i-1;
										s2=i;
										s3=i+1;
										if (sndg[s1][3]>-900.0 && sndg[s2][3] > -900.0 && sndg[s3][3]>-900) {
											dd1=sndg[s1][3]-sndg[s1][4];
											dd2=sndg[s2][3]-sndg[s2][4];
											dd3=sndg[s3][3]-sndg[s3][4];
											dz=(sndg[s3][2]-sndg[s1][2])/2.0;
											if (dz==0) dz=1;
											d2x=(dd3-2*dd2+dd1)/(dz*dz);
										}
									}
									/* Lowest level of drying found...compute LCL from s1*/
									drylift(sndg[s1][1],sndg[s1][3],sndg[s1][4],&p2,&t2);
									startpres=p2;
									if (startpres<endpres) {
										startpres=sndg[s1][1];
									}
									spsub=s1;
									top=0;
									basefound=-1;
									setcolor(2);
									XFillRectangle(XtDisplay(draw_reg), canvas, gc,50,pres_to_pix(endpres),
											90-50,pres_to_pix(startpres)-pres_to_pix(endpres));

								}
							}
						}
					}
				}
			}
		}
	}
	      
	/*NP*/
	void draw_hodo( void )
	/*************************************************************/
	/*  DRAW_HODO                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
        /*  Draws a standard Hodograph display.                      */ 
        /*************************************************************/
	{
	short x1, y1, x2, y2, i, y3;
	float scle, wdir, wspd, mnu, mnv, ix1, ix2, ix3, ix4;
	char st[10];
	char rtitle[200];

	setcolor(0);
	rectangle( 1, 1, 1, hov.brx+14, hov.bry+14);
	setcolor(1);
	set_font( 2 );
	setcliprgn(hov.tlx, hov.tly, hov.brx, hov.bry);
	setlinestyle( 1, 1 );
	rectangle( 0, hov.tlx, hov.tly, hov.brx, hov.bry);

	/* ----- Plot crosshairs ----- */
	 setcolor(31);
	hodo_to_pix( 180, 60, &x1, &y1);
	moveto( x1, hov.tly);
	lineto( x1, hov.bry);

	setcolor(31);
	hodo_to_pix( 270, 60, &x1, &y1);
	moveto( hov.tlx, y1);
	lineto( hov.brx, y1);

	/* ----- Plot Radius circles ----- */
	setcolor (24);
       setlinestyle( 2, 1 );
	hodo_to_pix( 0, 0, &x1, &y1);
	x2 = x1;
	y2 = y1;
	for(i = hov.scale; i <= hov.hodomag; i = i + hov.scale)
	   {
	   hodo_to_pix( 0, (float)i, &x1, &y1);
	   y3 = ( y1 - y2 );
	   ellipse( 0, x2-y3, y2-y3, x2+y3, y2+y3 );
	   }

	setcolor( 1 );
	/* ----- Plot X-Coord hash marks ----- */
	for(i = hov.scale; i <= hov.hodomag; i = i + hov.scale)
	   {
	   hodo_to_pix( 180, (float)i, &x1, &y1);
	   moveto( x1-3, y1);
	   lineto( x1+3, y1);
	   itoa( i, st, 10 );
	   outgtext ( st, x1 - getgtextextent( st ) - 4, y1 - 5 );

	   hodo_to_pix( 360, (float)i, &x1, &y1);
	   moveto( x1-3, y1);
	   lineto( x1+3, y1);
	   itoa( i, st, 10 );
	   outgtext ( st, x1 - getgtextextent( st ) - 4, y1 - 5 );
	   }

	/* ----- Plot Y-Coord hash marks ----- */
	setcolor(1);
	for(i = hov.scale; i <= hov.hodomag; i = i + hov.scale)
	   {
	   hodo_to_pix( 90, (float)i, &x1, &y1);
	   moveto( x1, y1-3);
	   lineto( x1, y1+3);
	   itoa( i, st, 10 );
	   outgtext ( st, x1 - (getgtextextent( st ) / 2), y1 + 5 );   

	   hodo_to_pix( 270, (float)i, &x1, &y1);
	   moveto( x1, y1-3);
	   lineto( x1, y1+3);
	   itoa( i, st, 10 );
	   outgtext ( st,  x1 - (getgtextextent( st ) / 2), y1 + 5 );
	   }

	/* ----- Plot Hodograph (Shear Vectors) ----- */
	 setcolor(2);
	 setlinestyle( 1, 2 );
	if ( numlvl > 0 ) trace_hodo( 3 );

	/* ----- Plot Mean Wind Vector ----- */
	setcolor(5);
	mean_wind( -1, -1, &mnu, &mnv, &wdir, &wspd);
	hodo_to_pix( wdir, wspd, &x1, &y1);
	moveto( x1, y1);
	rectangle( 0, x1-4, y1-4, x1+4, y1+4);


        /* ----- Plot 30/75 Storm Motion Vector ----- */        
        mean_wind( sndg[sfc()][1], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        setcolor(11);
        setlinestyle( 1, 1 );
        ix4 *= .75;
        ix3 += 30;
        if(ix3>360) ix3 -= 360;
        hodo_to_pix( ix3, ix4, &x1, &y1);
        moveto(x1-3, y1); lineto(x1+3, y1);
        moveto(x1, y1-3); lineto(x1, y1+3);
        ellipse( 0, x1-3, y1-3, x1+3, y1+3 );

        
        /* ----- Plot 15/85 Storm Motion Vector ----- */
        mean_wind( sndg[sfc()][1], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
        setcolor(12);
        setlinestyle( 1, 1 );
        ix4 *= .85;
        ix3 += 15;
        if(ix3>360) ix3 -= 360;
        hodo_to_pix( ix3, ix4, &x1, &y1);
        moveto(x1-3, y1); lineto(x1+3, y1);
        moveto(x1, y1-3); lineto(x1, y1+3);
        ellipse( 0, x1-3, y1-3, x1+3, y1+3 );


	/* ----- Plot Current Storm Motion Vector ----- */
	setcolor(31);
	setlinestyle( 1, 1 );
	hodo_to_pix( st_dir, st_spd, &x1, &y1);
	moveto(x1-6, y1); lineto(x1+6, y1);
	moveto(x1, y1-6); lineto(x1, y1+6);
	ellipse( 0, x1-6, y1-6, x1+6, y1+6 );

	/* ----- Display Hodograph Inset ----- */
	draw_hoinset();

       /* ----- Draw final outline of hodograph ----- */
       setcolor(1);
	setlinestyle( 1, 1 );
	rectangle( 0, hov.tlx, hov.tly, hov.brx, hov.bry);
	setcliprgn ( 1, 1, xwdth, xhght );
	setcolor(1);
	set_font( 1 );
        sprintf( rtitle, "%s (%s)", raobtitle, raob_type);
	outgtext ( rtitle, skv.tlx, 1 );

	XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg), 
		    gc, 0, 0, xwdth, xhght, 0, 0 );
	}

	/*NP*/
	void hodo_to_pix( float dir, float mag, short *x, short *y )
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

	scle = (hov.brx - hov.tlx) / hov.hodomag;
	midx = hov.tlx + ((hov.brx - hov.tlx) / 2) + (hov.xshift * scle);
	midy = hov.tly + ((hov.bry - hov.tly) / 2) - (hov.yshift * scle);

	*x = (short)(midx - (ucomp( dir, mag ) * scle));
	*y = (short)(midy + (vcomp( dir, mag ) * scle));
	}

	/*NP*/
	void pix_to_hodo( short x, short y, float *dir, float *mag )
	/*************************************************************/
	/*  PIX_TO_HODO                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the wind vector (dir, mag) in knots of the    */
	/*  screen location (x,y).                                   */
	/*************************************************************/
	{
	float midx, midy, scle, u, v;

	scle = (hov.brx - hov.tlx) / hov.hodomag;   /* pixels/knot */
	midx = hov.tlx + ((hov.brx - hov.tlx) / 2) + (hov.xshift * scle);
	midy = hov.tly + ((hov.bry - hov.tly) / 2) - (hov.yshift * scle);

	u = (midx - x) / scle;
	v = (y - midy) / scle;

	*dir = angle(u, v);
	*mag = (float)sqrt((u * u) + (v * v));
	}

	/*NP*/
	void trace_hodo( short width )
	/*************************************************************/
	/*  TRACE_HODO                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental wind shear vectors on Hodograph.     */
	/*************************************************************/
	{
	short i, j, x, y, xold, yold, ok = 0;

	setlinestyle( 1, width );
	for( i=0; i < numlvl; i++)
	   {
	   if( qc(sndg[i][5]) && qc(sndg[i][6]))
	      {

	      if ( sndg[i][2] <= msl(3000) )
	        setcolor(2);
	      else if ( sndg[i][2] > msl(3000) && sndg[i][2] <= msl(6000) )
	        setcolor(3);
	      else if ( sndg[i][2] > msl(6000) && sndg[i][2] <= msl(9000) )
	        setcolor(5);
	      else if ( sndg[i][2] > msl(9000) && sndg[i][2] <= msl(12000) )
	        setcolor(6);
	      else if ( sndg[i][2] > msl(12000) )
	        setcolor(28);

	      xold = x;
	      yold = y;
	      hodo_to_pix( sndg[i][5], sndg[i][6], &x, &y);
	      if( ok == 0)
		 {
		 moveto( x, y);
		 ok = 1;
		 }
	      else
		 {
	        moveto( xold, yold);
		 lineto( x, y);
		 }
	      }
	   }
	}

	/*NP*/
	void dry_adiabat( float thta)
	/*************************************************************/
	/*  DRY_ADIABAT                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws dry adiabat of theta (thta, c) on SkewT graphic.   */
	/*************************************************************/
	{
	float pres, temp;
	short x, y;

	setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
	for( pres = 1050; pres >= 100; pres = pres - 50 )
	   {
	   temp = ((thta + 273.15) / pow( 1000.0 / pres, ROCP )) - 273.15;

	   x = temp_to_pix( temp , pres );
	   y = pres_to_pix( pres );

	   /*
	   if( pres <= 200) { printf( "%f   %f   %d\n", pres, temp, x);}
	   */

	   if(pres == 1050)
	      { moveto(x, y); }
	   else
	      { lineto(x, y); }
	   }
	}
	
	/*NP*/
	void moist_adiabat(float thetae)
	/******************************************************************/
	/* Draw Moist adiabat of constant thetae from surface to top.     */
	/******************************************************************/
	{
	  float pres, startpres, temp,t1,t2;
	  short x,y;
	  setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
	  /* start at pres 1000 */
	  startpres=1000;
	  t1=thetae-273.16;
	  /* x=temp_to_pix(virtemp(startpres,t1,t1),startpres); */
          x=temp_to_pix(t1,startpres);
	  y=pres_to_pix(startpres);
	  moveto(x,y);
	  for(pres =950; pres>=100;pres-=50) {
	    t2=wetlift(startpres,t1,pres);
	    /* x=temp_to_pix(virtemp(pres,t2,t2),pres); */
            x=temp_to_pix(t2,pres);
	    y=pres_to_pix(pres);
	    lineto(x,y);
 	  }
	}
	
	void mixingratio(float w)
	/*****************************************************************/
	/* Draw line of constant mixing ratio from sfc to top of sounding*/
	/*****************************************************************/
	{
          float pres, startpres, temp,t1,t2;
	  short x,y;
	  setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
	  for (pres=1000;pres>=100;pres-=50) {
	    t1=temp_at_mixrat(w,pres);
	    x=temp_to_pix(t1,pres);
	    y=pres_to_pix(pres);	
	    if (pres==1000)
	      moveto(x,y);
	    else
	      lineto(x,y);
	  }
	}

	/*NP*/
	void isotherm( float temp)
	/*************************************************************/
	/*  ISOTHERM                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Draws temperature lines (temp, c) on SkewT graphic.      */
	/*************************************************************/
	{
	short x, y;
	char st[10];

        setcolor(24);
	x = temp_to_pix( temp, 1050 );
	y = skv.bry;
	if((temp >= -30) && (temp <= 50))
	   {
	    setcliprgn(1, 1, xwdth, xhght);
	    itoa( (short)temp, st, 10 );
            setcolor(1);
	    outgtext ( st, x - (getgtextextent( st ) / 2), y );
            if (temp != 0)
              setcolor(24);
            else {
              setcolor(26);
            }
	   }
	 setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
	moveto( x, y );
	x = temp_to_pix( temp, 100 );
	y = skv.tly;
	lineto( x, y );
	}

	/*NP*/
	void isobar( float pres, short flag)
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

	setcliprgn(1, 1, xwdth, xhght); 
	y = pres_to_pix( pres );
	if( flag == 0 )
	   {
	   moveto( skv.tlx, y);
	   itoa( (short)pres, st, 10 );
	   outgtext ( st, skv.tlx - getgtextextent( st ) - 2, y - 5 );
	   setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
	   moveto( skv.tlx, y);
	   lineto( skv.brx, y);
	   }
	else
	   {
	   moveto( skv.tlx, y);
	   lineto( skv.tlx + 5, y);
	   moveto( skv.brx, y);
	   lineto( skv.brx - 5, y);
	   }
	}

	/*NP*/
	void trace_temp( short width )
	/*************************************************************/
	/*  TRACE_TEMP                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental temperature trace on SkewT.          */
	/*************************************************************/
	{
	short i, j, x, y, xold, yold, ok = 0;

        setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
        setlinestyle( 1, width );
	for( i=0; i < numlvl; i++)
	   {
	   if( sndg[i][3] > -200)
	      {
	      xold = x;
	      yold = y;
	      x = temp_to_pix( sndg[i][3], sndg[i][1] );
	      y = pres_to_pix( sndg[i][1] );
	      if( ok == 0)
		 {
		 moveto( x, y);
		 ok = 1;
		 }
	      else
		 {
	        moveto( xold, yold);
	        lineto( x, y);
		 }
	      }
	   }
	}


        /*NP*/
        void trace_temp2( short width )
        /*************************************************************/
        /*  TRACE_TEMP2                                              */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Plots environmental temperature trace on SkewT.          */
        /*************************************************************/
        {
        short i, j, x, y, xold, yold, ok = 0, numlvl2;

        numlvl2 = sndg2[0][0];
        setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
        setlinestyle( 1, width );
        for( i=0; i < numlvl2; i++)
           {
           if( sndg2[i][3] > -200)
              {
              xold = x;
              yold = y;
              x = temp_to_pix( sndg2[i][3], sndg2[i][1] );
              y = pres_to_pix( sndg2[i][1] );
              if( ok == 0)
                 {
                 moveto( x, y);
                 ok = 1;
                 }
              else
                 {
                moveto( xold, yold);
                lineto( x, y);
                 }
              }
           }
        }


	/*NP*/
	void trace_vtmp( short width )
	/*************************************************************/
	/*  TRACE_VTMP                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots virtual temperature trace on SkewT.                */
	/*************************************************************/
	{
	short i, j, x, y, xold, yold, ok = 0;

       setlinestyle( 3, width );
	for( i = 0; i < numlvl; i++)
	   {
	   if( qc(sndg[i][3]) && qc(sndg[i][4]))
	      {
	      xold = x;
	      yold = y;
	      x = temp_to_pix( i_vtmp(sndg[i][1]), sndg[i][1] );
	      y = pres_to_pix( sndg[i][1] );
	      if( ok == 0)
		 {
		 moveto( x, y);
		 ok = 1;
		 }
	      else
		 {
		 moveto( xold, yold);
		 lineto( x, y);
		 }
	      }
	   }
	}


	/*NP*/
	void trace_dwpt( short width )
	/*************************************************************/
	/*  TRACE_DWPT                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental Dew Point trace on SkewT.            */
	/*************************************************************/
	{
	short i, j, x, y, xold, yold, ok = 0;

	setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
        setlinestyle( 1, width );
	for( i=0; i < numlvl; i++)
	   {
	   if( sndg[i][4] > -200)
	      {
	      xold = x;
	      yold = y;
	      x = temp_to_pix( sndg[i][4], sndg[i][1] );
	      y = pres_to_pix( sndg[i][1] );
	      if( ok == 0)
		 {
		 moveto( x, y);
		 ok = 1;
		 }
	      else
		 {
	        moveto( xold, yold);
	        lineto( x, y);
		 }
	      }
	   }
	}


        /*NP*/
        void trace_dwpt2( short width )
        /*************************************************************/
        /*  TRACE_DWPT2                                              */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Plots environmental Dew Point trace on SkewT.            */
        /*************************************************************/
        {
        short i, j, x, y, xold, yold, ok = 0, numlvl2;

        numlvl2 = sndg2[0][0];
        setcliprgn(skv.tlx, skv.tly, skv.brx, skv.bry);
        setlinestyle( 1, width );
        for( i=0; i < numlvl2; i++)
           {
           if( sndg2[i][4] > -200)
              {
              xold = x;
              yold = y;
              x = temp_to_pix( sndg2[i][4], sndg2[i][1] );
              y = pres_to_pix( sndg2[i][1] );
              if( ok == 0)
                 {
                 moveto( x, y);
                 ok = 1;
                 }
              else
                 {
                moveto( xold, yold);
                lineto( x, y);
                 }
              }
           }
        }


	/*NP*/
	void trace_wetbulb( short width )
	/*************************************************************/
	/*  TRACE_WETBULB                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots environmental Wetbulb trace on SkewT.              */
	/*************************************************************/
	{
	short i, j, x, y, xold, yold, ok = 0;
	float t1;

       setlinestyle( 1, width );
	for( i=0; i < numlvl; i++)
	   {
	   if(sndg[i][4] > -200)
	      {
	      xold = x;
	      yold = y;

	      t1 = wetbulb( sndg[i][1], sndg[i][3], sndg[i][4] );

	      x = temp_to_pix( t1, sndg[i][1] );
	      y = pres_to_pix( sndg[i][1] );
	      if( ok == 0)
		 {
		 moveto( x, y);
		 ok = 1;
		 }
	      else
		 {
		 moveto( xold, yold);
		 lineto( x, y);
		 }
	      }
	   }
	}

	/*NP*/
	short pres_to_pix( float pres )
	/*************************************************************/
	/*  PRES_TO_PIX                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given pressure (mb) to an Y coordinate on       */
	/*  Skewt graphic.                                           */
	/*************************************************************/
	{
	double scl1, scl2;
	scl1 = log(1050.) - log(100.);
	scl2 = log(1050.) - log(pres);
	return (short)(skv.bry - (scl2 / scl1) * (skv.bry - skv.tly));
	}

	/*NP*/
	float pix_to_pres( short pix )
	/*************************************************************/
	/*  PIX_TO_PRES                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given Y coordinate on Skewt graphic to          */
	/*  pressure(mb).                                            */
	/*************************************************************/
	{
	double scl1, scl2, scl3;
	scl1 = log(1050.) - log(100.);
	scl2 = skv.bry - (double)pix;
	scl3 = skv.bry - skv.tly + 1;
	return (float)(1050 / exp((scl2 / scl3) * scl1));
	}

	/*NP*/
	short temp_to_pix( float temp, float pres )
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

	if( skv.type == 1 )
	   {
	   scl1 = (float)skv.brtemp - ((((float)skv.bry -
	   (float)pres_to_pix( pres )) / ((float)skv.bry - (float)skv.tly)) *
	   (float)skv.vspread);
	   }
	else
	   {
	   scl1 = skv.brtemp;
	   }
	scl2 = skv.brx - (((scl1 - temp) / skv.hspread) * (skv.brx - skv.tlx));
	return (short)scl2;
	}

	/*NP*/
	float pix_to_temp( short x, short y )
	/*************************************************************/
	/*  PIX_TO_TEMP                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given X/Y coordinates to temperature(c) on      */
	/*  Thermodynamic diagram.                                   */
	/*************************************************************/
	{
	float scl1, scl2, scl3;

	scl1 = 1 - (((float)x - (float)skv.tlx) / ((float)skv.brx - (float)skv.tlx));
	scl2 = (float)skv.brtemp - (scl1 * skv.hspread);
	scl1 = 1 - (((float)y - (float)skv.tly) / ((float)skv.bry - (float)skv.tly));
	scl3 = scl2 - (scl1 * (float)skv.vspread);
	return scl3;
	}

	/*NP*/
	void trace_parcel(float pres, float temp, float dwpt)
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
        
        if(!qc(pres) || !qc(temp) || !qc(dwpt) ) return;

        setcolor(31);
        setlinestyle( 4, 1 );
        /* Doing Both non-virtual & virtual temp trace based on global
           switch settings*/
        if (g_Parcel) {
	  x = temp_to_pix( temp, pres );
	  y = pres_to_pix( pres );
	  moveto( x, y );

	  drylift(pres, temp, dwpt, &p2, &t2);
	  x = temp_to_pix( t2, p2 );
	  y = pres_to_pix( p2 );
	  lineto( x, y );

	  for(i = p2 - 50; i >= 100; i = i - 50)
	     {
	     t3 = wetlift(p2, t2, i);
	     x = temp_to_pix( t3, i );
	     y = pres_to_pix( i );
	     lineto( x, y );
	     }
	  t3 = wetlift(p2, t2, 100);
	  x = temp_to_pix( t3 , 100 );
	  y = pres_to_pix( 100 );
	  lineto( x, y );
        }
        
        if (g_TvParcelCor) {

	    x = temp_to_pix( virtemp(pres, temp, dwpt), pres );
	    y = pres_to_pix( pres );
	    moveto( x, y );

	    drylift(pres, temp, dwpt, &p2, &t2);
	    x = temp_to_pix( virtemp(p2, t2, t2), p2 );
	    y = pres_to_pix( p2 );
	    lineto( x, y );

	    for(i = p2 - 50; i >= 100; i = i - 50)
	       {
	       t3 = wetlift(p2, t2, i);
	       x = temp_to_pix( virtemp(i, t3, t3), i );
	       y = pres_to_pix( i );
	       lineto( x, y );
	       }
	    t3 = wetlift(p2, t2, 100);
	    x = temp_to_pix( virtemp(100, t3, t3), 100 );
	    y = pres_to_pix( 100 );
	    lineto( x, y );
        }
	}

	/*NP*/
	void wind_barb( float wdir, float wspd, short x, short y, short siz)
	/*************************************************************/
	/*  WIND_BARB                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots wind barb at location (x,y) for given wind.        */
	/*************************************************************/
	{
	short x1, y1, x2, y2, x3, y3, sped, flag, maxsiz=3;
	float dx, dy, spcx, spcy, wid, hgt;

	dx = ucomp(wdir, 10) * siz / 1.5;
	dy = vcomp(wdir, 10) * siz / 1.5;

	x1 = x;
	y1 = y;
	x2 = x1 + (short)dx;
	y2 = y1 - (short)dy;

	/* ----- Draw backbone of wind barb, along with origin dot ----- */
	if(siz > maxsiz)
	   { setlinestyle(1, 2); }
       else
          { setlinestyle( 1, 1 ); }

	rectangle( 0, x1-1, y1-1, x1+1, y1+1);
	moveto(x1, y1); lineto(x2, y2);

	sped = (short)wspd;
	x1 = x2;
	y1 = y2;

	wid = 5;                        /* Number of flags that will fit */
	spcx = dx / wid;
	spcy = dy / wid;
	x1 = x1 + (short)spcx;
	y1 = y1 - (short)spcy;

	/* ----- Draw wind flags (increments of 50kt) ----- */
	flag = 0;
	while (sped > 47)
	   {
	   flag = 1;
	   x1 = x1 - (short)spcx;
	   y1 = y1 + (short)spcy;

	   hgt = .5F;                   /* Heigth of flags */
	   x2 = x1 + (short)(dy * hgt);
	   y2 = y1 + (short)(dx * hgt);

	   x3 = x1 - (short)spcx;
	   y3 = y1 + (short)spcy;

	   moveto( x1, y1);
	   lineto( x2, y2);
	   lineto( x3, y3);

	   x2 = (x1 + x2 + x3) / 3;
	   y2 = (y1 + y2 + y3) / 3;

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

	   moveto( x3, y3); lineto( x2, y2);

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

	   moveto( x3, y3); lineto( x2, y2);
	   }
	}


	/*NP*/
	void plot_barbs( void )
	/*************************************************************/
	/*  PLOT_BARBS                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Plots wind barbs along side of thermo diagram.           */
	/*************************************************************/
	{
	short i, x, y;
	float lastpres;

	setcliprgn(1, 1, xwdth, xhght);
	lastpres = 1100;
	for( i = 0; i < numlvl; i++)
	   {
	   if( qc( sndg[i][5] ) )
	      {
	      y = pres_to_pix( sndg[i][1] );
	      x = skv.brx - 40;

	      if((sndg[i][2]-i_hght(lastpres)) > 400)
		 {
		 wind_barb( sndg[i][5], sndg[i][6], x, y, 4);
		 lastpres = sndg[i][1];
		 }
	      }
	   }
	}



        /*NP*/
        void vvel_profile( void )
        /*************************************************************/
        /*  VVEL_PROFILE                                             */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Plots vertical velocity profile                          */
        /*************************************************************/
        {
        short i, x1, x2, y, vvel, avail, leng;
        float lastpres;

        avail=0;
        setcliprgn(1, 1, xwdth, xhght);
        lastpres = 1100;
        for( i = 0; i < numlvl; i++)
           {
           if(qc(sndg[i][0]) && (sndg[i][0] < 1))
              {
              avail++;
              y = pres_to_pix( sndg[i][1] );
              x1 = skv.tlx + 40;
              if (g_pfs_model_sel_sw) 
                leng = sndg[i][0] * 10;
              else
                leng = (sndg[i][0] * 1000);   /* Convert to Mbs/sec */
              x2 = x1 - (leng * 2);         /* Determine screen scale */
              setcolor(25);
              if(sndg[i][0] < 0) setcolor(12);
              moveto(x1, y);
              lineto(x2, y);
              }
           }
        
        /* ----- Draw Scales ----- */
        if(avail > 5)
           {
           setcolor(7);
           x1 = skv.tlx + 40;
           moveto( x1, skv.tly+40 );
           lineto( x1, skv.bry-10 );
           
           setlinestyle( 3, 1 );
           x1 = skv.tlx + 20;
           moveto( x1, skv.tly+40 );
           lineto( x1, skv.bry-10 );
           x1 = skv.tlx + 60;
           moveto( x1, skv.tly+40 );
           lineto( x1, skv.bry-10 );
           
           set_font(2);
           outgtext( "OMEGA", skv.tlx+18, skv.tlx);
           outgtext( "+10", skv.tlx+3, skv.tlx+15);
           outgtext( "-10", skv.tlx+43, skv.tlx+15);
           }
        }
        
float F1(float x) {
	if (x >= -10)
		return 1.0;
	else
		return (-.1*(x+70)+7);
}

float F2(float x) {
	if (x>=0)
		return 2.0;
	else if (x>=-10 && x<0)
		return (-.025*(x+10)+2.5);
	else
		return(-.125*(x+70)+10.0);
}

float F3(float x) {
	if (x>=0)
		return 3.0;
	else if (x>=-10 && x<0)
		return(-0.1*(x+10)+4.0);
	else
		return(-0.15*(x+50)+10.0);
}


int getCloudAmount(float temp,float dd) {
	if (dd<F1(temp))
		return 1;
	else if (dd<F2(temp))
		return 2;
	else if (dd<F3(temp))
		return 3;
	else
		return 4;
}


void set_g_TvParcelCor(int value) {
  g_TvParcelCor=value;
}

void set_g_Parcel(int value) {
  g_Parcel=value;
}

int get_g_TvParcelCor() {
  return g_TvParcelCor;
}

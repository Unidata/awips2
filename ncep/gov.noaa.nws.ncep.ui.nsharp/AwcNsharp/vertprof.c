	/************************************************************/
        /* VERTPROF.C                                               */
        /* Author: Larry J. Hinson, AWC                             */
        /*                                                          */
        /* This module houses the routines to interpolate and       */
        /*  render the wind data at any given mouse location in     */
        /*  the domain of wind data given this module.              */
        /*  Graphical objects drawn, include a vertical wind        */
        /*  profile and a hodograph.                                */
        /*  Wind data passed on to this module is in units of knots */
        /*  with heights in feet.                                   */
        /*  Data is converted to metric units for computations of   */
        /*   helicity, and streamwise and crosswise vorticity       */
        /************************************************************/ 
        
        #include "vertprof.h"
	#include "expdraw.h"
	#include <math.h>
	#define PI 3.14159
	Pixmap pv_vp_window,pv_hodo_window;
	static int MAXWND=50.0;
	static int g_vppixcreated=0;
	static int g_hodopixcreated=0;

	void InitSaveVertProfBg(Widget pv_vertprof)
	{
	  int j;
	  char buf[80];
	  Window root;
	  InitWindowSystem(pv_vertprof,1);
	  setfontl(2);
	  view(0,0,VP_WDTH,VP_HGHT);
	  setwindow(0.0, 40000.0, 2.0, -5000.0);
    	  setcolorfg(6);
	  rectanglel(1,1,VP_WDTH-1,VP_HGHT-1);
	  for (j=0;j<=40000;j+=5000) {
	    sprintf(buf,"%u",j);
	    outtextxy(pmap(1.0,0), pmap((float) j,1), buf);
	  }
	  for (j=0;j<=40000;j+=1000)
	    if (j % 5000 != 0) {
              setcolorfg(24);
	      line_w(0.95,j,1.05,j);
	    }
	    else {
	      setcolorfg(6);
	      line_w(0.9,j,1.0,j);
	    }
	  ;
	  root=DefaultRootWindow ( XtDisplay(pv_vertprof) );
	  if (! g_vppixcreated) {
	    pv_vp_window=XCreatePixmap(XtDisplay(pv_vertprof), root, VP_WDTH,VP_HGHT,8);
	    g_vppixcreated=-1;
	  }
	  XCopyArea(XtDisplay(pv_vertprof), XtWindow(pv_vertprof), pv_vp_window, 
	      l_gc, 0, 0, VP_WDTH, VP_HGHT, 0, 0 );
	}
	
	void InitSaveHodoBg(Widget pv_hodo)
	{
	  int j;
	  char buf[80];
	  Window root;
	  float theta,r,xw,yw;
	  
	  InitWindowSystem(pv_hodo,1);
	  setfontl(2);
	  setlinestylel(1,1);
  	  view(0,0,HODO_WDTH,HODO_HGHT);
  	  rectanglel_fill(1,1,0,HODO_WDTH-1,HODO_HGHT-1);
  	  setcolorfg(1);
  	  rectanglel(1,1,HODO_WDTH-1,HODO_HGHT-1);
	  setwindowscreen(-MAXWND-10, -MAXWND-20, MAXWND+10, MAXWND+30);
	  setcolorfg(26);
	  line_w(-MAXWND,0,MAXWND,0);
	  line_w(0,MAXWND,0,-MAXWND);
	  setcolorfg(6);
	  outtextxy_w(-5.0,MAXWND-10.0,"180");
	  outtextxy_w(-5.0,-(MAXWND-10.0),"360");
	  outtextxy_w(-MAXWND,2.0,"90");
	  outtextxy_w(MAXWND-10.0,2,"270");
	  setcolorfg(1);
	  for (theta=0;theta<=2*PI;theta+=(PI/200)) {
	    for (r=0;r<=MAXWND;r+=10) {
	      xw=r*cos(theta);
	      yw=r*sin(theta);
	      pset_w(xw,yw);
	    }
	    XFlush(XtDisplay(pv_hodo));
	  }
	  outtextxy_fill(2,7,6,"CELL MOT    /    ");
	  /*outtextxy_fill(2,7+textheight(),6," ST MOT      /    "); */
	  root=DefaultRootWindow ( XtDisplay(pv_hodo) );
	  if (!g_hodopixcreated) {
	    pv_hodo_window=XCreatePixmap(XtDisplay(pv_hodo), root, HODO_WDTH,HODO_HGHT,8);
	    g_hodopixcreated=-1;
	  }
	  XCopyArea(XtDisplay(pv_hodo), XtWindow(pv_hodo), pv_hodo_window, 
	    l_gc, 0, 0, HODO_WDTH, HODO_HGHT, 0, 0 );
  
	}

	
	
	void GetWindProfAtCurs(int istart,int jstart, int nsta, 
	  float staxywrk[][200], unsigned levels[], float actva[][39], int wnddirs[],
	  int wndspds[], int wndhts[], int *wndcnt, int slashcount,
	  int MinSearchRadius, float clng, float clat)
	{
	float va[50],r2[50],v2[50][39],R,Rt;
	int x1,y1,x2,y2,wxc,wyc,crange;
	int i,j,j1,j2,k,range=80,rangeinc=20;
	if (nsta < 3) return;
	for (i=0;i<50;i++) va[i]=0.0;
	x2=istart;
	y2=jstart;
	j2=0;crange=range;
	while (j2 < 3) {
	  Rt=0; j2 =0;
	  /* for (j1=1;j1<=nsta;j1++) { */
	  for (j1=0;j1<nsta;j1++) {
	    x1=staxywrk[0][j1];
	    y1=staxywrk[1][j1];
	    R=pow(x2-x1,2)+pow(y2-y1,2);
	    if (R < 10) {
	      for (k=1;k<=slashcount;k++) {
	        va[k]=actva[j1][k];
	      }
	      goto bypassinterp;
	    }
	    if (sqrt(R) < crange) {
	      j2++;
	      r2[j2]=R;
	      for (k=1;k<=slashcount;k++)
	        v2[j2][k]=actva[j1][k];
	      Rt=Rt+1/r2[j2];
	    }
	  }
	  crange+=rangeinc;
	}
	for (j1=1;j1<=j2;j1++)
          for (k=1;k<=slashcount;k++)
            va[k]+=v2[j1][k]/r2[j1];
        for (k=1;k<=slashcount;k++)
           va[k]=va[k]/Rt;
bypassinterp:
        *wndcnt=0;
        for (i=1;i<=19;i++) {
          wxc=va[i*2-1];
          wyc=va[i*2];
          if (!(wxc == 0 && wyc == 0)) {
            (*wndcnt)++;
	    wnddirs[*wndcnt] = 180 + ARCCOSSINE(wyc / hypot(wxc, wyc)) * SGN(wxc) * 180 / PI;
	    wndspds[*wndcnt] = hypot(wxc, wyc);
	    wndhts[*wndcnt]=levels[i];
	  };
	}

        }	    
	        
float ARCCOSSINE (float x) {
	if (x == 1)
		return(0);
	else if (x == -1)
		return(PI);
	else
	        return(-atan(x / sqrt(-x * x + 1)) + PI / 2);

}

void InitProfWindow(Widget pv_vertprof) {
  g_display=XtDisplay(pv_vertprof);
  g_window=XtWindow(pv_vertprof);
  g_gc=XCreateGC(g_display, g_window, 0, 0);
  initturtle(VP_WDTH,VP_HGHT);
}

void DrawPVVertWinds(int wndcnt, int wnddirs[], int wndspds[], int wndhts[],
  Widget pv_vertprof) {
  int i;
  InitWindowSystem(pv_vertprof,1);
  setcolorfg(6);
  view(0,0,VP_WDTH,VP_HGHT);
  setwindow(0.0, 40000.0, 2.0, -5000.0);
  XCopyArea(XtDisplay(pv_vertprof), pv_vp_window, XtWindow(pv_vertprof),  
    l_gc, 0, 0, VP_WDTH, VP_HGHT, 0, 0 );
  for (i=1;i<= wndcnt;i++) {
    drawwind(pmap(1.0,0),pmap((float)wndhts[i],1),wnddirs[i],wndspds[i],5);
  }
  
}


void DrawPVHodograph(int wndcnt, int wnddirs[], int wndspds[], int wndhts[],
  Widget pv_hodo) {
  int i,startbaseht;
  float u1,u2,v1,v2,utot,vtot, um, vm, sangle, angle, su, sv, magS, magV;
  float helicity,heightwrk,uo,vo,suo,svo,su1,sv1,lhel;
  float hel1km=0,hel2km=0,hel3km=0;
  int sw,swndcnt,startbasesub;
  /* Use for computing streamwise vorticity: */
  float dz,WHi,WHj,WH,costheta,WS,wstot,WE,wetot;
  float umean,vmean,sumean,svmean,magwndmean,nspd,sumeanu,svmeanu,WSi,WSj;
  float wndshear;
  char buf[80];
  int sign=0;
  InitWindowSystem(pv_hodo,1);
  setcolorfg(31);
  setfontl(2);
  view(0,0,HODO_WDTH,HODO_HGHT);
  setwindowscreen(-MAXWND-10, -MAXWND-10, MAXWND+10, MAXWND+20);
  XCopyArea(XtDisplay(pv_hodo), pv_hodo_window, XtWindow(pv_hodo),  
    l_gc, 0, 0, HODO_WDTH, HODO_HGHT, 0, 0 );
  sw=0; utot=0,vtot=0,swndcnt=0;
  setcolorfg(2);
  setlinestylel(1, 3);
  for (i=1;i<=wndcnt;i++) {
    if (wnddirs[i]>0 && wndspds[i]>0) {
        startbaseht=wndhts[i];
        startbasesub=i;
        break;
    }
  }
  for (i=1;i<=wndcnt;i++) {
    u1=-wndspds[i] * sin(wnddirs[i] * PI / 180);
    v1=-wndspds[i] * cos(wnddirs[i] * PI / 180);
    if (wndhts[i] <= 19680+startbaseht) {   /* Compute 0-6KM AGL wind 19680 is feet */
	utot = utot + u1;
	vtot = vtot + v1;
	swndcnt = swndcnt + 1;
    }
    
    if (wndhts[i] <= 9840+startbaseht) {    /* Compute 0-3KM AGL Hodograph Shear Vectors */
      if (sw) {
	circle_w (u1, v1, 2);
	line_w(u1, v1, u2, v2);
	u2 = u1; v2 = v1;
      } else {
        circle_w (u1, v1, 2);
        u2 = u1; v2 = v1;
        sw = -1;
      }
    } else {
      if (wndhts[i] > 9840+startbaseht && wndhts[i] <= 19680+startbaseht) {
        setcolorfg(3);
        circle_w(u1,v1,2);
        line_w(u1,v1,u2,v2);
        u2 = u1; v2=v1;
      }
    }
    
  }
  um = utot / swndcnt;
  vm = vtot / swndcnt;
  setcolorfg(31);
  circle_w (um, vm, 5);
  
  if (hypot(um,vm)> 0.0) {
    (um>0.0001) ? (sign=1): (sign=-1);
    /* angle = PI + acos(vm/hypot(um,vm))*SGN(um); */
    angle = PI + acos(vm/hypot(um,vm)) * sign;
  }
  else
    angle=2*PI;
  /* printf("PI=%5.6f angle=%5.2f um=%5.2f vm=%5.2f SGN=%d hypot=%5.2f\n",PI, angle*180/PI,um,vm,SGN(um),hypot(um,vm));*/
  sangle = angle + 30 * PI / 180;
  if (sangle> 2*PI) sangle-=(2*PI);
  magV = hypot(um, vm);
  magS=.7*magV;
  su = -magS * sin(sangle);
  sv = -magS * cos(sangle);
  setcolorfg(11);
  circle_w(su,sv,5);
  sprintf(buf,"CELL MOT %3.0f/%2.0f kt",angle*180/PI,magV);
  outtextxy_fill(2,7,6,buf);
  /* sprintf(buf," ST MOT   %3.0f/%2.0f",sangle*180/PI,magS);
  outtextxy_fill(2,7+textheight(),6,buf); */
  /* Compute 1/2/3 KM helicity... 0-6 KM Integrated SR CR Vorticity */
  helicity = 0;
  wstot = 0;
  wetot = 0;
  for (i=1;i<=wndcnt;i++) {
    u1 = -wndspds[i] * sin(wnddirs[i] * PI / 180);
    v1 = -wndspds[i] * cos(wnddirs[i] * PI / 180);
    heightwrk = wndhts[i] / 3.28;
    if (i > 1 && heightwrk <= startbaseht/3.28 + 3000.0) {
      uo = -wndspds[i - 1] * sin(wnddirs[i - 1] * PI / 180);
      vo = -wndspds[i - 1] * cos(wnddirs[i - 1] * PI / 180);
      /*'storm rel vectors*/
      suo = uo - su; svo = vo - sv;
      su1 = u1 - su; sv1 = v1 - sv;
      lhel = (suo * sv1 - su1 * svo) * pow(.51,2) * -1;
      helicity = helicity + lhel;
      if (heightwrk <= startbaseht/3.28+1000.0)
        hel1km=helicity;
      else if (heightwrk <= startbaseht/3.28+2000.0)
          hel2km=helicity;
      else if (heightwrk <= startbaseht/3.28+3000.0)
          hel3km=helicity;
    }
    if (i>1 && heightwrk <= startbaseht/3.28 + 6000.0) {
      /* Horizontal Vorticity Vector: */
      dz=(wndhts[i]-wndhts[i-1])/3.28;  /* meters */
      WHi=-(v1-vo)*.51/dz;WHj=(u1-uo)*.51/dz;  /* velocity converted knots->m/s */
      WH = hypot(WHi, WHj);
      /* WH now in units of sec-1
      //  Compute Hws  Streamwise Vorticity vector
      // Use cos theta = WS/WH and solve for WS
      // Use Angle between two vectors formula
      // Use Mean Storm Relative Wind <um,vm> and WH */
      um = (su1+suo)/2 * .51;
      vm = (sv1+svo)/2 * .51;
      /* Plot WHi, WHj */
      /* setcolor(10); 
      umean=(u1+uo)/2;vmean=(v1+vo)/2;
      sumean=(suo+su1)/2;svmean=(svo+sv1)/2; */
      /* line_w(umean,vmean,umean+WHi*300,vmean+WHj*300); */
      if (fabs(WH)>.000001) 
        costheta = (um*WHi+vm*WHj)/(hypot(um,vm)*WH);
      else
        costheta = 0.0;
      /* Scalar representation of Magnitude Ws */
      WS = WH*costheta;
      /* Scalar representation of Magnitude We */
      WE = sqrt(pow(WH,2)-pow(WS,2));
      wstot+=WS;
      wetot+=WE;
    }
  }
  calcsfc_6kmshear(wnddirs[12],wndspds[12],wnddirs[startbasesub],wndspds[startbasesub],&wndshear);
  sprintf(buf,"0-6KmShear: %3.0f kt", wndshear);
  outtextxy_fill(2,7+textheight(),6,buf);
  sprintf(buf,"0-6Km WS:%3.0f WC:%3.0f", wstot*1000.0, wetot*1000.0);
  outtextxy_fill(2,7+2*textheight(),6,buf);
  sprintf(buf,"1/2/3km SR hel");
  outtextxy_fill(2,HODO_HGHT-2*textheight(),6,buf);
  sprintf(buf,"%3.0f/%3.0f/%3.0f(m%c/s%c)",hel1km,hel2km,hel3km,178,178);
  outtextxy_fill(2,HODO_HGHT-1*textheight(),6,buf);
}

void calcsfc_6kmshear(int dirupr,int spdupr,int dirlwr,int spdlwr, float *shear) {
      float u1,v1,uo,vo;
      u1 = -spdupr * sin(dirupr * PI / 180);
      v1 = -spdupr * cos(dirupr * PI / 180);
      uo = -spdlwr * sin(dirlwr * PI / 180);
      vo = -spdlwr * cos(dirlwr * PI / 180);
      if (fabs(u1-uo)>0.0001 && fabs(v1-vo)>0.0001)
        *shear = sqrt( pow(u1-uo,2)+pow(v1-vo,2));
      else
        *shear = 0.0;
}
  
void setmaxwindinhodo(float pvsndg[][3][200], unsigned levels[], int pvnstns, int nlvls) {
  float maxspd,speed;
  int i,j;
  maxspd=0;
  for (i=1;i<=pvnstns;i++)
    for (j=1;j<=nlvls;j++) {
      if (levels[j]<=12000) {
        speed=(int)(pvsndg[j][2][i]+.5);
        if (speed > maxspd) maxspd=speed;
      } else {
        break;
      }
    }
  ;
  if (maxspd > 20)
    MAXWND=maxspd;
  else
    MAXWND=20;
  
}
        

  
      
      
      
 

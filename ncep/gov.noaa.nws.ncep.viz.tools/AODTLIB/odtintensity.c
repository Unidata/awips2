/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

float aodtv64_Tnoraw(void); 
float aodtv64_Tnofinal( int ); 
float aodtv64_CIno( int * ); 
float aodtv64_TIEmodel(void);
void  aodtv64_maxtnoval( int,int *,float * );
float aodtv64_latbias( float,float,float );
int   aodtv64_tieflag(void);

int aodtv64_scenesearch( int,float * );

int aodtv64_calcintensity(void)
/* Compute intensity values CI, Final T#, and Raw T#.
    Inputs  : global structure odtcurrent_v64 containing current analysis
    Outputs : none
    Return : 71 : storm is over land
              0 : o.k.
*/
{
  int   iret,strength;
   

  if((odtcurrent_v64->IR.land==1)&&(oland_v64)) {
    aodtv64_initcurrent(1);
    iret=71;
  } else {
    /* calculate current Raw T# value */
    odtcurrent_v64->IR.Traw=aodtv64_Tnoraw();

    /* check for spot analysis or full analysis using history file */
    if(hfile_v64==(char *)NULL) { 
      /* perform spot analysis (only Traw) */
      odtcurrent_v64->IR.Tfinal = odtcurrent_v64->IR.Traw;
      odtcurrent_v64->IR.Tfinal3 = odtcurrent_v64->IR.Traw;
      odtcurrent_v64->IR.CI = odtcurrent_v64->IR.Traw;
      odtcurrent_v64->IR.CIadjp=aodtv64_latbias(odtcurrent_v64->IR.CI,odtcurrent_v64->IR.latitude,odtcurrent_v64->IR.longitude);
      odtcurrent_v64->IR.rule9 = 0;
      odtcurrent_v64->IR.TIEraw=aodtv64_TIEmodel();
      odtcurrent_v64->IR.TIEavg=odtcurrent_v64->IR.TIEraw;
      odtcurrent_v64->IR.TIEflag=aodtv64_tieflag(); 
    } else {
      /* perform full analysis (Tfinal and CI) */
      odtcurrent_v64->IR.Tfinal = aodtv64_Tnofinal(0);
      odtcurrent_v64->IR.Tfinal3 = aodtv64_Tnofinal(1);
      odtcurrent_v64->IR.CI = aodtv64_CIno(&strength);
      odtcurrent_v64->IR.rule9=strength;
      odtcurrent_v64->IR.TIEraw=aodtv64_TIEmodel();
      odtcurrent_v64->IR.TIEavg=aodtv64_Tnofinal(2);
      odtcurrent_v64->IR.TIEflag=aodtv64_tieflag();
    }
    iret=0;
  }

  return iret;
}

float aodtv64_Tnoraw(void)
/* Compute initial Raw T-Number value using original Dvorak rules
    Inputs  : global structure odtcurrent_v64 containing current analysis
    Outputs : return value is Raw T#

                       ODT SCENE/TEMPERATURE TABLE
    BD   | WMG   OW    DG    MG    LG     B     W    CMG   CDG | 
    TEMP |30.0   0.0 -30.0 -42.0 -54.0 -64.0 -70.0 -76.0 -80.0+|
---------------------------------------------------------------|
Atl EYE  | 3.5   4.0   4.5   4.5   5.0   5.5   6.0   6.5   6.5 |
    EMBC | 3.5   3.5   4.0   4.0   4.5   4.5   5.0   5.0   5.0 |
    CDO  | 3.0   3.0   3.5   4.0   4.5   4.5   4.5   5.0   5.0 |
---------------------------------------------------------------|
Pac EYE  | 4.0   4.0   4.0   4.5   4.5   5.0   5.5   6.0   6.5 |
    EMBC | 3.5   3.5   4.0   4.0   4.5   4.5   5.0   5.0   5.0 |
    CDO  | 3.0   3.5   3.5   4.0   4.5   4.5   4.5   4.5   5.0 |
---------------------------------------------------------------|
Cat diff |  0     1     2     3     4     5     6     7     8  |
    add  | 0.0   0.0   0.0   0.0   0.0-->0.5   0.5-->1.0   1.5 |
---------------------------------------------------------------|
*/
{
  float eno[2][10]={ {3.5,4.0,4.5,4.5,5.0,5.5,6.0,6.5,6.5,6.5},
                     {4.0,4.0,4.0,4.5,4.5,5.0,5.5,6.0,6.5,6.5} };
  float cno[2][10]={ {3.5,3.5,4.0,4.0,4.5,4.5,5.0,5.0,5.0,5.0},
                     {3.5,3.5,4.0,4.0,4.5,4.5,5.0,5.0,5.0,5.0} };
  float cdo[2][10]={ {3.0,3.5,3.5,4.0,4.5,4.5,4.5,5.0,5.0,5.0},
                     {3.5,3.5,4.0,4.0,4.5,4.5,4.5,4.5,5.0,5.0} };
  float iadd[10]  =  {0.0,0.0,0.0,0.0,0.0,0.5,0.5,1.0,1.5,1.5};
  float curbnd[7] =  {1.0,1.5,2.5,3.0,3.5,4.0,4.5};
  float shrdst[6] =  {0.0,35.0,50.0,80.0,110.0,140.0};
  float shrcat[6] =  {3.5, 3.0, 2.5, 2.0,  1.5,  1.0};
/*
  float diffchk[3][10] = { {0.0,0.5,1.0,1.5,2.0,2.5,0.0,0.0,0.1,0.5 },   / shear scene types... original Rule 8 rules /
                           {0.0,0.5,1.5,2.0,2.5,3.0,0.0,0.0,0.1,0.5 },   /   eye scene types... add 0.5 to Rule 8 rules /
                           {0.0,0.5,0.5,1.0,1.5,2.0,0.0,0.0,0.1,0.5 } }; / other scene types... subtract 0.5 from Rule 8 rules /
*/
  float diffchk[3][10] = { {0.0,0.5,1.2,1.7,2.2,2.7,0.0,0.0,0.1,0.5 },   /* shear scene types... original Rule 8 rules */
                           {0.0,0.5,1.7,2.2,2.7,3.2,0.0,0.0,0.1,0.5 },   /*   eye scene types... add 0.5 to Rule 8 rules */
                           {0.0,0.5,0.7,1.2,1.7,2.2,0.0,0.0,0.1,0.5 } }; /* other scene types... subtract 0.5 from Rule 8 rules */
  int diffchkcat;
  int ixx,cloudcat,eyecat,diffcat,rp,xrp,rb;
  float incval,lasttno,lastr9;
  float xpart,xaddtno,eyeadj,spart,ddvor,dvorchart;
  float sdist,cloudtemp,eyetemp;
  float t1val,t6val,t12val,t18val,t24val,delt1,delt6,delt12,delt18,delt24;
  float t1valraw,t1valrawx,txvalmin,txvalmax;
  double curtime,xtime,firsttime,firstlandtime;
  double ttime1,ttime6,ttime12,ttime18,ttime24,t1valrawxtime;
  struct odtdata *odthistory;
  logical oceancheck,adjustshear,firstland;
  logical t1found=FALSE,t6found=FALSE,t12found=FALSE,t18found=FALSE,t24found=FALSE;
  logical first6hrs=FALSE;

  if(((odthistoryfirst_v64==0)&&(ostartstr_v64==TRUE))&&(hfile_v64!=(char *)NULL)) {  
    odtcurrent_v64->IR.TrawO=osstr_v64;
    return osstr_v64;
  } else {
    odthistory=odthistoryfirst_v64;
    curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);
    /* find record just prior to current record */
    while(odthistory!=0) {
      xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
      if((xtime==curtime)&&(odthistory==odthistoryfirst_v64)&&(osstr_v64!=0.0)) {
        odtcurrent_v64->IR.TrawO=osstr_v64;
        return osstr_v64;
      }
      if(xtime>curtime) break;
      odthistory=odthistory->nextrec;
    }
  }

  cloudtemp=odtcurrent_v64->IR.cloudt;
  eyetemp=odtcurrent_v64->IR.eyet;

  for(ixx=0;ixx<9;ixx++) {
    /* compute cloud category */
    if((cloudtemp<=ebd_v64[ixx])&&(cloudtemp>ebd_v64[ixx+1])) {
      cloudcat=ixx;
      xpart=(cloudtemp-ebd_v64[cloudcat])/(ebd_v64[cloudcat+1]-ebd_v64[cloudcat]);
    }
    /* compute eye category for eye adjustment */
    if((eyetemp<=ebd_v64[ixx])&&(eyetemp>ebd_v64[ixx+1])) {
      eyecat=ixx;
    }
  }

  /* category difference between eye and cloud region */
  diffcat=A_MAX(0,cloudcat-eyecat);
  
  /* if scenetype is EYE */
  rp=odtcurrent_v64->IR.ringcbval;
  rb=odtcurrent_v64->IR.ringcb;
  sdist=odtcurrent_v64->IR.eyesize;
  if(odtcurrent_v64->IR.cloudscene==3) {
    /* CURVED BAND */
    rp=A_MIN(30,rp);
    xrp=rp/5;
    incval=0.1;
    if(xrp==1) incval=0.2;
    ddvor=curbnd[xrp];
    xaddtno=incval*(float)(rp-(xrp*5));
    ddvor=ddvor+xaddtno;
    if(rb==5) ddvor=A_MIN(4.0,ddvor+0.5);
    if(rb==6) ddvor=A_MIN(4.0,ddvor+1.0);
    diffchkcat=2;   /* added for test - non-eye/shear cases */
  } else if(odtcurrent_v64->IR.cloudscene==4) {
    /* POSSIBLE SHEAR -- new definition from NHC */
    ixx=0;
    ddvor=1.0;
    while(ixx<5) {
      if((sdist>=shrdst[ixx])&&(sdist<shrdst[ixx+1])) {
        spart=(sdist-shrdst[ixx])/(shrdst[ixx+1]-shrdst[ixx]);
        xaddtno=(spart*(shrcat[ixx+1]-shrcat[ixx]));
        ddvor=shrcat[ixx]+xaddtno;
        ixx=5;
      } else {
        ixx++;
      }
    }
    diffchkcat=0;   /* added for test - shear cases*/
  } else {
    /* EYE or NO EYE */
    if(odtcurrent_v64->IR.eyescene<=5) {
      /* EYE */
      xaddtno=(xpart*(eno[idomain_v64][cloudcat+1]-eno[idomain_v64][cloudcat]));
      eyeadj=iadd[diffcat]+(xpart*(iadd[diffcat+1]-iadd[diffcat]));
      ddvor=eno[idomain_v64][cloudcat]+xaddtno+eyeadj;
      if(odtcurrent_v64->IR.eyescene<=3) {
        if(odtcurrent_v64->IR.eyescene==2) {
          ddvor=A_MIN(ddvor,6.0);        /* LARGE EYE adjustment */
        }
        if(odtcurrent_v64->IR.eyescene==3) {
          ddvor=A_MIN(ddvor-0.5,5.5);    /* LARGE RAGGED EYE adjustment */
        }
      } else {
        ddvor=ddvor-0.5;           /* Ragged eye adjustment */
      }
      diffchkcat=1;   /* added for test - eye cases */
    } else {
      /* NO EYE */
      if(odtcurrent_v64->IR.cloudscene==0) {
        /* CDO */
        xaddtno=(xpart*(cdo[idomain_v64][cloudcat+1]-cdo[idomain_v64][cloudcat]));
        ddvor=cdo[idomain_v64][cloudcat]+xaddtno;
      } else if(odtcurrent_v64->IR.cloudscene==1) {
        /* EMB C */
        xaddtno=(xpart*(cno[idomain_v64][cloudcat+1]-cno[idomain_v64][cloudcat]));
        ddvor=cno[idomain_v64][cloudcat]+xaddtno;
      } else if(odtcurrent_v64->IR.cloudscene==2) {
        /* IRREGULAR CDO (PT=3.5) */
        xaddtno=(xpart*(cdo[idomain_v64][cloudcat+1]-cdo[idomain_v64][cloudcat]));
        ddvor=cno[idomain_v64][cloudcat];
        ddvor=A_MIN(3.0,ddvor-1.5);
      } else {
        ddvor=ddvor;
      }
      diffchkcat=2;   /* added for test - non-eye/shear cases */
    }
  }
  
  dvorchart=((float)(int)(ddvor*10.0))/10.0;
  odtcurrent_v64->IR.TrawO=dvorchart;


  /* perform Dvorak EIR Rule 8 Constrants on Raw T# value
     All cases     : delT of 0.5 over  1 hour  : rule8 = 9 "velden rule" (actually 86.4 minutes... 0.06 of a day)
                     delT of 0.1 over  1 hour  : rule8 = 8 "additional velden rule", only over first 6 hours
     Raw T# <  4.0 : delT of 1.0 over  6 hours : rule8 = 2 / we removed 0.5/6hr for weakening cases /
                     No threshold exceeded     : rule8 = 0
     Raw T# >= 4.0 : delT of 1.0 over  6 hours : rule8 = 2  (actually 86.4 minutes... 0.06 of a day)
                     delT of 1.5 over 12 hours : rule8 = 3
                     delT of 2.0 over 18 hours : rule8 = 4
                     delT of 2.5 over 24 hours : rule8 = 5
                     No threshold exceeded     : rule8 = 0
  */
  odthistory=odthistoryfirst_v64;
  if(odthistory!=0) {
    ttime1=curtime-0.06;   /* 0.0416 is one hour... round to 0.06 to make sure I catch the hour previous report */
    ttime6=curtime-0.26;
    ttime12=curtime-0.51;
    ttime18=curtime-0.76;
    ttime24=curtime-1.01;
    firsttime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
    if(firsttime>=ttime6) first6hrs=TRUE;
    adjustshear=FALSE;
    while(odthistory!=0) {
      xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
      oceancheck=TRUE;
      if(((oland_v64)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) { 
        oceancheck=FALSE;
        if (firstland) {
          firstlandtime=xtime;
          firstland=FALSE;
        }
        if((xtime-firstlandtime)>=0.25) adjustshear=TRUE;
      } else {
        firstland=TRUE;
      }
      if((xtime>=ttime24)&&(xtime<curtime)&&(!t24found)&&(oceancheck)) {
        t24found=TRUE;
        t24val=odthistory->IR.Tfinal;
      }
      if((xtime>=ttime18)&&(xtime<curtime)&&(!t18found)&&(oceancheck)) {
        t18found=TRUE;
        t18val=odthistory->IR.Tfinal;
      }
      if((xtime>=ttime12)&&(xtime<curtime)&&(!t12found)&&(oceancheck)) {
        t12found=TRUE;
        t12val=odthistory->IR.Tfinal;
      }
      if((xtime>=ttime6)&&(xtime<curtime)&&(!t6found)&&(oceancheck)) {
        t6found=TRUE;
        t6val=odthistory->IR.Tfinal;
        t1valrawx=odthistory->IR.Traw;
        t1valrawxtime=xtime;
      }
      if((xtime>=ttime1)&&(xtime<curtime)&&(!t1found)&&(oceancheck)) {
        t1found=TRUE;
        t1val=odthistory->IR.Tfinal;
        t1valraw=odthistory->IR.Traw;
      }
      if((xtime<curtime)&&(oceancheck)) {
        lasttno=odthistory->IR.Tfinal;
        lastr9=odthistory->IR.rule9;
        if((adjustshear)&&(odthistory->IR.cloudscene!=4)) {
          /* turn off shear scene adjustment */
          adjustshear=FALSE;
        }
      }
      odthistory=odthistory->nextrec;
    }
    if(adjustshear) {
      /* if storm was over land for >= 6 hours and current scene is a shear
         scene type, then add 0.5 to Raw T# value */
      dvorchart=dvorchart+0.5;
      odtcurrent_v64->IR.TrawO=dvorchart;
    }
    /* we are using the experimental "velden rule" which will limit the growth of the 
     * Raw T# to 0.5/hour to try and dampen out scene changes and obvious incorrect
     * jumps in intensity due to incorrect positioning (most likely in the 
     * automode analysis */
    odtcurrent_v64->IR.rule8=(diffchkcat*10)+0;
    if(lasttno<4.0) {
      /* Raw T# < 4.0 */
      if(first6hrs) {
        if(t1found) {
          delt1=A_ABS(t1valraw-dvorchart);
          if(delt1>diffchk[diffchkcat][8]) {
            dvorchart=A_MAX(t1valraw-diffchk[diffchkcat][8],A_MIN(t1valraw+diffchk[diffchkcat][8],dvorchart));
            odtcurrent_v64->IR.rule8=(diffchkcat*10)+8;
          }
        } else {
          /* no value available within past hour... must determine approx value */
          delt1=0.1*(A_ABS(curtime-t1valrawxtime)/.0416);
          txvalmin=t1valrawx-delt1;
          txvalmax=t1valrawx+delt1;
          if((dvorchart>txvalmax)||(dvorchart<txvalmin)) {
            dvorchart=A_MAX(txvalmin,A_MIN(txvalmax,dvorchart));
            odtcurrent_v64->IR.rule8=(diffchkcat*10)+8;
          }
        }
      } else {
        delt1=A_ABS(t1val-dvorchart);
        if((delt1>diffchk[diffchkcat][9])&&(t1found)) {
          dvorchart=A_MAX(t1val-diffchk[diffchkcat][9],A_MIN(t1val+diffchk[diffchkcat][9],dvorchart));
          odtcurrent_v64->IR.rule8=(diffchkcat*10)+9;
        }
        delt6=A_ABS(t6val-dvorchart);
        if(lastr9<2) {
          if((delt6>diffchk[diffchkcat][2])&&(t6found)) {
            dvorchart=A_MAX(t6val-diffchk[diffchkcat][2],A_MIN(t6val+diffchk[diffchkcat][2],dvorchart));
            odtcurrent_v64->IR.rule8=(diffchkcat*10)+2;
          }
        } else {
          if((delt6>diffchk[diffchkcat][1])&&(t6found)) {
            dvorchart=A_MAX(t6val-diffchk[diffchkcat][1],A_MIN(t6val+diffchk[diffchkcat][1],dvorchart));
            odtcurrent_v64->IR.rule8=(diffchkcat*10)+1;
          }
        }
      }
    } else {
      /* Raw T# >= 4.0 */
      delt1=A_ABS(t1val-dvorchart);
      if((delt1>diffchk[diffchkcat][9])&&(t1found)) {
        dvorchart=A_MAX(t1val-diffchk[diffchkcat][9],A_MIN(t1val+diffchk[diffchkcat][9],dvorchart));
        odtcurrent_v64->IR.rule8=(diffchkcat*10)+9;
      }
      delt6=A_ABS(t6val-dvorchart);
      delt12=A_ABS(t12val-dvorchart);
      delt18=A_ABS(t18val-dvorchart);
      delt24=A_ABS(t24val-dvorchart);
      if((delt6>diffchk[diffchkcat][2])&&(t6found)) {
        dvorchart=A_MAX(t6val-diffchk[diffchkcat][2],A_MIN(t6val+diffchk[diffchkcat][2],dvorchart));
        odtcurrent_v64->IR.rule8=(diffchkcat*10)+2;
      } else if((delt12>diffchk[diffchkcat][3])&&(t12found)) {
        dvorchart=A_MAX(t12val-diffchk[diffchkcat][3],A_MIN(t12val+diffchk[diffchkcat][3],dvorchart));
        odtcurrent_v64->IR.rule8=(diffchkcat*10)+3;
      } else if((delt18>diffchk[diffchkcat][4])&&(t18found)) {
        dvorchart=A_MAX(t18val-diffchk[diffchkcat][4],A_MIN(t18val+diffchk[diffchkcat][4],dvorchart));
        odtcurrent_v64->IR.rule8=(diffchkcat*10)+4;
      } else if((delt24>diffchk[diffchkcat][5])&&(t24found)) {
        dvorchart=A_MAX(t24val-diffchk[diffchkcat][5],A_MIN(t24val+diffchk[diffchkcat][5],dvorchart));
        odtcurrent_v64->IR.rule8=(diffchkcat*10)+5;
      } else {
        odtcurrent_v64->IR.rule8=odtcurrent_v64->IR.rule8;      
      }
    }
  }

  return dvorchart;

}

float aodtv64_Tnofinal(int itype)
/* Compute time averaged T-Number value using previous and current
   intensity estimates.  Average using a time-weighted averaging
   scheme.
    Inputs  : itype : time average duration flag : 0=6 hour;1=3 hour
              global structure odtcurrent_v64 containing current analysis
    Outputs : return value is Final T#
*/
{
  double curtime,xtime,tlimit,diff;
  double xint=1.0/24.0,baseval=6.0;
  float sumtop=0.0,sumbot=0.0;
  float dvorweight,weight,value;
  logical found=FALSE,oceancheck;
  struct odtdata *odthistory;

  odthistory=odthistoryfirst_v64;

  if(itype==1) baseval=3.0;   /* for NHC 3-hour time average value */
  if(itype==2) baseval=12.0;  /* for TIE Model time average value */

  curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);

  /* compute average with current value with any values
     from previous 6 hours */
  tlimit=curtime-(baseval/24.0);

  /* compute weighted time averaged value */
  while(odthistory!=0) {
    xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
    if((xtime>=tlimit)&&(xtime<curtime)) {
      oceancheck=TRUE;
      if(itype<=1) {
        value=odthistory->IR.Traw;
      } else {
        value=odthistory->IR.TIEraw;
        if(value<0.0) value=0.0;
      }
      if(((oland_v64)&&(odthistory->IR.land==1))||(value<1.0)) oceancheck=FALSE;
      if(oceancheck) {
        diff=curtime-xtime;
        if(itype==0) {
          weight=(float)(baseval-(diff/xint)); /* time weighted average */
        } else {
          weight=baseval;                      /* straight average */
        }
        sumtop=sumtop+(weight*value);
        sumbot=sumbot+weight;
        found=TRUE;
      }
    } else {
      if(found) break;
    }
    odthistory=odthistory->nextrec;
  }

  /* compute time-averaged T# value.
     if no previous records found, return Raw T# */
  if(itype<=1) {
    value=odtcurrent_v64->IR.Traw;
  } else {
    value=odtcurrent_v64->IR.TIEraw;
    if(value<=1.0) found=FALSE;
  }
  if(found) {
    sumtop=sumtop+(baseval*value);
    sumbot=sumbot+baseval;
    /* remove any value remainder past tenths */
    dvorweight=(float)((int)((sumtop/sumbot)*10.0))/10.0;
  } else {
    dvorweight=value;
  }

  return dvorweight;
}

float aodtv64_CIno(int *curstrength)
/* Compute final CI-Number applying various Dvorak Rules, such
   as the now famous Rule 9
    Inputs  : odtcurrent_v64  - structure containing current analysis
    Outputs : curstrength - current strengthening/weakening flag
              return value is Current Intensity (CI) # 
*/
{
  float  intensity;
  int    strength,lstrength;
  float  sigslope24=-1.0,sigslope,slopeval,r9v=1.0;
  float  lasttnomax,lasttno,lastCI,cival,swval1,swval2,ctno,ctnor9;
  float  tnomin=8.0,tnomax=0.0;
  double curtime,timem12,timem24,xtime;
/*  double  timem6; */
  struct odtdata *odthistory;
  logical allland=TRUE,tdts24=FALSE,oceancheck;

  odthistory=odthistoryfirst_v64;

  curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);

  /* for new test */
  if(odtcurrent_v64->IR.eyescene<=5) {
    intensity=odtcurrent_v64->IR.Tfinal3;
  } else {
    intensity=odtcurrent_v64->IR.Tfinal;
  }

  if(odthistoryfirst_v64==0) {
    /* no records in history file */
    *curstrength=0;
    /* this will trip the RULE 9 FLAG for an initial classification of >=6.0 */
    if(osstr_v64>=6.0) *curstrength=2;
    /* Apply Latitude Bias Adjustment to CI value */
    odtcurrent_v64->IR.CIadjp=aodtv64_latbias(intensity,odtcurrent_v64->IR.latitude,odtcurrent_v64->IR.longitude);
    return intensity;
  }

  /* determine various time threshold values */
/*  timem6=curtime-0.25;*/
  timem12=curtime-0.5;
  timem24=curtime-1.0;

  /* find record just prior to current record */
  odthistory=odthistoryfirst_v64;
  lasttnomax=0.0;
  lastCI=0.0;
  while(odthistory!=0) {
    xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
    if(xtime>=curtime) break;
    oceancheck=TRUE;
    if(((oland_v64)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) oceancheck=FALSE;
    if(oceancheck) {
      if(odtcurrent_v64->IR.eyescene<=5) {
        lasttno=odthistory->IR.Tfinal3;
      } else {
        lasttno=odthistory->IR.Tfinal;
      }
      lastCI=odthistory->IR.CI;
      lstrength=odthistory->IR.rule9;
      /* if(xtime>=timem6) {   / check Rule 9 -- must be over land for 6 hours to turn off */
      if(xtime>=timem12) {   /* check Rule 9 -- must be over land for 12 hours to turn off */
        allland=FALSE;
        /* find largest finalT# in last 12 hours prior to current record */
        if(lasttno>lasttnomax) lasttnomax=lasttno;
      }
      if(xtime>=timem24) { 
        /* find min and max finalT# in last 24 hours prior to current record */
        if(lasttno<tnomin) tnomin=lasttno;
        if(lasttno>tnomax) tnomax=lasttno;
        /* if storm was TD/TS at any time during last 24 hours, 
           don't allow trip of sig. strengthening flag */
        if(lasttno<=4.0) tdts24=TRUE;
      }
    }
    odthistory=odthistory->nextrec;
  }

  if(odthistory==odthistoryfirst_v64) {
    /* current record is before first record in history file */
    *curstrength=0;
    odtcurrent_v64->IR.CIadjp=aodtv64_latbias(intensity,odtcurrent_v64->IR.latitude,odtcurrent_v64->IR.longitude);
    return intensity;
  }

  /* 24-hour slope determination */
  slopeval=aodtv64_slopecal(24.0,1);
  sigslope=sigslope24;

  strength=lstrength;

  ctno=intensity;
  ctnor9=intensity+r9v;
  if(lastCI==0.0) lastCI=ctno;
  if(lstrength<=2) {
    cival=A_MIN(ctnor9,A_MAX(lasttnomax,ctno));
  } else {
    ctnor9=intensity+r9v;
    cival=A_MIN(ctnor9,A_MAX(lastCI,ctno));
  }

  /* determine current strengthening/weakening values */
  swval1=(ctno-lastCI);
  swval2=(cival-lastCI);

  /* strength flags : 0 - strengthening, but no significant strengthening cycle noted;
                      1 - applying Max's 12 hour max Tno. rule during insignificant weakening
                      2 - significant strengthening cycle noted
                      3 - storm has reached max intensity
                      4 - sig. strength. cycle noted; storm is now
                          weakening.  Will use 24 hour max Tno until
                          CI and Tno merge.  Rule 9 add value is 1.0
  */  

  if(lstrength<=1) {
    if(cival>ctno) strength=1;
    if((lstrength==1)&&(lastCI<=ctno)) strength=0;  /* changed from < to <= */
    if((slopeval<=sigslope)&&(cival>=5.0)&&(!tdts24)) strength=2;
  }
  if(lstrength==2) {
    if(swval1<0.0) strength=3;
  }
  if(lstrength==3) {
    if(swval2>0.0) strength=2;           /* added to "turn off" Rule 9 if CI starts to increase again */
    if(swval1>=0.0) strength=2;          /* changed to 2 to turn off Rule 9 until if reaches max again */
    if(swval2<0.0) strength=4;
  }
  if(lstrength==4) {
    if(swval1>=0.0) strength=2;          /* changed to 2 to turn off Rule 9 until it reaches max again */
  }

  /* check for land interaction
     if undergoing interactation with land "turn off" Rule 9 
     application and return CI value to Tno value for >= 12 hours. */
  if(lstrength>=2) {
    if(allland) {
    /* if land flag is TRUE,
       turn off Rule 9 and let CI value be equal to the
       current Final T# value, and return
       current intensity flag to "insignificant value" until
       another significant strengtheing cycle occurs */
      strength=0;
      /*cival=(lastCI+intensity)/2.0;*/
      cival=intensity; 
    }
  }

  /* Apply Latitude Bias Adjustment to CI value */
  odtcurrent_v64->IR.CIadjp=aodtv64_latbias(cival,odtcurrent_v64->IR.latitude,odtcurrent_v64->IR.longitude);

  *curstrength=strength;
  return cival;
} 

float aodtv64_latbias(float initval,float latitude,float longitude)
/* Apply Latitude Bias Adjustment to CI value
    Inputs  : initval  - initial CI value 
              latitude - current latitude of storm
    Outputs : adjusted MSLP value as return value
*/
{
  float initvalp;
  float value;      /* lat bias adjustement amount (0.00-1.00) */
  int   sceneflag;  /* contains lat bias adjustment flag
                       0=no adjustment
                       1=intermediate adjustment (6 hours)
                       2=full adjustment */

  sceneflag=aodtv64_scenesearch(0,&value);   /* 0 means search for EIR based parameters... cdo, etc */
  odtcurrent_v64->IR.LBflag=sceneflag; 
  /* initvalp=aodtv64_getpwval(0,initval); TLO */
  initvalp=0.0;
  if(sceneflag>=2) {
    /* EIR scene */
    if((latitude>=0.0)&&((longitude>=-100.0)&&(longitude<=-40.0))) {
      /* do not make adjustment in N Indian Ocean */
      return initvalp;
    } 
    /* apply bias adjustment to pressure */
    initvalp=-1.0*value*(-20.60822+(0.88463*A_ABS(latitude))); 
  }

  return initvalp;
}

int aodtv64_scenesearch(int type,float *pvalue)
{
  int     curflag=1,flag,eirflag;
  double  curtime,xtime,curtimem6,mergetimefirst,firsttime=-9999.0;
  struct  odtdata *odthistory;
  logical oceancheck,eirfound=TRUE,foundmergefirsttime=FALSE;
  
  if(((odthistoryfirst_v64==0)&&(ostartstr_v64==TRUE))&&(hfile_v64!=(char *)NULL)) { 
    flag=0;
    *pvalue=-999.9;
    return flag;
  }

  eirflag=0;
  if(type==0) {
    /* check current value and set return flag */
    if((odtcurrent_v64->IR.cloudscene>=2)&&(odtcurrent_v64->IR.cloudscene<6)) curflag=0;

    /* search for EIR type scene for lat bias adjustment */
    curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);
    curtimem6=curtime-0.26;
    mergetimefirst=curtime;
    odthistory=odthistoryfirst_v64;
    while(odthistory!=0) {
      xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
      oceancheck=TRUE;
      if(((oland_v64)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) oceancheck=FALSE;
      if((xtime<curtime)&&(oceancheck)) {
        /* store last values in case I need them for six hour gap */
        eirflag=odthistory->IR.LBflag;
        if((xtime>=curtimem6)&&(eirfound)) {
          if(firsttime<0.0) firsttime=xtime;
          if(eirflag==0) eirfound=FALSE;
          if(eirflag==2) {
            if(!foundmergefirsttime) {
              mergetimefirst=xtime;
              foundmergefirsttime=TRUE;
            }
          }
        }
      }
      odthistory=odthistory->nextrec;
    }
    if(firsttime<0.0) {
      /* there is a six hour gap in the data for some reason... 
         I will use the last value available */
      flag=eirflag;
      if(eirflag>=1) flag=2;
      *pvalue=1.0;
    } else {
      if(eirfound) {
        /* entering or in valid lat bias adjustment period */
        flag=2;
        *pvalue=(curtime-mergetimefirst)/(curtime-firsttime);   /* return value from 0 to 1 */
      } else {
        *pvalue=-999.9;
        flag=curflag;
      }
    }
  } else {
    /* printf("not valid search type\n"); */
  }

  return flag;
}


float aodtv64_TIEmodel(void)
/* determine TIE model intensity estimate based upon linear 
   regression model using various AODT parameters and SST 
   value at cursor location.
   Inputs  : values in odtcurrent_v64 structure
   Outputs : TIE model predicted value is return value
*/
{
  float ert,crt,cwcrt,xlat,sst,xmpi,tiemodel;
  float a=0.1813;
 
  /* get current sst value, if available */
  sst=odtcurrent_v64->IR.sst;
  if(sst<-10.0) return 0.0;

  /* get additional current predictor values */
  ert=odtcurrent_v64->IR.eyet;
  crt=odtcurrent_v64->IR.cloudt2;
  cwcrt=odtcurrent_v64->IR.cwcloudt;
  xlat=odtcurrent_v64->IR.latitude;

  /* determine SST-based predictor */
  xmpi=A_EXP(a*sst);

  /* calculate TIE model predicted intensity value */
  tiemodel=1003.1324076 - (0.3980090*ert)   + (0.6746796*crt) 
                        + (0.4220070*cwcrt) - (0.5440850*xlat) 
                        + (0.1780407*xmpi);

  return tiemodel;
}

void aodtv64_maxtnoval(int stime,int *iweak,float *maxtno)
/* Determine maximum final T# value over period defined by 
   stime.  Return value and strengthening/weakening flag 
    Inputs  : stime  - number of hours to calculate slope over
              global structureodtcurrent_v64 structure containing current analysis
    Outputs : iweak  - weakening intensity flag (over last 12 hours)
                       1=weakening for less than 12 hours, 0=otherwise
              maxtno - maximum T# during last 12 hours if storm has
                       been or is currently in a significant
                       strengthening cycle 
*/
{
  int weak=0;
  float tmax=0.0;
  double curtime,xtime,slopetime=stime/24.0;
  logical found=FALSE,oceancheck;
  struct odtdata *odthistory;

  odthistory=odthistoryfirst_v64;
  
  curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);

  /* Determine maximum T-number.  If weakening has been occuring for
     less than 12 hours, return iweak value of 1, else return 0 */
  while(odthistory!=0) {
    xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
    if(xtime<curtime) {
      oceancheck=TRUE;
      if(((oland_v64)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) oceancheck=FALSE;
      if(oceancheck) {
        if(odthistory->IR.Tfinal>tmax) tmax=odthistory->IR.Tfinal;
        if(xtime>=(curtime-slopetime)) weak=1;
      }
      found=TRUE;
    } else {
      if(found) break;
    }
    odthistory=odthistory->nextrec;
  }  

  *iweak=weak;
  *maxtno=tmax;

}

int aodtv64_tieflag(void)
{
  int    flagval=0;
  float  tietno;
  double curtime,xtime;
  logical found=FALSE;
  struct odtdata *odthistory;

  tietno=aodtv64_ptotno(odtcurrent_v64->IR.TIEavg);
  if(A_ABS(tietno-odtcurrent_v64->IR.Tfinal)<=0.5) {
    /* if difference is <= 0.5, then return "found" TIE flag */
    flagval=1;
  } else {
    /* check for previous "found" TIE flag */
    odthistory=odthistoryfirst_v64;
    curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);
    while((odthistory!=0)&&(!found)) {
      xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
      if(xtime<curtime) {
        if(odthistory->IR.TIEflag==1) {
          flagval=1;
          found=TRUE;
        }
      }
      odthistory=odthistory->nextrec;
    }
  }
  
  return flagval;
}

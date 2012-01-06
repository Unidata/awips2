/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"
#define maxsec 24
#define maxsecA 2000

int aodtv64_calcscene(void);
void aodtv64_classify(void);
void aodtv64_eyeshearcalc( float,float,float,int,float *,float * );
void aodtv64_xxsort( float *, float *, float *, int );
void aodtv64_fminx( float *, int, int *, float * );
void aodtv64_fmaxx( float *, int, int *, float * );
int aodtv64_rmw(float *,float *);

/*=====================================================================*/

int aodtv64_calcscene(void)
/* Perform Fast Fourier Transform (FFT) analysis and determine 
   scene type via empirically defined threshold values.
    Inputs  : global structure odtcurrent_v64 containing current intensity values
    Outputs : various elements within structure odtcurrent_v64
    Return  : -41 : error with FFT routine
              -51 : cloud temperature value <-100C or >+40C
	        0 : o.k.
**									*
* Log:									*
* T. Piper/SAIC		05/06	Properly allocate two-dimensional array	*
* T. Piper/SAIC		05/06	Properly free sector			*
************************************************************************/
{
  int   nbin=64,iok;
  int   ixx,iyy,izz,iscene,idxcnt,maxsecd2,sxscnt;
  int   *sectorcnt,cnteye;
  float bd[534],cbd[534],tbd[534];
  float radb,rade,radeye,teye,dx2,eyecnt,rngcnt;
  float xangle,xdist,xtemp,slice;
  float *sectordiffa,*sectormin;
  float **sector,*eyearr,*sxs,sectang1,sectang2;
  float *avex,*stdvx,*skewx,*xs,*i2;
  float Aaveext,Astdvext,Askewext,Aavesym,Astdvsym,Askewsym;
  float Eaveeye,Estdveye,Eskeweye;
  float alsdist,alsradb,alsrade,alssum,alst,alscnt;
  struct ringdata *tcirc;
  size_t sofflt;

  /* initialize temperature histogram bin values */
  for(iyy=0;iyy<nbin;iyy++) {
    bd[iyy]=26.0-(float)iyy*2.0;
  }
  /* set up arrays for FFT ananlysis.
     iscene=0 will perform FFT for cloud top region while
     iscene=1 will perform FFT for eye region */
  for(iscene=0;iscene<=1;iscene++) {
    for(iyy=0;iyy<nbin;iyy++) {
      cbd[iyy]=0.0;
      tbd[iyy]=0.0;
    }

    /* define start and end radii values */
    if(iscene==0) {
      /* CLOUD TOP */
      radb=(float)kstart_v64;
      rade=(float)kend_v64;
    } else {
      /* EYE REGION */
      radb=0.0;
      rade=(float)kstart_v64;
    }

    /* load arrays for FFT analysis */
    tcirc=tcircfirst_v64;
    while(tcirc!=NULL) {
      if((tcirc->dist>=radb)&&(tcirc->dist<=rade)) {
        teye=(tcirc->temp - 273.16);
        for(ixx=0;ixx<(nbin-1);ixx++) {
          if((teye<=bd[ixx])&&(teye>=bd[ixx+1])) {
            cbd[ixx]=cbd[ixx]+1.0;
            tbd[ixx]=tbd[ixx]+teye;
          }
        }
      }
      tcirc=tcirc->nextrec;
    }

    /* perform FFT analysis */
    iok=aodtv64_fft(cbd,&dx2,&idxcnt);
    if(iok<0) return -41;
    
    /* assign variables based upon region being analyzed */
    if(iscene==0) {
      rngcnt=idxcnt; /* CLOUD TOP */
    } else {
      eyecnt=idxcnt; /* EYE REGION */
    }
  }

  /* compute various cloud and eye region parameters for classification scheme */

  /* allocate memory for arrays */
  sectorcnt=(int *)calloc((size_t)maxsec,sizeof(int));
  sofflt=sizeof(float);
  sectormin=(float *)calloc((size_t)maxsec,sofflt);
  sector=(float **)calloc((size_t)maxsec,sizeof(float*));
  for(ixx=0;ixx<maxsec;ixx++) {
    sector[ixx]=(float *)calloc((size_t)maxsecA,sofflt);
  }
  eyearr=(float *)calloc((size_t)maxsecA,sofflt);

  for(ixx=0;ixx<maxsec;ixx++) {
    sectorcnt[ixx]=0;
    sectormin[ixx]=999.0;
    for(iyy=0;iyy<maxsecA;iyy++) {
      sector[ixx][iyy]=-999.99;
    }
  }

  /* load array for analysis */
  radb=(float)kstart_v64;
  rade=(float)kend_v64;
  radeye=(float)kstart_v64;
  tcirc=tcircfirst_v64;
  slice=360.0/(float)maxsec;
  cnteye=0;
  while(tcirc!=NULL) {
    xangle=tcirc->angle;
    xdist=tcirc->dist;
    xtemp=tcirc->temp-273.16;
    if(xangle==360.0) xangle=0.0;
    ixx=0;
    if((xdist>=radb)&&(xdist<=rade)) {
      while(ixx<maxsec) {
        sectang1=A_MAX(0.0,(float)ixx*slice);
        sectang2=A_MIN(360.0,(float)(ixx+1)*slice);
        if((xangle>=sectang1)&&(xangle<sectang2)) {
          sector[ixx][sectorcnt[ixx]]=xtemp;
          sectorcnt[ixx]++;
          ixx=maxsec;
        } else {
          ixx++;   
        }
      }
    }
    if((xdist>=0)&&(xdist<=radeye)) {
      eyearr[cnteye]=xtemp;
      cnteye++;
    }
    /* the following will count all values w/in the different BD curve
       ranges for the whole cloud region so we can examine the structure
       of the cloud region later */
    tcirc=tcirc->nextrec;
  }

  /* position annulus at CW max temp distance and 
     determine mean temp w/in +/- 40km from this distance.  If dist
     is less than 68km from center, annulus will start at 28km */
  alscnt=0;
  alssum=0.0;
  alsdist=(float)odtcurrent_v64->IR.cwring;
  alsradb=A_MAX(28.0,alsdist-40.0);
  alsrade=A_MAX(108.0,alsdist+40.0);
  tcirc=tcircfirst_v64;
  while(tcirc!=NULL) {
    xdist=tcirc->dist;
    xtemp=tcirc->temp-273.16;
    if((xdist>=alsradb)&&(xdist<=alsrade)) {
      alssum=alssum+xtemp;
      alscnt++;
    }
    tcirc=tcirc->nextrec;
  }

  alst=alssum/(float)alscnt;
  odtcurrent_v64->IR.cloudt=alst;
  if((odtcurrent_v64->IR.cloudt<-100.0)||(odtcurrent_v64->IR.cloudt>40.0)) return -51;

  /* determine "minimum" temperature for each sector.  This is not the actual
     lowest temperature, but instead is the temperature value of the coldest
     90% of the values within the sector.  This will, hopefully, eliminate some
     outliers */

  /* allocate memory */
  xs=(float *)calloc((size_t)maxsecA,sofflt);
  i2=(float *)calloc((size_t)maxsecA,sofflt);
  sxs=(float *)calloc((size_t)maxsecA,sofflt);

  for(ixx=0;ixx<maxsec;ixx++) {
    sxscnt=sectorcnt[ixx];
    for(iyy=0;iyy<sxscnt;iyy++) {
      sxs[iyy]=sector[ixx][iyy];
    }
    aodtv64_xxsort(sxs,xs,i2,sxscnt);
    izz=sxscnt-(sxscnt*.1);
    sectormin[ixx]=xs[izz];
  }

  /* free memory */
  free(sxs);
  free(i2);
  free(xs);

  /* determine averages, standard deviations and skews for each sector */
  /* allocate memory */
  avex=(float *)calloc((size_t)maxsec,sofflt);
  stdvx=(float *)calloc((size_t)maxsec,sofflt);
  skewx=(float *)calloc((size_t)maxsec,sofflt);
  sectordiffa=(float *)calloc((size_t)maxsec,sofflt);
  for (ixx=0;ixx<maxsec;ixx++) {
    aodtv64_calcskew(sector[ixx],sectorcnt[ixx],&avex[ixx],&stdvx[ixx],&skewx[ixx]);
  }

  aodtv64_calcskew(avex,maxsec,&Aaveext,&Astdvext,&Askewext);
  odtcurrent_v64->IR.cloudt2=Aaveext;

  /* these are used to examine symmetry of convection */
  maxsecd2=maxsec/2;
  for (ixx=0;ixx<maxsecd2;ixx++) {
    sectordiffa[ixx]=A_ABS(avex[ixx]-avex[maxsecd2+ixx]);
  }
  aodtv64_calcskew(sectordiffa,maxsecd2,&Aavesym,&Astdvsym,&Askewsym);
  /* these are used to examine properties of the eye region */
  aodtv64_calcskew(eyearr,cnteye,&Eaveeye,&Estdveye,&Eskeweye);
  odtcurrent_v64->IR.eyestdv=Estdveye;
  odtcurrent_v64->IR.cloudsymave=Aavesym;
  odtcurrent_v64->IR.eyefft=(int)eyecnt;
  odtcurrent_v64->IR.cloudfft=(int)rngcnt;

  /* free memory */
  free(sectordiffa);
  free(skewx);
  free(stdvx);
  free(avex);
  for(ixx=0;ixx<maxsec;ixx++) {
      free(sector[ixx]);
  }
  free(sector);
  free(eyearr);
  free(sectormin);
  free(sectorcnt);

  /* assign scenetype value in structure odtcurrent_v64 */
  aodtv64_classify();

  return 0;
}

/*=============================================================*/

void aodtv64_classify(void)
/* Classify scene type based on FFT analysis and histogram temperatures
   using empirically defined threshold values.
    Inputs  : global structure odtcurrent_v64 containing current image analysis
    Outputs : scenetype in structure odtcurrent_v64 is modified

    SCENE TYPES : EYE REGION         CLOUD REGION
                  0 - clear          0 - uniform
                  1 - pinhole        1 - embedded center
                  2 - large          2 - irregular cdo
                  3 - large ragged   3 - curved band
                  4 - ragged         4 - shear
                  5 - obscured 
                  6 - none
*/
{
  int   ixx,cloudfft,cloudcat,eyefft,eyecat,cwtcat,diffcat;
  int   cbring,cbringval,diffcloudcat;
  int   sceneeye,scenecloud,spiral,spiralmax;
  int   cbringvalmax;
  float xlat,xlon,tempval,lasttno,lasttno12;
  float eyetemp,cloudtemp,cloudcwt,eyestdv,cloudsyma;
  float essizeDG=0.0,esdiffDG=0.0;
  float lat1,lon1,lat2,lon2;
  float eyesize,cdosize,cdosizex,rmw;
  float cbringlatmax,cbringlonmax;
  double curtime,curtimem12,xtime;
  struct odtdata *odthistory;
  logical cbfound,eyefound,cbscene,cbgray,shear,irrcdo,landcheck;

  eyetemp=odtcurrent_v64->IR.eyet;
  eyefft=odtcurrent_v64->IR.eyefft;
  eyestdv=odtcurrent_v64->IR.eyestdv;
  cloudtemp=odtcurrent_v64->IR.cloudt;
  cloudcwt=odtcurrent_v64->IR.cwcloudt;
  cloudfft=odtcurrent_v64->IR.cloudfft;
  cloudsyma=odtcurrent_v64->IR.cloudsymave;
  xlat=odtcurrent_v64->IR.latitude;
  xlon=odtcurrent_v64->IR.longitude;

  /* compute cloud category */
  for(ixx=0;ixx<9;ixx++) {
    if((cloudtemp<=ebd_v64[ixx])&&(cloudtemp>ebd_v64[ixx+1])) {
      cloudcat=ixx;
    }
  }
  /* compute eye category */
  for(ixx=0;ixx<9;ixx++) {
    if((eyetemp<=ebd_v64[ixx])&&(eyetemp>ebd_v64[ixx+1])) {
      eyecat=ixx;
    }
  }
  /* compute ave eye category */
  for(ixx=0;ixx<9;ixx++) {
    if((cloudcwt<=ebd_v64[ixx])&&(cloudcwt>ebd_v64[ixx+1])) {
      cwtcat=ixx;
    }
  }
  diffcat=A_MAX(0,A_MIN(cloudcat,cwtcat)-eyecat);
  diffcloudcat=cloudcat-cwtcat;

  curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);
  curtimem12=curtime-0.5;

  /* determine last Final T# value for curved band/other scene check */
  eyefound=FALSE;
  if((odthistoryfirst_v64==0)||(hfile_v64==(char *)NULL)) { 
    lasttno12=A_MIN(8.0,A_MAX(1.0,osstr_v64));
  } else {
    odthistory=odthistoryfirst_v64;
    cbfound=FALSE;
    while(odthistory!=0) {
      xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
      landcheck=TRUE;
      if(((oland_v64)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) landcheck=FALSE;
      if((xtime<curtime)&&(landcheck)) {
        if((xtime>=curtimem12)&&(!cbfound)) {
          lasttno12=odthistory->IR.Tfinal;
          cbfound=TRUE;
        }
        lasttno=odthistory->IR.Tfinal;
	if(odthistory->IR.eyescene<=2) eyefound=TRUE;
      }
      odthistory=odthistory->nextrec;
    }
    /* check for large break in history file */
    if(!cbfound) lasttno12=lasttno;
  }

  /* CLASSIFY EYE REGION */
  if(eyefft<=2) {
    sceneeye=6;           /* NO EYE */
    if(eyecat==0) {
      sceneeye=0;         /* CLEAR EYE */
    }
    if((diffcat>=2)&&(eyecat>0)) sceneeye=5;
  }
  if((eyefft>=3)&&(eyefft<=6)) {
    if(eyecat==0) {
      sceneeye=0;         /* CLEAR EYE */
    } else if(eyecat==1) {
      sceneeye=4;         /* RAGGED EYE */
      if(eyestdv>=25.0) {
        sceneeye=0;       /* CLEAR EYE */
      }
    } else {
      sceneeye=6;         /* NO EYE */
      if((diffcat>=2)&&(eyecat>0)) sceneeye=5;
    }
  }
  if((eyefft>=7)&&(eyefft<=9)) {
    if(eyecat==0) {
      sceneeye=0;         /* CLEAR EYE */
    } else if(eyecat==1) {
      sceneeye=4;         /* RAGGED EYE */
      if(eyestdv>=20.0) {
        sceneeye=0;       /* CLEAR EYE */
      }
    } else {
      sceneeye=5;         /* OBSCURED EYE */
    }
  }
  if(eyefft>=10) {
    if(eyecat==0) {
      sceneeye=0;         /* CLEAR EYE */
    } else if(eyecat==1) {
      sceneeye=4;         /* RAGGED EYE */
    } else {
      sceneeye=5;         /* OBSCURED EYE */
    }
  }
  if(eyecat<=1) {
    if(sceneeye==6) {
      sceneeye=2;           /* LARGE EYE */
      if(cwtcat<4) {
        sceneeye=3;         /* LARGE RAGGED EYE */
      }
    }
  }

  eyesize=0.0;
  if(sceneeye<=4) {
    /* obtain eye size and shape parameters /
    tempval=ebd_v64[2]+273.16;
    aodtv64_eyeshearcalc(xlat,xlon,tempval,2,&essizeDG,&esdiffDG);
    eyesize=essizeDG; */
    /* compute radius of maximum wind and eye size */
    aodtv64_rmw(&rmw,&eyesize);
    odtcurrent_v64->IR.rmw=rmw;
    odtcurrent_v64->IR.eyesize=eyesize;

  }

  /* adjustments to FFT determined scenes */
  if(sceneeye==0) {
    if(eyesize>35.0) {
      sceneeye=2;                     /* large eye */
    }
  }
  if(sceneeye==4) {
    if(eyesize>35.0) sceneeye=3;      /* large ragged eye */
  }
  if((sceneeye==2)||(sceneeye==3)) {
    if(eyesize<35.0) {  
      sceneeye=0;                     /* clear eye */
    }
  }

  /* CLASSIFY CLOUD REGION */
  cbring=0;
  cbringval=0;
  cbringvalmax=0;
  cbringlatmax=xlat;
  cbringlonmax=xlon;

  /* check threshold for curved band or other anaysis */
  shear=FALSE;
  cbgray=TRUE;
  irrcdo=FALSE;
  cbscene=FALSE;
  if(cwtcat==0) {
    /* shear */
    shear=TRUE;
    cbscene=TRUE;
    irrcdo=FALSE;
    if(cloudcat>=2) {
      /* curved band */
      shear=FALSE;
    }
  } else if(cwtcat==1) {
    irrcdo=FALSE;
    cbscene=TRUE;
    if(cloudcat<=1) {
      /* shear */   
      shear=TRUE;
    } else if(cloudcat==2) {
      if(cloudsyma>=30) {
        /* shear */       
        shear=TRUE;
	if(eyetemp<=cloudcwt) {
          /* irregular cdo */
          irrcdo=TRUE;
          shear=FALSE;
        }
      } else {
        /* curved band */       
        shear=FALSE;
      }
    } else {
      if(cloudsyma>=30) {
        /* shear */       
        shear=TRUE;
	if(eyetemp<=cloudcwt) {
          /* irregular cdo */
          irrcdo=TRUE;
          shear=FALSE;
        }
      } else {
        shear=FALSE;
        /* curved band */       
        if((lasttno12>=3.5)||(eyefound)) {
          /* eye */
          cbscene=FALSE;
        }
      }
    }
  } else if(cwtcat==2) {
    irrcdo=FALSE;
    cbscene=TRUE;
    if(cloudcat<=2) {
      if(cloudsyma>=50) {
        /* shear */
        shear=TRUE;
	if(eyetemp<=cloudcwt) {
          /* irregular cdo */
          irrcdo=TRUE;
          shear=FALSE;
        }
      } else {
        shear=FALSE;
        /* curved band */       
        if((lasttno12>=3.5)||(eyefound)) {
          /* eye */
          cbscene=FALSE;
        } else {
          /* curved band */       
          if(eyecat>=3) {
            /* irregular cdo */
            irrcdo=TRUE;
          }
        }
      }
    } else {
      shear=FALSE;
      irrcdo=FALSE;
      cbscene=TRUE;
      /* curved band */
      if((lasttno12>=3.5)||(eyefound)) {
        /* eye */
        cbscene=FALSE;
      } else {
        if(eyecat>=3) {
          /* irregular cdo */
          irrcdo=TRUE;
        }
      }
    }
  } else if(cwtcat==3) {
    shear=FALSE;
    irrcdo=FALSE;
    cbscene=TRUE;
    if((lasttno12>=3.5)||(eyefound)) {
      /* eye */
      cbscene=FALSE;
    }
  } else {
    /* eye scene */
    cbscene=FALSE;
    shear=FALSE;
    irrcdo=FALSE;
  }

L100:
  if(cbscene) {
    if(shear) {
      scenecloud=4;                 /* SHEAR */
      tempval=ebd_v64[3]+273.16;
      aodtv64_eyeshearcalc(xlat,xlon,tempval,3,&essizeDG,&esdiffDG);
      eyesize=A_MAX(4.0,essizeDG);
    } else if(irrcdo) {
      scenecloud=2;                 /* IRREGULAR CDO */
    } else {
      cbfound=FALSE;
L200:
      if(cbgray) {
        /* perform Curved Band analysis */
        ixx=4;  /* start with DARK GRAY */
        while((ixx>=2)&&(!cbfound)) {
          tempval=ebd_v64[ixx]+273.16;
	  aodtv64_logspiral(xlat,xlon,tempval,1,&spiral,&lat1,&lon1);
	  if((spiral>=9)||(ixx==2)) {   /* 10 = .375% -- 10 ==> 9 arcs of 15 degrees */
            if(spiral>25) {
              if(ixx==4) {
                cbgray=FALSE;
                goto L200;
              } else {
                ixx=0;
              }
            } else {
              if((ixx==2)&&(spiral<7)) {  /* 7 = .25% -- 10 ==> 6 arcs of 15 degrees */ 
                /* probably shear */
                cbfound=FALSE;
                shear=TRUE;
                goto L100;
              } else {
                cbfound=TRUE;
              }
            }
          } else {
            ixx--;
          }
        }
      } else {
        /* try BLACK and WHITE rings */
        cbscene=FALSE;
        ixx=cwtcat;
        if(ixx>6) goto L100;
        while((ixx<=6)&&(!cbfound)) {
          if((eyetemp-cloudtemp)<2.0) {
            ixx=99;
          } else {
            tempval=ebd_v64[ixx]+273.16;
            aodtv64_logspiral(xlat,xlon,tempval,1,&spiral,&lat1,&lon1);
            if(((spiral<9)||(spiral>25))&&(ixx==6)) { 
              /* probably CDO/EMBC */
              ixx=99;
            } else {
              if((spiral>=9)&&(spiral<=25)) {
                cbfound=TRUE;
              } else {
                ixx++;
              }
            }
          }
        }
      }
      if(cbfound) {
        cbring=ixx;
        cbringval=spiral;
        scenecloud=3;                  /* CURVED BAND */
        /* search for maximum curved band analysis location within 1-degree box */
        tempval=ebd_v64[cbring]+273.16;
        if(osearch_v64) {
          aodtv64_logspiral(xlat,xlon,tempval,2,&spiralmax,&lat2,&lon2);
          cbringvalmax=spiralmax;
          cbringlatmax=lat2;
          cbringlonmax=lon2;
        } 
      } else {
        scenecloud=0;
        if(eyecat>0) sceneeye=4; 
        if(eyecat>=cloudcat) sceneeye=6;
      }
    }
    if(scenecloud>=2) sceneeye=6;    /* NO EYE */
  } else {
    if(cloudfft==0) {
      scenecloud=0;           /* UNIFORM */
    } else if((cloudfft>=1)&&(cloudfft<=7)) {
      scenecloud=0;           /* UNIFORM */
      if((diffcloudcat>=1)&&(cloudsyma>=8.0)) {
        scenecloud=1;     /* EMBEDDED CENTER */
      }
    } else {
      if((diffcloudcat<=0)&&(cloudsyma>=35.0)) {
        scenecloud=2;       /* IRREGULAR CDO */
      } else {
        scenecloud=0;           /* UNIFORM */
        if((diffcloudcat>=1)&&(cloudsyma>=8.0)) {
          scenecloud=1;     /* EMBEDDED CENTER */
        } 
      } 
    }
  }
  if((scenecloud==0)&&(sceneeye==6)) {
    /* CDO...check size */
    ixx=A_MIN(4,eyecat);
    tempval=ebd_v64[ixx]+273.16;
    aodtv64_eyeshearcalc(xlat,xlon,tempval,1,&cdosize,&cdosizex);
    eyesize=cdosize;  /* not used at the moment, but I'll leave it */
  }

  odtcurrent_v64->IR.eyescene=sceneeye;
  odtcurrent_v64->IR.cloudscene=scenecloud;
  odtcurrent_v64->IR.eyesceneold=-1;
  odtcurrent_v64->IR.cloudsceneold=-1;
  odtcurrent_v64->IR.eyesize=eyesize;
  odtcurrent_v64->IR.ringcb=cbring;
  odtcurrent_v64->IR.ringcbval=cbringval;
  odtcurrent_v64->IR.ringcbvalmax=cbringvalmax;
  odtcurrent_v64->IR.ringcblatmax=cbringlatmax;
  odtcurrent_v64->IR.ringcblonmax=cbringlonmax;

}

void aodtv64_classifyredo(void)
{
  int   ixx,scene,cbring,cbringval,spiral,spiralmax;
  int   cbringvalmax;
  float xlat,xlon,tempval;
  float lat1,lon1,lat2,lon2,essizeDG,esdiffDG;
  float eyesize,cbringlatmax,cbringlonmax;
  logical cbfound=FALSE;

  xlat=odtcurrent_v64->IR.latitude;
  xlon=odtcurrent_v64->IR.longitude;
  scene=odtcurrent_v64->IR.cloudscene;
  cbringvalmax=0;
  cbringlatmax=xlat;
  cbringlonmax=xlon;
  if(scene==3) {
    /* perform curved band analysis -- find length of spiral arc */
    ixx=2;
    while((ixx<=4)&&(!cbfound)) {
      tempval=ebd_v64[ixx]+273.16;
      /* determine curved band analysis at user selected center */
      aodtv64_logspiral(xlat,xlon,tempval,1,&spiral,&lat1,&lon1);
      cbring=ixx;
      cbringval=spiral;
      /* search for maximum curved band analysis location within 1-degree box */
      if(osearch_v64) {
        aodtv64_logspiral(xlat,xlon,tempval,2,&spiralmax,&lat2,&lon2);
        cbringvalmax=spiralmax;
        cbringlatmax=lat2;
        cbringlonmax=lon2;
      }
      if(spiral<=25) {
        cbfound=TRUE;
      }
      ixx++;
    }
    if(cbring==4) {
      cbringval=A_MIN(25,cbringval);
      cbringvalmax=A_MIN(25,cbringvalmax);
      cbringlatmax=xlat;
      cbringlonmax=xlon;
    } 
    eyesize=0.0;
  } else {
    tempval=ebd_v64[3]+273.16;
    aodtv64_eyeshearcalc(xlat,xlon,tempval,3,&essizeDG,&esdiffDG);
    eyesize=essizeDG;
    cbring=0;
    cbringval=0;
  }
  odtcurrent_v64->IR.eyesize=eyesize;
  odtcurrent_v64->IR.ringcb=cbring;
  odtcurrent_v64->IR.ringcbval=cbringval;
  odtcurrent_v64->IR.ringcbvalmax=cbringvalmax;
  odtcurrent_v64->IR.ringcblatmax=cbringlatmax;
  odtcurrent_v64->IR.ringcblonmax=cbringlonmax;

}

void aodtv64_eyeshearcalc( float xlat,float xlon,float tempval,int atype,float *valb,float *valc)
/* Determine eye size or shear distance for a given scene.
    Inputs  : xlat    - center latitude of analysis grid
              xlon    - center longitude of analysis grid
              tempval - temperature threshold value to be used
              atype   - analysis type (1-cdo size,2-eye size,3-shear distance)
    Outputs : valb    - eye/cdo radius or shear distance
              valc    - eye/cdo symmetry value or 0
*/
{
  int ixx,iyy,np;
  float xdist,xangle,smalldist,vc;
  float *zlat,*zlon;
  float a1,a2,a3,a4;

  /* allocate memory */
  zlat=(float *)calloc((size_t)bufsiz,sizeof(float));
  zlon=(float *)calloc((size_t)bufsiz,sizeof(float));

  /* initialize arrays */
  np=0;
  if(atype==1) {
    a1=0.0;a2=0.0;a3=0.0;a4=0.0;
  } else {
    a1=999.9;a2=999.9;a3=999.9;a4=999.9;
  }
  for(ixx=0;ixx<areadata_v64->numx;ixx++) {
    for(iyy=0;iyy<areadata_v64->numy;iyy++) {
      if(areadata_v64->temp[iyy][ixx]<=tempval) {
        zlat[np]=areadata_v64->lat[iyy][ixx];
        zlon[np]=areadata_v64->lon[iyy][ixx];
        np++;
      }
    }
  }

  /* eye size determination - RETURNS RADIUS */
  if(atype<=2) {
    for(ixx=0;ixx<np;ixx++) {
      aodtv64_distance(xlat,xlon,zlat[ixx],zlon[ixx],1,&xdist,&xangle);
      if(atype==1) {
        /* determine size of CDO */
        if((A_ABS(xangle-45.0)<=15.0)&&(xdist>a1)) a1=xdist;
        if((A_ABS(xangle-135.0)<=15.0)&&(xdist>a2)) a2=xdist;
        if((A_ABS(xangle-225.0)<=15.0)&&(xdist>a3)) a3=xdist;
        if((A_ABS(xangle-315.0)<=15.0)&&(xdist>a4)) a4=xdist;
      } else {
        /* determine size of EYE */
        if((A_ABS(xangle-45.0)<=15.0)&&(xdist<a1)) a1=xdist;
        if((A_ABS(xangle-135.0)<=15.0)&&(xdist<a2)) a2=xdist;
        if((A_ABS(xangle-225.0)<=15.0)&&(xdist<a3)) a3=xdist;
        if((A_ABS(xangle-315.0)<=15.0)&&(xdist<a4)) a4=xdist;
      }
    }
    *valb=(a1+a2+a3+a4)/4.0;
    /* *valc=A_ABS(((a1+a3)/2.0)-((a2+a4)/2.0))/2.0; */
    vc=(a1+a3)/(a2+a4);
    if(vc<1.0) vc=1.0/vc;
    *valc=vc;
  }

  /* shear distance determination */
  if(atype==3) {
    smalldist=999.9;
    for(ixx=0;ixx<np;ixx++) {
      aodtv64_distance(xlat,xlon,zlat[ixx],zlon[ixx],1,&xdist,&xangle);
      if(xdist<smalldist) smalldist=xdist;
    }
    *valb=smalldist;
    *valc=0.0;
  }

  /* free memory */
  free(zlon);
  free(zlat);

}

/*=====================================================================*/

void aodtv64_xxsort(float *x1,float *x2,float *i2,int ndim)
{
  int   ih,i;
  float x,top;

  aodtv64_fminx(x1,ndim,&ih,&x);
  top=x-1.0;
  for(i=0;i<ndim;i++) {
    aodtv64_fmaxx(x1,ndim,&ih,&x);
    i2[i]=ih;
    x2[i]=x1[ih];
    x1[ih]=top;
  }
}

void aodtv64_fminx(float *f,int ndim,int *im,float *x)
{
  int i;

  *im=0;
  *x=f[0];
  for(i=1;i<ndim;i++) {
    if(f[i]<*x) {
      *x=f[i];
      *im=i;
    }
  }
}

void aodtv64_fmaxx(float *f,int ndim,int *im,float *x)
{
  int i;

  *im=0;
  *x=f[0];
  for(i=1;i<ndim;i++) {
    if(f[i]>*x) {
      *x=f[i];
      *im=i;
    }
  }
}

int aodtv64_rmw(float *rmw,float *eyesize)
/* Determine radius of maximum wind based upon Jim Kossin's
   regression based scheme
    Inputs  : ix0     - element location of center point
              iy0     - line location of center point
    Outputs : rmw     - radius of maximum wind distance
              eyesize - eye size radius (km)
              -1 = error/eyewall not found
               0 = radius found
*/
{
   int   ixmin,ixmax,iymin,iymax;
   int   ixc,iyc,i,ix,iy,idx1,idy1,idx2,idy2;
   float dx1,dx2,dy1,dy2,dav,xlat,xlon,xclat,xclon,xangle;
   float tcrit=228.0,warm=223.0;;
                                                                                                                                                                                             
   /* calculate cursorx/cursory from numx/numy... values should be 0.5*numx/y */
   ixc=areadata_v64->numx/2;
   iyc=areadata_v64->numy/2;
   ixmax=A_MIN(areadata_v64->numx,ixc+320);
   ixmin=A_MAX(0,ixc-320);
   iymax=A_MIN(areadata_v64->numy,iyc+240);
   iymin=A_MAX(0,iyc-240);
                                                                                                                                                                                             
   if(odtcurrent_v64->IR.cloudt>=warm) {
     tcrit=(odtcurrent_v64->IR.eyet+(2.0*odtcurrent_v64->IR.cloudt))/3.0;
   }
                                                                                                                                                                                             
   *rmw=-99.9;
   *eyesize=-99.9;
   /* iterate five times */
   for(i=0;i<5;i++) {
     ix=ixc;
     while(areadata_v64->temp[iyc][ix]>tcrit) {
       ix=ix-1;
       if(ix==ixmin) {
         return -1;
       }
     }
     idx1=ix;
     ix=ixc;
     while(areadata_v64->temp[iyc][ix]>tcrit) {
       ix=ix+1;
       if(ix==ixmax) {
         return -1;
       }
     }
     idx2=ix;
     iy=iyc;
     while(areadata_v64->temp[iy][ixc]>tcrit) {
       iy=iy-1;
       if(iy==iymin) {
         return -1;
       }
     }
     idy1=iy;
     iy=iyc;
     while(areadata_v64->temp[iy][ixc]>tcrit) {
       iy=iy+1;
       if(iy==iymax) {
         return -1;
       }
     }
     idy2=iy;
     ixc=(int)((((float)(idx1+idx2))/2.0));
     iyc=(int)((((float)(idy1+idy2))/2.0));
   }
   xclat=areadata_v64->lat[iyc][ixc];
   xclon=areadata_v64->lon[iyc][ixc];
   xlat=areadata_v64->lat[iyc][idx1];
   xlon=areadata_v64->lon[iyc][idx1];
   aodtv64_distance(xlat,xlon,xclat,xclon,1,&dx1,&xangle);
   xlat=areadata_v64->lat[iyc][idx2];
   xlon=areadata_v64->lon[iyc][idx2];
   aodtv64_distance(xlat,xlon,xclat,xclon,1,&dx2,&xangle);
   xlat=areadata_v64->lat[idy1][ixc];
   xlon=areadata_v64->lon[idy1][ixc];
   aodtv64_distance(xlat,xlon,xclat,xclon,1,&dy1,&xangle);
   xlat=areadata_v64->lat[idy2][ixc];
   xlon=areadata_v64->lon[idy2][ixc];
   aodtv64_distance(xlat,xlon,xclat,xclon,1,&dy2,&xangle);
   dav=(dx1+dx2+dy1+dy2)/4.0;
   if(dav>0.0) {
     *rmw=2.8068+(0.8361*dav);    /* Howard's coeffs */
     *eyesize=dav;
   }
                                                                                                                                                                                             
   return 0;
}

/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"
/* include file containing all AODT remapping functions */
#include "../inc/odtremap.h"

#include <time.h>
#include <sys/types.h>
#define IND(y,x) ((y-1)*remap_vars.ncl+x)
#define BTEST(word, bit) (word & (0x01 << ((bit)%8)))
#define BSET(word, bit)  (word | (0x01 << ((bit)%8)))
#define  A_LOG(x)     (float)( log( (double)(x) ) )
#define  A_ROUND(x)   (int)( (x+0.5) )

/* internal routines */
int  aodtv64_automode1( float *, float *, int * );
int  aodtv64_automode2( float,float,float *, float *, int * );
int  aodtv64_readforecast( FILE *, int * );
void aodtv64_pickfinallocation( float, float, float, float, int,
                   float, float, float, float, int,
                   float, float, float,
                   float, float, float,int,
                   float *, float *, float *,int * );
void aodtv64_laplacian( float ** );
int  aodtv64_polint( double [],float [],int,double,float *,float *);

/* Tony Wimmers center finding routines */
int  aodtv64_spiralCenterLowRes(float, float, float *, float *, float *,float **);
void aodtv64_gradient(float [][maxd], int, int, float, float, float **, float **);
int  aodtv64_ringFit(float, float, float *, float *, int *, float **);
int  aodtv64_lalo2indsFloat(float, float, float [][maxd], float [][maxd],
			int, int, int *, int *);
int  aodtv64_inds2laloFloat(int, int, float [][maxd], float [][maxd],
			int, int, float *, float *);
int  aodtv64_circleFilt(int, int **, int **);
int  aodtv64_calcScores(float, float, float, float, float, float **,
			float, float, float **, float *, float *,
			float *, int *);
int  aodtv64_findRingScore(float, float, float **, float *);

/* Dave Santek remapping routines */
int  aodtv64_remapdata(void);
int  aodtv64_uinit(void);
int aodtv64_init(int lspline, int espline, int *nc);
void aodtv64_determinedest(void);
void aodtv64_corner(int, int, int, float [], float []);
int  aodtv64_umap(int, int, int *, int *);
int  aodtv64_domap(int, float [], float [], int, int);
int  aodtv64_findpoint (float, float, int *, int *);

struct  datagrid *areadataRM;

int aodtv64_automode1( float *retlat,float *retlon,int *retpos ) 
/* Determine storm position at time curtime using NHC/JTWC 
   forecast discussion products.  Time and location information
   from these products are then interpolated to time in question
   to derive a estimated storm position.  If position estimation
   cannot be calculated, a lat/lon position of -99,99/-99.99 will
   be returned.
    Inputs  : none
    Outputs : retlat  - estimated latitude position
              retlon  - estimated longitude position
    Note    : autopos - method used to derive estimated storm location 0-error
    Return : -43 : Error w/ forecast file open and BAD extrapolation
             -44 : Invalid forecast file and BAD extrapolation
             -45 : Error w/ forecast file read and BAD extrapolation
             -46 : Error w/ forecast interpolation and BAD extrapolation
              42 : GOOD INTERPOLATION
              43 : Error w/ forecast file open but GOOD EXTRAPOLATION
              44 : Invalid forecast file but GOOD extrapolation
              45 : Error w/ forecast file read but GOOD extrapolation
              46 : Error w/ forecast interpolation but GOOD EXTRAPOLATION
*/
{
  FILE *fp;
  int iok,ioklat,ioklon,usethis,iret,fnum;
  float telat,telon,dtelat,dtelon;
  double curtime;
  logical getextrap=FALSE;

  curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);
  iret=0;
  /* get file information */
  fp=fopen(fixfile_v64,"r+");
  if(fp==0) {
    telat=-99.99;
    telon=-99.99;
    usethis=0;
    getextrap=TRUE;
    iret=43;
  } else {
    /* Get forecast positions from NHC (or other) forecast file, if avaialble.
       Otherwise, use linear extrapolation of previous locations.
       Will use as "first guess" for Laplacian */
    iok=aodtv64_readforecast(fp,&fnum);

    if(iok!=0) {
      telat=-99.99;
      telon=-99.99;
      usethis=0;
      getextrap=TRUE;
      iret=46+iok; /* -1 will givd 45=invalid index / -2 will give 44=invalid file/dates */
    } else {
      /* Call Interpolation procedure.  Routine is called 
         for both latitude and longitude interpolations. */
      ioklat=aodtv64_polint(fcsttime_v64,fcstlat_v64,fnum,curtime,&telat,&dtelat);
      ioklon=aodtv64_polint(fcsttime_v64,fcstlon_v64,fnum,curtime,&telon,&dtelon);
      if((ioklat==0)&&(ioklon==0)) {
        /* Good interpolated values... use 'em */
        usethis=1;
        getextrap=FALSE;
        iret=42;
      } else {
        /* Bad interpolated values... try extrapolated values */
        getextrap=TRUE;
        usethis=6;
        iret=46;
      }
    }
  } 

  /* try to extrapolate storm location from previous storm locations in history file */
  if(getextrap) {
    /* call slopecal to get y-intercept values for lat/lon values */
    telat=aodtv64_slopecal((double)12.0,2);
    telon=aodtv64_slopecal((double)12.0,3);
    if((A_ABS(telat)>90.0)||(A_ABS(telon)>180.0)) {
      usethis=0;
      iret=-iret;   /* invalid interp and extrap... negative error code returns */
    } else {
      usethis=6;
      iret=iret;   /* invalid interp but good extrap... positive error code returns */
    }
  }
  
  *retlat=telat;
  *retlon=telon;
  *retpos=usethis;

  return iret;
}

/*=====================================================================*/

int aodtv64_automode2(float taclat,float taclon,
                   float *retlat, float *retlon, int *retpos )
/* Additional automatic positioning of storm center location 
   using official forecasts from NHC or JTWC as input.  Storm
   location will be estimated using Laplacian analysis (looking
   for large gradients in temperature field, i.e. the eye) along
   with new spiral fitting and ring fitting routines derived 
   by Tony Wimmers in his MatLab routines (which have been converted).
   The final position will be determined utilizing 
   empirically defined confidence factors for each method.
   The final storm position will be returned along with a 
   position determination flag.
    Inputs  : taclat  - input storm center latitude position (forecast interpolation)
              taclon  - input storm center longitude position (forecast interpolation)
    Outputs : retlat  - final storm center latitude position
              retlon  - final storm center longitude position
              autopos - method used to derive estimated storm location
                         0-error
                         1-interpolation of operational forecast
                         2-Laplacian analysis
                         3-Warm Spot location
                         4-10^ log spiral analysis 
                         5-Combo method of spiral and ring analyses
                         6-linear extrapolation from prior locations
**									*
* Log:									*
* T. Piper/SAIC		05/06	Properly allocate two-dimensional arrays*
* T. Piper/SAIC		05/06	Properly free lapvals, ringScores,	*
*				and spGrid1				*
************************************************************************/
{ 
  int   ixx,iyy,ix,iy,ic1=0,ic2=0;
  int   searchrad2=90,threshold1=12.0,threshold2=7.5,usethis;
  int   ringSize,xn,yn;
  int   maxRingSize,scoreMethod;
  float latdiff,londiff;
  float tempfcst;
  float xdist,xangle,uselat,uselon,useconf;
  float avelat1,stdvlat1,skewlat1,avelon1,stdvlon1,skewlon1;
  float avelat2,stdvlat2,skewlat2,avelon2,stdvlon2,skewlon2;
  float **lapvals, **ringScores, **spGrid1;
  float *templat1,*templon1;
  float *templat2,*templon2;
  float latFirstGuess,lonFirstGuess,lonNW,lonNE,lonSW,lonSE;
  float latSpiralCenter,lonSpiralCenter,maxSpiralCenter;
  float latRingCenter,lonRingCenter;
  float latFinal,lonFinal,scoreFinal;
  logical odocross=TRUE,dateline=FALSE;
  size_t sofflt, soffpr;

/*---------------------------------------------------------------------*/

  /* allocate memory */
  sofflt=sizeof(float);
  soffpr=sizeof(float*);
  lapvals=(float **)calloc((size_t)maxd,soffpr);
  for(iy=0;iy<maxd;iy++) {
    lapvals[iy]=(float *)calloc((size_t)maxd,sofflt);
  }
  templat1=(float *)calloc((size_t)(maxd*4),sofflt);
  templon1=(float *)calloc((size_t)(maxd*4),sofflt);
  templat2=(float *)calloc((size_t)(maxd*4),sofflt);
  templon2=(float *)calloc((size_t)(maxd*4),sofflt);

  if(odocross) {
    /* crosshatch stuff */
    areadataRM=(struct datagrid *)malloc(sizeof(struct datagrid)); 
    londiff=areadata_v64->lon[0][0]-areadata_v64->lon[0][1];
    latdiff=areadata_v64->lat[0][0]-areadata_v64->lat[1][0];
    if(A_ABS(londiff-latdiff)<0.001) {
      /* data is already remapped */
      /* crosses dateline check */
      xn=areadata_v64->numx-1;
      yn=areadata_v64->numy-1;
      lonNW=areadata_v64->lon[0][0];
      lonNE=areadata_v64->lon[0][xn];
      lonSW=areadata_v64->lon[yn][0];
      lonSE=areadata_v64->lon[yn][xn];
      if((lonNW<lonNE)||(lonSW<lonSE)) dateline=TRUE;
      lonNW=lonNW+360.0;
      for(iyy=0;iyy<areadata_v64->numy;iyy++) {
        for(ixx=0;ixx<areadata_v64->numx;ixx++) {
          areadataRM->lon[iyy][ixx]=areadata_v64->lon[iyy][ixx];
          /* check for dateline crossing */
          if (dateline&&(areadataRM->lon[iyy][ixx]<0.0)) areadataRM->lon[iyy][ixx]=areadataRM->lon[iyy][ixx]+360.0;
          areadataRM->lat[iyy][ixx]=areadata_v64->lat[iyy][ixx];
          areadataRM->temp[iyy][ixx]=areadata_v64->temp[iyy][ixx];
        }
      }
      areadataRM->numx=areadata_v64->numx;
      areadataRM->numy=areadata_v64->numy;
    } else {
      /* remap data to rectilinear projection */
      aodtv64_remapdata( ); 
    }
  
    /* allocate space for final analysis grids from Ring Fit and Spiral Analysis */
    spGrid1=(float **)calloc((size_t)bufsiz,soffpr);
    for(iy=0;iy<bufsiz;iy++) {
      spGrid1[iy]=(float *)calloc((size_t)3,sofflt);
    }
    ringSize=(bufsiz*15)+1;
    ringScores=(float **)calloc((size_t)ringSize,soffpr);
    for(iy=0;iy<ringSize;iy++) {
      ringScores[iy]=(float *)calloc((size_t)3,sofflt);

    }

    /* perform spiral analysis to determine storm center location */
    aodtv64_spiralCenterLowRes(taclat,taclon,&latSpiralCenter,
			&lonSpiralCenter,&maxSpiralCenter,spGrid1);

    /* redefine first guess for ring analysis as input storm location point */
    latFirstGuess=latSpiralCenter;
    lonFirstGuess=lonSpiralCenter;
  
    /* perform ring analysis to determine storm center location */
    /* (void)time(&timestart); */
    aodtv64_ringFit(latFirstGuess,lonFirstGuess,&latRingCenter,
			&lonRingCenter,&maxRingSize,ringScores);
    /* caluculate confidence factor for combined spiral/ring analyses */
    aodtv64_calcScores(taclat,taclon, latSpiralCenter,lonSpiralCenter,
			maxSpiralCenter,spGrid1, latRingCenter,lonRingCenter,
			ringScores, &latFinal,&lonFinal, &scoreFinal,&scoreMethod);

    /* free memory */
    for(iy=0;iy<ringSize;iy++) {
	free(ringScores[iy]);
    }
    free(ringScores);
    for(iy=0;iy<bufsiz;iy++) {
	free(spGrid1[iy]);
    }
    free(spGrid1);
    free(areadataRM);

  } else {
    latSpiralCenter=-99.9;
    lonSpiralCenter=-99.9;
    maxSpiralCenter=-1.0;
    latRingCenter=-99.9;
    lonRingCenter=-99.9;
    latFinal=-99.9;
    lonFinal=-99.9;
    scoreFinal=-99.9;
    scoreMethod=1;
  }

  /* compute Laplacian of scene */
  aodtv64_laplacian(lapvals);
  /* check Laplacian values within search radii and set up analysis arrays */
  for(iy=0;iy<areadata_v64->numy;iy++) {
    for(ix=0;ix<areadata_v64->numx;ix++) {
      aodtv64_distance(odtcurrent_v64->IR.warmlatitude,odtcurrent_v64->IR.warmlongitude,
			areadata_v64->lat[iy][ix],areadata_v64->lon[iy][ix],1,&xdist,&xangle);
      if(xdist<=(float)searchrad2) {
        if((A_ABS(lapvals[iy][ix])>=(float)threshold2)&&(areadata_v64->temp[iy][ix]>=ebd_v64[2])) {
          templat1[ic1]=areadata_v64->lat[iy][ix];
          templon1[ic1]=areadata_v64->lon[iy][ix];
          ic1++;
        }
        if((A_ABS(lapvals[iy][ix])>=(float)threshold1)&&(areadata_v64->temp[iy][ix]>=ebd_v64[2])) {
          templat2[ic2]=areadata_v64->lat[iy][ix];
          templon2[ic2]=areadata_v64->lon[iy][ix];
          ic2++;
        }
      }
    }
  }

  /* determine stdv and skew of Laplacian analysis for
     two different thresholds.  Will try and pinpoint
     eye/warm spot regions from these values */
  if(ic1>0) {
    aodtv64_calcskew(templat1,ic1,&avelat1,&stdvlat1,&skewlat1);
    aodtv64_calcskew(templon1,ic1,&avelon1,&stdvlon1,&skewlon1);
  } else {
    avelat1=-99.9;
    avelon1=-99.9;
  }
  if(ic2>0) {
    aodtv64_calcskew(templat2,ic2,&avelat2,&stdvlat2,&skewlat2);
    aodtv64_calcskew(templon2,ic2,&avelon2,&stdvlon2,&skewlon2);
  } else {
    avelat2=-99.9;
    avelon2=-99.9;
  }

  /* free memory */
  free(templon2);
  free(templat2);
  free(templon1);
  free(templat1);
  for(iy=0;iy<maxd;iy++) {
    free(lapvals[iy]);
  }
  free(lapvals);

  /* load forecast interpolation/extrapolation location and temperature
   * value into variables for location determination scheme */
  tempfcst=odtcurrent_v64->IR.eyet;
  usethis=odtcurrent_v64->IR.autopos;

  aodtv64_pickfinallocation(avelat1,avelon1,stdvlat1,stdvlon1,ic1,
               avelat2,avelon2,stdvlat2,stdvlon2,ic2,
               taclat,taclon,tempfcst,
               latFinal,lonFinal,scoreFinal,scoreMethod,
               &uselat,&uselon,&useconf,&usethis);

  /* printf("TAC CENTER    :  lat=%f  lon=%f \n",taclat,taclon); */
  /* printf("TonyW CENTER  :  lat=%f  lon=%f  SCORE=%f  METHOD=%d\n",latFinal,lonFinal,scoreFinal,scoreMethod); */
  /* printf("FINAL LOCATION:  lat=%f  lon=%f  SCORE=%f  METHOD=%d\n",uselat,uselon,useconf,usethis); */


  /* store final location and determination method in structure */
  *retlat=uselat;
  *retlon=uselon;
  *retpos=usethis;

  return 0;
}

/*========================================================================*/

int aodtv64_calcScores(float latFG, float lonFG, float latSP, float lonSP,
			float scoreSP, float **spGrid, float latRG,
			float lonRG, float **ringScores, float *finalLat,
			float *finalLon,float *finalMax,int *finalType)
/* This routine will determine the confidence scores for the spiral fitting 
   and ring fitting routines and calculate the best possible position for the 
   storm center based upon a combination of the two methods, if available.  If the
   ring fitting routine does not determine a good candidate position, the spiral
   fitting routine will be used alone.  If the spiral fitting routine candidate point
   is not "good", the forecast point will be selected.
   Inputs  : latFG    - First Guess (forecast interpolation) latitude
             lonFG    - First Guess (forecast interpolation) longitude
             latSP    - Spiral Analysis latitude at maximum score location
             lonSP    - Spiral Analysis longitude at maximum score location
             scoreSP  - Spiral Analysis Score value
             spGrid   - Grid of Spiral Analysis scores in analysis region
             latRG    - Ring Analysis latitude at maximum score location
             lonRG    - Ring Analysis longitude at maximum score location
             ringGrid - Grid of Ring Analysis scores in analysis region
   Outputs : finalLat - Latitude of final selected location
             finalLon - Longitude of final selected location
             finalMax - Confidence Score (value) of final selected location
             finalType- Method used to determine final selected location
                        1-First Guess,4-Enhanced Spiral Analysis,5-Combo Ring/Spiral Analysis
*/
{
  int   iy,spnum,iok;
  float maxFxErr=1.0,expectedMaxFxErr=maxFxErr;
  float maxAllowableDisplacement=expectedMaxFxErr*1.15;
  float SPIRALWEIGHT=10.0,DISTPENALTYWEIGHT=(0.5/expectedMaxFxErr);   /* changed 20 to 10 */
  float PROXIMITYBONUS=4.5,PROXIMITYTHRESH=0.25,COMBOSCORETHRESH=15.0;  /* changed 5.0 to 4.5; changed 0.175 to 0.25 */
  float xdenom=111.0;  /* convert distance in km to degrees */
  float xdist,xangle,spiralPartScore;
  float latOfESPmax,lonOfESPmax,espDistFromGuess,ESPindexMax=-999.0;
  float foundRingScore;
  float latMaxComboScore,lonMaxComboScore,comboScore,maxComboScore=-99.0;
  float *distPenalty,*spiralPart,*distFromSpCent,*distBonus,*enhancedSpiralPart;
  float *comboScoreSpiralPart;
  size_t sofflt;

/*---------------------------------------------------------------------*/

  /* allocate memory */
  sofflt=sizeof(float);
  spnum=(int)spGrid[0][0]+1;
  distPenalty=(float *)calloc((size_t)spnum,sofflt);
  spiralPart=(float *)calloc((size_t)spnum,sofflt);
  distFromSpCent=(float *)calloc((size_t)spnum,sofflt);
  distBonus=(float *)calloc((size_t)spnum,sofflt);
  enhancedSpiralPart=(float *)calloc((size_t)spnum,sofflt);
  comboScoreSpiralPart=(float *)calloc((size_t)spnum,sofflt);

  /* Spiral Score Calculations */
  aodtv64_distance(latFG,lonFG,latSP,lonSP,1,&xdist,&xangle);
  spiralPartScore=scoreSP;
  for(iy=1;iy<spnum;iy++) {
    aodtv64_distance(latFG,lonFG,spGrid[iy][1],spGrid[iy][2],1,&xdist,&xangle);
    distPenalty[iy]=-DISTPENALTYWEIGHT*(xdist/xdenom);
    spiralPart[iy]=SPIRALWEIGHT*(spGrid[iy][0]-spiralPartScore);
    aodtv64_distance(latSP,lonSP,spGrid[iy][1],spGrid[iy][2],1,&xdist,&xangle);
    distFromSpCent[iy]=xdist/xdenom;
    if(distFromSpCent[iy]<=PROXIMITYTHRESH) {
      distBonus[iy]=PROXIMITYBONUS;
    } else {
      distBonus[iy]=0.0;
    }
    enhancedSpiralPart[iy]=spiralPart[iy]+distPenalty[iy];
    /* printf("spGrid :   iy: %d  Lat: %f  Lon: %f  Value: %f ",iy,spGrid[iy][1],spGrid[iy][2],spGrid[iy][0]); */
    /* printf(" distPentaly=%f  distFromSpCent=%f  distBonus=%f   max=%f\n", distPenalty[iy],distFromSpCent[iy],distBonus[iy],ESPindexMax); */
    if(enhancedSpiralPart[iy]>ESPindexMax) {
      ESPindexMax=enhancedSpiralPart[iy];
      latOfESPmax=spGrid[iy][1];
      lonOfESPmax=spGrid[iy][2];
    }
    comboScoreSpiralPart[iy]=spiralPart[iy]+distPenalty[iy]+distBonus[iy];
  }
  /* printf("latFG=%f  lonFG=%f   latOfESPmax=%f  lonOfESPmax=%f\n",latFG,lonFG,latOfESPmax,lonOfESPmax); */
  aodtv64_distance(latFG,lonFG,latOfESPmax,lonOfESPmax,1,&xdist,&xangle);
  espDistFromGuess=xdist/xdenom;
  
  /* printf("espDistFromGuess=%f  maxAllowableDisplacement=%f\n",espDistFromGuess,maxAllowableDisplacement); */
  if(espDistFromGuess<=maxAllowableDisplacement) {
    /* Ring Score Calculations */
    aodtv64_distance(latFG,lonFG,latRG,lonRG,1,&xdist,&xangle);
  
    for(iy=1;iy<spnum;iy++) {
      iok=aodtv64_findRingScore(spGrid[iy][1],spGrid[iy][2],ringScores,&foundRingScore);
      if(iok==1) {
        comboScore=comboScoreSpiralPart[iy]+foundRingScore;
        /* printf("    FOUND  spnum=%d  lat=%f  lon=%f  ringScore=%f  comboScore=%f  maxComboScore=%f\n",iy,spGrid[iy][1],spGrid[iy][2],foundRingScore,comboScore,maxComboScore); */
        if(comboScore>maxComboScore) {
          maxComboScore=comboScore;
          latMaxComboScore=spGrid[iy][1];
          lonMaxComboScore=spGrid[iy][2];
        }
      }
    }
    /* printf("maxComboScore=%f   RingPart=%f   ESPindexMax=%f\n",maxComboScore,foundRingScore,ESPindexMax); */
    if(maxComboScore>=COMBOSCORETHRESH) {
      /* use Combo Method */
      *finalMax=maxComboScore;
      *finalLat=latMaxComboScore;
      *finalLon=lonMaxComboScore;
      *finalType=5;
    } else {
      /* use ESP method */
      *finalMax=1.0+ESPindexMax;
      *finalLat=latOfESPmax;
      *finalLon=lonOfESPmax;
      *finalType=4;
    }
  } else {
    /* use Forecast Position */
    *finalMax=0.0;
    *finalLat=latFG;
    *finalLon=lonFG;
    *finalType=1;
  }

  /* free memory */
  free(distPenalty);
  free(spiralPart);
  free(distFromSpCent);
  free(distBonus);
  free(enhancedSpiralPart);
  free(comboScoreSpiralPart);

  return 0;
}

int aodtv64_findRingScore(float lat,float lon,float **ringScores,float *value)
/* Find Ring score at selected location (spiral analysis location)
   Inputs  : lat        - latitude of search location
             lon        - longitude of search location
             ringScores - Array/Grid of Ring Analysis scores
   Outputs : value      - Value at search location in Ring Analysis Grid (if found)
   Return  : -1 if not found, 1 if found
*/
{
  int iyy=1,iret=-1,a;
  float incy,incx;

  a=ringScores[0][0];
  *value=-99.9;
  incy=A_ABS(areadataRM->lat[0][0]-areadataRM->lat[1][0])/2.0;
  incx=A_ABS(areadataRM->lon[0][1]-areadataRM->lon[0][0])/2.0;
  while(iyy<=a) {
    if((A_ABS(ringScores[iyy][1]-lat)<=incy)&&(A_ABS(ringScores[iyy][2]-lon)<=incx)) {
      if(ringScores[iyy][0]>*value) *value=ringScores[iyy][0];
      iret=1;
    }
    iyy++;
  }
 
  return iret;
}

/*=======================================================================*/

int aodtv64_readforecast( FILE *fp,int *fnum )
/* Read the forecast file and obtain the forecast positions
   for interpolation of the storm position
    Inputs  : fp      - file pointer
    Outputs : fnum    - number of points used in forecast interpolation
              return value : -4 for error reading forecast file
                             -5 for invalid forecast file
                              0 for good read of forecast file
              global variables : fcsttime_v64, fcstlat_v64, fcstlon_v64
                                 arrays containing previous, current,
                                 and forecast times/positions for use
                                 in quadratic interpolation scheme
                                 [0] - 12 hour old time/postion (if available)
                                 [1] - 6 hour old time/postion (if available)
                                 [2] - current time/postion
                                 [3] - forecast time/postion
                                 [4] - forecast time/postion
*/
{
  char line[200];
  char l1[30],l2[30],l3[30],l4[30],l5[30],l6[30],l7[30],l8[30],l1a[30];
  int maxline=200,ixx,iyy,izz1,izz2,iy,iz,maxfcst;
  int imon,imonx,iyear,iyearx,idate,idatex,julday,juldayx;
  int ftime,ftimelast,itime;
  float ftemp, lat[6],lon[6];
  double curtime,time[6],xtime,ttime;
  logical getmonth=FALSE,findyear=TRUE;

  curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);
  ixx=0;
  iyy=0;
  if(ifixtype_v64==1) {  /* NHC */
    /* read NHC forecast file, quit at EOF */
    while(((fgets(line,maxline,fp))!=NULL)&&(ixx<6)) {
      strcpy(l6," ");
      if(ixx==0) {
        (void)sscanf(line,"%s %s %s %s %s %s %s",l1,l2,l3,l4,l5,l6,l7);
      } else {
        (void)sscanf(line,"%s %s %s %s %s %s %s",l1,l1a,l2,l3,l4,l5,l6);
      }
      if((!getmonth)&&((!strncmp(l2,"AM",2))||(!strncmp(l2,"PM",2)))) {
        /* this is reading the header at the top of the forecast file.
           it is done to obtain the current month and year of the forecast
           so that we can check it accurately against the image date/time */
        imonx=0;
	ftemp = aodtv64_atoif(l6,1,2);
	idatex=(int)ftemp;
        ftemp = aodtv64_atoif(l7,1,4);
	iyearx=(int)ftemp;;
        while((strncmp(l5,cmon_v64[imonx],3)!=0)&&(imonx<12)) { imonx++; }
        juldayx=aodtv64_idmyyd(idatex,imonx+1,iyearx);
        getmonth=TRUE;
      }
      imon=imonx;
      iyear=iyearx;
      if(!strncmp(l6,"KT",2)) {
        if((!strncmp(l1,"INITIAL",7))||(!strncmp(l1a,"VT",2))) {
          ftemp = aodtv64_atoif(l2,1,7);
	  idate= (int)ftemp/10000;
	  itime=((int)ftemp%10000)*100;
          if(idate<idatex) {
            imon++;
            if(imon==12) {
              iyear++;
              imon=0;
            }
            julday=aodtv64_idmyyd(idate,imon+1,iyear);
          } else {
            julday=juldayx+(idate-idatex);
          }
          time[ixx]=aodtv64_calctime(julday,itime);
          lat[ixx]=aodtv64_atoif(l3,1,strlen(l3));
          lon[ixx]=aodtv64_atoif(l4,1,strlen(l4));
          ixx++;
        }
      }
    }
  } else if(ifixtype_v64==2) {
    /* read JTWC forecast file, quit at EOF */
    /* o.k... since JTWC does not put the month and year
       on their bulletins, they will need to be "made up".
       We will assume that the forecast is current and the
       month and year from the image is equal to the month
       and year of the forecast file.  */
    (void)aodtv64_yddmy((int)curtime,&idatex,&imonx,&iyearx);
    while(((fgets(line,maxline,fp))!=NULL)&&(ixx<6)) {
      (void)sscanf(line,"%s %s %s %s %s",l1,l2,l3,l4,l5);
      if(!strncmp(l2,"---",3)) {
        if(ixx==0) {
          ftemp = aodtv64_atoif(l1,1,6);
	  idate=(int)ftemp/10000;
	  itime=((int)ftemp%10000)*100;
          if(idate==idatex) {
            /* dates are the same... probably o.k.... should check times */
            julday=(int)curtime;
          } else if((idatex-idate)<=2) {
            /* this is probably o.k too... forecast date is before image by one day */
            julday=(int)curtime+(idate-idatex);
          } else {
            /* dates are invalid.  Either image date is before forecast or
               is well beyond forecast availability */
            return -2;
          } 
          time[ixx]=aodtv64_calctime(julday,itime);
          lat[ixx]=aodtv64_atoif(l4,1,strlen(l4));
          lon[ixx]=aodtv64_atoif(l5,1,strlen(l5));
        } else {
          ftemp = aodtv64_atoif(l1,1,6);
	  idate=(int)ftemp/10000;
	  itime=((int)ftemp%10000)*100;
          julday=(int)curtime+(idate-idatex);
          time[ixx]=aodtv64_calctime(julday,itime);
          if (time[ixx]<time[ixx-1]) {
            imon=imonx+1;
            iyear=iyearx;
            if(imon==13) {
              iyear=iyearx+1;
              imon=1;
            }
            julday=aodtv64_idmyyd(idate,imon,iyear);
            time[ixx]=aodtv64_calctime(julday,itime);
          }
          lat[ixx]=aodtv64_atoif(l3,1,strlen(l3));
          lon[ixx]=aodtv64_atoif(l4,1,strlen(l4));
        }
      }
      if((!strncmp(l1,"MAX",3))&&(!strncmp(l4,"-",1))) {
        ixx++;
      }
    }
  } else if(ifixtype_v64==3) {
    /* generic forecast file input */
    while(((fgets(line,maxline,fp))!=NULL)&&(ixx<6)) {
      (void)sscanf(line,"%s %s %s %s %s %s",l1,l2,l3,l4,l5,l6);
      ftemp = aodtv64_atoif(l1,1,2);
      idate=(int)ftemp;
      ftemp = aodtv64_atoif(l2,1,2);
      imon=(int)ftemp;
      ftemp = aodtv64_atoif(l3,1,4);
      iyear=(int)ftemp;
      ftemp = aodtv64_atoif(l4,1,4);
      itime=(int)ftemp*100;
      julday=aodtv64_idmyyd(idate,imon,iyear);
      time[ixx]=aodtv64_calctime(julday,itime);
      lat[ixx]=aodtv64_atoif(l5,1,strlen(l5));
      lon[ixx]=aodtv64_atoif(l6,1,strlen(l6));
      ixx++;
    }
  } else if(ifixtype_v64==0) {
    /* ATCF forecast file input */
    while(((fgets(line,maxline,fp))!=NULL)) {
      (void)sscanf(line,"%s %s %s %s %s %s %s %s",l1,l2,l3,l4,l5,l6,l7,l8);
      if(!strncmp(l5,atcftype_v64,4)) {
        ftemp = aodtv64_atoif(l3,1,4);
	iyear=(int)ftemp;
        ftemp = aodtv64_atoif(l3,5,6);
	imon=(int)ftemp;
        ftemp = aodtv64_atoif(l3,7,8);
	idate=(int)ftemp;
        ftemp = aodtv64_atoif(l3,9,10);
	itime=(int)ftemp*10000;
        ftemp = aodtv64_atoif(l6,1,strlen(l6));
	ftime=(int)ftemp*10000;
        julday=aodtv64_idmyyd(idate,imon,iyear);
        ttime=aodtv64_calctime(julday,itime);
        if((ttime<curtime)&&(ftimelast!=ftime)&&(iyy<6)) {
          izz1=(int)(ftime+itime)/240000;
          izz2=((int)(ftime+itime)%240000);
          time[iyy]=aodtv64_calctime(julday+izz1,izz2);
          lat[iyy]=aodtv64_atoif(l7,1,strlen(l7))/10.0;
          lon[iyy]=aodtv64_atoif(l8,1,strlen(l8))/10.0;
          iyy++;
          ftimelast=ftime;
          ixx=iyy;
        }
      } else {
        iyy=0;
      }
    }
  } else if(ifixtype_v64==9) {
    /* best track file input */
    while(((fgets(line,maxline,fp))!=NULL)&&(ixx<6)) {
      if(findyear) {
        (void)sscanf(line,"%s %s",l1,l2);
        ftemp = aodtv64_atoif(l2,1,4);
	iyear=(int)ftemp;
        findyear=FALSE;
      } else {
        (void)sscanf(line,"%s %s %s %s %s %s",l1,l2,l3,l4,l5,l6);
        ftemp = aodtv64_atoif(l1,1,2);
	imon=(int)ftemp;
        ftemp = aodtv64_atoif(l1,4,5);
	idate=(int)ftemp;
        ftemp = aodtv64_atoif(l1,7,8);
	itime=(int)ftemp*10000;
	ftemp = aodtv64_atoif(l1,10,11);
	itime = itime + (int)ftemp*100;
        julday=aodtv64_idmyyd(idate,imon,iyear);
        xtime=aodtv64_calctime(julday,itime);
	if(xtime<curtime) {
          time[0]=xtime;
          lat[0]=aodtv64_atoif(l2,1,strlen(l2));
          lon[0]=aodtv64_atoif(l3,1,strlen(l3));
        } else {
	  if((xtime>=curtime)&&(ixx<4)) { 
            ixx++;
            time[ixx]=xtime;
            lat[ixx]=aodtv64_atoif(l2,1,strlen(l2));
            lon[ixx]=aodtv64_atoif(l3,1,strlen(l3));
	  }
        }
      }
    }
  } else {
    return -1;
  }
  ixx--;

  /* determine number of valid forecast times for given image time */
  iy=0;
  maxfcst=A_MIN(ixx,3);
  while((time[iy]<=curtime)&&(iy<maxfcst)) {
    iy++;
  }

  if((iy==0)||(iy==maxfcst)) {
    /* image before/after forecast time */
    return -2;
  }

  /* initialize forecast info array */
  for(iz=0;iz<5;iz++) {
    fcsttime_v64[iz]=9999999.9;
    fcstlat_v64[iz]=999.0;
    fcstlon_v64[iz]=999.0;
  }

  for(iz=0;iz<maxfcst;iz++) {
    /* load forecast times into return arrays */
    fcsttime_v64[iz]=time[iz];
    fcstlat_v64[iz]=lat[iz];
    fcstlon_v64[iz]=lon[iz];
  }
  *fnum=maxfcst;
  return 0;
}

void aodtv64_pickfinallocation(float lplat1,float lplon1,float lplats1,float lplons1,int lpnum1,
                  float lplat2,float lplon2,float lplats2,float lplons2,int lpnum2,
                  float nhclat,float nhclon,float nhctemp,
                  float splat,float splon,float spscore,int spmethod,
                  float *uselat,float *uselon,float *useconf,int *usethis)
/* Determine method location scheme to use by examining
   various empirically-defined confidence factors.  Laplacian
   analysis, 10^ Log Spiral analysis, and NHC/JTWC forecast
   interpolation analysis confidence factors will be derived,
   with the "most confident" value used as the final automatically
   determined storm position.
    Inputs  : lplat1  - Laplacian latitude position (threshold 1)
              lplon1  - Laplacian longitude position (threshold 1)
              lplats1 - Laplacian latitude values std. dev. value (threshold 1)
              lplats1 - Laplacian longitude values std. dev. value (threshold 1)
              lpnum1  - number of points with Laplacian values less than threshold 1
              lplat2  - Laplacian latitude position (threshold 2)
              lplon2  - Laplacian longitude position (threshold 2)
              lplats2 - Laplacian latitude values std. dev. value (threshold 2)
              lplats2 - Laplacian longitude values std. dev. value (threshold 2)
              lpnum2  - number of points with Laplacian values less than threshold 2
              splat   - Ring/Spiral Analysis latitude position
              splon   - Ring/Spiral Analysis longitude position
              spscore - Ring/Spiral Analysis confidence factor score
              spmethod- Ring/Spiral Analysis position derivation method
              nhclat  - NHC/JTWC interpolated latitude position
              nhclon  - NHC/JTWC interpolated longitude position
              nhctemp - NHC/JTWC interpolated temperature value
    Outputs : uselat  - latitude value to be used
              uselon  - longitude value to be used
              usethis - method utilized in determination of storm postion values
*/
{
  int   warmcat,nhccat,diffcat,ixx,origpos,count;
  int   lastescene,lastcscene;
  float xdist,xangle,cescene,ccscene;
  float lasttno;
  float laplsd1,laplsd2,laplss1,laplss2;
  float conflapl,conf1,conf2,confdist;
  struct odtdata *odthistory;
  double curtime,xtime;
  logical landcheck,foundeye=FALSE;

  curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);
  cescene=odtcurrent_v64->IR.eyescene;
  ccscene=odtcurrent_v64->IR.cloudscene;
  odthistory=odthistoryfirst_v64;
  if(hfile_v64==(char *)NULL) {
     lasttno=8.0; 
  } else if(odthistoryfirst_v64==0) {
     lasttno=osstr_v64;
  } else {
    count=0;
    while(odthistory!=0) {
      xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
      landcheck=TRUE;
      if(((oland_v64)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) landcheck=FALSE;
      if((xtime<curtime)&&(landcheck)) {
        lasttno=odthistory->IR.Tfinal;
        lastescene=odthistory->IR.eyescene;
        lastcscene=odthistory->IR.cloudscene;
        if((lastescene<6)||((lastcscene==1)&&(lastescene==6))) {
          count=count+1;
          /* checking for eye or embedded center scene types */
          if(count==3) foundeye=TRUE;
        } else {
          count=0;
        }
      }
      odthistory=odthistory->nextrec;
    }
    if(((cescene>6)||((ccscene==1)&&(cescene==6)))&&(count==2)) foundeye=TRUE;
  }
  origpos=*usethis;
  if(lasttno>=3.5) {
    /* save original auto positioning method */
    /* current Tfinal is greater than 3.5 */
    /* Determine the confidence factor for Laplacian lat/lon location.
       "1" values are Laplacian values using first Laplacian threshold value
       "2" values are Laplacian values using second Laplacian threshold value */
    if((lplat1>-99.)&&(lplat2>-99.)) {
      aodtv64_distance(lplat1,lplon1,lplat2,lplon2,1,&xdist,&xangle);
      confdist=0.01*A_MAX(0.0,xdist-20);
    } else {
      xdist=-99.0;
      confdist=0.0;
    }
    laplsd1=A_MAX(0.0,A_ABS(lplats1-lplons1)-.15);
    laplss1=A_MAX(0.0,(lplats1+lplons1)-.5);
    conf1=(5.0*laplsd1)+(2.5*laplss1)+((.1*A_MAX(0,(20-lpnum1))));
    laplsd2=A_MAX(0.0,A_ABS(lplats2-lplons2)-.15);
    laplss2=A_MAX(0.0,(lplats2+lplons2)-.5);
    conf2=(5.0*laplsd2)+(2.5*laplss2)+((.1*A_MAX(0,(5-lpnum2))));
    conflapl=1.00-conf1-conf2-confdist;

    /* compute cloud category */
    for(ixx=0;ixx<9;ixx++) {
      if((nhctemp<=ebd_v64[ixx])&&(nhctemp>ebd_v64[ixx+1])) {
        nhccat=ixx;
      }
      if((odtcurrent_v64->IR.warmt<=ebd_v64[ixx])&&(odtcurrent_v64->IR.warmt>ebd_v64[ixx+1])) {
        warmcat=ixx;
      }
    }

    /* printf("spscore=%f  conflapl=%f\n",spscore,conflapl); */

    /* if lasttno 3.5 <= X < 4.5 we will only use the laplacian to determine the center point unless
       we have found three or more consecutive eyes and/or embcenters, in which case we will use both
       the laplacian technique and spiral fitting routines for selecting the storm center */
    if((lasttno<=4.5)&&(!foundeye)) spscore=-99.0;

    /* compare laplacian score with Spiral/Ring Analysis confidence factor/score */
    if(spscore>conflapl) {
      /* use Spiral/Ring methodology for center point */
      *uselat=splat;
      *uselon=splon;
      *useconf=spscore;
      *usethis=spmethod;
    } else {
      /* try using Laplacian Analysis techniques to find a center */
      diffcat=nhccat-warmcat;
      if(conflapl>=0.80) {
        /* EYES */
        /* we are very confident in Laplacian Analysis has found an eye, use laplacian location */
        *usethis=2;
        *uselat=(lplat1+lplat2)/2.0;
        *uselon=(lplon1+lplon2)/2.0;
        *useconf=conflapl;
        /* perform gross error check to make sure we are not just grabbing the edge of a storm 
         * for the laplacian analysis */
        aodtv64_distance(*uselat,*uselon,odtcurrent_v64->IR.warmlatitude,odtcurrent_v64->IR.warmlongitude,1,&xdist,&xangle);
        if(xdist>60.0) {
          *usethis=origpos;
          *uselat=nhclat;
          *uselon=nhclon;
          *useconf=0.0;
        }
      } else if(conflapl>=0.50) {
        /* WARM SPOTS... maybe */
        if((conf2<=0.05)&&(lpnum2>=10)) {
          *usethis=2;
          *uselat=lplat2;
          *uselon=lplon2;
          *useconf=conflapl;
        } else if((conf1<=0.05)&&(lpnum1>=40)) {
          *usethis=2;
          *uselat=lplat1;
          *uselon=lplon1;
          *useconf=conflapl;
        } else {
          *usethis=origpos;
          *uselat=nhclat;
          *uselon=nhclon;
          *useconf=0.0;
        }
      } else if(conflapl>0.00) {
        /* broader, but semi-organized centers */
        /* we are somewhat confident in Laplacian Analysis has found a forming eye, check additional criteria */
        aodtv64_distance(nhclat,nhclon,odtcurrent_v64->IR.warmlatitude,odtcurrent_v64->IR.warmlongitude,1,&xdist,&xangle);
        if((xdist<=40)&&((warmcat<=1)||(diffcat>=2))) {
          *usethis=3;
          *uselat=odtcurrent_v64->IR.warmlatitude;
          *uselon=odtcurrent_v64->IR.warmlongitude;
          *useconf=0.1;
        } else {
          *usethis=origpos;
          *uselat=nhclat;
          *uselon=nhclon;
          *useconf=0.0;
        }
      } else {
        /* CDO... can't find anything to focus on */
        *usethis=origpos;
        *uselat=nhclat;
        *uselon=nhclon;
        *useconf=0.0;
      }
    }
  } else {
    /* current Tfinal is less than 3.5 or current scene is not an eye or embedded center
     * WILL USE FORECAST POSITION FOR AODT ANALYSIS */
    *usethis=origpos;
    *uselat=nhclat;
    *uselon=nhclon;
    *useconf=0.0;
  }

}

/*==================================================================*/

void aodtv64_logspiral(float inlat, float inlon, float searchtemp,
			int searchtype, int *bestspiral,
			float *bestlat, float *bestlon)
/* Determine storm location using 10^ Log-spiral analysis.
   Algorithm will attempt to match the spiral with the image
   pixels at or below the threshold temperature based on 
   BD-enhancement curve values 
    Inputs  : inlat      - center latitude of analysis grid
              inlon      - center longitude of analysis grid
              searchtemp - temperature threshold value
              searchtype - 1=search at single point;2=search over 2^box
    Outputs : bestlat    - best latitude location from analysis
              bestlon    - best longitude location from analysis
              bestspiral - number of consecutive arcs through which spiral passes
*/
{
  int ixx,iyy,izz,iskip,rotfac,theta;
  int imaxx,iminx,imaxy,iminy,ycount;
  int spiralconsec,maxconsec,spiralbest,spiralbestrot,bestrot;
  float xrad=57.29578,A=25.0,B=10.0/xrad;
  float maxx,minx,maxy,miny,xmindist;
  float glat,glon,xlat,xlon,xdist,xangle;
  float ylatdiff,ylondiff;
  float thetax,r,thetaval,thetaval2;
  float *zlat,*zlon;
  int np,ipt;
  size_t sofflt;

/*---------------------------------------------------------------------*/

  /* allocate memory */
  sofflt=sizeof(float);
  zlat=(float *)calloc((size_t)bufsiz, sofflt);
  zlon=(float *)calloc((size_t)bufsiz, sofflt);
  if(searchtype==2) {
    /* search over 2.0 degree box */
    maxx=inlat+1.0;
    minx=inlat-1.0;
    maxy=inlon+1.0;
    miny=inlon-1.0;
    imaxx=(int)(maxx*100.0);
    iminx=(int)(minx*100.0);
    imaxy=(int)(maxy*100.0);
    iminy=(int)(miny*100.0);
  } else {
    /* search at a single point */
    imaxx=(int)(inlat*100.0);
    iminx=(int)(inlat*100.0);
    imaxy=(int)(inlon*100.0);
    iminy=(int)(inlon*100.0);
  }

  *bestspiral=0;

  /* initialize arrays */
  np=0;
  for(ixx=0;ixx<areadata_v64->numx;ixx++) {
    for(iyy=0;iyy<areadata_v64->numy;iyy++) {
      if(areadata_v64->temp[iyy][ixx]<=searchtemp) {
        zlat[np]=areadata_v64->lat[iyy][ixx];
        zlon[np]=areadata_v64->lon[iyy][ixx];
        np++;
      }
    }
  }

  bestrot=0;
  *bestlat=0.0F;
  *bestlon=0.0F;
  
/* loop through x-axis/elements of analysis grid box */
  for(ixx=iminx;ixx<=imaxx;ixx=ixx+20) {
    xlat=(float)ixx/100.0;
    /* loop through y-axis/lines of analysis grid box */
    for(iyy=iminy;iyy<=imaxy;iyy=iyy+20) {
      xlon=(float)iyy/100.0;
      iskip=0;
      /* determine distance from each point in box to current location */
      if(searchtype==2) {
        xmindist=12.0;
        for(izz=0;izz<np;izz++) {
          aodtv64_distance(xlat,xlon,zlat[izz],zlon[izz],1,&xdist,&xangle);
          if(xdist<=xmindist) { 
            /* if the lat/lon point is too close to cold cloud tops, do
               not calculate log spiral at this point.  Trying to eliminate
               "false" arc locations by forcing the system to use some
               of the arc points on the spiral away from the start of
               the spiral (were getting "false echos" without this". */
            iskip=1;
            break;
          }
        }
      }

      spiralbest=0;
      spiralbestrot=0;
      /* if arc location passes analysis above, proceed with placement of spiral */
      if(iskip==0) { 
        /* rotate the arc spiral thru entire revolution at 30 degree intervals */
        for(rotfac=0;rotfac<=330;rotfac=rotfac+30) {
          spiralconsec=0;
          maxconsec=0;

          /* calculate position of each point on spiral from 0 to 540^ */
          for(theta=0;theta<=540;theta=theta+15) {
            thetax=(float)theta/xrad;
            r=A*A_EXP((B*thetax));
            thetaval=(float)theta+(float)rotfac;
            if(xlat<0.0) thetaval=(float)(-1*theta)+(float)rotfac;
            thetaval2=thetaval+180.0;
            aodtv64_distance2(xlat,xlon,r,thetaval2,&glat,&glon);
            ycount=0;
            for(izz=0;izz<np;izz++) {
              ylatdiff=A_ABS(glat-zlat[izz]);
              ylondiff=A_ABS(glon-zlon[izz]);
              /* if a point is within 0.1^ latitude/longitude determine distance */
              if((ylatdiff<=0.1)&&(ylondiff<=0.1)) {
                aodtv64_distance(glat,glon,zlat[izz],zlon[izz],1,&xdist,&xangle);
                /* if distance from spiral point is within 6km from an accepted
                   temperature threshold point, count it */
                if(xdist<=6.0) ycount++;
              }
            }
            /* if there are 4 or more threshold points associated with each 
               spiral point, count within consecutive spiral point counter */
            if(ycount>=4) {
              spiralconsec++;
              /* save spiral that has the maximum consecutive spiral counts
                 for each rotation though 360^ at each center location */
              if(spiralconsec>maxconsec) maxconsec=spiralconsec;
            } else {
              spiralconsec=0;
            }
            /* if this spiral has the greatest number of consecutive spiral
               points, save the location and number of points */
            if(maxconsec>spiralbest) {
              spiralbest=maxconsec;
              spiralbestrot=rotfac;
            }
          }
        } /* rotfac loop */
        if(spiralbest>*bestspiral) {
          *bestspiral=spiralbest;
          *bestlat=xlat;
          *bestlon=xlon;
          bestrot=spiralbestrot;
        }
      } /* iskip if */
    } /* iyy loop */
  } /* ixx loop */

  /* free memory */
  free(zlon);
  free(zlat);

  /* load array for best spiral band */
  ipt=0;
  for(theta=0;theta<=540;theta=theta+15) {
    thetax=(float)theta/xrad;
    r=A*A_EXP((B*thetax));
    thetaval=(float)theta+(float)bestrot;
    if(xlat<0.0) thetaval=(float)(-1*theta)+(float)rotfac;
    thetaval2=thetaval+180.0;
    aodtv64_distance2(*bestlat,*bestlon,r,thetaval2,&glat,&glon);
    /* load array for external plotting of spiral band */
    spiralband_v64[0][ipt]=glat;
    spiralband_v64[1][ipt]=glon;
    ipt++;
  }
}

/*=======================================================================*/

void aodtv64_laplacian(float **data2)
/* Compute Laplacian values for scene.
   Derived from program laplacian.c by John Gauch and Edu Metz
   of the University of Kansas (1994).
    Inputs  : none 
    Outputs : data2 - array containing Laplacian values for each point
*/
{
  int x,y;
  float dxx,dyy;

  for(y=1;y<areadata_v64->numy-1;y++) {
    for(x=1;x<areadata_v64->numx-1;x++) {
/*  dx and dy not used 
      dx = (areadata_v64->temp[y+1][x+1] + (2.0*areadata_v64->temp[y][x+1]) +
            areadata_v64->temp[y-1][x+1] - areadata_v64->temp[y+1][x-1] +
            (2.0*areadata_v64->temp[y][x-1]) - areadata_v64->temp[y-1][x-1])/8.0;
      dy = (areadata_v64->temp[y+1][x+1] + (2.0*areadata_v64->temp[y+1][x]) +
            areadata_v64->temp[y+1][x-1] - areadata_v64->temp[y-1][x+1] +
            (2.0*areadata_v64->temp[y-1][x]) - areadata_v64->temp[y-1][x-1])/8.0;
*/
      dxx = (areadata_v64->temp[y+1][x+1] + (4.0*areadata_v64->temp[y][x+1]) +
             areadata_v64->temp[y-1][x+1] - (2.0*areadata_v64->temp[y+1][x]) -
             (8.0*areadata_v64->temp[y][x]) - (2.0*areadata_v64->temp[y-1][x]) +
             areadata_v64->temp[y+1][x-1] + (4.0*areadata_v64->temp[y][x-1]) +
             areadata_v64->temp[y-1][x-1])/6.0;
      dyy = (areadata_v64->temp[y+1][x+1] - (2.0*areadata_v64->temp[y][x+1]) +
             areadata_v64->temp[y-1][x+1] + (4.0*areadata_v64->temp[y+1][x]) -
             (8.0*areadata_v64->temp[y][x]) + (4.0*areadata_v64->temp[y-1][x]) +
             areadata_v64->temp[y+1][x-1] - (2.0*areadata_v64->temp[y][x-1]) +
             areadata_v64->temp[y-1][x-1])/6.0;
      /* calculate directional derivative */
      /* data3[y][x]=(int)(A_SQRT((dx*dx)+(dy*dy))); */
      data2[y][x]=dxx+dyy;
    }
  }

  /* handle boundary rows and columns */
  for(x=0;x<areadata_v64->numx;x++) {
    data2[0][x]=data2[1][x];
    data2[areadata_v64->numy-1][x]=data2[areadata_v64->numx-2][x];
  }
  for(y=0;y<areadata_v64->numy;y++) {
    data2[y][0]=data2[y][1];
    data2[y][areadata_v64->numx-1]=data2[y][areadata_v64->numx-2];
  }

}

int aodtv64_polint(double xa[],float ya[],int n,double x,float *y,float *dy)
/* Polynomial interpolation scheme program derived from FORTRAN 
   program POLINT in : Numerical Recipies - The Art of Scientific 
   Computing, 1986, Press, Flannery, Teukolsky, and Vetterling, Cambridge Press
*/
{
  float c[5],d[5],dif,dift,hp,ho,w,den,dyx,yx;
  int   ns,i,m;

  ns=1;
  dif=A_ABS(x-xa[0]);
  for(i=1;i<=n;i++) {
    dift=A_ABS(x-xa[i-1]);
    if(dift<dif) {
      ns=i;
      dif=dift;
    }
    c[i-1]=ya[i-1];
    d[i-1]=ya[i-1];
  }
  yx=ya[ns-1];
  ns--;
  for(m=1;m<=n-1;m++) {
    for(i=1;i<=n-m;i++) {
      ho=xa[i-1]-x;
      hp=xa[(i-1)+m]-x;
      w=c[i]-d[i-1];
      den=ho-hp;
      if(den==0.0) return -1;
      den=w/den;
      d[i-1]=hp*den;
      c[i-1]=ho*den;
    }
    if((2*ns)<(n-m)) {
      dyx=c[ns];
    } else {
      dyx=d[ns-1];
      ns--;
    }
    yx=yx+dyx;
  }
  *dy=dyx;
  *y=yx;

  return 0;
}

/*===================================================================*/

int aodtv64_spiralCenterLowRes ( float nhcLat, float nhcLon,
				float *spiralLatCenter, float *spiralLonCenter,
				float *spiralScore, float **spGrid)
/*************************************************************************
 * Perform Tony Wimmers' Spiral Analysis.  Analysis is a 5-degree Log
 * Spiral (not sure why he does not use a 10-degree method.
   Inputs  : nhcLat          - First Guess latitude position (from interpolated forecast)
             nhcLon          - First Guess longitude position (from interpolated forecast)
   Outputs : spiralLatCenter - Spiral Analysis latitude position at analysis grid maxiumum score value
             spiralLonCenter - Spiral Analysis longitude position at array/grid maxiumum score value
             spiralScore     - Spiral Analysis maxiumum score value
             spGrid          - Array containting Spiral Analysis grid field
**									*
* Log:									*
* T. Piper/SAIC		05/06	Properly allocate two-dimensional arrays*
* T. Piper/SAIC		05/06	Properly free gradE and gradN		*
************************************************************************/
{
  float pi=3.141592;
  float alpha=5.0*pi/180.0;
  float outSideFactor=0.62;

  int   ixx,iyy,yx=1;
  int   numPoints;
  float xOff,yOff,asign,lonMult;
  float inFilterDisc,gradOrigMag,gradLogMag,gradLogReduction,denom;
  float maxCenterMeanCross,allCenterMeanCross,maxCenterMeanCrossX,maxCenterMeanCrossY;
  float proxyX,proxyY,spiralX,spiralY,rawCrossScore,crossScoreClean,crossScoreSum,searchRad,searchRadMax;
  float latInc,lonInc;
  float **gradN,**gradE;
  float filterRadiusDeg=4.5;
  float searchRadiusDeg,spacingDeg;
  struct datagrid *areadataNC;
  size_t sofflt, soffpr;

/*---------------------------------------------------------------------*/

  inFilterDisc=A_POW(filterRadiusDeg,2);
  searchRadiusDeg=1.20;
  searchRadiusDeg=1.75;
  spacingDeg=0.1;
  inFilterDisc=A_POW(searchRadiusDeg+(2.0*spacingDeg),2);

  asign=A_ABS(nhcLat)/nhcLat;

  /* define data grid around first guess */
  areadataNC=(struct datagrid *)malloc(sizeof(struct datagrid));
  for(iyy=0;iyy<areadataRM->numy;iyy++) {
    for(ixx=0;ixx<areadataRM->numx;ixx++) {
      /* compute normalized coordinate system arrays */
      areadataNC->lon[iyy][ixx]=(areadataRM->lon[iyy][ixx]-nhcLon)*(A_COS(pi*nhcLat/180.0));
      areadataNC->lat[iyy][ixx]=areadataRM->lat[iyy][ixx]-nhcLat;
      areadataNC->temp[iyy][ixx]=areadataRM->temp[iyy][ixx];
    }
  }
  areadataNC->numx=areadataRM->numx;
  areadataNC->numy=areadataRM->numy;

  /* determine lat/lon grid increment */
  lonInc=A_ABS(areadataNC->lon[0][0]-areadataNC->lon[0][1]);    /* W to E gradient */
  latInc=A_ABS(areadataNC->lat[0][0]-areadataNC->lat[1][0]);    /* N to S gradient */
  /* This is to determine longitude multiplier factor... original routines were developed 
     using negative Western Hemisphere, but McIDAS is positive in WH.  So if lonMult is negative,
     we know (assume) we are using non-McIDAS input imagery/values, otherwise make lonMult positive.
     This all assumes that image is loaded from NW to SE */
  lonMult=areadataNC->lon[0][0]-areadataNC->lon[0][1];
  lonMult=(lonMult<0.0) ? 1.0 : -1.0;
  /* printf("latInc=%f  lonInc=%f  lonMult=%f\n",latInc,lonInc,lonMult); */

  /* allocate memory */
  sofflt=sizeof(float);
  soffpr=sizeof(float*);
  gradN=(float **)calloc((size_t)maxd,soffpr);
  gradE=(float **)calloc((size_t)maxd,soffpr);
  for(iyy=0;iyy<maxd;iyy++) {
    gradN[iyy]=(float *)calloc((size_t)maxd,sofflt);
    gradE[iyy]=(float *)calloc((size_t)maxd,sofflt);
  }

  /* calculate gradient field */
  aodtv64_gradient(areadataNC->temp,areadataNC->numx,areadataNC->numy,lonInc,latInc,gradN,gradE);
    for(ixx=0;ixx<areadataNC->numx;ixx++) {
  for(iyy=0;iyy<areadataNC->numy;iyy++) {
      /* printf("iyy=%d ixx=%d  lat=%f lon=%f  lat=%f lon=%f  gradN=%f gradY=%f\n",iyy,ixx,areadataRM->lat[iyy][ixx],areadataRM->lon[iyy][ixx],areadataNC->lat[iyy][ixx],areadataNC->lon[iyy][ixx],gradN[iyy][ixx],gradE[iyy][ixx]); */
      gradOrigMag=A_SQRT(A_POW(gradN[iyy][ixx],2)+A_POW(gradE[iyy][ixx],2));
      gradLogMag=A_LOG(1.0+gradOrigMag);
      if(gradLogMag==0.0) {
        gradLogReduction=0.0;
      } else {
        gradLogReduction=gradLogMag/gradOrigMag;
      }
      gradN[iyy][ixx]=gradLogReduction*gradN[iyy][ixx];
      gradE[iyy][ixx]=gradLogReduction*gradE[iyy][ixx];
    }
  }

  /* calculate cross product score at each grid point */
  /* ixx/iyy are "starting point" coordinates */
  maxCenterMeanCross=-99.0;
  searchRadMax=A_POW(searchRadiusDeg+((2.0*spacingDeg)/3.0),2);
  for(xOff=-searchRadiusDeg;xOff<searchRadiusDeg;xOff=xOff+spacingDeg) {
    for(yOff=-searchRadiusDeg;yOff<searchRadiusDeg;yOff=yOff+spacingDeg) {
      /* xOff/yOff are offset coordinates from "starting point" */
      searchRad=A_POW(xOff,2)+A_POW(yOff,2);
      if(searchRad<=searchRadMax) {
        crossScoreSum=0.0;
	numPoints=0;
        for(iyy=1;iyy<areadataNC->numy-1;iyy++) {
          for(ixx=1;ixx<areadataNC->numx-1;ixx++) {
            searchRad=A_POW(areadataNC->lon[iyy][ixx],2)+A_POW(areadataNC->lat[iyy][ixx],2);
            if(searchRad<inFilterDisc) {
              proxyX=lonMult*areadataNC->lon[iyy][ixx]-xOff;
              proxyY=areadataNC->lat[iyy][ixx]-yOff;
              denom=A_SQRT((1.0+A_POW(alpha,2))*(A_POW(proxyX,2)+A_POW(proxyY,2)));
              spiralX=(alpha*proxyX+(asign*proxyY))/denom;
              spiralY=(alpha*proxyY-(asign*proxyX))/denom;
              rawCrossScore=(spiralX*gradN[iyy][ixx])-(spiralY*gradE[iyy][ixx]);
              crossScoreClean=A_MAX(0.0,-rawCrossScore)+(outSideFactor*A_MAX(0.0,rawCrossScore));
	      crossScoreSum=crossScoreSum+crossScoreClean;
   /* printf("lat=%f lon=%f  xoff=%f yoff=%f  proxyX=%f proxyY=%f  spiralX=%f spiralY=%f  rawScore=%f clean=%f\n",areadataRM->lat[iyy][ixx],areadataRM->lon[iyy][ixx],xOff,yOff,proxyX,proxyY,spiralX,spiralY,rawCrossScore,crossScoreClean); */
	      numPoints++; 
            }
          }
        }
        allCenterMeanCross=crossScoreSum/(float)numPoints;   /* calculate mean of all values in crossScore array */
	/* store location of maximum score position */
	if(allCenterMeanCross>maxCenterMeanCross) {
          maxCenterMeanCross=allCenterMeanCross;
          maxCenterMeanCrossY=yOff;
          maxCenterMeanCrossX=xOff;
        }
        spGrid[yx][0]=allCenterMeanCross;
        spGrid[yx][1]=(float)yOff;
        spGrid[yx][2]=(float)xOff;
        spGrid[0][0]=(float)yx;
        yx++;
      }
    }
  }
  spGrid[0][1]=0.0;
  spGrid[0][2]=0.0;

  /* free memory */
  free(areadataNC);
  for(iyy=0;iyy<maxd;iyy++) {
    free(gradN[iyy]);
    free(gradE[iyy]);
  }
  free(gradN);
  free(gradE);

  /* determine lat/lon point from x/y coordinates */
  *spiralLatCenter=maxCenterMeanCrossY+nhcLat;
  *spiralLonCenter=((lonMult*maxCenterMeanCrossX)/(A_COS(pi*nhcLat/180.0)))+nhcLon;
  *spiralScore=maxCenterMeanCross;
  for(yx=1;yx<=(int)spGrid[0][0];yx++) {
    spGrid[yx][1]=spGrid[yx][1]+nhcLat;
    spGrid[yx][2]=((lonMult*spGrid[yx][2])/(A_COS(pi*nhcLat/180.0)))+nhcLon;
  }

  return 0;
}

/*===========================================================================*/

void aodtv64_gradient(float tG[][maxd],int neles,int nlines,float lonInc,float latInc,float **gradN,float **gradE)
/* Calculate gradient field of input temperature value array.
   Inputs  : tG     - Temperature array
             neles  - Number of elements in Temperature array
             nlines - Number of lines in Temperature array
             lonInc - Longitudinal increment (elements)
             latInc - Latitudinal increment (lines)
   Outputs : gradN  - Gradient field of temperatures in N-S direction (along elements or Latitudinally)
             gradE  - Gradient field of temperatures in E-W direction (along lines or Longitudunally)
*/
{
  int   ixx,iyy;

  /* initialize arrays */
  for(iyy=0;iyy<nlines;iyy++) {
    for(ixx=0;ixx<neles;ixx++) {
      gradN[iyy][ixx]=0.0;
      gradE[iyy][ixx]=0.0;
    }
  }
  for(iyy=1;iyy<nlines-1;iyy++) {
    for(ixx=1;ixx<neles-1;ixx++) {
      /* determine N-S gradient at point */
      gradN[iyy][ixx]=(tG[iyy-1][ixx]-tG[iyy+1][ixx])/(2.0*latInc);
      /* determine W-E gradient at point */
      /* gradE[iyy][ixx]=(tG[iyy][ixx-1]-tG[iyy][ixx+1])/(2.0*lonInc);  */
      /* determine E-W gradient at point */
      gradE[iyy][ixx]=(tG[iyy][ixx+1]-tG[iyy][ixx-1])/(2.0*lonInc);   
    }
  }
}

/*========================================================================*/

int aodtv64_ringFit(float latFirstGuess,float lonFirstGuess,
                 float *latRing1,float *lonRing1,int *maxRing1,float **ringScores)
/* Perform Tony Wimmers' Ring Analysis.  
   Inputs  : nhcLat     - First Guess latitude position (from interpolated forecast)
             nhcLon     - First Guess longitude position (from interpolated forecast)
   Outputs : latRing1   - Ring Analysis latitude position at analysis grid maxiumum score value
             lonRing1   - Ring Analysis longitude position at array/grid maxiumum score value
             maxRing1   - Ring Analysis maxiumum score value
             ringScores - Array containting Ring Analysis grid field
**									*
* Log:									*
* T. Piper/SAIC		05/06	Properly allocate two-dimensional arrays*
* T. Piper/SAIC		05/06	Properly free gx, gy, filtRows and	*
*								filtCols*
************************************************************************/
{

  float  **gx,**gy;
  float  dotScoreMax,dotScoreFinal;
  float  degPerPix,dotScore,inDisk,finalLat,finalLon;
  float  dotScoreSum,dotProductI,dotProductJ,dotProducts,asign;
  float  latInc,lonInc;
  float  xlatx,xlonx,NanAdj;
  float  ringSearchRadiusDeg=0.75;   /* original was 1.0 */
  float  minRadiusDeg=0.06;
  float  maxRadiusDeg=0.40;
  int    maxRad,locI,locJ;
  int    iFirstGuess,jFirstGuess;
  int    ixx,izz,iyy,circleFilterI,circleFilterJ,radi,ipts,numNans=0;
  int    searchRadiusPix,minRadiusPix,maxRadiusPix,numCirclePts;
  int    **filtRows,**filtCols;
  size_t sofint, sofipr, sofflt, soffpr;

/*---------------------------------------------------------------------*/

  *latRing1=0.0;
  *lonRing1=0.0;

  /* derive values */
  degPerPix=A_ABS(areadataRM->lat[0][0]-areadataRM->lat[1][0]);
  searchRadiusPix=(int)A_ROUND(ringSearchRadiusDeg/degPerPix);
  minRadiusPix=A_MAX(2,A_ROUND(minRadiusDeg/degPerPix));
  maxRadiusPix=A_ROUND(maxRadiusDeg/degPerPix);

  lonInc=1.0; 
  /* latInc would be 1.0, but Tony does not flip this gradient, unlike with spiral routine */
  latInc=-1.0;

  /* allocate memory */
  sofint=sizeof(int);
  sofipr=sizeof(int*);
  filtRows=(int **)calloc((size_t)50,sofipr);
  filtCols=(int **)calloc((size_t)50,sofipr);
  for(iyy=0;iyy<50;iyy++) {
    filtRows[iyy]=(int *)calloc((size_t)maxd,sofint);
    filtCols[iyy]=(int *)calloc((size_t)maxd,sofint);
  }
  sofflt=sizeof(float);
  soffpr=sizeof(float*);
  gx=(float **)calloc((size_t)maxd,soffpr);
  gy=(float **)calloc((size_t)maxd,soffpr);
  for(iyy=0;iyy<maxd;iyy++) {
    gx[iyy]=(float *)calloc((size_t)maxd,sofflt);
    gy[iyy]=(float *)calloc((size_t)maxd,sofflt);
  }

  /* calculate gradient field */
  aodtv64_gradient(areadataRM->temp,areadataRM->numx,areadataRM->numy,lonInc,latInc,gy,gx);

  /* make matricies of row and column numbers */
  aodtv64_lalo2indsFloat(latFirstGuess,lonFirstGuess,areadataRM->lat,areadataRM->lon,areadataRM->numx,areadataRM->numy,&iFirstGuess,&jFirstGuess);

  /* initialize circle/ring filter arrays */
  for(radi=0;radi<50;radi++) {  /* radius in pixels */
    for(ipts=0;ipts<maxd;ipts++) {   /* number of points on circle at radius */
      filtRows[radi][ipts]=0;
      filtCols[radi][ipts]=0;
    }
  }

  /* determine digital pixel coordinates for ring analysis for different radii sizes */
  for(radi=minRadiusPix;radi<=maxRadiusPix;radi++) {   /* this should be less than 100 total radius pixels, I hope */
    aodtv64_circleFilt(radi,filtRows,filtCols);
  }

  /* search image box */
  dotScoreMax=-99999.0;
  izz=1;
  /* develop the accumulator */
  for(radi=minRadiusPix;radi<maxRadiusPix;radi++) { 
    numCirclePts=filtRows[radi][0];
    /* determine each main point in analysis disc */
    for(ixx=0;ixx<areadataRM->numx;ixx++) {
      for(iyy=0;iyy<areadataRM->numy;iyy++) {
        inDisk=((ixx-iFirstGuess)*(ixx-iFirstGuess))+((iyy-jFirstGuess)*(iyy-jFirstGuess));
        if(inDisk<=(searchRadiusPix*searchRadiusPix)) {
          /* if main point (iyy,ixx) is in disc, calculate dotproduct for each subpoint on ring around main point */
          dotScoreSum=0.0;
          numNans=0;
          for(ipts=1;ipts<=numCirclePts;ipts++) {
            circleFilterI=ixx+filtCols[radi][ipts];
            circleFilterJ=iyy+filtRows[radi][ipts];
            if((circleFilterI<0)||(circleFilterJ<0)||(circleFilterI>=areadataRM->numx)||(circleFilterJ>=areadataRM->numy)) {
              dotProductI=-999.9;
              dotProductJ=-999.9;
            } else {
              dotProductI=((float)filtRows[radi][ipts]/(float)radi)*gy[circleFilterJ][circleFilterI];
              /* printf(" | %d %d j=%d i=%d     j=%d i=%d  gy=%f gx=%f | \n",ipts,radi,filtRows[radi][ipts],filtCols[radi][ipts],
                 circleFilterJ,circleFilterI,gy[circleFilterJ][circleFilterI],gx[circleFilterJ][circleFilterI]); */
              dotProductJ=((float)filtCols[radi][ipts]/(float)radi)*gx[circleFilterJ][circleFilterI];
            }
            if((dotProductI<-999.0)||(dotProductJ<-999.0)) {
              numNans++;
            } else {
              dotProducts=dotProductI+dotProductJ;
              if(dotProducts==0.0) {
                asign=0.0;
              } else {
                asign=A_ABS(dotProducts)/dotProducts;  /* return -1/+1 for -/+ value */
                }
              dotScore=asign*(A_LOG(1.0+A_ABS(dotProducts)));
              dotScoreSum=dotScoreSum+dotScore;
            }
          } /* if indisk */
          /* check for missing data and adjust dotScoreFinal accordingly */
          if(((float)numNans)>(0.575*(float)numCirclePts)) {
            dotScoreFinal=0.0;
          } else {
            NanAdj=(float)numCirclePts/(float)(numCirclePts-numNans);
            dotScoreFinal=-NanAdj*dotScoreSum/A_SQRT((float)numCirclePts);
          }
          if(dotScoreFinal>dotScoreMax) {
            dotScoreMax=dotScoreFinal;
            locI=ixx;
            locJ=iyy;
            maxRad=radi;
          }
          ringScores[izz][0]=dotScoreFinal;
          xlatx=areadataRM->lat[iyy][ixx];
          xlonx=areadataRM->lon[iyy][ixx];
          ringScores[izz][1]=xlatx;
          ringScores[izz][2]=xlonx;
          ringScores[0][0]=(float)izz;
          izz++;
          /* printf("ixx=%d iyy=%d lat=%f lon=%f radi=%d  dotScoreFinal=%f  dotScoreMax=%f \n",ixx,iyy,xlatx,xlonx,radi,dotScoreFinal,dotScoreMax); */
        } /* ixx */
      } /* iyy */
    } /* radi */
  }

  /* free memory */
  for(iyy=0;iyy<maxd;iyy++) {
    free(gx[iyy]);
    free(gy[iyy]);
  }
  free(gx);
  free(gy);
  for(iyy=0;iyy<50;iyy++) {
    free(filtRows[iyy]);
    free(filtCols[iyy]);
  }
  free(filtRows);
  free(filtCols);
  ringScores[0][1]=0.0;
  ringScores[0][2]=0.0;

  /* make matricies of row and column numbers */
  
  aodtv64_inds2laloFloat(locI,locJ,areadataRM->lat,areadataRM->lon,areadataRM->numx,areadataRM->numy,&finalLat,&finalLon);
  *latRing1=finalLat;
  *lonRing1=finalLon;
  *maxRing1=maxRad;

  return 0;
}

/*=====================================================================*/

int aodtv64_lalo2indsFloat(float lat, float lon, float latGrid[][maxd],
				 float lonGrid[][maxd], int neles,
				 int nlines, int *i, int *j)
/* Determine array index given latitude/longitude position 
   Inputs  : lat     - latitude position
             lon     - longitude position
             latGrid - latitude position grid at i/j points
             lonGrid - longitude position grid at i/j points
             neles   - number of elements in lat/lonGrid
             nlines  - number of lines in lat/lonGrid
   Outputs : i       - i (x-axis) position
             j       - j (y-axis) position
*/
{
  float  latLo,latHi,lonLo,lonHi;

  latLo=latGrid[nlines-1][0];
  latHi=latGrid[0][0];
  lonLo=lonGrid[0][neles-1];
  lonHi=lonGrid[0][0];

  *j=(int)(((float)(nlines-1.0)/(latHi-latLo))*(latHi-lat));
  *i=(int)(((float)(neles-1.0)/(lonHi-lonLo))*(lonHi-lon));

  return 0;
}

int aodtv64_inds2laloFloat(int i,int j,float latGrid[][maxd],float lonGrid[][maxd],int neles,int nlines,float *lat,float *lon)
/* Determine latitude/longitude position from array index 
   Inputs  : i       - i (x-axis) position
             j       - j (y-axis) position
             latGrid - latitude position grid at i/j points
             lonGrid - longitude position grid at i/j points
             neles   - number of elements in lat/lonGrid
             nlines  - number of lines in lat/lonGrid
   Outputs : lat     - latitude position
             lon     - longitude position
*/
{
  float  latLo,latHi,lonLo,lonHi;
  
  latLo=latGrid[nlines-1][0];
  latHi=latGrid[0][0];
  lonLo=lonGrid[0][neles-1];
  lonHi=lonGrid[0][0];

  *lon=lonHi-(((float)(i)/((float)neles-1.0))*(lonHi-lonLo));
  *lat=latHi-(((float)(j)/((float)nlines-1.0))*(latHi-latLo));

  return 0;
}

int aodtv64_circleFilt(int radius,int **fR,int **fC)
/* Determine index positions from i/j centerpoint on array for given radius
   Inputs  : radius - radius of ring to be determined (in number of pixels)
   Outputs : fR     - ring position (in row/y-axis direction)
             fC     - ring position (in column/x-axis direction)
   Note    : Array position [#][0] will be number of points on given radius (#=radius size)
*/
{
  int   ixx,iyy,icnt,radp1;
  float diff,inThresh;

  icnt=1;
  inThresh=0.5*(float)(((radius+1)*(radius+1))-(radius*radius));
  radp1=radius+1;
  for(ixx=-radp1;ixx<=radp1;ixx++) {
    for(iyy=-radp1;iyy<=radp1;iyy++) {
      diff=(float)((iyy*iyy)+(ixx*ixx)-(radius*radius));
      if(A_ABS(diff)<=inThresh) {
        fR[radius][icnt]=iyy;
        fC[radius][icnt]=ixx;
	icnt++;
      }
    }
  }
  /* number of points on given radius size */
  fR[radius][0]=icnt-1;
  fC[radius][0]=icnt-1;

  return 0;
}

/* the following routines were originally developed by Dave Santek of UW/SSEC and
   were added to the AODT under permission.
   If executed, an array of latitude and longitude position arrays will be remapped
   to a rectilinear projection for Tony Wimmers routines
*/
int aodtv64_remapdata(void)
{

	/* Calls routines to setup transformation, transform, data move */
	
	int rc;				/* Return code */
	int nc;				/* Number of corners */

	float *zlin;		/* Line coords */
	float *zele;		/* Elem coords */

        int lspline=1;          /* spline function for line */
        int espline=lspline;    /* spline function for element */

        tiff_vars.in_elems=areadata_v64->numx;
        tiff_vars.in_lines=areadata_v64->numy;

        rc = aodtv64_uinit( );

        (void) aodtv64_determinedest( );

        rc = aodtv64_init(lspline, espline, &nc);

	zlin = malloc((size_t)nc * sizeof(zlin));
	zele = malloc((size_t)nc * sizeof(zele));

	(void) aodtv64_corner(nc, lspline, espline, zlin, zele);

        if((espline>1)||(lspline>1)) {
	  rc = aodtv64_domap(nc, zlin, zele, lspline, espline);
        }

	free(zlin);
	free(zele);

	return rc;
}

int aodtv64_uinit(void)
{

	/* Setup output file size and global variables needed */
	/* This module is for no changes for checking distortion */

	dis_vars.xrectl = (double) tiff_vars.in_lines;
	dis_vars.xrecte = (double) tiff_vars.in_elems;

	return 0;
}

/*=========================================================================*/

int aodtv64_init(int lspline, int espline, int *nc) 
{
	/* Compute number of corners for transformation & block sizes */

        remap_vars.nspl = (tiff_vars.out_elems + espline -1)/espline;
	remap_vars.nspe = (tiff_vars.out_lines + lspline -1)/lspline;

        remap_vars.ncl = remap_vars.nspl + 1;
	remap_vars.nce = remap_vars.nspe + 1;

        if((tiff_vars.out_elems + espline - 1) % espline == 0) 
		remap_vars.ncl = remap_vars.nspl;
        if((tiff_vars.out_lines + lspline - 1) % lspline == 0) 
		remap_vars.nce = remap_vars.nspe;

        *nc = remap_vars.ncl * remap_vars.nce;

	remap_vars.in_bfw = A_MAX( MINBFW, MINBLKSIZ * tiff_vars.in_elems );
	remap_vars.out_bfw = A_MAX( MINBFW, A_MAX( lspline, MINBLKSIZ) * tiff_vars.out_elems );

	remap_vars.slb = remap_vars.in_bfw/tiff_vars.in_elems;
	remap_vars.dlb = ((remap_vars.out_bfw/tiff_vars.out_elems)/lspline) * lspline;

    return 0;
}

/*======================================================================*/

void  aodtv64_determinedest(void)
{
        int xn,yn,il,ie;
        float latNW,lonNW,latNE,lonNE,latSW,lonSW,latSE,lonSE,lat,lon;
        float dmaxlat,dmaxlon,dminlat,dminlon,dinclat,dinclon,inclatlon;

        xn=areadata_v64->numx-1;
        yn=areadata_v64->numy-1;

        latNW=areadata_v64->lat[0][0];
        lonNW=areadata_v64->lon[0][0];
        latNE=areadata_v64->lat[0][xn];
        lonNE=areadata_v64->lon[0][xn];
        latSW=areadata_v64->lat[yn][0];
        lonSW=areadata_v64->lon[yn][0];
        latSE=areadata_v64->lat[yn][xn];
        lonSE=areadata_v64->lon[yn][xn];
  
        /* crosses dateline check */
        if(lonNW<lonNE) {
          lonNW=lonNW+360.0;
          if(lonSW<lonSE) lonSW=lonSW+360.0;
        }

        dmaxlat=A_MIN(latNW,latNE);
        dminlat=A_MAX(latSW,latSE);
        dmaxlon=A_MIN(lonNW,lonSW);
        dminlon=A_MAX(lonNE,lonSE);

        dinclat=(dmaxlat-dminlat)/(float)areadata_v64->numy;
        dinclon=(dmaxlon-dminlon)/(float)areadata_v64->numx;

        inclatlon=A_MAX(dinclat,dinclon);

        /* printf("Source Array Bounds\n"); */
        /* printf("     NW Corner : %7.2f/%7.2f\n",latNW,lonNW); */
        /* printf("     NE Corner : %7.2f/%7.2f\n",latNE,lonNE); */
        /* printf("     SW Corner : %7.2f/%7.2f\n",latSW,lonSW); */
        /* printf("     SE Corner : %7.2f/%7.2f\n",latSE,lonSE); */
        /* printf("Destination Array Bounds\n"); */
        /* printf("    Max Lat/Lon: %7.2f/%7.2f\n",dmaxlat,dmaxlon); */
        /* printf("    Min Lat/Lon: %7.2f/%7.2f\n",dminlat,dminlon); */
        /* printf("    Inc Lat/Lon:   %5.3f/  %5.3f\n",inclatlon,inclatlon); */

        tiff_vars.out_lines=(int)A_ABS((dmaxlat-dminlat)/inclatlon);
        tiff_vars.out_elems=(int)A_ABS((dmaxlon-dminlon)/inclatlon);  /* dmaxlon-dminlon maybe < 0 */
        /* printf("lines=%d  elems=%d\n",tiff_vars.out_lines,tiff_vars.out_elems); */
	for (il = 0; il < tiff_vars.out_lines; il++) {
          lat=dmaxlat-(il*inclatlon);
	  for (ie = 0; ie < tiff_vars.out_elems; ie++) {
            lon=dmaxlon-(ie*inclatlon);
            areadataRM->lat[il][ie]=lat;
            areadataRM->lon[il][ie]=lon;
          }
        }
        areadataRM->numx=tiff_vars.out_elems;
        areadataRM->numy=tiff_vars.out_lines;
}
void  aodtv64_corner(int nc, int lspline, int espline, float lines[], float elems[])
{
	/* Compute transformations at corners */

	int countL;
	int countE;
	int i;
	int index;
	int num_lines;
	int num_elems;
	int rc;

	int Dline;
	int Delem;
	int Sline;
	int Selem;
	
	/* initialize array of corners */
	
	for (i = 0; i< nc; i++) 
	{
		lines[i] = (float) -99.9;
		elems[i] = (float) -99.9;
	}

	/* loop through destination file and record source coords */

	index = -1;
	num_lines = tiff_vars.out_lines + lspline - 1;
	num_elems = tiff_vars.out_elems + espline - 1;

	for (countL = 0; countL < num_lines; countL = countL + lspline) {
	  Dline = countL;

	  for (countE = 0; countE < num_elems; countE = countE + espline )
	  {
	    Delem = countE;
	    rc = aodtv64_umap(Dline, Delem, &Sline, &Selem);
            if((espline==1)&&(lspline==1)) {
              areadataRM->temp[Dline][Delem]=areadata_v64->temp[Sline][Selem];
            } else {

              ++index;
 	      if( rc == 0) {
	        lines[index] = Sline;
	        elems[index] = Selem;
	      }
	    }
	  }
	}
}

/*========================================================================*/

int aodtv64_umap(int yl, int ye, int *zl, int *ze)
{
        int srcline,srcele;
        float destlat,destlon;

/*------------------------------------------------------------------------*/

	/* Convert destination yl, ye to source coords zl, ze */

        destlat=areadataRM->lat[yl][ye];
        destlon=areadataRM->lon[yl][ye];

        aodtv64_findpoint(destlat,destlon,&srcline,&srcele);
	*zl = srcline;
	*ze = srcele;

	return 0;
}

/*=======================================================================*/

int aodtv64_domap(int nc, float zlin[], float zele[], int lspline, int espline)
{
/*  Move data according to tranformation */

    int acclin,block,dloc,dne0,doff,doff0,doff1,doff2,doffe,doffl;
    int ie,ie1,ie2,ifxedg,il,il1,il2,ind1,ind2,ipt,isps;
    int k,kmaxe,kmaxl,kmine,kminl,maxsl,mxpt,npt,opoint;
    int point,psps,sblk,se,sl,sloc,soff,sppix,read;
    float ze11,ze12,ze21,ze22,zea,zeac,zeac0,zeb,zebb,zec,zecc;
    float zl11,zl12,zl21,zl22,zla,zlac,zlac0,zlb,zlbb,zlc,zlcc;
    float zminl,zmaxl,zmine,zmaxe,zsume,zsuml;

    char *source;			/* Source buffer */
    char *dest;				/* Destination buffer */
    char *temp;				/* Temporary buffer */

    float *source4;
    float *dest4;

    int buf_length;

    int nrad= 8;
    static int jl1[8] = {-2,-2, 0, 2, 2, 2, 0,-2};
    static int jl2[8] = {-1,-1, 0, 1, 1, 1, 0,-1};
    static int je1[8] = { 0, 2, 2, 2, 0,-2,-2,-2};
    static int je2[8] = { 0, 1, 1, 1, 0,-1,-1,-1};

    int indx=0;

/*--------------------------------------------------------------------------*/

/* 
 *  To smooth edged, extrapolate out one corner
 *  Use mean value from all radially extrapolated calculations
 */

	source4 = malloc(remap_vars.in_bfw * sizeof(float));
	dest4 = malloc(remap_vars.out_bfw * sizeof(float));

    	source = (char *) source4;
	dest = (char *) dest4;

	buf_length = remap_vars.in_bfw * sizeof(float);
	

	temp = malloc(nc);

	if(source == NULL || dest == NULL || temp == NULL)
	{
	  /* printf("Error allocating memory in domap\n"); */
	  /* printf("Sizes: %d %d %d\n", remap_vars.in_bfw,remap_vars.out_bfw, nc); */
	  return -1;
	}

	sppix = lspline * espline;

        (void) memset(temp, 0, nc);
 
	for (il = 0; il < tiff_vars.out_lines; il++) {
	  for (ie = 0; ie < tiff_vars.out_elems; ie++) {
            indx=(il*tiff_vars.out_elems)+ie;
            source4[indx]=areadata_v64->temp[il][ie];
          }
        }

	for (il = 1; il < remap_vars.nce+1; il++) {
	for (ie = 1; ie < remap_vars.ncl+1; ie++) {
	  ipt = IND(il, ie);
	  if(zlin[ipt-1] == (float) -99.9) {
	    zsuml = (float) 0.0;
	    zsume = (float) 0.0;
	    npt = 0;
	    for (k = 0; k < nrad; k++) {
	      il1=il+jl1[k];
	      il2=il+jl2[k];
	      if( il1 <   1 || il2 <   1) goto skip;
	      if( il1 > remap_vars.nce || il2 > remap_vars.nce) goto skip;

	      ie1 = ie + je1[k];
	      ie2 = ie + je2[k];
	      if( ie1 <   1 || ie2 <   1) goto skip;
	      if( ie1 > remap_vars.ncl || ie2 > remap_vars.ncl) goto skip;

	      ind1 = IND(il1, ie1);
	      ind2 = IND(il2, ie2);

	      if(zlin[ind1-1] == (float) -99.9 || zlin[ind2-1] ==(float) -99.9) goto skip;
	      if(temp[ind1-1] != 0) goto skip;
	      if(temp[ind2-1] != 0) goto skip;
	      npt = npt + 1;
	      zsuml = zsuml + (float) 2. * zlin[ind2-1] - zlin[ind1-1];
	      zsume = zsume + (float) 2. * zele[ind2-1] - zele[ind1-1];
	      skip:;
	    }

	    if( npt > 0) {
	      zlin[ipt-1] = zsuml/npt;
	      zele[ipt-1] = zsume/npt;
	      temp[ipt-1] = 1;
	    }
	  }
	}
	}

	free(temp);

	/* Loop through by destination blocks */
	
	block = 0;

	for (dloc = 1; dloc < tiff_vars.out_lines +1; dloc = dloc + remap_vars.dlb) {
	  /* Accumulated lines/block */
	  acclin = block * remap_vars.dlb;
  
	  /* Pointer to first corner of splines for this dest block */
	  point = block * remap_vars.ncl * remap_vars.dlb /lspline;
	  opoint = point;

	  /* Pointer to last corner for this dest block */
	  mxpt = ((block + 1) * remap_vars.ncl * remap_vars.dlb / lspline) - 1;
	  mxpt = A_MIN(mxpt, nc - remap_vars.ncl);
  
	  (void) memset(dest, 0, buf_length);

	  /* For each destination block loop through entire source */

	  for (sloc = 1; sloc < tiff_vars.in_lines +1; sloc = sloc + remap_vars.slb) {
	    maxsl = A_MIN(tiff_vars.in_lines, sloc + remap_vars.slb-1);
	    read = FALSE;

	    /* Loop through splines and move any data */

	    point = opoint;
	    sblk = 0;
	    while (point < mxpt) {

	      for (isps = 1; isps < remap_vars.nspl+1; isps++) {
	        doff0 = sblk/remap_vars.nspl*lspline*tiff_vars.out_elems;
	        doff1 = (sblk % remap_vars.nspl) * espline;
	        doff2 = doff1 + doff0+1;
	        psps = point + isps - 1;

	        /* Get 4 corners in line space and check for out of bounds */

	        zl11=zlin[psps];
	        zl12=zlin[psps + 1];
	        zl21=zlin[psps + remap_vars.ncl];
	        zl22=zlin[psps + 1 + remap_vars.ncl];
	        zminl = A_MIN(zl11,zl12);
	        zminl = A_MIN(zminl, zl21);
	        zminl = A_MIN(zminl, zl22);

	        /* Test for the presence of a limb in the spline box */

	        if( zminl == (float) -99.9) goto label30;

	        kminl = (int) (zminl + (float) 0.5);
	        if(kminl > maxsl) goto label30;
	        zmaxl = A_MAX(zl11, zl12);
	        zmaxl = A_MAX(zmaxl, zl21);
	        zmaxl = A_MAX(zmaxl, zl22);
	        kmaxl = (int) (zmaxl + (float) 0.5);
	        if(kmaxl < sloc) goto label30;

	        /* Get 4 corners in elem space & check for out of bound */
        
	        ze11=zele[psps];
	        ze12=zele[psps + 1];
	        ze21=zele[psps + remap_vars.ncl];
	        ze22=zele[psps + 1 + remap_vars.ncl];
        
	        zmaxe = A_MAX(ze11, ze12);
	        zmaxe = A_MAX(zmaxe, ze21);
	        zmaxe = A_MAX(zmaxe, ze22);
	        kmaxe = (int) (zmaxe + (float) 0.5);
	        if(kmaxe < 1) goto label30;

	        zmine = A_MIN(ze11, ze12);
	        zmine = A_MIN(zmine, ze21);
	        zmine = A_MIN(zmine, ze22);
	        kmine = (int) (zmine + (float) 0.5);

	        if(kmine > tiff_vars.in_elems) goto label30;
	        ifxedg = 0;

	        /* If the max & min element fall off the image...pitch it */

	        if( kmaxe > tiff_vars.in_elems && kmine < 1) goto label30;

	        /* Fix if left & right edge should be continuous */

	        if(kmaxe - kmine > (int) (.75 * tiff_vars.in_elems) ) {
	          if(ze11 < tiff_vars.in_elems/2) ze11 = ze11 + tiff_vars.in_elems;
	          if(ze12 < tiff_vars.in_elems/2) ze12 = ze12 + tiff_vars.in_elems;
	          if(ze21 < tiff_vars.in_elems/2) ze21 = ze21 + tiff_vars.in_elems;
	          if(ze22 < tiff_vars.in_elems/2) ze22 = ze22 + tiff_vars.in_elems;
	          ifxedg = 1;
	        }

	        zla=(zl12-zl11)/espline;
	        zlb=(zl21-zl11)/lspline;
	        zlc=(zl22+zl11-zl12-zl21)/sppix;
	        zea=(ze12-ze11)/espline;
	        zeb=(ze21-ze11)/lspline;
	        zec=(ze22+ze11-ze21-ze12)/sppix;

	        dne0 = 0;
                zlbb=zl11+ (float) 0.5;
	        zlcc= (float) 0.0;
	        zebb=ze11+ (float) 0.5;
	        zecc= (float) 0.0;

	        if(read == FALSE) {
	          read = TRUE;
	        }

	        if( (isps == remap_vars.nspl ) || (kmine < 1 || kmaxe > tiff_vars.in_elems) ||
	            (kminl < sloc || kmaxl > maxsl) || (point+2*remap_vars.ncl-1 > mxpt)) {
	          for (il = 1; il < lspline+1; il++) {
	            zlac=zlcc+zla;
	            zeac=zecc+zea;
	            zlac0= (float) 0.0;
	            zeac0= (float) 0.0;
	            for (ie =1; ie < espline+1; ie++) {
	              sl = (int) (zlbb+zlac0);
	              if( sl < sloc) goto label38;
	              if( sl > maxsl) goto label38;
	              se= (int) (zebb+zeac0);
	              if( se < 1) goto label38;
	              if( se > tiff_vars.in_elems && ifxedg == 0) goto label38;
	              if (se > tiff_vars.in_elems) se = se - tiff_vars.in_elems;
	              soff=(sl-sloc)*tiff_vars.in_elems+se;
	              doffe=doff1+ie;
	              if(doffe >  tiff_vars.out_elems) goto label38;
	              doffl=doff0+dne0;
	              if(doffl/tiff_vars.out_elems+acclin-1 > tiff_vars.out_lines) goto label38;
	              doff=doffl+doffe;
	              dest4[doff-1]=source4[soff-1];
	              label38:;
	              zlac0=zlac0+zlac;
	              zeac0=zeac0+zeac;
	            }
	            zlbb=zlbb+zlb;
	            zlcc=zlcc+zlc;
	            zebb=zebb+zeb;
	            zecc=zecc+zec;
	            dne0=dne0+tiff_vars.out_elems;
	          }
	        } else {
	          if( ifxedg == 0) {
	            for (il = 1; il < lspline+1; il++) {
	              zlac=zlcc+zla;
	              zeac=zecc+zea;
	              zlac0= (float) 0.0;
	              zeac0= (float) 0.0;
	              doff = doff2 + dne0;
	              for (ie =1; ie < espline+1; ie++) {
		        sl=(int) (zlbb+zlac0);
		        se=(int) (zebb+zeac0);
		        soff=(sl-sloc)*tiff_vars.in_elems+se;
		        dest4[doff-1]=source4[soff-1];
		        doff = doff + 1;
		        zlac0=zlac0+zlac;
		        zeac0=zeac0+zeac;
	              }
	              zlbb=zlbb+zlb;
	              zlcc=zlcc+zlc;
	              zebb=zebb+zeb;
	              zecc=zecc+zec;
	              dne0=dne0+tiff_vars.out_elems;
	            }
	          } else if( ifxedg == 1) {
	            for (il = 1; il < lspline+1; il++) {
	              zlac=zlcc+zla;
	              zeac=zecc+zea;
	              zlac0= (float) 0.0;
	              zeac0= (float) 0.0;
	              doff = doff2 + dne0;
	              for (ie =1; ie < espline+1; ie++) {
		        sl=(int) (zlbb+zlac0);
		        se=(int) (zebb+zeac0);
	                if (se > tiff_vars.in_elems) se = se - tiff_vars.in_elems;
	                soff=(sl-sloc)*tiff_vars.in_elems+se;
	                dest4[doff-1]=source4[soff-1];
	                doff = doff + 1;
	                zlac0=zlac0+zlac;
	                zeac0=zeac0+zeac;
	              }
	              zlbb=zlbb+zlb;
	              zlcc=zlcc+zlc;
	              zebb=zebb+zeb;
	              zecc=zecc+zec;
	              dne0=dne0+tiff_vars.out_elems;
	            }
	          }
	        }


	        label30:;
	        sblk = sblk + 1;
	      }	
	      point = point + remap_vars.ncl;
	    }
	  }

	  block = block + 1;
	}

	for (il = 0; il < tiff_vars.out_lines; il++) {
	  for (ie = 0; ie < tiff_vars.out_elems; ie++) {
            indx=(il*tiff_vars.out_elems)+ie;
            if(dest4[indx]==0.0) {
              dest4[indx]=dest4[indx-1];
            } 
            areadataRM->temp[il][ie]=dest4[indx];
            /*printf("il=%d ie=%d  temp=%f\n",il,ie,areadataRM->temp[il][ie]);*/
          }
        }

	free(dest);
	free(source);
	free(dest4);
	free(source4);

	return 0;
}

int aodtv64_findpoint (float slat,float slon,int *outy,int *outx)
{

  int x, y, numx, numy, value;
  float latxy[4], lonxy[4], xdist[4];
  float lastdist, xdistx, xangle;
  logical found=FALSE, oob=FALSE;
  logical inlat=FALSE, inlon=FALSE;
 
  numx=tiff_vars.in_elems;
  numy=tiff_vars.in_lines;
  x=0;
  y=0;
  lastdist=9999.9;
  while(!found&&!oob) {
    latxy[0]=areadata_v64->lat[y][x];
    lonxy[0]=areadata_v64->lon[y][x];
    latxy[3]=areadata_v64->lat[y+1][x+1];
    lonxy[3]=areadata_v64->lon[y+1][x+1];
    if((slon>lonxy[0])||(slon<lonxy[3])) {
      inlon=FALSE;
      if(slon<lonxy[3]) {
        x++;
      } else {
        if(slon>lonxy[0]) x--;
      }
    } else {
      inlon=TRUE;
    }
    if((slat>latxy[0])||(slat<latxy[3])) {
      inlat=FALSE;
      if(slat<latxy[3]) {
        y++;
      } else {
        if(slat>latxy[0]) y--;
      }
    } else {
      inlat=TRUE;
    }
    aodtv64_distance(slat,slon,latxy[0],lonxy[0],1,&xdistx,&xangle);
    if(inlat&&inlon) found=TRUE;
    if(lastdist<=xdistx) found=TRUE; 
    if((x<0)||(x>numx-1)) oob=TRUE;
    if((y<0)||(y>numy-1)) oob=TRUE;
    lastdist=xdistx;
  }
  if(found) {
    latxy[1]=areadata_v64->lat[y][x+1];
    lonxy[1]=areadata_v64->lon[y][x+1];
    latxy[2]=areadata_v64->lat[y+1][x];
    lonxy[2]=areadata_v64->lon[y+1][x];
    aodtv64_distance(slat,slon,latxy[0],lonxy[0],1,&xdist[0],&xangle);
    aodtv64_distance(slat,slon,latxy[1],lonxy[1],1,&xdist[1],&xangle);
    value=(xdist[0]<xdist[1]) ? 0 : 1;
    aodtv64_distance(slat,slon,latxy[2],lonxy[2],1,&xdist[2],&xangle);
    value=(xdist[value]<xdist[2]) ? value : 2;
    aodtv64_distance(slat,slon,latxy[3],lonxy[3],1,&xdist[3],&xangle);
    value=(xdist[value]<xdist[3]) ? value : 3;
    *outx=((value==0)||(value==2)) ? x : x+1;
    *outy=((value==0)||(value==1)) ? y : y+1;
    return 0;
  } else {
    *outx=-1;
    *outy=-1;
    return -1;
  }
}

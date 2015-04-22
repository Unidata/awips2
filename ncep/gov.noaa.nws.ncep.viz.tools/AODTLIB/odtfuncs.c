/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

double aodtv64_calctime( int, int );
float  aodtv64_slopecal( double,int );
float  aodtv64_getpwval( int,float );
float  aodtv64_PWlandsea( float );
float  aodtv64_ptovmax( float );
float  aodtv64_ptotno( float );
int    aodtv64_cmonth2julian( char * );
void   aodtv64_julian2cmonth( int, char * );
void   aodtv64_distance( float,float,float,float,int,float *,float * );
void   aodtv64_distance2( float, float, float, float, float *, float * );
float  aodtv64_atoif( char *, int, int );
void   aodtv64_calcskew( float *, int, float *, float *, float * );
int    aodtv64_idmyyd( int,int,int );
void   aodtv64_yddmy( int,int *,int *,int * );
int    aodtv64_oceanbasin( float,float );
int    aodtv64_sattypes( int,char * ); 
int    aodtv64_initcurrent(int);

float aodtv64_slopecal(double tval,int itype)
/* Calculate slope or y-intercept of all points over tval time period
    Inputs  : tval    - time period to calculate slope or y-intercept
              itype   - flag value indicating slope or y-intercept calculation 
                        and parameter to utilize
                        1 = Final T# (will return slope)
                        2 = latitude (will return y-intercept)
                        3 = longitude (will return y-intercept)
              global structure odtcurrent_v64 containing current analysis
    Outputs : return value is slope or y-intercept of line over time period desired
*/
{
  int icnt=0,icntmin=4;
  float sumx=0.0,sumy=0.0,sumsqx=0.0,sumsqy=0.0,sumxy=0.0;
  float xvalue,yvalue,xbar,ybar,varx,vary,covxy,r,b,slope;
  double curtime,xtime,tlimit;
  logical found=FALSE,landcheck;
  struct odtdata *odthistory;

  odthistory=odthistoryfirst_v64;

  curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);
  tlimit=curtime-(tval/24.0);

  while(odthistory!=0) {
    xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
    if((xtime<curtime)&&(xtime>=tlimit)) {
      landcheck=TRUE;
      if(((oland_v64)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) landcheck=FALSE;
      if(landcheck) {
        icnt++;
        xvalue=(float)(curtime-xtime);
        if(itype==1) yvalue=odthistory->IR.Tfinal;
        if(itype==2) yvalue=odthistory->IR.latitude;
        if(itype==3) yvalue=odthistory->IR.longitude;
        sumx=sumx+xvalue;
        sumy=sumy+yvalue;
        sumsqx=sumsqx+(xvalue*xvalue);
        sumsqy=sumsqy+(yvalue*yvalue);
        sumxy=sumxy+(yvalue*xvalue);
        found=TRUE;
      }
    } else {
      if(found) break;
    }
    odthistory=odthistory->nextrec;
  }
  /* if calculating slope of Final T# values, add in current value */
  if(itype==1) {
    icntmin=6;
    /* add current record to slope calculation */
    yvalue=odtcurrent_v64->IR.Tfinal;
    sumy=sumy+yvalue;
    sumsqy=sumsqy+(yvalue*yvalue);
    icnt++;
  }

  /* compute least squares fit of line for data points
     using the equation  Y = Y* + r(varX/varY)(X - X*) = Mx + B
     X* = mean of X values (time values)  = xbar
     Y* = mean of Y values (T# values)    = ybar
     varX = variance of X                 = varx
     varY = variance of Y                 = vary
     r = covariance - Y*X*                = r
     M = slope of line (desired value)    = slopecal = r*(sqrt(vary/varx))
     B = y-intercept                      = ybar-(slopecal*xbar)  */
  /* must have more than icntmin data values to calculate slope */
  if(icnt<icntmin) {
    slope=0.0;
    if((itype==2)||(itype==3)) slope=999.99;
    return slope;
  }

  xbar=sumx/(float)icnt;
  ybar=sumy/(float)icnt;
  varx=(sumsqx/(float)icnt)-(xbar*xbar);
  vary=(sumsqy/(float)icnt)-(ybar*ybar);
  covxy=(sumxy/(float)icnt)-(xbar*ybar);
  r=covxy/A_SQRT(varx*vary);
  if((A_ABS(varx)<=0.0001)||(A_ABS(vary)<=0.0001)) {
    slope=0.0;
  } else {
    slope=r*(A_SQRT(vary/varx));
  }

  slope=(float)((int)(slope*10.0))/10.0;
  if((itype==2)||(itype==3)) {
    b=ybar-(slope*xbar);
    return b;      /* y-intercept for latitude/longitude extrapolation */
  } else {
    return slope;  /* slope for Final T# slope calculation */
  }
  
}

double aodtv64_calctime(int date,int time)
/* Compute time in xxxxx.yyy units, where xxxxx is the
   day and yyy is the percentage of the day.  This routine
   will also correct for Y2K problems.
    Inputs  : date - Julian date
              time - time in HHMMSS format
    Outputs : function return value
*/
{
  int iyy;
  float sec,min,hour,partday;
  double timeout;

  if((date%1000)==0) {
    return 0;
  }
  if(time<0) {
    return 0;
  }

  iyy=date/1000;  /* obtain year */
  /* check for millenium designation in the year.
     if it is not there, add it onto the beginning */
  if(iyy<1900) {
    if(iyy>70) {
      date=1900000+date;
    } else {
      date=2000000+date;
    }
  }
  
  sec=((float)(time%100))/3600.0;
  min=((float)((time/100)%100))/60.0;
  hour=(float)(time/10000);
  partday=(hour+min+sec)/24.0;
  timeout=(double)date+(double)partday;

  return timeout;
}

float aodtv64_getpwval(int ival,float cival)
/* Obtain pressure or wind value (for Atlantic or
   West Pacific storms) given the intensity estimate
   value.
    Inputs  : ival  - flag for wind (1) or pressure (0) output
              cival - Current Intensity (CI) value
    Outputs : return value is pressure/wind value
*/
{
  float value;
  int ixx=2;

  /* determine correct pressure/wind array bin */
  while((cival>tno_v64[ixx])&&(ixx<72)) { ixx++; }

  /* convert CI value to wind/pressure value */
  if(ival==1) {
    value=wind_v64[ixx];           /* WIND */
  } else {
    value=pres_v64[idomain_v64][ixx];  /* PRESSURE */
  }

  return value;
}

float aodtv64_PWlandsea( float pval )
/* obtain Landsea Vmax value given mslp value for a given 
   storm at a given latitude.
   lat < 25N : V = 12.016 * (1013 - P0)**(0.5337)
   25-35N    : V = 14.172 * (1013 - P0)**(0.4778)
   35-45N    : V = 16.086 * (1013 - P0)**(0.4333)

   Inputs  : pval - input pressure value (in mb)
   Outputs : return value of Vmax in knots
*/
{
  int   ixx;
  float vmax,xlat;
  float a[3]={12.016,14.172,16.086};
  float b[3]={0.5337,0.4778,0.4333};

  xlat=odtcurrent_v64->IR.latitude;
  ixx=0;
  if(xlat>=25.0) ixx=1;
  if(xlat>=35.0) ixx=2;

  vmax=a[ixx]*(A_POW((1013.0-pval),b[ixx]));

  return vmax;
 
}

float aodtv64_ptovmax( float pval )
/* derive wind Vmax value from pressure (MSLP) value
   Inputs  : pressure value
   Outputs : return value is Vmax value 
*/
{
  int ixx=2;
  float vmax,xpart;

  while((pval<pres_v64[idomain_v64][ixx])&&(ixx<72)) { ixx++; }

  if(ixx==2) {
    vmax=wind_v64[2];
  } else {
    xpart=(pres_v64[idomain_v64][ixx-1]-pval)/(pres_v64[idomain_v64][ixx-1]-pres_v64[idomain_v64][ixx]);
    vmax=wind_v64[ixx-1]+((wind_v64[ixx]-wind_v64[ixx-1])*xpart);
  }

  return vmax;
}

float aodtv64_ptotno( float pval )
/* derive T# value from pressure (MSLP) value
   Inputs  : pressure value
   Outputs : return value is T# value 
*/
{
  int ixx=2;
  float xtno,xpart;

  while((pval<pres_v64[idomain_v64][ixx])&&(ixx<72)) { ixx++; }

  if(ixx==2) {
    xtno=tno_v64[2];
  } else {
    xpart=(pres_v64[idomain_v64][ixx-1]-pval)/(pres_v64[idomain_v64][ixx-1]-pres_v64[idomain_v64][ixx]);
    xtno=tno_v64[ixx-1]+((tno_v64[ixx]-tno_v64[ixx-1])*xpart);
  }

  return xtno;
}

int aodtv64_cmonth2julian( char *cval1 )
/* Convert YYYYMMMDD format to julian date.
   Inputs  : cval1 - character representation of date in YYYYmonDD format
                     where DD and YYYY are integers and mon is a three
                     character abbreviation for the month (e.g. 2000MAR13)
   Outputs : return value is julian date
*/
{
  int  iday,iyear;
  int  imon=0,iyyddd=0;
  char cmonx[4];

  /* break down character string into various components */
  (void)sscanf(cval1,"%4d%3s%2d",&iyear,cmonx,&iday);

  /* calculate integer month value */
  while((strncmp(cmonx,cmon_v64[imon],3)!=0)&&(imon<12)) { imon++; }

  /* determine julian date from day/month/year */
  if(imon<12) iyyddd=aodtv64_idmyyd(iday,imon+1,iyear);

  return iyyddd;
}

void aodtv64_julian2cmonth( int julian,char *cvalue )
/* convert julian date to YYYYMMMDD format for output.
   Inputs  : julian - Julian date representation of date
   Outputs : cvalue - character representation of date in YYYYmonDD format
             where DD and YYYY are integers and mon is a three
             character abbreviation for the month (e.g. 2000MAR13)
*/
{
  int iday,imon,iyear;

  /* calculate date/month/year from julian date */
  (void)aodtv64_yddmy(julian,&iday,&imon,&iyear);

  /* form character string representation from various components */
  if(iday<10) {
    (void)sprintf(cvalue,"%4d%3s0%1d",iyear,cmon_v64[imon-1],iday);
  } else {
    (void)sprintf(cvalue,"%4d%3s%2d",iyear,cmon_v64[imon-1],iday);
  }

}

void aodtv64_distance(float rrlat,float rrlon,float pplat,float pplon,int iunit,
              float *dist,float *ang)
/* Calculate distance and angle between two points 
   (rrlat,rrlon and pplat,pplon).
   Inputs  : rrlat - latitude of starting point
             rrlon - longitude of starting point
             rrlat - latitude of ending point
             rrlon - longitude of ending point
             iunit - flag for output unit type (1-km,2-mi,3-nmi)
   Outputs : dist  - distance between two points
             ang   - angle between two points (0=north)
*/
{
  float z=0.017453292,r=6371.0;
  float rlat,rlon,plat,plon;
  float crlat,crlon,srlat,srlon;
  float cplat,cplon,splat,splon;
  float xx,yy,zz,idist,xdist,xang;

  rlat=rrlat*z;
  rlon=rrlon*z;
  plat=pplat*z;
  plon=pplon*z;
  crlat=A_COS(rlat);
  crlon=A_COS(rlon);
  srlat=A_SIN(rlat);
  srlon=A_SIN(rlon);
  cplat=A_COS(plat);
  cplon=A_COS(plon);
  splat=A_SIN(plat);
  splon=A_SIN(plon);
  xx=(cplat*cplon)-(crlat*crlon);
  yy=(cplat*splon)-(crlat*srlon);
  zz=splat-srlat;
  idist=A_SQRT((xx*xx)+(yy*yy)+(zz*zz));
  /* xdist is distance in kilometers */
  xdist=2.0*A_ASIN(idist/2.0)*r;

  if(iunit==2) xdist=((69.0*xdist)+55)/111.0;  /* conversion to miles */
  if(iunit==3) xdist=((60.0*xdist)+55)/111.0;  /* conversion to nautical miles */
  *dist=xdist;

  xang=0.0;
  if(A_ABS(xdist)>0.0001) xang=(A_SIN(rlon-plon)*A_SIN((3.14159/2.0)-plat))/A_SIN(idist);
  if(A_ABS(xang)>1.0) xang=A_SIGN(1.000,xang);
  xang=A_ASIN(xang)/z;
  if(plat<rlat) xang=180.0-xang;
  if(xang<0.0) xang=360.0+xang;
  *ang=xang;
}

/*========================================================================*/

void aodtv64_distance2(float rlat,float rlon,float xdist,float xang,
               float *plat,float *plon)
/* Calculate a latitude and longitude position from an 
   initial latitude/longitude and distance/angle values.
    Inputs  : rlat - initial latitude
              rlon - initial longitude
              dist - distance from initial position
              ang  - angle from initial position
    Outputs : plat - derived latitude
              plon - derived longitude
*/
{
  float z=0.017453292,z90=1.570797;
  float clat,clon,cltv,cdis,cang;
  float qlat,qlon,argu,tv;
  int iang;

  clat=(90.0-rlat)*z;
  cltv=clat;
  clon=rlon*z;
  if(rlat<0.0) {
    cltv=-(90.0+rlat)*z;
    clon=(rlon-180.0)*z;
    xang=360-xang;
  }
  iang=(int)xang;
  cang=-(float)(((540-iang)%360)*z);
  cdis=(xdist/111.1)*z;
  qlat=A_ACOS((A_COS(clat)*A_COS(cdis))+(A_SIN(clat)*A_SIN(cdis)*A_COS(cang)));
  if(A_ABS(qlat)<0.0000001) {
    qlon=0.0;
  } else {
    argu=(A_SIN(cdis)*A_SIN(cang))/A_SIN(qlat);
    if(A_ABS(argu)>1.0) argu=A_SIGN(1.0,argu);
    qlon=A_ASIN(argu);
    tv=A_ATAN(A_SIN(z90-cang))/A_TAN(z90-cdis);
    if(tv>cltv) qlon=(2.0*z90)-qlon;
  }
  qlon=clon-qlon;
  *plat=90.0-(qlat/z);
  *plon=(float)((int)(10000*(qlon/z))%3600000)/10000.0;
  if(*plon<-180) *plon=*plon+360;

}

float aodtv64_atoif(char *ptr,int beg,int end)
/* routine to convert from character to floating point 
    Inputs  : ptr  - pointer to beginning of character string
              beg  - beginning character to convert
              end  - ending character to convert
    Outputs : return value is converted floating point value
*/
{
  int ixx,ichar,mul=1;
  float atoif,fact=1.0,value=0.0,denom=1.0;

  for(ixx=end-1;ixx>=beg-1;ixx--) {
    if((ptr[ixx]=='S')||(ptr[ixx]=='E')) mul=-1;
    ichar=(int)(ptr[ixx])-48;
    if((ichar>=0)&&(ichar<=9)) {
      value=value+((float)ichar*fact);
      fact=fact*10.0;
    } else if(ichar==-2) {
      denom=fact;
    } else {
      continue;
    }
  }

  atoif=mul*(value/denom);
  return (atoif);

}

void aodtv64_calcskew( float *bin, int nbin, float *ave, float *stdv, float *skew )
/* Calculate average, standard deviation, and skew for a given data set.
    Inputs  : bin  - data array
              nbin - number of points in data array
    Outputs : ave  - average value in array
              stdv - standard deviation of data array
              skew - skew of data array histogram
*/
{
  int ixx;
  float xave,xstdv,xskew;
  float a2sum=0.0,a3sum=0.0,nsum=0.0;

  for(ixx=0;ixx<nbin;ixx++) {
    nsum=nsum+bin[ixx];
  }
  /* compute average value of data array */
  xave=nsum/(float)nbin;

  for(ixx=0;ixx<nbin;ixx++) {
    a2sum=a2sum+((bin[ixx]-xave)*(bin[ixx]-xave));
    a3sum=a3sum+((bin[ixx]-xave)*(bin[ixx]-xave)*(bin[ixx]-xave));
  }
  if(nbin<=1) {
    xstdv=0.0;
    xskew=0.0;
  } else {
    /* calculate standard deviation of data array */
    xstdv=sqrt((1.0/((float)nbin-1.0))*a2sum);
    /* calculate skew of data array */
    xskew=((1.0/((float)nbin-1.0))*a3sum)/(xstdv*xstdv*xstdv);
  }

  /* return desired values */
  *ave=xave;
  *stdv=xstdv;
  *skew=xskew;

}

int aodtv64_idmyyd(int day,int mon,int year)
/* Convert dd/mm/yy to yyyyddd format.
   this routine was originally taken from McIDAS
   program idmyyd.for.
   Inputs  : day   - day
             month - month (integer)
             year  - year (YY or YYYY)
   Outputs : return value is Julian date or 0 (bad input data)
*/
{
  int mtbl[12]={0,31,59,90,120,151,181,212,243,273,304,334};
  int julday=0,iday;

  /* perform a couple quality checks for day/month */
  if(day<1||day>31) return julday;
  if(mon<1||mon>12) return julday;

  iday=day+mtbl[mon-1];
  if(year<1900) {
    if(year>70) {
      year=1900+year;
    } else {
      year=2000+year;
    }
  }
  /* Leap year check */
  if(((year%4)==0)&&(mon>2)) iday=iday+1;
  julday=(year*1000)+iday;

  return julday;
}

void aodtv64_yddmy(int syd,int *day,int *mon,int *year)
/* Convert yyyyddd to dd/mm/yy format.
   Inputs  : syd   - Julian day (yyyyddd)
   Outputs : day   - date
             month - month
             year  - year (yyyy)
*/
{
  int dn[2][13]={ {0,31,59,90,120,151,181,212,243,273,304,334,365},
                  {0,31,60,91,121,152,182,213,244,274,305,335,366} };
  int iyy,idd,imm,ily=0;

  iyy=syd/1000;
  if(iyy<1900) {
    if(iyy>70) {
      iyy=iyy+1900;
    } else {
      iyy=iyy+2000;
    }
  }
  idd=(syd%1000);
  if((iyy%4)==0) ily=1;
  for(imm=0;imm<13;imm++) {
    if(idd<=dn[ily][imm]) break;
  }
  *mon=imm;
  *day=idd-dn[ily][imm-1];
  *year=iyy;

}

int aodtv64_sattypes(int isat,char *csat) 
/* obtain satellite name given ID number */
{

  if(isat== 0) strcpy(csat," GOES8");
  if(isat== 1) strcpy(csat," GOES9");
  if(isat== 2) strcpy(csat,"GOES10");
  if(isat== 3) strcpy(csat,"GOES11");
  if(isat== 4) strcpy(csat,"GOES12");
  if(isat== 5) strcpy(csat,"  GMS5");
  if(isat== 6) strcpy(csat,"MTSAT1");
  if(isat== 7) strcpy(csat,"MTSAT2");
  if(isat== 8) strcpy(csat,"  MET5");
  if(isat== 9) strcpy(csat,"  MET6");
  if(isat==10) strcpy(csat,"  MET7");
  if(isat==11) strcpy(csat,"  MSG1");
  if(isat==12) strcpy(csat,"  MSG2");
  if(isat==13) strcpy(csat,"  MSG3");
  if(isat==14) strcpy(csat,"  FY2B");
  if(isat==15) strcpy(csat,"  FY2C");
  if(isat==16) strcpy(csat,"  FY2D");
  csat[strlen(csat)]='\0';

  return 0;
}


int aodtv64_oceanbasin(float xlat,float xlon)
/* determine ocean basin given latitude and longitude position of storm
   Inputs  : xlat - latitude
             xlon - longitude
   Outputs : basin type (0=atlantic,1=pacific,2=Indian)
*/
{
  int   ibasin;
  float m;

  if(xlat>=0.0) {
    /* northern hemisphere */
    if(xlon<=-100.0) {
      ibasin=1;      /* Pacific */
    } else if((xlon>-100.0)&&(xlon<=-20.0)) {
      ibasin=2;      /* Indian */
    } else if(xlon>=100.0) {
      ibasin=1;      /* Pacific */
    } else {
      /* -20 to ~+100 */
      if(xlat>20.0) {
        ibasin=0;      /* Atlantic */
      } else if(xlat<10.0) {
        if(xlon<80.0) {
          ibasin=0;      /* Atlantic */
        } else {
          ibasin=1;      /* Pacific */
        }
      } else {
        /* latitude between 10 and 20 north */
        /* slope of line between (100W,20N) and (80W,10N) is 2
           if slope of new point and (100,20) is greater than 2, storm is in atlantic
           if slope of new point and (100,20) is less than 2, storm is in pacific */
         m=(100.0-xlon)/(20-xlat);
         if(m>2.0) {
           ibasin=0;      /* Atlantic */
         } else {
           ibasin=1;      /* Pacific */
         }
      }
    }
  } else {
    /* southern hemisphere */
    if(xlon<=-150.0) {
      ibasin=1;      /* Pacific */
    } else if((xlon>-150.0)&&(xlon<=-20.0)) {
      ibasin=2;      /* Indian */
    } else if((xlon>-20.0)&&(xlon<=67.0)) {
      ibasin=0;      /* Atlantic */
    } else {
      ibasin=1;      /* Pacific */
    }
  }

  return ibasin;
}

int aodtv64_initcurrent(int redo )
/* initialize odtcurrent_v64 array or reset values for land interaction situations */
{
  char comm[50]="\0";

  if(!redo) {
    odtcurrent_v64=(struct odtdata *)malloc(sizeof(struct odtdata)); /* ABCD */
    odtcurrent_v64->IR.latitude=999.99;
    odtcurrent_v64->IR.longitude=999.99;
    odtcurrent_v64->IR.land=0;
    odtcurrent_v64->IR.autopos=0;
    strcpy(odtcurrent_v64->IR.comment,comm);
    diagnostics_v64=(char *)calloc((size_t)50000,sizeof(char)); 
    hfile_v64=(char *)calloc((size_t)200,sizeof(char)); 
    fixfile_v64=(char *)calloc((size_t)200,sizeof(char)); 
    atcftype_v64=(char *)calloc((size_t)5,sizeof(char)); 
  }

  odtcurrent_v64->IR.Traw=0.0;
  odtcurrent_v64->IR.TrawO=0.0;
  odtcurrent_v64->IR.Tfinal=0.0;
  odtcurrent_v64->IR.Tfinal3=0.0;
  odtcurrent_v64->IR.CI=0.0;
  odtcurrent_v64->IR.eyet=99.99;
  odtcurrent_v64->IR.warmt=99.99;
  odtcurrent_v64->IR.cloudt=99.99;
  odtcurrent_v64->IR.cloudt2=99.99;
  odtcurrent_v64->IR.cwcloudt=99.99;
  odtcurrent_v64->IR.warmlatitude=999.99;
  odtcurrent_v64->IR.warmlongitude=999.99;
  odtcurrent_v64->IR.eyesize=0.0;
  odtcurrent_v64->IR.eyestdv=0.0;
  odtcurrent_v64->IR.cloudsymave=0.0;
  odtcurrent_v64->IR.eyescene=0;
  odtcurrent_v64->IR.cloudscene=0;
  odtcurrent_v64->IR.eyesceneold=-1;
  odtcurrent_v64->IR.cloudsceneold=-1;
  odtcurrent_v64->IR.rule9=0;
  odtcurrent_v64->IR.rule8=0;
  odtcurrent_v64->IR.LBflag=0;
  odtcurrent_v64->IR.eyefft=0;
  odtcurrent_v64->IR.cloudfft=0;
  odtcurrent_v64->IR.cwring=0;
  odtcurrent_v64->IR.ringcb=0;
  odtcurrent_v64->IR.ringcbval=0;
  odtcurrent_v64->IR.ringcbvalmax=0;
  odtcurrent_v64->IR.CIadjp=0.0;
  odtcurrent_v64->IR.TIEflag=0;
  odtcurrent_v64->IR.TIEraw=0.0;
  odtcurrent_v64->IR.TIEavg=0.0;
  odtcurrent_v64->IR.sst=-99.9;
  odtcurrent_v64->IR.rmw=-99.9;
  odtcurrent_v64->nextrec=NULL;  /* added by CDB */

  return 0;
}

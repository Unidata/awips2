/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int   aodtv64_gettemps( int );
float aodtv64_calceyetemp( int, float, float *, float * ); 
float aodtv64_calccloudtemp( int * );
void  aodtv64_readcirc( int, int );

/*=================================================================================*/

int aodtv64_gettemps( int keyer )
/* Determine eye and coldest-warmest cloud temperatures.
    Inputs  : keyer      - eye radius (km)
    Outputs : odtcurrent_v64 - structure containing current image information
    Return  : -51 : eye, cloud, or warmest temperature <-100C or >+40C
*/
{

  int cursorx,cursory;
  int idist,iret;
  float cursortemp,eyet,cloudt;
  float cursorlat,cursorlon,warmlat,warmlon,eyeangle,eyedist;

  /* calculate cursorx/cursory from numx/numy... values should be 0.5*numx/y */
  cursorx=areadata_v64->numx/2;
  cursory=areadata_v64->numy/2;
  cursortemp=areadata_v64->temp[cursory][cursorx];
  cursorlat=areadata_v64->lat[cursory][cursorx];
  cursorlon=areadata_v64->lon[cursory][cursorx];

  tcircfirst_v64=(struct ringdata *)calloc(1, sizeof(struct ringdata));
  /* load array containing data on rings around center location */
  aodtv64_readcirc(cursorx,cursory);

  iret=0;
  /* compute eye/warmest pixel temperature */ 
  eyet=aodtv64_calceyetemp(keyer,cursortemp,&eyeangle,&eyedist);
  if((eyet<-100.0)||(eyet>40.0)) iret=-51;

  if(keyer==keyerA_v64) {
    /* set warmest pixel temperature */
    odtcurrent_v64->IR.warmt=eyet;
    /* calculate warmest pixel location */
    aodtv64_distance2( cursorlat, cursorlon, eyedist, eyeangle, &warmlat, &warmlon);
    odtcurrent_v64->IR.warmlatitude=warmlat;
    odtcurrent_v64->IR.warmlongitude=warmlon;
    /* store forecast location temperature in eyet */
    odtcurrent_v64->IR.eyet=cursortemp-273.16;
    free(tcircfirst_v64);
  } else {
    /* set eye temperature */
    odtcurrent_v64->IR.eyet=eyet;
    /* set cloud temperature and ring distance */
    cloudt=aodtv64_calccloudtemp(&idist);
    if((eyet<-100.0)||(eyet>40.0)) iret=-51;
    odtcurrent_v64->IR.cwcloudt=cloudt;
    odtcurrent_v64->IR.cwring=idist;

    printf("eyeTemp=%f, cloudT=%f Rdist=%d, Cux=%d, cuy=%d, eyeRadius=%d\n",eyet,cloudt,idist,cursorx, cursory,keyer);
  }

  return iret;
}

/*===========================================================================*/

float aodtv64_calceyetemp(int keyer, float cursortemp,float *eyeangle, float *eyedist)
/* Determine eye/warmest temperature by examining the satellite
   data between 0 and 24/75 km from the storm center location.
   Eye temperature will be warmest temperature found.
    Inputs  : keyer      - analysis region radius
              cursortemp - temperature of pixel at cursor location
    Outputs : eyeangle   - angle to warmest temperature in eye region (if found)
              eyedist    - distance to warmest temperature in eye region (if found)
    Return  : return value is warmest eye temperature
*/
{
  float eyetemp;
  struct ringdata *tcirc;

  /* set eye temp to cursor location temperature */
  eyetemp=cursortemp;
  
  *eyedist=0.0;
  *eyeangle=0.0;
  tcirc=tcircfirst_v64;
  while(tcirc!=NULL) {
    if(tcirc->dist<=(float)keyer) {
      if(tcirc->temp>eyetemp) {
        eyetemp=tcirc->temp;
        *eyedist=tcirc->dist;
        *eyeangle=tcirc->angle;
      }
    }
    tcirc=tcirc->nextrec;
  }

  /* convert temperature to C from K */
  eyetemp=(eyetemp-273.16);

  return eyetemp;
}

/*=====================================================================*/

float aodtv64_calccloudtemp(int *irdist)
/* Determine surrounding cloud top region temperature by
   examining the satellite data between kstart_v64(24 km) and
   kend_v64 (136 km) from the storm center location.  Cloud
   temperature will be the coldest value of an array
   of warmest ring temperatures (4 km ring sizes for 4 km
   infrared data).  This is the "coldest-warmest" pixel.
    Inputs  : none
    Outputs : irdist - distance (km) from center to cloud top
                       temperature value (distance to ring)
              return value is cloud top temperature value
*/
{
  int iyy;
  int numrings=(kend_v64-kstart_v64)/kres_v64,kring;
  float *maxtemp;
  float cloudtemp;
  struct ringdata *tcirc;

/*---------------------------------------------------------------------*/
  
  cloudtemp=10000.0;

  maxtemp=(float *)calloc((size_t)numrings,sizeof(float));

  /* initialize maxtemp array */
  for(iyy=0;iyy<numrings;iyy++) {
    maxtemp[iyy]=-10000.0;
  }

  tcirc=tcircfirst_v64;
  while(tcirc!=NULL) {
    if((tcirc->dist>=(float)kstart_v64)&&(tcirc->dist<=(float)kend_v64)) {
      kring=((int)tcirc->dist-kstart_v64)/kres_v64;
      if(tcirc->temp>maxtemp[kring]) maxtemp[kring]=tcirc->temp;
    }
    tcirc=tcirc->nextrec;
  }

  /* search maxtemp array for coldest temperature */
  for(iyy=0;iyy<numrings;iyy++) {
    if(maxtemp[iyy]<cloudtemp) {
      cloudtemp=maxtemp[iyy];
      *irdist=(iyy*kres_v64)+kstart_v64;
    }
  }
  cloudtemp=(cloudtemp-273.16);
  free(maxtemp);

  return cloudtemp;
}

/*=====================================================================*/

void aodtv64_readcirc(int ixc,int iyc)
/* Read array of satellite data temperatures and load array
   containing temperatures and lat/lon positions.  
    Inputs  : ixc   - element location of center point
              iyc   - line location of center point
    Outputs : global structure tcirc containing temperature/locations
 **									*
 * Log:									*
 * T. Piper/SAIC	05/06	Use calloc in link list			*
 ***********************************************************************/
{
  int ixx,iyy,count=0;
  float xclat,xclon,xlat,xlon,xdist,xangle;
  struct ringdata *tcirc;

  /* obtain center/cursor x,y position */
  xclat=areadata_v64->lat[iyc][ixc];
  xclon=areadata_v64->lon[iyc][ixc];

  /* load tcirc array with distance, angle, and temp info */
  for(iyy=0;iyy<areadata_v64->numy;iyy++) {
    for(ixx=0;ixx<areadata_v64->numx;ixx++) {
      xlat=areadata_v64->lat[iyy][ixx];
      xlon=areadata_v64->lon[iyy][ixx];
      aodtv64_distance(xlat,xlon,xclat,xclon,1,&xdist,&xangle);
      if(xdist<=(float)(kend_v64+40)) {    /* add 40.0 to allow for correct calculation of annulus temp */
        if(count==0) {
          tcirc=tcircfirst_v64;
        } else {
          tcirc->nextrec=(struct ringdata *)calloc(1, sizeof(struct ringdata));
	  tcirc=tcirc->nextrec;
	} 
        count++;
        tcirc->dist=xdist;
        tcirc->angle=xangle;
        tcirc->temp=areadata_v64->temp[iyy][ixx];
      }
    }
  }
}

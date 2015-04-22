/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"

int aodtv64_readtopo( char *,float,float,int * ); 

int aodtv64_readtopo( char *topofile, float mlat, float mlon, int *ichar) 
/* Determine land/water flag for data point using low resolution
   topograpy file TOPOLRES.  
    Inputs  : mlat  - latitude of data point in question
              mlon  - longitude of data point in question
    Outputs : ichar - <0=error,1=land,2=water 
    Return  : -31 : error accessing topography file
              -32 : error reading topography file
	       71 : cyclone is over land
	        0 : cyclone is over water
*/
{
 FILE *fdi;
 int error,iret;
 short buf;
 long llat, llon, position,bytes=768;

/*---------------------------------------------------------------------------*/

 *ichar=-99;
 llat=(long)mlat;
 llon=(long)mlon;
 fdi = fopen(topofile, "rb") ;
 if (fdi == NULL) {
   /* failed to open topography file */
   return -31;
 }
 llat=89-llat;
 llon=-1*(llon+1);
 if(llon<0) llon=llon+360;
 position=(llat*bytes)+((2*llon)+48);
 error=fseek (fdi,position ,SEEK_SET ) ;
 if(error!=0) {
   /* error reading topography file */
   return -32;
 }
 fread(&buf,sizeof(buf),1,fdi);
 *ichar=(buf==0)?2:1;
 fclose(fdi);
 iret=0;
 if(*ichar==1) iret=71;
 return iret;

}

/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

extern int aodtv64_yddmy(int,int *,int *,int *);

int aodtv64_atcfoutputfile( char *inpath, int itype, char *outfile ) 
/* return atcf full path and file name given path and storm number.  Will use
   current record date and time info to create name.
   Inputs  : inpath - atcf file path name
             itype  - storm number
   Outputs : outfile- output filename
   Return  :  0 : o.k.
*/
{
    int rawflag,finalflag,stormID;
    int iday,imon,iyear,itime;
    char *filename; 

    filename=(char *)calloc((size_t)50,sizeof(char)); 
 
    rawflag=itype/1000;
    finalflag=(itype-(rawflag*1000))/100;
    stormID=itype-(rawflag*1000)-(finalflag*100);
    (void)aodtv64_yddmy(odtcurrent_v64->IR.date,&iday,&imon,&iyear);
    itime=odtcurrent_v64->IR.time/100;
    sprintf(filename,"%10s_%4d%2.2d%2.2d%4.4d_%2.2d_%3s","CIMSS_AODT",iyear,imon,iday,itime,stormID,"FIX");
    filename[strlen(filename)]='\0';
    strcpy(outfile,inpath); 
    strncat(outfile,filename,strlen(filename));
    
    free(filename);
    return 0;
}

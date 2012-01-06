/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv64_msg(int, int, float, float, char *, char *); 

int aodtv64_qmessage(int errorcode,int inint1,char *instrng, char *message)
/* Subroutine to load AODT specified diagnostic or error message into character string
   for output within APPL
   Inputs : errorcode : code for error (negative) or diagnostic (positive) message
            init1     : integer value for message
	    instrng   : character string for message (e.g. file name)
   Outputs: message   : error message string (diagnostic messages are stored for later output)
   Return : 0 : o.k.
*/
{
  char *retstrng; 
  float infloat1=0.0,infloat2=0.0;

  retstrng=(char *)calloc((size_t)5000,sizeof(char)); 

  /* assign floating point values (positions) for messages */
  if(((errorcode>=20)&&(errorcode<=29))||
     ((errorcode>=40)&&(errorcode<=59))) {
    infloat1=odtcurrent_v64->IR.latitude;
    infloat2=odtcurrent_v64->IR.longitude;
  }
  if((errorcode>=90)&&(errorcode<=99)) {
    infloat1=odtcurrent_v64->IR.warmlatitude;
    infloat2=odtcurrent_v64->IR.warmlongitude;
  }

  /* assign extra floating point value to integer value for output */
  if(errorcode==51) inint1=odtcurrent_v64->IR.autopos;
  if(errorcode==81) inint1=(int)(10.0*odtcurrent_v64->IR.sst);
  if(errorcode==91) inint1=(int)(10.0*odtcurrent_v64->IR.warmt);

  aodtv64_msg(errorcode,inint1,infloat1,infloat2,instrng,retstrng);

  if(errorcode<0) {
    /* error message, return string */
    strcpy(message,retstrng); 
    message[strlen(retstrng)]='\0';
  } else {
    /* diagnostic message, store value for in diagnostics_v64 string */
    strcat(diagnostics_v64,retstrng);
    message[0]='\0';
  }

  free(retstrng);

  return 0;
}

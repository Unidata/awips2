/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int recordnum;

int aodtv64_historygetnextrec(int reset,struct odtdata **histrec)
/* Subroutine to return next valid history file record structure
   within analysis dates defined by user in API.
   Inputs : reset variable (first record in history structure check)
   Outputs: pointer to next history structure entry
   Return : -1 : error reading history file
             0 : end of history file
*/
{
  double curtime;
  logical found=FALSE;

  /* reset starting record number if flag is set to 0 */
  if(reset==0) recordnum=0;

  if(reset==-1) {
    *histrec=odtcurrent_v64;
    if(*histrec == (struct odtdata *)NULL) {
      return -2;    /* empty/invalid history file */
    }
  } else {
    if(recordnum==0) {
      *histrec=odthistoryfirst_v64;  /* pointer to start of structure */
      /*if(*histrec<=0) {*/
      if(*histrec == (struct odtdata *)NULL) {
        return -1;    /* empty/invalid history file */
      }
    } else {
      *histrec=(*histrec)->nextrec;   /* pointer to next record */
      /* if(*histrec==0) { */
      if(*histrec == (struct odtdata *)NULL) {
        return 0;    /* end of history file */
      }
    }
    /* while((*histrec!=0)&&(!found)) { */
    while((*histrec != (struct odtdata *)NULL)&&(!found)) {
      curtime=aodtv64_calctime((*histrec)->IR.date,(*histrec)->IR.time);
      /* perform date/time check, if desired */
      if((curtime>=starttime_v64)&&(curtime<=endtime_v64)) {
        found=TRUE;
      } else {
        *histrec=(*histrec)->nextrec;
      }
      recordnum++;
    }
  }

  return 0;
}

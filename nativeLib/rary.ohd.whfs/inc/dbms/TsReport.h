
#ifndef TsReport_h
#define TsReport_h

#include <time.h>
#include <Xm/Xm.h>  /* just for Boolean */

typedef struct _TsReport
{
   
   time_t  validtime;
   time_t  basistime;
   
   double  value;
   
   Boolean isMissing;
   
} TsReport;

#endif

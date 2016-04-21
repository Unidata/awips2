#include <Xm/Xm.h>

#include "GeneralUtil.h"
#include "hv_refreshTimer.h"
#include "map_resource.h"
#include "PointDisplayControl.h"

/************************************************************************/

void addRefreshTimeOut( Widget top_widget )
{
     XtAppContext app = _get_map_context ( ) ;
     static XtIntervalId  xtid = 0 ;

     long minutes = REFRESH_MINUTES;
     int rv;
     char minute_string[128];
     int  gad_token_len=0, gad_value_len=0;
     
     gad_token_len = strlen("hv_refresh_minutes");
     get_apps_defaults("hv_refresh_minutes", &gad_token_len, minute_string, 
                       &gad_value_len);
     if (strlen(minute_string) > 0)
     {
          rv = sscanf(minute_string,"%ld", &minutes);
	  
	  if ((rv < 1) || (minutes < 1) || (minutes > 1440) )
	  {
	       minutes = REFRESH_MINUTES; 
	       fprintf(stderr,"Problem with hv_refresh_minutes env variable:\n"
		       "Using default refresh time of %ld minutes \n",minutes);
	  }
     }
          
     if (xtid)
     {
	 XtRemoveTimeOut(xtid);	
     }
   
     xtid = XtAppAddTimeOut(app,
		    (minutes * SECONDS_PER_MINUTE * MILLISECS_PER_SECOND),
		    refreshDataCycle, ( XtPointer ) top_widget );	    
     
     return;   
}

/************************************************************************/

void refreshDataCycle(XtPointer clientdata, XtIntervalId *id)
{
   Widget top_widget = ( Widget ) clientdata ;
   
   /*
        1. adds another timeout
	2. does the actual refreshing of the data
   */
   refreshStationData ( top_widget ) ;
   
   return;
}

/************************************************************************/


/************************************************************************/
/************************************************************************/
/************************************************************************/

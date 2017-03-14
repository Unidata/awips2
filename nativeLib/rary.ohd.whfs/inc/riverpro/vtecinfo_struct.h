/************************************************************************
   vtecinfo_struct.h
   
   PURPOSE
   Include file for structures containing vtec information.
   
   ***********************************************************************/

#ifndef VTECINFO_STRUCT_H
#define VTECINFO_STRUCT_H

#include <time.h>                      /* time library */

#include "DbmsDefs.h"                  /* database definitions */


/* values for states of inidividual vtecinfo */

#define VTEC_FULLINFO_UNMODIFIED  42  
#define VTEC_FULLINFO_MODIFIED    43


/* values for vtec significance field */

#define SIGNIF_WARNING   "W"
#define SIGNIF_WATCH     "A"
#define SIGNIF_ADVISORY  "Y"
#define SIGNIF_STATEMENT "S"


/* used for rec_action field */

#define NO_ACTION "N/A"


/* keep track of the previous event matching this geo id.
   this info is used for the recommendations.
   don't bother tracking the office and cat.
   the prev_active variable is used to define whether the previous
   event is active at the time the recommendations were determined.
   within a session, the prev event may become inactive since the 
   clock moves on and an event may expire naturally.  the prev_active 
   variable is needed to reflect the other prev info since the 
   recommendations are not computed regularly, but only at certain
   times within a session. */

typedef struct 
{      
   int		event_found;
   int		active;
   char		product_id[PRODUCT_LEN + 1];
   time_t	producttime;
   time_t       expiretime;
   
   char 	action[VTEC_ACTION_LEN + 1];          
   char 	phenom[VTEC_PHENOM_LEN + 1];         
   char 	signif[VTEC_SIGNIF_LEN + 1];         
   int		etn;                     
   time_t 	begintime;      
   time_t 	endtime;
   
   char 	severity[VTEC_SEVER_LEN + 1];         
   char		immed_cause[VTEC_CAUSE_LEN + 1];         
   time_t	risetime;      
   time_t	cresttime;      
   time_t	falltime;      
   char 	record[VTEC_RECORD_LEN + 1]; 
   char         rise_ts[SHEF_TS_LEN + 1];
   char         fall_ts[SHEF_TS_LEN + 1];
   char         crest_ts[SHEF_TS_LEN + 1];
   double       crest_value;   

} prev_vtecinfo_struct;


/* structure to store the vtec events for the the current 
   product definition.  the office_id field is not 
   stored with each event since it doesn't change.
   the product id and time are also not stored since they
   also are the same for all, and the product time isn't even
   known until issuance. the geomode is not stored since it
   is associated with the array element for this vtec structure.
   The type source for the rise,fall and crest
   are used to track the rise/fall/crest time is derived based on forecast
   data or observed data, thus determine the H-VTEC risetime,falltime and 
   cresttime */

typedef struct
{
   /* flag indicating the state of the info in this structure.
      the possible values are defined above. */

   int 		vtecinfo_state;
   
   
   /* recommendations determined by RiverPro.  of the VTEC fields, only
      the P-vtec action is determined/stored as a true recommendation.  the
      actual vtec fields are initialized separately to these recommended
      values. the other vtec fields do not have recommended values
      determined with the recommendations primarily because the
      ETN is dependent on the how many events are included */
      
   char 	rec_action[VTEC_ACTION_LEN + 1];             
   int		rec_prod_reason;
   int		rec_prod_index;        
  
   
   /* P-vtec line info. if a double_pvtec situation is requested, this line
      of info is actually used to represent the second of the two p-vtec lines.
      the office id is manageed externally via the sevice backup switching
      mechanisms, while the geo id is */
  
   char         vtec_cat[VTEC_PRODMODE_LEN + 1]; 
   char 	action[VTEC_ACTION_LEN + 1];          
   char 	phenom[VTEC_PHENOM_LEN + 1];         
   char 	signif[VTEC_SIGNIF_LEN + 1];         
   int		etn;                     
   time_t 	begintime;      
   time_t 	endtime;
      
      
   /* H-vtec line info */
      
   char         geoid[VTEC_GEOID_LEN + 1];
   char 	severity[VTEC_SEVER_LEN + 1];         
   char		immed_cause[VTEC_CAUSE_LEN + 1];         
   time_t	risetime;      
   time_t	cresttime;      
   time_t	falltime;      
   char 	record[VTEC_RECORD_LEN + 1]; 
   char         rise_ts[SHEF_TS_LEN + 1];
   char         fall_ts[SHEF_TS_LEN + 1];
   char         crest_ts[SHEF_TS_LEN + 1];
   double       crest_value;       
   
   time_t       expiretime;
   
   
   /* previous event info for each possible event tracked;
      i.e. the FL.W warning, FL.A watch, and FL.Y advisory.
      this is the previous event, whether it be active or 
      inactive */
   
   prev_vtecinfo_struct   prev_flw;
   prev_vtecinfo_struct   prev_fla;
   prev_vtecinfo_struct   prev_fly;
   
    
   /* info for the previous event, but for the most recent 
      inaactive event.  this info is needed for determining
      the time window for which to retrieve stage/discharge
      info.  only stage/discharge data after the most recent
      inactive product is used */
   
   prev_vtecinfo_struct   inactive_flw;
   prev_vtecinfo_struct   inactive_fla;
   prev_vtecinfo_struct   inactive_fly;
               
} vtecinfo_struct;



/* define information used in the VTEC gui operations.
   includes index to vtecinfo for included fp; 
   names of included fp ids */

#define AREA_LEN 60


typedef struct 
{
   int	infoindex;              
   char	area_str[AREA_LEN];  
} vtecindex_struct;



   
#endif

/*****************************************************************
   load_variable_special.h
   
   PURPOSE
   Include file for function prototypes.
 
   NOTES
   
   ***************************************************************/

#ifndef LOAD_VARIABLE_SPECIAL_H
#define LOAD_VARIABLE_SPECIAL_H

#include <time.h>


#include "rpf_general_defs.h"        /* definitions */
#include "rpf_err_defs.h" 
#include "template_err_defs.h"
#include "template_defs.h"
#include "format_vals.h"

#include "fp_struct.h" 
#include "county_struct.h"              /* structures */
#include "misc_struct.h" 
#include "pcc_struct.h"
#include "vtecinfo_struct.h"

#include "rpf_converts.h"            /* other functions protos */
#include "rpf_logs.h"
#include "check_crs_info.h"
#include "format_vals.h"

#include "Zonenum.h"                 /* database */
#include "Countynum.h"
#include "Counties.h"
#include "Riverstat.h"
#include "Location.h"
#include "State.h"
#include "Descrip.h"
#include "DbmsUtils.h"
#include "Admin.h"
#include "LocArea.h"

#include "NWRTransmitter.h"
#include "LocTransmit.h"
#include "Countynum.h"
#include "CountyTransmit.h"
		    

void build_countylist(const grp_struct		*grp,
	              const fp_struct		*fp,
		            misc_struct		*misc,
		      const pcc_struct		*pcc,
		            char		*longstring);

void write_counties(const	int		numlist,
	            		char 		*statecounty[],
	       			char		*longstring,
				int             state_al_found);

void build_riverlist(const grp_struct	*grp,
	             const fp_struct	*fp,
		           misc_struct	*misc,
		     const pcc_struct	*pcc,
			   char		*longstring);

void write_rivers(const	int		numlist,
	            	char 		*rivers[],
	       		char		*longstring);

void build_grplist(const grp_struct	*grp,
		   const fp_struct	*fp,
		         misc_struct	*misc,
		   const pcc_struct	*pcc,
		   char			*longstring);

void build_grp_fplist(const int		grpindex,
		      const grp_struct	*grp,
		      const fp_struct	*fp,
		            misc_struct	*misc,
		      const pcc_struct	*pcc,
		      char		*longstring,
		      const int         numcnty,
		      const county_struct     *cnty);

void build_grps_fplist(const grp_struct		*grp,
		       const fp_struct		*fp,
		             misc_struct	*misc,
		       const pcc_struct		*pcc,
		       char			*longstring,
		       const int                numcnty,
		       const county_struct      *cnty);



/* ------------------------------------------------- */


char * load_location_str(const	char 	*lid,
			 char		*fieldname);

char * load_riverstat_str(const char 	*lid,
			  char		*fieldname);

float load_riverstat_float(const char *lid,
			   char		*fieldname);

char * load_descrip_str(const char 	*lid, 
			char		*fieldname);

int alpha_compare(const void *name1, 
                  const void *name2);
		  
char * load_action_str(const int              fpindex,
                             vtecinfo_struct  *vtecinfo,
		             const pcc_struct *pcc);	
		      
		      	  
char * load_eventtime_str(const int          fpindex,
                          vtecinfo_struct  *vtecinfo,
		          const pcc_struct  *pcc,
			  misc_struct     *misc);	
		       

char * load_officename_str(char *fieldname);

char * load_stgflow_name(char *locid);
			
char * load_stgflow_units(char *locid);
			 
void build_loccntylist(char	*locid,
		     char     *longstring);
			 
void build_statecntylist_byaction(const int    		fpindex1,
	                      const fp_struct	     *fp,
			      const grp_struct       *grp,
			            misc_struct      *misc,
			      const pcc_struct       *pcc,
			      vtecinfo_struct      *vtecinfo,
			      char	  	     *longstring);
			      
void build_statelist_byaction(const int    		 fpindex1,
	                      const fp_struct		     *fp,
			      const grp_struct               *grp,
			            misc_struct              *misc,
			      const pcc_struct               *pcc,
			      vtecinfo_struct          *vtecinfo,
			      char	  	              *longstring);
			      
void write_statecounties(const	int	numlist,
	            	 char 		*statecounty[],
	       		 char		*longstring,
			 int            state_al_found);
				
				
				
char * load_locarea_str(const	char 	*lid,
		        char		*fieldname);
		          
		       

void build_riverlist_byaction(const int    		 fpindex1,
	                      const fp_struct		 *fp,
			      const grp_struct           *grp,
			            misc_struct          *misc,
			      const pcc_struct           *pcc,
			      vtecinfo_struct       *vtecinfo,	       
			      char	  	    *longstring);
			      	
			  
char *load_impcomp_units(const fp_struct   *fp,   
                         const int          fpindex);
		       			  		       
float  load_loclat(char *locid);
float  load_loclon(char *locid);		       
		       
#endif

/*****************************************************************
   build_ugc.h
   
   PURPOSE
   Include file for function prototypes.
 
   NOTES
   
   ***************************************************************/

#ifndef BUILD_UGC_H
#define BUILD_UGC_H

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


void build_ugc(grp_struct	*grp,
	       fp_struct	*fp,
	       pcc_struct	*pcc,
	       misc_struct	*misc,
	       vtecinfo_struct	*vtecinfo,
	       char		county_zone_flag,	
	       char 		*longstring);

void build_ugc_nwr(int			numfps,
		   fp_struct		*fp,
		   misc_struct		*misc,
		   NWRTransmitter	*ntransPtr,
		   LocTransmit		*loctransHead,
		   char 		*area_codes);

void build_ugc_segment(grp_struct	*grp,
	               fp_struct	*fp,
		       pcc_struct	*pcc,
	               misc_struct	*misc,
		       vtecinfo_struct	*vtecinfo,
                       int              grpindex_used,
                       int              fpindex_used,
	               char		county_zone_flag,	
	       	       char 		*longstring);
		       
void build_ugc_segment_county(county_struct    	*cnty,
                              int              	cntyindex,
			      misc_struct	*misc,
			      char             	*longstring);
                       
void write_ugc_areas(int		numstates,
	             char		county_zone_flag,
	             char 		*stateid[],
	             int 		*ugcmask[],
	             misc_struct	*misc,
	             int		nwr_flag,
	             char		*longstring,
		     int		*linepos);
	       
char * write_ugc_expiretime(misc_struct  	*misc,
                            pcc_struct	 	*pcc,
			    vtecinfo_struct	*vtecinfo,
			    int			fpindex);
			    
		       			  		       
		      		       
#endif

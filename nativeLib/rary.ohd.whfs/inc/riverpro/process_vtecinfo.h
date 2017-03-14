/*********************************************************************
   process_vtecinfo.h
   
   PURPOSE
   
   ******************************************************************/

#ifndef PROCESS_VTECINFO_H
#define PROCESS_VTECINFO_H

#include <stdio.h>

#include "rpf_err_defs.h"            /* definitions */
#include "rpf_general_defs.h"
#include "rpf_file_defs.h"
#include "pcc_keyvalue_defs.h"

#include "vtecinfo_struct.h"         /* structures */
#include "fp_struct.h"               
#include "grp_struct.h"
#include "county_struct.h"
#include "misc_struct.h"
#include "pcc_struct.h"
		     
#include "rpf_converts.h"            /* protos */
#include "rpf_logs.h"
#include "select_pid_and_fps.h"
#include "get_stages.h"
#include "DbmsUtils.h"

#include "VTECevent.h"               /* database tables */
#include "VTECpractice.h"

/* constants */

#define VTEC_TIMECODE_LEN   12
#define	MAX_ETN           9999

#define DEFAULT_VTECRECORD_STAGE 2.0
#define DEFAULT_VTECRECORD_FLOW  5000.
#define DEFAULT_ENDTIME_SHIFT_HOURS 6
#define DEFAULT_FLYENDTIME_SHIFT_HOURS 36

#define ENDTIME_WITHIN  1800   /* 30 minutes in seconds */

#define ETN_QCMESSAGE_MAXLEN 5000

#define VTEC_BEGINTIME     10
#define VTEC_ENDTIME       11
#define VTEC_OTHERTIME     12


/* prototypes */

void init_vtecinfo(int			numvtecs,
                   pcc_struct           *pcc,
		   vtecinfo_struct	*vtecinfo);
		   
void init_vtecevent(pcc_struct          *pcc,
		    vtecinfo_struct	*vtecevent);

void load_prevevent_info(int 			numfps,
			 fp_struct 		*fp,
	                 misc_struct            *misc,
			 vtecinfo_struct	*vtecinfo);
		    
void rec_vtecproduct(int		numfps,
		     fp_struct		*fp,
		     int		numgrps,
		     grp_struct		*grp,
		     misc_struct	*misc,
		     vtecinfo_struct	*vtecinfo);
		
void load_vtecinfo_lines(int			numfps, 
			 fp_struct		*fp,
			 int 			numgrps,
			 grp_struct		*grp,
			 int			numcnty,
			 county_struct 		*cnty,
			 pcc_struct		*pcc,
			 misc_struct		*misc,
			 vtecinfo_struct	*vtecinfo);

void load_vtecinfo_1stline(fp_struct		*fp,
			   grp_struct		*grp,
			   county_struct 	*cnty,
			   pcc_struct		*pcc,
			   misc_struct		*misc,
			   VTECevent		*eventHead,
			   int			fpindex,
			   int			numfps,
			   vtecinfo_struct	*vtecinfo);

void load_vtecinfo_2ndline(fp_struct		*fp,
			   grp_struct		*grp,
			   county_struct 	*cnty,
			   pcc_struct		*pcc,
			   misc_struct		*misc,
			   int			fpindex,
			   vtecinfo_struct	*vtecinfo);
			   
VTECevent * get_previous_event(char		*geoid,
		               VTECevent	*eventHead,
			       char		*mode_filterstr,
			       char		*signif_str);
			       
void load_previous_event(VTECevent	        *prevPtr,
			 prev_vtecinfo_struct 	*prev_xxx);  
			       
			 
void copy_previous_event(prev_vtecinfo_struct   prev_xxx_vtecinfo, 
                          prev_vtecinfo_struct   *prev_vtecinfo);
			  
VTECevent * get_previous_inactive_event(char		*geoid,
		                        VTECevent	*eventHead,
			                char		*mode_filterstr,
			                char		*signif_str,
					time_t		curtimet);
			 			 
int check_if_event_active(char			*prev_action,
			  time_t		prev_endtime,
			  time_t		curtimet);

int compute_new_etn(int			fpindex,
		    pcc_struct		*pcc,
		    misc_struct		*misc,
		    int			num_events,
		    vtecinfo_struct	*vtecinfo,
		    VTECevent		*eventHead);

int find_max_prev_etn(VTECevent *eventHead,
		      char	*signif_filter,
		      time_t	current_time);

char * compute_vtec_severity(int maxcat);

char * compute_vtec_record(float max_value,
                           float record_value,
			   char  *primary_pe);

void  create_vteclines(vtecinfo_struct	*vtecinfo,
		       misc_struct	*misc,
		       int		fpindex,
		       fp_struct        *fp,
		       char		*Pvtec_line,
		       char		*Hvtec_line);

int set_num_events(pcc_struct 	*pcc,
		   int		numfps,
		   int		numgrps,
		   int		numcnty);

void save_vtec_events(int		numfps,
		      fp_struct		*fp,
		      int		numgrps,
		      grp_struct	*grp,
		      int		numcnty,
		      county_struct	*cnty,
		      pcc_struct	*pcc,
		      misc_struct	*misc,
		      vtecinfo_struct	*vtecinfo);
		      
void save_corvtec_events(int		 numfps,
			 fp_struct	 *fp,
			 int		  numgrps,
			 grp_struct	 *grp,
			 int		 numcnty,
			 county_struct	 *cnty,
			 pcc_struct	 *pcc,
			 misc_struct	 *misc,
			 vtecinfo_struct *vtecinfo);		      

int check_if_vtec(pcc_struct	*pcc);

char *format_vtectime(time_t		timeval,
		      misc_struct	*misc,
		      int		field_index);

char *format_rtime(time_t timeval);

time_t set_fallbelow_time(int		fpindex,
			  fp_struct 	*fp,
			  int		shift_flag); 
			  
time_t setFLYendtime(misc_struct  *misc);			  

float get_flood_level(int	fpindex,
		      fp_struct	*fp);



/* former, retired versions */

void load_previous_eventOLD(int 		fpindex,
			 char			*signif_str,
			 VTECevent		*prevPtr,
                         vtecinfo_struct 	*vtecinfo);
			 
void copy_previous_eventOLD(int               i, 
                         vtecinfo_struct      *vtecinfo, 
			 char                 *signif_str, 
                         prev_vtecinfo_struct *prev_vtecinfo);

#endif

/*********************************************************************
   load_variable_value.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef LOAD_VARIABLE_VALUE_H
#define LOAD_VARIABLE_VALUE_H

#include "rpf_err_defs.h"           /* definitions */
#include "rpf_general_defs.h"
#include "function_defs.h"
#include "template_defs.h"
#include "template_err_defs.h"

#include "fp_struct.h"              /* structures */
#include "grp_struct.h" 
#include "county_struct.h"
#include "misc_struct.h"
#include "temp_item_struct.h"
#include "temp_records_struct.h"
#include "pcc_struct.h"
#include "fcsttrend_info_struct.h"

#include "rpf_logs.h"               /* other functions protos */
#include "rpf_converts.h"
#include "build_ugc.h"
#include "get_stages.h"
#include "load_variable_special.h"

void load_variable_value(fp_struct		*fp,                       
			 grp_struct  		*grp,
			 const int              numcnty,
			 county_struct          *cnty,
			 misc_struct	*misc,
			 vtecinfo_struct       *vtecinfo,
			 pcc_struct	*pcc,
			 varinfo_struct         *varinfo,
			 const	int 		idindex,
			 char			locid[],
			 const	spectime_struct	*spectime,
			 const	int		spectime_index,
			 values			*rawvalue,
			 char			dqcode[],
			 char			longstring[]);

void load_fp_variable_value(int		varindex,
			    const int		fpindex,
			    	  char		locid[],
			          fp_struct	*fp,
				  vtecinfo_struct *vtecinfo,
                                  pcc_struct     *pcc,
			   	  misc_struct    *misc,				  
			   	  values 	*rawvalue,
			          char		longstring[]);
				  
void load_locinfo_variable_value(int         		varindex,
			    	 char		   	locid[],
			   	 values 	  	*rawvalue,
			         char		   	longstring[]);				 

void load_grp_variable_value(int		varindex,
			     const int		grpindex,
			           grp_struct	*grp,
			   	   values 	*rawvalue,
			           pcc_struct   *pcc,
			     const int          numcnty,
			     county_struct 	*cnty);

void load_pcc_variable_value(int		varindex,
			     const int		fpindex,
			           pcc_struct	*pcc,
			   	   values 	*rawvalue);

void load_stagegrp_variable_value(int		varindex,
	 		          const int		grpindex,
			          grp_struct		*grp,
				  misc_struct	*misc,
			   	        values 		*rawvalue,
				  county_struct   	*cnty,
				  pcc_struct      *pcc);


void load_stage_ofp_variable_value(int		varindex,
	 		           const int		fpindex,
				         fp_struct	*fp,
				         misc_struct	*misc,
			   	         values 	*rawvalue,
					 char		dqcode[]);

void load_stage_ffp_variable_value(int		varindex,
	 		           const int		fpindex,
				         fp_struct	*fp,
				   	 misc_struct	*misc,
			   	         values 	*rawvalue,
					 char		dqcode[]);

void load_stage_xfp_variable_value(int		varindex,
	 		           const int		fpindex,
				         fp_struct	*fp,
				         misc_struct	*misc,
			   	         values 	*rawvalue);

void load_stagespec_variable_value(int			varindex,
				   const int			fpindex,
				         fp_struct		*fp,
				   const spectime_struct 	*spectime,
				   const int			spectime_index,
					 values 		*rawvalue,
					 char			dqcode[]);

void load_misc_variable_value(int		varindex,
			      const int		grpindex,
			      grp_struct	*grp,
			      fp_struct	  	*fp,
			      misc_struct	*misc,
			      pcc_struct	*pcc,
			      values 	  	*rawvalue,
			      char	  	longstring[],
			      int           	numcnty,
			      county_struct 	*cnty,
			      vtecinfo_struct   *vtecinfo);
			      
void load_stage_trendfp_variable_value(int   varindex,
                                       const int   fpindex,
				       fp_struct   *fp,
				       char        longstring[],
				       misc_struct *misc,
				       pcc_struct  *pcc);
				       

void load_event_variable_value(int		varindex,
		  	         const int		fpindex,
			    	  char		locid[],
			          fp_struct	*fp,
				  grp_struct    *grp,
				  values        *rawvalue,
				  vtecinfo_struct *vtecinfo,
                                  pcc_struct     *pcc,
			          misc_struct    *misc,				  			   	
			          char		longstring[]);				       
				       
char *load_trendfp_phrase(int                   k,
                          const int             fpindex,
			  fp_struct             *fp,
			  fcsttrend_info_struct *fcsttrend_info);
			  
char *load_trendfp_stage(int        k,
                         const int  fpindex,
			 fp_struct  *fp);	
			 
char *load_trendfp_time(int               k,
                        const int         fpindex,
			fp_struct         *fp,
			misc_struct 	*misc);			 		  				       
                                       			      


#endif

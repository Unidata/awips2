/*********************************************************************
   order_fps.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef ORDER_FPS_H
#define ORDER_FPS_H

#include <stdio.h>

#include "rpf_err_defs.h"            /* definitions */
#include "rpf_general_defs.h"       
#include "pcc_keyvalue_defs.h"

#include "fp_struct.h"               /* structures */
#include "grp_struct.h"
#include "county_struct.h"
#include "vtecinfo_struct.h" 

#include "rpf_logs.h"                /* other function protos */


void order_fps(const	int		numfps,
	       const	fp_struct	*fp,
	       const	int		numgrps,
	       const    grp_struct	*grp,
	       vtecinfo_struct         *vtecinfo,
	       const	int		order_choice,
	       		int		*fporder);

void order_fp_alpha(const int		numfps,
		    const fp_struct	*fp,
		    const int		numgrps,
		    const grp_struct	*grp,
		          int		*fporder);

void order_fp_grpalpha(const int		numfps,
		       const fp_struct		*fp,
		       const int		numgrps,
		       const grp_struct		*grp,
			     int		*fporder);

void order_fp_grpfp(const int		numfps,
		    const fp_struct	*fp,
		    const int		numgrps,
		    const grp_struct	*grp,
		          int		*fporder);

void order_grps(const	int		numgrps,
	        const   grp_struct	*grp,
		vtecinfo_struct        *vtecinfo,
	        const	int		order_choice,
	       		int		*grporder);

void order_grp_alpha(const int		numgrps,
		     const grp_struct	*grp,
		    	   int		*grporder);

void order_grp_grp(const	int		numgrps,
	           const	grp_struct	*grp,
		            	int		*grporder);
				
void order_fp_action(const int		numfps,
		    const fp_struct	*fp,
		    const int		numgrps,
		    const grp_struct	*grp,
		    vtecinfo_struct    *vtecinfo,
		          int		*fporder);	

void order_grp_action(const int		   numgrps,
	             const grp_struct     *grp,
		     vtecinfo_struct     *vtecinfo,
		     int		 *grporder);
				
							  			
void order_cntys(const	int		numcntys,
	        const county_struct    *cnty,
		vtecinfo_struct       *vtecinfo,
	        const	int		order_choice,
	       		int	       *cntyorder);
			
void order_cnty_alpha(const int		    numcntys,
		     const county_struct   *cnty,
		    	   int		   *cntyorder);
			   
void order_cnty_action(const int	    numcntys,
		     const county_struct   *cnty,
		     vtecinfo_struct   *vtecinfo,
		    	   int		*cntyorder);
			   			   			
#endif

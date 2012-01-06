/*********************************************************************
   create_product.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef CREATE_PRODUCT_H
#define CREATE_PRODUCT_H

#include <stdio.h>

#include "rpf_err_defs.h"            /* definitions */
#include "rpf_general_defs.h"
#include "rpf_file_defs.h"
#include "template_defs.h"

#include "fp_struct.h"               /* structures */
#include "grp_struct.h"
#include "county_struct.h"
#include "vtecinfo_struct.h"
#include "misc_struct.h"
#include "pcc_struct.h"
#include "temp_info_struct.h"
#include "temp_item_struct.h"
		     
#include "check_conditions.h"       /* protos */  
#include "create_tabular_section.h"
#include "load_variable_value.h"
#include "order_fps.h"
#include "check_crs_info.h"
#include "select_pid_and_fps.h"
#include "load_phrase_data.h"
#include "build_ugc.h"
#include "process_vtecinfo.h"

#include "format_vals.h"
#include "buf_template.h"

#include "rpf_converts.h"
#include "rpf_logs.h"

#include "NWRTransmitter.h"
#include "LocTransmit.h"

#include "LoadUnique.h"

void create_product(int			numfps,
		    fp_struct		*fp,
		    int			numgrps,
		    grp_struct		*grp,
		    int                 numcnty,
		    county_struct       *cnty,
		    vtecinfo_struct	*vtecinfo,
		    pcc_struct		*pcc,
		    misc_struct		*misc,
		    char		output_file[]);


void create_nwr_product(int                   numfps,
                        fp_struct             *fp,
			int                   numgrps,
			grp_struct            *grp,
			int                   numcnty,
			county_struct         *cnty,
			misc_struct           *misc,
			vtecinfo_struct       *vtecinfo,
                        pcc_struct            *pcc,
                        FILE                  *outfile_ptr,
			template_info_struct  *template_info);

void create_segment_product(int                   numfps,
			    fp_struct             *fp,
			    int                   numgrps,
			    grp_struct            *grp,
			    int                   numcnty,
			    county_struct         *cnty,                   
			    vtecinfo_struct	  *vtecinfo,
			    pcc_struct            *pcc,
			    misc_struct           *misc,
			    FILE                  *outfile_ptr,
			    template_info_struct  *template_info);
			
void create_regular_product(int                   numfps,
			    fp_struct             *fp,
			    int                   numgrps,
			    grp_struct            *grp,
			    int                   numcnty,
			    county_struct         *cnty,                        
                            pcc_struct            *pcc,
			    misc_struct           *misc,
			    vtecinfo_struct       *vtecinfo,
			    FILE                  *outfile_ptr,
			    template_info_struct  *template_info);
			    
			    
void create_nwr_product_body(int		  numfps,
                             int		  numgrps,
			     int                  numcnty,
		    	     fp_struct	          *fp,			     
			     grp_struct	          *grp,
			     county_struct        *cnty,
			     pcc_struct 	  *pcc,
			     misc_struct	  *misc,
			     vtecinfo_struct      *vtecinfo,
			     FILE		  *outfile_ptr,			    
			     template_info_struct *template_info);
			     
void create_segment_product_body(int		      	numfps,
		    		 fp_struct	      	*fp,     
                        	 int		      	numgrps,
				 grp_struct	      	*grp,
				 int                  	numcnty,
				 county_struct        	*cnty,
				 vtecinfo_struct	*vtecinfo,
				 pcc_struct 	      	*pcc,
				 misc_struct	      	*misc,
				 FILE		      	*outfile_ptr,
				 template_info_struct 	*template_info);
				 
void create_regular_product_body(int		      numfps,
                                 int		      numgrps,
				 int                  numcnty,
		    		 fp_struct	      *fp,			     
				 grp_struct	      *grp,
				 county_struct        *cnty,
				 pcc_struct 	      *pcc,
				 misc_struct	      *misc,
				 vtecinfo_struct      *vtecinfo,
				 FILE		      *outfile_ptr,
				 template_info_struct *template_info);
				 				 			     			    			
void create_segment_section(int			numfps,
			    fp_struct		*fp,
			    int			numgrps,
			    grp_struct		*grp,
			    int                 numcnty,
			    county_struct       *cnty,
			    vtecinfo_struct	*vtecinfo,
			    pcc_struct		*pcc,
			    misc_struct		*misc,
			    FILE		*outfile_ptr,
			    template_info_struct *template_info);

void create_header_section(int			numfps,
			   fp_struct		*fp,
			   int			numgrps,
			   grp_struct		*grp,
			   int                  numcnty,
			   county_struct        *cnty,
			   pcc_struct  		*pcc,
			   misc_struct		*misc,
			   vtecinfo_struct      *vtecinfo,
			   template_info_struct	*template_info,
			   FILE 		*outfile_ptr);

void create_header_nwr(int			numfps,
		       fp_struct		*fp,
		       int			numgrps,
		       grp_struct		*grp,
		       pcc_struct  		*pcc,
		       misc_struct		*misc,
		       NWRTransmitter		*ntransPtr,
		       LocTransmit		*loctransPtr,
		       int			tower_wildcard,
		       FILE 			*outfile_ptr);

void create_summary_prologue(fp_struct			*fp,
			     int			numgrps,
			     grp_struct			*grp,
			     int                        numcnty,
			     county_struct              *cnty,
			     pcc_struct			*pcc,
			     misc_struct		*misc,
			     template_info_struct	*template_info,
			     FILE 			*outfile_ptr);

void create_summary_body(fp_struct		*fp,
			 int			numgrps,
			 grp_struct		*grp,
			 int                    numcnty,
			 county_struct          *cnty,
			 pcc_struct		*pcc,
			 misc_struct		*misc,
			 vtecinfo_struct        *vtecinfo,
			 template_info_struct	*template_info,
			 FILE 			*outfile_ptr,
			 int                   	grpindex_used);

void create_basis_section(fp_struct		*fp,
			  pcc_struct		*pcc,
			  misc_struct		*misc,
			  template_info_struct 	*template_info,
			  FILE			*outfile_ptr);

void create_headline_section(int		numfps,
			   fp_struct		*fp,
			   int			numgrps,
			   grp_struct		*grp,
			   int                  numcnty,
			   county_struct        *cnty,
			   pcc_struct  		*pcc,
			   misc_struct		*misc,
			   vtecinfo_struct      *vtecinfo,
			   template_info_struct	*template_info,
			   FILE 		*outfile_ptr);



void create_pointspecific_section(int			numfps,
				  fp_struct		*fp,
				  int			numgrps,
				  grp_struct		*grp,
				  int                   numcnty,
				  county_struct         *cnty,
				  pcc_struct		*pcc,
				  misc_struct		*misc,
				  vtecinfo_struct       *vtecinfo,
				  template_info_struct	*template_info,
				  FILE 			*outfile_ptr);

void create_pointspecific_segment_section(fp_struct		*fp,
				          int			numgrps,
				          grp_struct		*grp,
					  int                   numcnty,
					  county_struct         *cnty,
					  pcc_struct		*pcc,
					  misc_struct		*misc,
					  vtecinfo_struct       *vtecinfo,
					  template_info_struct	*template_info,
					  FILE 			*outfile_ptr,
					  int                   pt_to_proc);

void create_roundup_subsection(int 			fpindex,
			       fp_struct		*fp,
			       grp_struct		*grp,
			       int                      numcnty,
			       county_struct            *cnty,
			       pcc_struct		*pcc,
			       misc_struct		*misc,
			       vtecinfo_struct          *vtecinfo,
			       int			*line_written,
			       template_info_struct	*template_info,
			       FILE 			*outfile_ptr);

void create_impact_subsection(int 			fpindex,
			      fp_struct			*fp,
			      grp_struct		*grp,
			      int                       numcnty,
			      county_struct             *cnty,
			      pcc_struct		*pcc,
			      misc_struct		*misc,
			      vtecinfo_struct           *vtecinfo,
			      int			*line_written,
			      template_info_struct	*template_info,
			      FILE 			*outfile_ptr);

void create_comparison_subsection(int 			fpindex,
				  fp_struct		*fp,
				  grp_struct		*grp,
				  int                   numcnty,
				  county_struct         *cnty,
				  pcc_struct		*pcc,
				  misc_struct		*misc,
				  vtecinfo_struct       *vtecinfo,
				  int			*line_written,
				  template_info_struct	*template_info,
				  FILE 			*outfile_ptr);

void create_cta_section(fp_struct		*fp,
			pcc_struct		*pcc,
			misc_struct		*misc,
			template_info_struct	*template_info,
			FILE 			*outfile_ptr); 

void get_impact_chosen(int                     fpindex,
		       fp_struct               *fp,
		       pcc_struct              *pcc,
		       misc_struct             *misc,
		       int                     *impact_found);

			 			      
void create_summary_body_county(fp_struct             *fp,
                                int                   numgrps,
				grp_struct            *grp,
				int                   numcnty,
				county_struct         *cnty,
				pcc_struct            *pcc,
				misc_struct           *misc,
				vtecinfo_struct       *vtecinfo,
				template_info_struct  *template_info,
				FILE                  *outfile_ptr,
				int                   cntyindex);		 	      			 	       

int get_test_setting(int workstation_mode,
                     int vtec_flag,
		     int vtec_mode);

#endif

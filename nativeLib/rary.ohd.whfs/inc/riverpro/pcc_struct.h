/************************************************************************
   pcc_struct.h
   
   PURPOSE
   Include file for structures on the product content control information.
   
   NOTES
   Some of the product level information is contained in the following
   other variables: product_chosen, product_reason, fps_included
   
   MODIFICATION HISTORY
           DATE         PROGRAMER          DESCRIPTION
           02/2002        Jingtao Deng      Add vtec_flag  
           02/2002        R.Erb             Add timeZoneFlag 
           10/2003        Jingtao Deng      Add vtec_cat 
           03/2004        Jingtao Deng      Add ugc_mode and
		                            expiration_time
           04/2004        Jingtao Deng      add headline structure
                                            remove include_prologueflag
			                    and prologue_template() in
				            the summary structure.
           05/2005                         add flow_lwindow, flow_uwindow,
	                                   flow_offset_filter in impact
					   structure and comparison struct					    
   ***********************************************************************/


#ifndef PCC_STRUCT_H
#define PCC_STRUCT_H

#include <time.h>

#include "rpf_general_defs.h"   /* general rpf program definitions */
#include "template_defs.h"

#include "DbmsDefs.h"



/* included are the product index and included points, if specified, 
   and the number of product sections and an index to each
   of the product sections included; also included are the number of
   point-specific-section subsections and an index to each of the
   subsections. these are included in the product structure, even though
   they apply to the pss section, because there is no structure for
   the pss section - there are only ones for its subsections. */
/* note that the prod_index is a poor name, it is actually the
   product category index */

typedef struct 
{
   char	prod_categ[PROD_CATEG_LEN + 1];   
   char product_cnx[PRODUCT_LEN + 1];
   
   int include_fp_flag;
   int *include_fp; 
     
   int num_sections;
   int section_index[MAX_SECTIONS];
   int num_ps_subsections;
   int ps_subsection_index[MAX_PS_SUBSECTIONS];
   
   int tabular_within_flag;
   int grpfp_order;
   int tcase;  /* text case*/
   
   int nwr_flag;
   int segment_mode;
   int vtec_flag;
   
   int	vtec_cat;
   char vtec_phenom[VTEC_PHENOM_LEN + 1];
   char vtec_default_signif[VTEC_SIGNIF_LEN + 1];
   int  ugc_mode;
   float expiration_time;
   
   int timeZoneFlag;
   
} product_struct;


/* this structure contains info related to the product header;
   this includes the template to use; note that the issuance time
   is not specified in the structure - its in misc_struct.
   store the periodicity in minutes, store the active and delete
   as literal strings matching the CRS definitions, store the confirm,
   interrupt as booleans to be translated into CRS definitions,
   and store the alert flag as coded integer */

typedef struct
{
   char template[MAXLEN_TEMPLATENAME];
   
   char	nwr_msg_format[NWR_FORMAT_LEN + 1];
   int	nwr_periodicity;
   char	nwr_active[CODE_LEN + 1];
   char	nwr_delete[BOOL_LEN + 1];
   int	nwr_confirm;
   int	nwr_interrupt;
   int	nwr_alert_index;   
} header_struct;


/* this structure contains info related to the summary section;
   this includes the template to use */

typedef struct
{
   int	includeflag;
   char default_template[MAXLEN_TEMPLATENAME];
   char **template;
} summary_struct;


/* this structure contains info related to the general headline;
   this include the template to use */
   
typedef struct
{
    int  includeflag;
    char template[MAXLEN_TEMPLATENAME];
} headline_struct; 
      

/* this structure contains info related to the basis section;
   this includes the template to use */

typedef struct
{
   int	includeflag;
   char template[MAXLEN_TEMPLATENAME];
} basis_struct;


/* this structure contains info related to the tabular section; 
   this includes the template to use */

typedef struct
{
   int	includeflag;
   char template[MAXLEN_TEMPLATENAME];
} tabular_struct;


/* this structure contains info related to the dataroundup subsection; 
   this includes the template to use and whether to include the subsection 
   for a particular forecast point */

typedef struct
{
   int	includeflag;
   char default_template[MAXLEN_TEMPLATENAME];
   char **template;
} roundup_struct;


/* this structure contains info related to the impact subsection;
   the refstage_index and the system date are used for determining
   the stage range for each forecast point;
   the lower and upper range is determined while the other info is read in */

typedef struct
{
   int	includeflag;
   char template[MAXLEN_TEMPLATENAME];
   int	refstage_index;
   float fs_filter_offset;
   float stage_lwindow;
   float stage_uwindow;
   int	search_type;
   float flow_lwindow;
   float flow_uwindow;
   float fq_filter_offset;
      
   /* these variables are not entered into the pcc file per say but 
      instead are computed or somehow support the processing */
   
   float *lower_stage;
   float *upper_stage;
   int *range_set;
   float *refstage;
   int *reftype;
} impact_struct;


/* this structure contains info related to the historical comparison
   subsection; the stagesearch index and window are used for determining
   the compare_time for each forecast point */

typedef struct
{
   int	includeflag;
   char template[MAXLEN_TEMPLATENAME];
   int refstage_index;
   float fs_filter_offset;
   float stage_lwindow;
   float stage_uwindow;
   int lookback_years;
   int search_type;
   float flow_lwindow;
   float flow_uwindow;
   float fq_filter_offset;
      
   /* these variables are not entered into the pcc file per say but 
      instead are computed or somehow support the processing */
   
   char **compare_date;
   float *compare_stage;
   int *compare_set;   
   float *refstage;
   int	*reftype;
} histcomp_struct;


/* this structure contains info related to the call-to-action information */

typedef struct
{
  int	includeflag;
  int	skipline_flag;
  int	num_ctas;
  char	template[MAX_CTAS][MAXLEN_TEMPLATENAME];
} cta_struct;


/* this structure contains all the product content control info */

typedef struct 
{
   product_struct	product;
   header_struct	header;
   headline_struct      headline;
   summary_struct	summary;
   basis_struct		basis;
   tabular_struct	tabular;
   roundup_struct	roundup;
   impact_struct	impact;
   histcomp_struct	comparison;
   cta_struct		cta;
} pcc_struct;


#endif

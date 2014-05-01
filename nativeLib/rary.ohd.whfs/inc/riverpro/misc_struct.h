/************************************************************************
   misc_struct.h
   
   PURPOSE
   Include file for structures containing misc information.
     
   NOTES
   
  ***********************************************************************/

#ifndef MISC_STRUCT_H
#define MISC_STRUCT_H

#include <time.h>                      /* time library */

#include "rpf_general_defs.h"          /* general program wide definitions */
#include "DbmsDefs.h"                  /* database definitions */

#include "NWRTransmitter.h"            /* table definitions */
#include "LocTransmit.h"


/* ------------------------------------------------------------ */

typedef struct
{
   char	id[HYD_SERV_LEN + 1];
   int	num_default_pcc;
   int  num_total_pcc;
   int	num_fps;
} office_struct;

/* ------------------------------------------------------------ */

typedef struct
{
   char	prodid[PRODUCT_LEN + 1];
   char prodtype[BOOL_LEN + 1];
   char prodcateg[PROD_CATEG_LEN + 1];
   char prod_ansi_time[ANSI_YEARSEC_TIME_LEN + 1];
   char post_ansi_time[ANSI_YEARSEC_TIME_LEN + 1];
   char **cor_fpid;
   int  corevent_cnt;
   int  cor_flag;
   int  issue_cor_flag;
   int  event_cnt;
   char **event_id;
   char ccx_str[CCX_LEN + 1];
} cor_prevprod_struct;


/* ------------------------------------------------------------ */

typedef struct
{
   /* parameter info read from Informix */
   
   char	hsa[HYD_SERV_LEN + 1];
   char miss_val[MISS_LEN + 1];
   char miss_cat[MISS_LEN + 1];
   char miss_tim[MISS_LEN + 1];
   int rvs_expirehrs;
   int fls_expirehrs;
   int flw_expirehrs;
   
   
   /* candidate offices for service backup purposes */
   
   office_struct	offices[MAX_OFFICES];
   int			num_offices;
   char			selected_office[HYD_SERV_LEN + 1];
   
   
   /* user requested defaults */
   
   char startup_pcc_file[MAXLEN_FILENAME];
   
   
   /* user defined product-wide values (may be based on recommendations).
      for the few cases where numfps_included is helpful, simply 
      compute it rather than store it in this structure. */
   
   int *fps_included;   
   int *grps_included;
   int *cnty_included;
   
   int numgrps_included;
   int numcnty_included;  
   
   
   /* program recommended product-wide values.  the vtec structure 
      contains the details on the point-specific recommendations. */
   
   int rec_prod_index;
   
   int *rec_fps_included;   
   int *rec_grps_included;  
         
   
   /* temporary holding areas for special data */
   
   char  longstring[MAXLEN_LONGSTR];
   double flt;
   
   NWRTransmitter	*nwrtransPtr;
   LocTransmit		*loctransPtr;
   
   
   /* miscellaneous variables */
   
   time_t	system_time;
   
   int		issuance_set;
   int 		issnum;
   int		expire_set;
   time_t 	expire_time;
   
   int		batch_mode;
   
   char         defaultTZ[TZ_LEN + 1];
   
   
   char 	pcc_file[MAXLEN_FILENAME];
   
   int		issue_workmode;     /* boolean */
   int		workstation_mode;   /* multiple mode values possible */
   
   /* parameters for corrected previous product */
   
   cor_prevprod_struct cor_prevprod;
   
} misc_struct;
   
#endif

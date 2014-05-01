/* File: preaccum_show.h  */

/* includes */
#include <stdio.h>
#include <time.h>
#include <Xm/Xm.h>

#include "ShefDur.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "Xtools.h"
#include "time_convert.h"
#include "QualityCode.h"
#include "precip_total.h"

/* definitions */
#define MAX_FILE_SIZE  	500000
#define MAXLEN_ENDTIME 30

#define PA_HDRFILE  "paccum_hdr"
#define PA_DATAFILE "paccum_data"
#define PA_OUTFILE  "paccum_out"

#define NUMDURATIONS 8

#define MISSING_PRECIP  -9999.

#define SORT_BY_LID    0
#define SORT_BY_VALUE  1



/* prototypes */

void    preaccum_show(Widget w);

enum FilterStation { IgnoreStation , UseStation } ;

/* internal global control structure */
typedef struct 
{
   int		details_switch;
   int          hsa_switch ;
   int		loc_switch;
   int          pets_switch ;
   int		PPaccum_switch;
   
   int	 	hr_window;
   
   char		locid[LOC_ID_LEN + 1];
   int  	sort_option;
      
   int  	duration_set[NUMDURATIONS];
   int          other_duration ;
   
   int          *HSA_set;
   int		*PCTS_set;
   int		*PPTS_set;
   int          numHSA_defined;
   int 		numPC_defined;            	       
   int 		numPP_defined;            	       
   int 		numPC_selected;            	       
   int 		numPP_selected;            	       
   int          numHSA_selected;
   
   char		endtime_str[MAXLEN_ENDTIME];
     
   char 	hdrfile[120];
   char 	datafile[120];
   char 	outfile[120];
   int		tempfile_created;
} preaccum_options_struct;


/* internal global data structure for single lid-pe-ts group */

typedef struct 
{  
   char    	lid[LOC_ID_LEN+1];
   char   	pe[SHEF_PE_LEN+1];
   char    	ts[SHEF_TS_LEN+1];
   
   int		summed_flag[NUMDURATIONS];
   double  	hrfill[NUMDURATIONS];
   double  	amount[NUMDURATIONS];
   
   double  	max_value;
   
} preaccum_data_struct;

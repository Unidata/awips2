/**********************************************************************
   rpf_converts.h
   
   PURPOSE
   Include file for function prototypes.
   
   MODIFICATION
   Added two function prototypes for VTEC product structure on 02/15/02  
   Added function prototypes for convert_timezoneflag_to_index and 
   convert_index_to_timezoneflag - Erb (2/25/2002)
   Added two function prototypes for convert_vtecOTEmode_to_index and
   convert_index_to_vtecOTEmode - Jingtao Deng(10/28/2003)
   convert_index_to_ugcmode, convert_ugcmode_to_index -Jingtao Deng (Mar. 2004)
   ********************************************************************/

#ifndef RPF_CONVERTS_H
#define RPF_CONVERTS_H

#include <time.h>

#include "rpf_general_defs.h"     /* definitions */ 
#include "rpf_err_defs.h"        
#include "pcc_keyvalue_defs.h"    
#include "cat_and_product_defs.h" 

#include "fp_struct.h"            /* structures */
#include "grp_struct.h"

#include "rpf_logs.h"             /* other functions protos */

#include "time_convert.h"

#include "DbmsDefs.h"             /* database */


int convert_prodid_to_index(char prodid[]);

char *convert_prodindex_to_id(int prodindex);

int convert_prodreason_to_index(int prodreason);

char *convert_prodreason_to_descr(int prodreason);

int convert_grpid_to_index(char		*grpid,
			   int		numgrps,
			   grp_struct	*grp);

int convert_fpid_to_index(char		*fpid,
			  int		numfps,
			  fp_struct	*fp);


int convert_sectionname_to_index(char *sectionname);
char *convert_index_to_sectionname(int index);  /*Added by Hank Herr (1/17/01)*/

int convert_subsectionname_to_index(char *subsectionname);
char *convert_index_to_subsectionname(int index);  /*Added by Hank Herr (1/17/01)*/

int convert_timeformat_to_index(char *timeformat);

int convert_dateformat_to_index(char *dateformat);

int convert_refstage_to_index(char *refstage);
char *convert_index_to_refstage(int index);

char *convert_index_to_refstagevar(int index);

int convert_nwrflag_to_index(char *nwroption);
char *convert_index_to_nwrflag(int index);

int convert_nwralert_to_index(char *nwroption);
char *convert_index_to_nwralert(int index);

int convert_searchtype_to_index(char *searchtype);
char *convert_index_to_searchtype(int index);

int convert_qcfilter_to_index(char *filtertype);
char *convert_index_to_qcfilter(int index);

int convert_forecastts_to_index(char *tstype);
char *convert_index_to_futurets(int index);

int convert_textcase_to_index(char *casetype);
char * convert_index_to_textcase(int caseindex);

char *convert_catindex_to_name(int catindex);
int convert_catname_to_index(char catname[]);

void convert_str_to_upcase(char *string);
void convert_str_to_lowcase(char *string);

int convert_fporder_to_index(char *orderstr);
char *convert_index_to_fporder(int index);

char *convert_trendval_to_descr(int index);

int convert_vtecflag_to_index(char *vtec_option);
char *convert_index_to_vtecflag(int index);

int convert_timezoneflag_to_index(char *timezone_option);
char *convert_index_to_timezoneflag(int index);

time_t convert_locdatetime_to_time(char dait[],
				   char tyme[]);

time_t convert_gmdatetime_to_time(char dait[],
			  	  char tyme[]);

void convert_time_to_gmdatetime(time_t	data_time,
			              char 	*dait,
				      char	*tyme);

int convert_segment_mode_to_index(char mode[]);
char *convert_index_to_segment_mode(int index);

char *convert_index_to_vtecmodestr(int segment_mode);   

char *	trim_countyname(char *countyin);

int convert_vtecOTEmode_to_index(char mode[]);
char *convert_index_to_vtecOTEmode(int index);

int convert_ugcmode_to_index(char mode[]);
char *convert_index_to_ugcmode(int index);


#endif

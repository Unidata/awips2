/*********************************************************************
   format_vals.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef FORMAT_VALS_H
#define FORMAT_VALS_H

#include <stdio.h>
#include <time.h>
#include <stdlib.h>

#include "template_err_defs.h"      /* definitions */
#include "rpf_general_defs.h"
#include "function_defs.h"
#include "rpf_file_defs.h"
#include "template_defs.h"

#include "temp_info_struct.h"       /* structures */
#include "temp_item_struct.h"
#include "time_format_struct.h"
#include "temp_records_struct.h"
#include "pcc_struct.h" 

#include "misc_struct.h"

#include "rpf_converts.h"           /* other functions protos */
#include "rpf_logs.h"

#define DEFAULT_LINE_WIDTH 80



void format_vals(const values		 rawvalue,
		 const char		 dqcode[],
		 const int		 var_index,
		 const format_struct 	 *format,
		 const int		 format_index,
		       misc_struct	 *misc,
		 const int		 pad_switch,
		       char		 msg_str[],
		       char		 valuestr[],
		       int 		 *valuelen,
		 const pcc_struct	 *pcc,
		 const char		 local_lid[]);

int get_format_index(const int		 	var_index,
		     const variable_struct 	*variable,
		     const format_struct 	*format,
		     const int			type);

void format_datetime(const time_t	rawtime,
		     const int     	index,
		     const char		*usertime,
		           misc_struct	*misc,
		     const int		pad_switch,
		     	   char 	valuestr[],
		     const pcc_struct	*pcc,
		     const char		local_lid[],
		     int               varindex,
		     char              missing_val_str[]);

void round_hour(time_t rawtime, 
		time_t *rounded_timet);

void format_timephrase(misc_struct	*misc,
		       const time_t 		rawtime,
		       	     char 		dtstr[MAXLEN_STRING]);
			     
void format_wwa_timephrase(misc_struct	*misc,
		          const time_t 		rawtime,
		       	  char 		dtstr[MAXLEN_STRING]);			     

void format_date(const	char	dait[],
		 const	int     index,
			char 	valuestr[]);

void setdate_mixedcase(char *dtstr);



/* write_phrase_text functions */

void write_phrase_text(int	textcase_type,
		       char 	*phrase,
		       FILE 	*outfile_ptr);

char * get_phrase_token(char 	*phrase,
			int  	phrase_pos,
			int	*return_token_len,
			int	*return_num_trailing_blanks,
			int	*return_forced_nl_found);

void write_line_text(char 	*rawline,
		     FILE 	*outfile_ptr);

int get_line_width();

/*write bullet text function*/

void write_bullet_text(int	textcase_type,
		       char 	*phrase,
		       FILE 	*outfile_ptr);
		       
/*write indent text (starting with 2 spaces> */

void write_indent_text(int	textcase_type,
		       char 	*phrase,
		       FILE 	*outfile_ptr);		       
		       
void create_MND_datetime(const pcc_struct   *pcc,
                        fp_struct         *fp,
			int                fpindex, 
                        misc_struct  	*misc,
                        char               *datetime_str);
			
			
			 		       

#endif

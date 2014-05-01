/*    
       JUL 22 96 DGB
                Add cfg to tempfiles extern extern struct externure.
       OCT 26 96 dgb
                Add xw extern struct xw and xc
       JAN 10 00 dgb
                Add test_.century_string and test_.century_flag
		to test_ structure.
        FEB 4 2000 DGB
                Add mtr_names array.       

        FEB 18 2000 DGB
	        Add sm_alias, sm_names arrays.    
        OCT 31 2000 DGB
	        Add new FILE descriptor *just
		Add new array for national_category_table.template
		file in struct files_
        NOV 20 2004 DGB
	        Add pc_names for reset times.

*/

extern struct t_smdata_ {
    char s000[SIZE_OF_MSG], s111[SIZE_OF_MSG], s222[SIZE_OF_MSG],
         s333[SIZE_OF_MSG], s444[SIZE_OF_MSG], s555[SIZE_OF_MSG],
         s_11[SIZE_OF_MSG]; 
    }    sm;
extern struct t_forms_ {
         char  type_format[15], type_sensor[15], type_report[15];
         }     forms_;
extern struct t_luns_ {
	FILE *lchn, *jchn, *kchn, *mchn, *mrec, *icher, *nchn, *just;
	}	luns_;
extern struct t_buffer_ {
	short int ibuf[200], tdata[256], idate[6], idate1[6];
    char wmo[7], wmo_line[40], tempid[5], time[6];
    float ibufchar[2];
	}	buffer_;
extern struct t_databuf_ {
	char sdata[SIZE_OF_MSG], record[SIZE_OF_RECORD], main[SIZE_OF_MSG], 
         remarks[SIZE_OF_REMARKS];
    int  len_of_msg;
	}	databuf_;
extern struct t_contro_ {
	char control[10];
	}	contro_;
extern struct t_files_ {
    char shef_decode_err[MAX_F], shef_in[MAX_F], shef_out[MAX_F], shef_log[MAX_F],
         national_cat[MAX_F];                               /* dgb:10/31/00 */
    }  files_;
extern struct t_tempfiles_ {
    char shef_decode_err[MAX_F],shef_in[MAX_F], shef_out[MAX_F], shef_log[MAX_F];
    char log_file[MAX_F], err_file[MAX_F], ifile[MAX_F], cfg[MAX_F];
    }  tempfiles_;
extern struct t_cont_ {
	char out_flag[13], error_flag[13], post_flag[13],
	miss_flag[13], messages[13];
	}	cont_;
extern struct t_datim_ {
	short int idate[6];
	}	datim_;
extern struct t_error_ {
	short int nerror, nwarn;
	}	error_;
extern struct s_test {
    int test_flag, century_flag;                            /* dgb:01/10/00 */
    char century_string[13];                                /* dgb:01/10/00 */
   } test_;
extern struct t_stats_ {
    long time_current;
    long time_start_log;
    long time_stop_log;
    long time_start_prod;
    long time_stop_prod;
    int  post_stats[MAX_ACCEPTABLE_PE + 1][MAX_ACCEPTABLE_TS + 1];
    int  parse_stats[MAX_ACCEPTABLE_CATEGORIES +1];
    char acceptable_pe[MAX_ACCEPTABLE_PE+1][3];
    char acceptable_ts[MAX_ACCEPTABLE_TS+1][3];
    int num_pe;
    int num_ts;
    char dtg[40];
    int flag;
    char product_name[30];
    } stats_;
extern struct t_shefstr_ {
	char pecodes[200];
	}	shefstr_;

extern char mtr_names[MAX_NUM_MTR_NAMES][6];                        /* dgb:02/04/00 */
extern char pc_names[MAX_NUM_PC_NAMES][6];                          /* dgb:11/20/04 */
extern char pc_times[MAX_NUM_PC_NAMES][3];                             /* dgb:11/20/04 */
extern char sm_alias[MAX_NUM_MTR_NAMES][6];                         /* dgb:02/18/00 */
extern char sm_name[MAX_NUM_MTR_NAMES][6];                          /* dgb:02/18/00 */
extern char xref_file[MAX_XREF][11];                                    /* dgb:10/31/00 */

extern struct xc_ {
    int s;
    }   xc;
extern struct xwx_ {
    int s;
    char t[7];
    } xw[SIZE_XW];
     

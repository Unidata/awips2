/*    
       JUL 22 96 DGB
                Add cfg to tempfiles extern struct externure.
       OCT 26 96 dgb
                Add xw struct xw and xc
		
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
struct t_smdata_ {
    char s000[SIZE_OF_MSG], s111[SIZE_OF_MSG], s222[SIZE_OF_MSG],
         s333[SIZE_OF_MSG], s444[SIZE_OF_MSG], s555[SIZE_OF_MSG],
         s_11[SIZE_OF_MSG]; 
    }    sm;
struct t_forms_ {
         char  type_format[15], type_sensor[15], type_report[15];
         }     forms_;
struct t_luns_ {
	FILE *lchn, *jchn, *kchn, *mchn, *mrec, *icher, *nchn, *just;
	}	luns_;                                      /* dgb:10/31/00 */
struct t_buffer_ {
	short int ibuf[200], tdata[256], idate[6], idate1[6];
    char wmo[7], wmo_line[40], tempid[5], time[6];
    float ibufchar[2];
	}	buffer_;
struct t_databuf_ {
	char sdata[SIZE_OF_MSG], record[SIZE_OF_RECORD], main[SIZE_OF_MSG], 
         remarks[SIZE_OF_REMARKS];
    int  len_of_msg;
	}	databuf_;
struct t_contro_ {
	char control[10];
	}	contro_;
struct t_files_ {
    char shef_decode_err[MAX_F], shef_in[MAX_F], shef_out[MAX_F], shef_log[MAX_F],
         national_cat[MAX_F];                              /* dgb:10/31/00 */
    }  files_;
struct t_tempfiles_ {
    char shef_decode_err[MAX_F],shef_in[MAX_F], shef_out[MAX_F], shef_log[MAX_F];
    char log_file[MAX_F], err_file[MAX_F], ifile[MAX_F], cfg[MAX_F];
    }  tempfiles_;                                          
struct t_cont_ {
	char out_flag[13], error_flag[13], post_flag[13],
	miss_flag[13], messages[13];
	}	cont_;
struct t_datim_ {
	short int idate[6];
	}	datim_;
struct t_error_ {
	short int nerror, nwarn;
	}	error_;
struct s_test {
    int test_flag, century_flag;                            /* dgb:01/10/00 */
    char century_string[13];                                /* dgb:01/10/00 */
   } test_;
struct t_stats_ {
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
struct t_shefstr_ {
	char pecodes[200];
	}	shefstr_;

char mtr_names[MAX_NUM_MTR_NAMES][6];                        /* dgb:02/04/00 */
char pc_names[MAX_NUM_PC_NAMES][6];                          /* dgb:11/20/04 */
char pc_times[MAX_NUM_PC_NAMES][3];                             /* dgb:11/20/04 */
char sm_alias[MAX_NUM_MTR_NAMES][6];                         /* dgb:02/18/00 */
char sm_name[MAX_NUM_MTR_NAMES][6];                          /* dgb:02/18/00 */
char xref_file[MAX_XREF][11];                                    /* dgb:10/31/00 */
struct xc_ {
    int s;
    }   xc;
struct xwx_ {
    int s;
    char t[7];
    } xw[SIZE_XW];


  /* first value is an integer representing the synoptic code number */
  /* second value is a string containing the metar wx token          */ 
  /* the order is from the most important to the least important     */
  /* If more than one present wx value is determined from the metar  */
  /* report, the most important value will be transmitted.           */

struct xwx_  xw[SIZE_XW] = 
         { 
         { 19,	"+FC"    }, /* +FC     tornado/waterspout                     */
         { 19,	"FC"     }, /*  FC     funnel cloud                           */
         { 97,	"+TSRA"  }, /* +TSRA   thunderstorm/heavy rain                */
         { 96,	"TSRA"   }, /*  TSRA   thunderstorm/moderate rain             */
         { 95,	"-TSRA"  }, /* -TSRA   THUNDERSTORM/LIGHT RAIN                */
         { 81,	"+SHRA"  }, /* +SHRA   RAIN SHOWER/HEAVY                      */ 
         { 81,	"SHRA"   }, /*  SHRA   RAIN SHOWER/MODERATE                   */
         { 80,	"-SHRA"  }, /* -SHRA   RAIN SHOWER/LIGHT                      */
         { 64,	"+RA"    }, /* +RA     RAIN HEAVY                             */
         { 62,	"RA"     }, /*  RA     RAIN MODERATE                          */
         { 60,	"-RA"    }, /* -RA     RAIN LIGHT                             */
         { 97,	"+TSSN"  }, /* +TSSN   THUNDERSTORM/HEAVY SNOW                */ 
         { 97,	"TSSN"   }, /*  TSSN   THUNDERSTORM/MODERATE SNOW             */
         { 97,	"-TSSN"  }, /* -TSSN   THUNDERSTORM/LIGHT SNOW                */
         { 74,	"+SN"    }, /* +SN     SNOW/HEAVY                             */                
         { 72,	"SN"     }, /*  SN     SNOW/MODERATE                          */
         { 70,	"-SN"    }, /* -SN     SNOW/LIGHT                             */
         { 86,	"+SHSN"  }, /* +SHSN   SNOW SHOWER/HEAVY                      */                
         { 86,	"SHSN"   }, /*  SHSN   SNOW SHOWER/MODERATE                   */
         { 85,	"-SHSN"  }, /* -SHSN   SNOW SHOWER/LIGHT                      */
         { 97,	"+TSPE"  }, /* +TSPE   THUNDERSTORM/HEAVY ICE PELLETS         */
         { 97,	"TSPE"   }, /*  TSPE   THUNDERSTORM/MODERATE ICE PELLETS      */
         { 97,	"-TSPE"  }, /* -TSPE   THUNDERSTORM/LIGHT LIGHT PELLETS       */
         { 67,	"+FZRA"  }, /* +FZRA   FREEZING RAIN/HEAVY                    */              
         { 67,	"FZRA"   }, /*  FZRA   FREEZING RAIN/MODERATE                 */
         { 66,	"-FZRA"  }, /* -FZRA   FREEZING RAIN/LIGHT                    */
         { 57,	"+FZDZ"  }, /* +FZDZ   FREEZING DRIZZLE/HEAVY                 */            
         { 57,	"FZDZ"   }, /*  FZFZ   FREEZING DRIZZLE/MODERATE              */
         { 56,	"-FZDZ"  }, /* -FZFZ   FREEZING DIRZZLE/LIGHT                 */
         { 54,	"+DZ"    }, /* +DZ     DRIZZLE/HEAVY                          */            
		 { 52,	"DZ"     }, /*  DZ     DRIZZLE/MODERATE                       */
		 { 50,	"-DZ"    }, /* -DZ     DRIZZLE/LIGHT                          */
		 { 87,	"+SHPE"  }, /* +SHPE   ICE PELLET SHOWERS/HEAVY               */   
		 { 87,	"SHPE"   }, /*  SHPE   ICE PELLET SHOWERS/MODERATE            */
		 { 87,	"-SHPE"  }, /* -SHPE   ICE PELLET SHOWERS/LIGHT               */
		 { 79,	"+PE"    }, /* +PE     ICE PELLET/HEAVY                       */
		 { 79,	"PE"     }, /*  PE     ICE PELLET/MODERATE                    */
		 { 79,	"-PE"    }, /* -PE     ICE PELLET/LIGHT                       */
		 { 87,	"SHGR"   }, /*  SHGR   HAIL/SNOW PELLET SHOWER  > 1/4"        */
		 { 87,	"SHGS"   }, /*  SHGS   HAIL/SNOW PELLET SHOWER  < 1/4"        */
		 { 76,	"IC"     }, /*  IC     ICE CRYSTALS NO INTENSITY              */
		 { 17,	"TS"     }, /*  TS     THUNDERSTORM/NO RAIN                   */
		 { 16,	"VCSH"   }, /*  VCSH   SHOWERS IN VICINITY BUT NOT AT STATION */
		 { 41,	"BR"     }, /* +BR     MIST/ FOG                              */
		 { 41,	"FG"     }, /*  FG     FOG                                    */
		 { 42,	"MIFG"   }, /*  MIFG   FOG SHALLOW                            */
		 { 43,	"PRFG"   }, /*  PRFG   FOG PARTIAL                            */
		 { 42,	"BCFG"   }, /*  BCFG   FOG PASTCHES                           */
		 { 40,	"VCFG"   }, /*  VCFG   FOG VICINITY NOT AT STATION            */
		 { 41,	"FZFG"   }, /*  FZFG   FOG TEMP BELOW 0 C                     */
		 {  4,	"FU"     }, /*  FU     SMOKE                                  */
		 {  4,	"VA"     }, /*  VA     VOLCANIC ASH                           */
		 {  5,	"HZ"     }, /*  HZ     HAZE                                   */
		 { 38,	"BLSN"   }, /*  BLSN   BLOWING SNOW 6 FT OR HIGHER            */
		 { 38,	"VCBLSN" }, /*  VCBLSN BLOWING SNOW VICINITY                  */
		 { 37,	"DRSN"   }, /*  DRSN   LOW DRIFTING SNOW                      */
		 {  6,	"DU"     }, /*  DU     WIDESPREAD DUST                        */
		 {  7,	"BLDU"   }, /*  BLDU   BLOWING DUST                           */
		 {  7,	"VCBLDU" }, /*  VCBLDU BLOWING DUST VICINITY    FOG           */
		 {  7,	"DRDU"   }, /*  DRDU   LOW DRIFTING DUST                      */
		 {  7,	"SA"     }, /*  SA     SAND                                   */
		 {  8,	"BLSA"   }, /*  BLSA   BLOWING SAND                           */
		 {  8,	"VCBLSA" }, /*  VCBLSA BLOWING SAND VICINITY                  */
		 {  7,	"DRSA"   }, /*  DRSA   LOW DRIFTING SAND                      */
		 {  7,	"PY"     }, /*  PY     SPARY                                  */
		 {  7,	"BLPY"   }, /*  BLPY   BLOWING SPRAY                          */
		 {  8,	"PO"     }, /*  PO     DUST WHIRLS                            */
		 {  8,	"VCPO"   }, /*  VCPO   DUST WHIRLS IN VICINITY                */
		 { 18,	"SQ"     }, /*  SQ     SQUALLS                                */
		 { 30,	"SS"     }, /*  SS     SANDSTORM LIGHT > 1/4 M                */
		 { 33,	"+SS"    }, /* +SS     SANDSTORM HEAVY < 1/4 M                */
		 { 30,	"VCSS"   }, /*  VCSS   SANDSTORM VICINITY                     */
		 { 30,	"DS"     }, /*  DS     DUSTSORM LIGHT > 1/4 M                 */
		 { 33,	"+DS"    }, /*  +DS    DUSTROMS HEAVY < 1/4 M                 */
		 { 30,	"VCDS"   }, /*  VCDS   DUSTSTORM IN VICINITY                  */
         { 77,  "+SG"    }, /*  +SG    SNOW GRAINS HEAVY                      */
         { 77,  "SG"     }, /*  SG     SNOW GRAINS MODERATE                   */
         { 77,  "-SG"    }  /*  -SG    SNOW GRAINS LIGHT                      */
         };


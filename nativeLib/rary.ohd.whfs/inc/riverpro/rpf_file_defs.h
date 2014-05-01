/*******************************************************************
   rpf_file_defs.h
   
   PURPOSE
   Serves as include file for file names for the RiverPro program
   
   
   ********************************************************************/

#ifndef RPF_FILE_DEFS_H
#define RPF_FILE_DEFS_H


/* input files */

#define TIMEPHRASE_FILE           "timephra.dat"
#define FCSTTREND_FILE            "fcst_trend_phrase"
#define TIMEPHRASE_NUM         32
#define TIMEPHRASE_DAY_NUM     8


/* diagnostic log files */

#define FP_GRP_LOGFILE       "fp_grp.log"
#define STAGES_LOGFILE       "stages.log"
#define PP_LOGFILE 	     "pp.log"
#define MISC_LOGFILE         "misc.log"
#define PCC_LOGFILE          "pcc.log"
#define TEMPNAME_LOGFILE     "tempnames.log"
#define PCCNAME_LOGFILE      "pccnames.log"

#define TEMPLATE_INFO_LOGFILE     "template.log"
#define TEMPLATE_BUFFERS_LOGFILE  "buffers.log"



/* default product content control files */

#define NUM_DEFAULT_PCC_FILES 3

#define RVS_PCCFILE   "rvs_def.pcc"
#define FLS_PCCFILE   "fls_def.pcc"
#define FLW_PCCFILE   "flw_def.pcc"


/* note that only a partial template file name is given here */

#define NUM_TEMPLATE_FILES 9

#define HEADER_TEMPLATEFILE      "header.tpl"
#define HEADLINE_TEMPLATEFILE    "headline.tpl"
#define SUMMARY_TEMPLATEFILE     "summary.tpl"
#define BASIS_TEMPLATEFILE       "basis.tpl"
#define TABULAR_TEMPLATEFILE     "tabular.tpl"
#define ROUNDUP_TEMPLATEFILE     "roundup.tpl"
#define COMPARISON_TEMPLATEFILE  "compare.tpl"
#define IMPACT_TEMPLATEFILE      "impact.tpl"
#define CTA_TEMPLATEFILE         "cta.tpl"


/* log output files */

#define MESSAGE_LOGFILE           "rpf_message.log"
#define ISSUE_LOGFILE             "rpf_issue.log"
#define RECEIPT_LOGFILE           "process_backup.log"
#define VTEC_QCINFO_LOGFILE       "vtec_qcinfo.log"


/* output files */

#define WORK_FILE    "work_product"
#define ISSUE_FILE   "rpf_product"
#define COR_FILE     "cor_prev_product"
#define FPVTECINFO_OUTPUT_FILE  "fpvtecinfo.dat"
#define FPVTECINFO_INPUT_FILE   "curvteceditsaved.dat"
#endif

/*******************************************************************
   rpf_general_defs.h
   
   PURPOSE
   Serves as include file for definitions used in RPF program
   
   NOTES
   
   ********************************************************************/

#ifndef RPF_GENERAL_DEFS_H
#define RPF_GENERAL_DEFS_H


/* ----- numeric limits -------------------------------------------------- */

#define MAX_SECTIONS       6   /* max num of product sections */
#define MAX_PS_SUBSECTIONS 3   /* max num of point specific subsections */
#define MAX_CTAS           5   /* max num of ctas allowed to be selected */
#define MAX_PROD_COMPS     1   /* max num of comparison statements per point */
#define MAX_UGCVAL       999   /* max ugc value for a given state */
#define MAX_UGCSTATES     20   /* max number of states allowed in ugc list */
#define MAX_COUNTIES     120   /* max number of counties in county list */
#define MAX_RIVERS        60   /* max number of rivers in river list */
#define MAX_OFFICES       20   /* max number of offices to support */
#define MAXLEN_SECTION_NAME 30  /*max number of length of section/subsection
                                         name*/
                                            

/* ---- maximum length of variable length strings ------------------------- */

#define MAXLEN_STRING       90   /* max length of text string */

#define MAXLEN_LONGSTR    1520   /* max length for a special long string value;
				    this value must not be greater than the 
				    newphrase length; it is set this high to
				    allow for impacts and list-type vars 
				    GrpsFpList is probably the longest variable*/
#define MAXLEN_NEWPHRASE  1700   /* max length of the new phrase generated
				    for output in product; this allows for 
				    long impacts plus phrase text */

#define MAXLEN_FILENAME    160   /* max length for the name of a file */
#define MAXLEN_CATNAME       9   /* max length of stage category name */
#define MAXLEN_REFSTGTYPE    8   /* max length for imp/cmp ref stage type  */

#define CCX_LEN              3   /* correction sequence indicator */

#define MAXLEN_TEXT_PRODUCT  200000  /* the maximum char number in a string that
                                      store the text product content */

/* --- character assignments ----------------------------------------------- */

#define COMMENT_SYMBOL   '#'   /* comment symbol for use in templates */
#define CONT_SYMBOL      '&'   /* continuation symbol for use in pcc files */
#define HEADER_CODE      'H'   /* access codes for template variables */
#define HEADLINE_CODE    'L'
#define SUMMARY_CODE     'S'
#define TABULAR_CODE     'T'
#define ROUNDUP_CODE     'R'
#define IMPACT_CODE      'I'
#define COMPARISON_CODE  'C'


#define	NWR_COMMENT        "###"
#define	NWR_TOWER_WILDCARD "XXX"
#define NWR_PRODUCT_START  "PRODUCT_START"

#define BACKGROUND_STR     "&"   /* for use in invoking system commands */

#define OUP_STR  	"OUP"    /* for use in invoking rpf_issue script */
#define NWR_STR  	"NWR"
#define LOCAL_STR  	"LOCAL"
#define NONLOCAL_STR	"NONLOCAL"

/* --- arbitrary fixed assignment stuff ------------------------------------ */

#define MISSINGVAL  -9999 
#define MISSING_TOKEN -9898
#define TRUE  1              
#define FALSE 0
#define CURRENT_TIME -1
#define BAD_VTECTIME -2

#define OBS_DATA    1         /* for use by the compute_stage_info function */
#define FCST_DATA   2

#define OPERATIONAL_MODE 14
#define TEST_MODE        15
#define PRACTICE_MODE    16

#define ISSUE_NORMAL_MODE  17
#define ISSUE_SAVEWORK_MODE 18
#define ISSUE_PRACTICE_MODE 19

#define TIMECODE_UGC_OFFSET    38   /* product text translation operations */
#define TIMECODE_UGC_RELATIVE  39   
#define TIMECODE_UGC_ABSOLUTE  40   
#define TIMECODE_MND           41
#define TIMECODE_VTEC_BEGIN    42
#define TIMECODE_VTEC_END      43
#define TIMECODE_HDR           44
#define TIMECODE_NONE          45

#define TIMESTR_UGC_OFFSET   "<UGC+"          /* UGC non-VTEC relative */
#define TIMESTR_UGC_RELATIVE "<UGCR:"         /* UGC VTEC relative */
#define TIMESTR_UGC_ABSOLUTE "<UGCA:"         /* UGC VTEC absolute */
#define TIMESTR_MND          "<MND:"
#define TIMESTR_VTEC_BEGIN   "<BEGIN_NOW_>"   /* make these 12 chars to */
#define TIMESTR_VTEC_END     "<END_NOW___>"   /* match actual length */
#define TIMESTR_HDR          "<HDR_TIME>"

#define UGC_ABS_MANUAL    10     /* how UGC expire time set */
#define UGC_RELATIVE      11

#define TIMESTR_MSG_ENDTIME "000000000000"   /* used in time code */


#endif

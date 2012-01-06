/*******************************************************************
   rpf_err_defs.h
   
   PURPOSE
   To serve as include file for warning, error, and
   fatal messages for the RiverPro program
      
   NOTES
   A message is treated as warning if the first character is "w";
   as error if not "w" or "f", and as fatal if "f".
   
 
********************************************************************/

#ifndef RPF_ERR_DEFS_H
#define RPF_ERR_DEFS_H


/* WARNING MESSAGES */

/* misc warnings  --------------------------------------------------*/

#define PREV_ISSNUM_NOTFOUND   "wCould not find previous issuance of product at"
#define SPECIAL_CATVALS        "wRecord value treated as missing since < cat value for"
#define INVALID_CATVALS        "wNon-increasing order of categories for"
#define NO_PREVPRODS           "wNo log of previous products found in database"
#define MISSING_IMPACT         "wNo impact statements for"
#define MISSING_HISTCREST      "wNo historical crests for"
#define MISSING_CATNAME        "wMissing data for category name for"
#define MISSING_TIMVAL         "wMissing time data for "
#define MISSING_INTVAL         "wMissing integer data for"
#define MISSING_FLTVAL         "wMissing float data for"
#define MISSING_TABULAR_FP     "wTabular template missing FP_ID keyword for"
#define MISSING_STAGEFLOW_DATA "wNo river data for"
#define MISSING_REFCREST       "wNo crest reference valuee for"
#define MISSING_REFIMP         "wNo impact reference value for"
#define MISSING_CARRYOVER      "wNo prev prod info for"
#define MULT_TS_FOUND          "wMultiple type-source codes found for"
#define NO_ASSIGNED_TOWER      "wNo NWR tower is associated with"
#define OTHER_WFO_TOWER        "wOther WFO programs NWR tower asociated with"
#define SKIP_TAB_LINE          "wSkipping line because data missing"
#define RATING_MISSING         "wRating table value unavailable"
#define ZDATUM_MISSING         "wZero datum value unavailable"


/* ERROR MESSAGES */

/* misc errors --------------------------------------------------*/

#define PRODUCT_SAVEFAIL     "Failed to save product to database"
#define FPPREVPROD_SAVEFAIL  "Failed to save forecast point product info to database for "
#define VTECEVENT_SAVEFAIL   "Failed to save VTEC event info to database for "
#define FILE_OPENWARN        "Could not open file"
#define FILE_CLOSEWARN       "Could not close file"
#define WRITERR_PRODUCT      "Error writing to output product"
#define QUEST_FP_IN_PROD     "Non-flood obs/max fcst category in FLS/FLW for "
#define INCOMPLETE_OFFICE_FILES "Missing template and/or pcc files for"

#define EXCEED_LINELIMIT     "Exceeded length for tabular section line"
#define EXCEED_LINE_NOBLANK  "Exceeded line length limit and no blanks present"
#define INVALID_CATINDEX     "Invalid category index value"
#define INVALID_CATNAME      "Invalid category name; assumed NONFLOOD"
#define NO_CTA_TEMPLATES     "No Call-to-action templates specified"

#define NO_ZONENUM           "No zone numbers defined for "
#define NO_COUNTYNUM         "No county numbers defined for "
#define NO_TOWERS_HSA        "No towers associated for forecast points"
#define EXCEED_UGCSTATES     "Exceeded number of states supported for UGC list"
#define INVALID_ZONENUM      "Invalid zone number for forecast point "
#define INVALID_COUNTYNUM    "Invalid county number for forecast point "

#define BADPRODINDEX         "Invalid product index value"
#define BADPRODID            "Invalid product category"
#define BADPRODCNX           "Invalid product id (must be 9 chars), using default id"
#define BADPRODREASON        "Invalid reason for issuing product, assumed RVS"
#define BADNWRFLAG           "Invalid NWR switch, assumed not-for-NWR"
#define BADNWRALERT          "Invalid NWR alert string, assuming no alert/tone"

#define BADSEGMODE           "Segment mode invalid, assuming NONE"
#define BADSEGMODEINDEX      "Segment mode index invalid, assuming 0 (NONE)"
#define BADSECTIONNAME_INDEX "Section name index invalid"
#define BADSUBSECTIONNAME_INDEX "Subsection name index invalid"

#define BADVTECFLAG          "Invalid VTEC switch, assuming not-for-VTEC"
#define BAD_OTE_MODE         "Invalid VTEC product mode"
#define BAD_PHENOM_CODE      "Invalid two-character VTEC phenomena code"
#define BAD_SIGNIF_CODE      "Invalid one-character VTEC significane code"
#define MISSING_REFVTEC      "No match found for referenced VTEC event"
#define MISSING_VTEC_ETN     "VTEC Event Tracking Number undefined for "
#define BADTIMEZONEFLAG      "Invalid Time Zone switch, will use default TZ"
#define BAD_UGC_MODE         "Invalid UGC mode "
#define BAD_EXPTIME          "Invalid product expiration look-foward time in hours"
/*========================================================*/

#define TOWERCODE_NOT_FOUND  "No towers defined for specified product code "
#define DUP_NWR_PRODCODES    "Duplicate products codes for multiple towers"

#define NO_TABULAR_IN_NWR    "Tabular section not permitted for NWR products."
#define INVALID_MISSINGLABEL "Invalid label specified for missing data, assumed MSG"
#define INVALID_TIMEPHRASE   "Invalid time phrase specified"
#define INVALID_MKTIME       "Time conversion failed (mktime); assumed time of 01/01/1970 00:00"
#define INVALID_TIME         "Time conversion failed; assumed time of 01/01/1970 00:00"
#define INVALID_TESTDATE     "Failure during read of date line in test data"
#define MISSING_CATVAL       "Missing categorical value for"
#define MISSING_FS           "Missing flood stage for"
#define MISSING_FQ           "Missing flood flow for"
#define MISSING_BF           "Missing bankfull stage for"
#define MISSING_WSTG         "Missing warning stage for"
#define MISSING_WFLOW        "Missing action flow for"
#define MISSING_CHGTHRESHOLD "Missing the change of threshold for"
#define MISSING_BACKHRS      "Missing the look back hours for"
#define MISSING_FORWARDHRS   "Missing the look forward hours for"
#define MISSING_ADJUSTENDHRS "Missing the adjusted end hours in PVTEC for"
#define UNDEFINED_LID	     "Undefined location identifier"
#define INVALID_MSGCONDITION "Invalid relational operator used with MISSING in condition"

#define INVALID_PE_VAR       "Invalid item in PE variable;"
#define NO_RECORD_CONTENT    "Missing content for"
/* read errors ----------------------------------------------------------*/

#define INVALID_IMPACTDATE  "Invalid format for impact date"
#define INVALID_DATE        "Invalid format for date"
#define INVALID_DATETIME    "Invalid format for date or time"
#define BAD_GRP_ID          "Invalid group id specified"
#define BAD_FP_ID           "Invalid forecast point id specified"
 
/* read pcc file errors --------------------------------------------------*/

#define BADKEYWORD_VALUE           "Invalid keyword value in pcc file"
#define SUMMARY_TEMPLATE_NOTFOUND  "Excluded summary section since could not find template"
#define BASIS_TEMPLATE_NOTFOUND    "Excluded basis section since could not find template"
#define HEADLINE_TEMPLATE_NOTFOUND "Excluded headline section since could not find template"
#define TABULAR_TEMPLATE_NOTFOUND  "Excluded tabular section since could not find template"
#define ROUNDUP_TEMPLATE_NOTFOUND  "Excluded roundup subsection since could not find template"
#define IMPACT_TEMPLATE_NOTFOUND   "Excluded impact subsection since could not find template"
#define COMP_TEMPLATE_NOTFOUND     "Excluded comparison subsection since could not find template"
#define CTA_TEMPLATE_NOTFOUND      "Excluded call-to-action since could not find template"
#define EXCEED_MAXCTAS             "Exceeded number of call-to-action permitted"
#define NO_PRODUCTTYPE             "No PRODUCT_TYPE specified - add to pcc file"
#define NO_PRODUCTID               "No PRODUCT_ID specified - add to pcc file"
#define NO_NWRFLAG                 "No NWR_FLAG specified - add to pcc file"
#define NO_VTECFLAG                "No VTEC_FLAG specified - add to pcc file"
#define NO_VTECOTEFLAG             "NO VTEC PRODUCT MODE-O,T or E spcified - add to pcc file"
#define NO_TIMEZONEFLAG            "No TIMEZONE_FLAG specified - add to pcc file"
#define NO_UGCMODE                 "NO PRODUCT UGC MODE-COUNTY or ZONE specified - add to pcc file"
#define INVALID_PROLOGUE_TEMP      "Prologue template names must include the phrase PROLOGUE; "
#define INVALID_SUMMARY_TEMP       "Summary template names must not include the phrase PROLOGUE; "
#define INVALID_NWR_HEADERINFO     "Invalid NWR header info specified"

/* read pcc info errors ----------------------------------------------------*/

#define BADPRIMARY_KEYWORD         "Invalid pcc primary keyword: "
#define BADPRIMARYKEYWORD_VALUE    "Invalid value for pcc primary keyword:"
#define ENDSECTION_KEYWORD_MISSING "No ENDSECTION keyword in pcc file"
#define BADKEYWORD                 "Invalid pcc keyword:"
#define BADREFSTAGE                "Invalid reference type specified"
#define BADCRESTSEARCH             "Invalid crest search type specified"
#define BADTEXTCASE                "Invalid product text case specified"
#define INVALID_FPORDER            "Invalid forecast point order specified"
#define EXCEED_TEMPLATENAME        "Exceeded length of template name"
#define BADSECTIONNAME             "Invalid product section name in pcc file"
#define BADSUBSECTIONNAME          "Invalid product subsection name in pcc file"
#define BADQCFILTER                "Invalid qc filter specified in pcc file"
#define BADFORECASTTS              "Invalid forecast ts filter specified in pcc file"


/* read trend phrase file errors -------------------------------------------*/

#define INVALID_TRENDPHRASE        "Invalid trend phrase:"
#define EXCEED_MAXNUM_OF_PHRASES  "Exceeded the maximum number of phrases allowed in file:"
#define EXCEED_MAXNUM_OF_TRENDPHRASES "Exceeded the allowable number of trend phrases in file:"

/* critical errors from which it'll try to continue... */

#define EXCEED_UGCLIST         "Exceeded length for UGC list - contact HRL"
#define FREE_NULL_MEMORY       "Attempted to free null memory"

/*********************** FATAL ERRORS  ****************************/

#define DB_OPENFAIL               "fFailed to access database"
#define NO_GRPS                   "fNo forecast groups defined"
#define NO_FPS                    "fNo forecast points defined"
#define NO_PS_INCNTY              "fNo forecast points defined"
#define MISMATCH_FPS		  "fMismatch with forecast points. Restart program"
#define NO_RPFPARAMS              "fNo RiverPro parameters defined"
#define NO_ADMIN                  "fNo ADMIN table info defined"
#define FILE_OPENERR              "fError opening file"
#define FAILED_MALLOC             "fMemory allocation request failed"
#define HEADER_TEMPLATE_NOTFOUND  "fCould not find header template"
#define PCCFILELIST_ERR           "fCould not access pcc definition files from "
#define PARAMFILEACCESS_ERR       "fCould not access pcc and template files from "
#define NO_PCCINFO                "fNo product definition sets available"
#define INVALID_SUFFIX		  "fInvalid file suffix on command-line"
#define INVALID_OFFICE		  "fInvalid office id on command-line"
#define MISSING_OFFICE_FILES      "fMissing required template and/or pcc files for"
#define WKST_MODE_ERR             "fNo work station mode defined-TEST, PRACTICE or OPERATIONAL"
 
#endif

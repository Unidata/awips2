/*******************************************************************
   template_defs.h
   
   PURPOSE
   Serves as include file for template related definitions
      
   ********************************************************************/

#ifndef TEMPLATE_DEFS_H
#define TEMPLATE_DEFS_H

/* ----- numeric limits -------------------------------------------------- */

#define MAX_TEMPLATES    100   /* max num of templates in template file */
#define MAX_FORMAT_ITEMS  50   /* max num of format or variable items */
#define MAX_SPECTIMES     10   /* max num of specific times for the template */
#define MAX_STACKSIZE    100   /* max num of items in condition stack */
#define MAX_NUMPHRASES   300   /* max num of phrases in a template */


/* ---- maximum length of variable length strings ------------------------- */

#define MAXLEN_TOKENNAME     4   /* max length of template token names */
#define MAXLEN_VARNAME      70   /* max length of template variable names  */
#define MAXLEN_FORMAT_ITEM   6   /* max length of template format specifiers */
#define MAXLEN_TIME_FORMAT  14   /* max length for template time format name */
#define MAXLEN_TEMPLATENAME 21   /* max length of template name */
#define MAXLEN_TEMPLATEREC 512   /* max length of record in template; whether
				    it be on one line or multiple lines */
#define MAXLEN_CODES         8   /* max length of variable access codes */
#define MAXLEN_VALUESTR     81   /* max length of string variable value, string
				    constant in the condition stack, or 
				    literal string in tabular format. if string
				    variable value is longer than this, it
				    must be stored as a long string */

/* --- character assignments ----------------------------------------------- */

#define CONT_STR          "&"    /* continuation symbol for template recs */

#define FORCED_NL_CHARS "||"     /* definitions for forced newline sequence */
#define FORCED_NL_NUMCHARS 2


/* arbitrary fixed assignment stuff, used like enums ---------------------- */

#define TEMPLATE_NAME       10   /* record types in template file */
#define TEMPLATE_CONDITION  11   /* including the tabular template */
#define TEMPLATE_PHRASE     12
#define TEMPLATE_COMMENT    13
#define TEMPLATE_FORMAT     14  
#define TEMPLATE_VARIABLE   15
#define TEMPLATE_SPECTIME   16
#define TEMPLATE_BULLETSTR  27
#define TEMPLATE_INDENTSTR  28

#define TEMPLATE_LITERAL    17   /* these are for tabular template only */
#define TEMPLATE_FPID       18
#define TEMPLATE_GRPNAME    19
#define TEMPLATE_MISCWRT    20
#define TEMPLATE_LOCID      21
#define TEMPLATE_MSGDATA    22
#define TEMPLATE_ACTIONBEGIN 23
#define TEMPLATE_ACTIONEND  24
#define TEMPLATE_EVENTBEGIN 25
#define TEMPLATE_EVENTEND   26

#define PHRASE_WITH_CONDITION     31  /* template phrase types */
#define PHRASE_WITH_SUBSTITUTION  32
#define PHRASE_FIXED              33


/* --- enumerated values -------------------------------------------- */


/* define the index to the date time formats */

typedef enum
{
   T_HEADER,
   T_MMDDYYYY, T_MMDDYY, T_MMDD, T_MMDDS, T_MMDDXM, T_AWXM, T_WXM, T_HHXM,
   T_CMDDYYYY, T_WCMDDYYYY, T_WCAMDDYYYY, T_AWCAMDDYYYY,
   T_CAMDDYYYY, T_CMDD, T_CAMDD, T_WCMDD,
   T_WCAMDD, T_AWCAMDD, T_WHH, T_AWHH, T_AWH,
   T_HHW, T_HW, T_HHAW, T_HHMMDD, T_W,
   T_AW, T_USER, T_DEFAULT, T_PHRASE, T_AWHHNN, T_DDHHNN, T_WWA
} time_format_types;


/* define the types of items that are permitted in the template;
   these may be in the conditional expression or in the phrase itself;
   the SPACE item is only used for the tabular section template */

typedef enum 
{
   BEGIN_VAR, VAR_INT, VAR_FLT, VAR_STR, VAR_TIM, VAR_DAT, END_VAR,
   RPF_INT, RPF_FLT, RPF_STR, RPF_TIM, RPF_DAT, RPF_SPACE, RPF_BOOLEAN,
   LEFT_PAREN, RIGHT_PAREN,
   BEGIN_RELOP, RPF_GT, RPF_GE, RPF_EQ, RPF_LE, RPF_LT, RPF_NE, END_RELOP,
   BEGIN_STROP, RPF_SEQ, RPF_SNE, END_STROP,
   BEGIN_LOGOP, RPF_AND, RPF_OR, END_LOGOP,
   RPF_INVALID
} itemtypes;


/* define the various structure identifiers that are the source of the
   values of the variables */

typedef enum 
{ 
   FP, GRP, LOCINFO, MISC, PCC,
   STAGES_SPEC,
   STAGES_FP_OBS, STAGES_FP_FCST, STAGES_FP_OTHER,
   STAGES_GRP, STAGES_FP_TREND, EVENT  
} structnames;

#endif

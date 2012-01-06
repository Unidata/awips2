/*******************************************************************
   template_err_defs.h
   
   PURPOSE
   To serve as include file for warning, error, and
   fatal messages for the template related functions
      
   NOTES
   A message is treated as warning if the first character is "w";
   as error if not "w" or "f", and as fatal if "f".

********************************************************************/

#ifndef TEMPLATE_ERR_DEFS_H
#define TEMPLATE_ERR_DEFS_H


/* WARNING MESSAGES */

#define MISSING_DATA_IN_CONDITION "wPhrase excluded since data missing in condition"
#define ASSUMED_PHRASETYPE    "wUnspecified template record type; assumed phrase type"


/* ERROR MESSAGES */

/* misc errors --------------------------------------------------*/

#define EXCEED_MAXTEMPLATES   "Too many templates in template file"
#define INVALID_SPECTIME     "Invalid time in SPECTIME template record"

/* template read errors --------------------------------------------------*/

#define INVALID_FORMAT        "Invalid format specified"
#define INVALID_X_FORMAT      "Invalid space (X) format"
#define INVALID_I_FORMAT      "Invalid integer (I) format"
#define INVALID_F_FORMAT      "Invalid float (F) format"
#define INVALID_S_FORMAT      "Invalid string (S) format"
#define INVALID_T_FORMAT      "Invalid time (T) format"
#define INVALID_L_FORMAT      "Invalid tabular section literal (L) format"
#define TOOFEW_VARS           "More variable formats than variables in template"
#define MISMATCH_FORMVAR      "Format does not match the variable"
#define EXCEED_NUM_FORMAT     "Exceeded max number of formats in template record"
#define EXCEED_NUM_VAR        "Exceeded max number of variable specs"
#define EXCEED_NUM_SPECTIMES  "Exceeded max number of spectimes"
#define MISSING_SPECTIME      "No specific time defined for value"
#define FORM_VAR_MISMATCH     "Unequal number of formats and variables in template"
#define BLANKFORMAT_IN_NONTAB "Blank-space format not permitted in non-tabular section"
#define LITFORMAT_IN_NONTAB   "Literal format not permitted in non-tabular section"
#define MISMATCH_PHRASELEN    "Phrase lengths don't match - contact HRL"
#define EXCEED_VARNAME         "Exceeded max variable name length in phrase"
#define INVALID_VARNAME_PHRASE "Invalid variable name specified in phrase"
#define NO_VARSTART            "No start of variable name defined in phrase"
#define MISSING_CONT_LINE      "Missing the continuation line"
#define TABREC_IN_NONTAB       "Tabular record type specified in non-tabular template"
#define BAD_ITEMTYPE_IN_LOGIC    "Unknown field in conditional expression"
#define BAD_RECORD_TYPE          "Unknown template file record type for record"
#define TEMPLATENAME_NOTFOUND    "Could not find specified template"
#define TEMPLATECONDITION_NOTFOUND  "Conditional expression not found in template"
#define TEMPLATEPHRASE_NOTFOUND  "Phrase not found in the template"
#define INVALID_RECORD_TYPE      "Invalid record specified in tabular template"
#define EXCEED_MAXNUM_PHRASES    "Too many phrases for product section in template"
#define INVALID_VAR_IN_CONDITION "Variable is not allowed within condition"
#define INVALID_VARIABLE         "Invalid variable in template varlist line"
#define VAR_NOT_ALLOWED          "Usage of variable not permitted"

/* verify template condition syntax --------------------------------------*/

#define INVALID_LPAREN_LOC  "Invalid right parenthesis location"
#define INVALID_RPAREN_LOC  "Invalid left parenthesis location"
#define INVALID_NUMBER_LOC  "Invalid numeric location"
#define INVALID_STRING_LOC  "Invalid string location"
#define INVALID_STROP_LOC   "Invalid string operator location"
#define INVALID_LOGOP_LOC   "Invalid logical operator location"
#define INVALID_RELOP_LOC   "Invalid relational location"
#define UNBAL_PAREN         "Unbalanced parenthesis"
#define MISSING_ENDPAREN    "Terminating right paren not found"
#define INVALID_CONDITION_STR "Invalid condition string"

/* critical errors from which it'll try to continue... */

#define EXCEED_TEMPLATEREC     "Exceeded length for template record"
#define EXCEED_NEWPHRASE       "Exceeded length for new phrase length - contact HRL"
#define EXCEED_VALUESTR        "Exceeded length for raw value string - contact HRL"
#define EXCEED_LONGSTR         "Exceeded length for long string - contact HRL"
#define EXCEED_STRING          "Exceeded length for generic string - contact HRL"


/*********************** FATAL ERRORS  ****************************/


#endif

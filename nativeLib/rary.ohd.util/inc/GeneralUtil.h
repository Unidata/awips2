/*
	File:		GeneralUtil.h
	Date:		4/14/1997
	Author:		Paul Taylor

	Purpose:	General Header file for General Utilities.
*/


#ifndef GENERAL_UTIL_H
#define GENERAL_UTIL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include <limits.h>


#define INF	INT_MAX

#define	GT	">"
#define	GE	">="

#define	LT	"<"
#define	LE	"<="

/* shef_duplicate token's acceptable values */
#define ALWAYS_OVERWRITE            1
#define USE_REVCODE                 2
#define IF_DIFFERENT                3
#define IF_DIFFERENT_OR_REVCODE     4
#define IF_DIFFERENT_AND_REVCODE    5

#define DONT_UPDATE_ACTION          0
#define UPDATE_ACTION               1
#define IF_DIFFERENT_UPDATE_ACTION  2

#define STMT_LEN	512

/*
typedef enum bool
{
    false,
    true
} bool;
*/

/*
	Check Bounds function.
*/
int	CheckBounds(char* buf,
		    char* lo_cond, double* lo,
		    char* hi_cond, double* hi);


/*
	General String functions.
*/
	/* (Since Text_AddString uses malloc, be sure to use Text_FreeString
	    on "buf_str" when you are done with it!!) */

void	Text_AddString(char** buf_str, char* append_str, int init_flag);
void	Text_FreeString(char** free_string);
int	IsBlankStr(char *str);
void    strip_tblanks(char *str);
void    strip_lblanks(char *str);
void    compress_blanks (char *str);
void    chglower (char *str);
void    chgupper (char *str);

/*
   C-callbable routine which supplies the length of a FORTRAN
   style string which is not null terminated.
*/
int Fstrlen ( const char * Fstr, const int endpos ) ;

/*
   FORTRAN callable routine which returns the length of a C-style string.
*/
int cstrlen ( const char * Cstr ) ;

/*
	"Word Wrap" functions that can be performed on text.
*/
char*	Text_WordWrap(char* buffer, int num_cols, int left_margin);

char*	Text_sgets(char* buffer);

char*	Text_RemoveEmbeddedNewlines(char* buffer);
char*	Text_RemoveLeadingWhitespace(char* buffer);
char*	Text_AddLeftMargin(char *buffer, int left_margin);

void	Text_GetLine(char** text, int num_cols, char** line_str);

int	Text_CountNewLines(char* text);
void	Text_Paginate(char** text); /* concat with '\n' until end of page */


/* apps_defaults functions */

void get_hydro_tokens(char	*file_env,
		     char 	*file_name,
		     char	*token_name,
		     char	*token_val,
		     int	*val_len);

int get_apps_defaults(char *request, int *request_len, char *reply, int *reply_len);

char * getAppDefault(char *);

/* getcpu is in .../nwsrfs/util/src/date_time dir */

void getcpu(long *);

/* Function to send emails.*/
int mail_send(char* mail_from, char* rcpt_list, char* subject, char* filename, FILE* logfp);
char * log_time ( );

#endif

/* Returns the value of the shef_duplicate token translated from
   a string to one of the constants defined above. */
void get_and_translate_duplicate_token ( int * shef_duplicate_value );

/* Returns the value of upd_action */
int determine_update_action(int options_duplicate,
                            int shefrec_rev);


/*
    Rounds while maintains a number of decimal places equal to the
    second argument.  The decimalPlacesToMaintain should be in the
    range 0..9.  If not, the function will adjust the argument to the
    nearest legal value.
*/
double round_to_places(double numberToRound,
		       int decimalPlacesToMaintain);

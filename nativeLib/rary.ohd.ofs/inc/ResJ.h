/* ----------------------------------------------------------------------------
** Resj.h - include file for general utility routines.
** ============================================================================
** Copyright:	See the ../src/COPYRIGHT.
** ============================================================================
** History:
**
** 17 Feb 1998 MJR, RTi			Copied from AHPS libUtil.
** 09 May 2001 James R. VanShaar, RTi	Added PrintNWSError, PrintNWSWarning
** 12 Feb 2004 JRV, RTi		Added FindMatch
** ----------------------------------------------------------------------------
*/

#ifndef ResJ_INCLUDED
#define ResJ_INCLUDED

#include <ctype.h>
#include <math.h>		/* for some reason messes up the HP C++ */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#ifdef UNIX
#include <sys/types.h>		/* for mkdir among other things */
#include <sys/stat.h>		/* file status routines */

#include <dirent.h>		/* directory routines */
#include <time.h>
#include <unistd.h>		/* for "getwd" */
#endif	/* UNIX */

#define mvRESJFilesToPerm mvresjfilestoperm
#define rmRESJfs5files rmresjfs5files
#ifdef LINX
  #define generateRESJFileName generateresjfilename_
#else
  #define generateRESJFileName generateresjfilename
#endif


#define DIR_NOHIDDEN		0x1	/* ignore hidden files */
#define DIR_NOCURRENT		0x2	/* ignore current dir, parent dir */
#define DIR_NOFILES		0x4	/* do files */
#define DIR_NODIRS		0x8	/* do directories */
#define DIR_NOLINECOUNT	0x10		/* do not count lines in file */

#ifdef UNIX
#define DIRSEP_CHAR		'/'		/* directory separator */
#define DIRSEP_CHAR_STRING	"/"
#else
#define DIRSEP_CHAR		'\\'
#define DIRSEP_CHAR_STRING	"\\"
#endif

#define EMPTY_FIELD_CHAR  '?'     /* indicates empty field */
#define FILEEXT_DELIM "."	/* default file extension delimiter */
#define DELIM_SKIP_BLANKS 0x1 /* skip delimiters that are sequential*/
#define DELIM_ALLOW_STRINGS 0x2 /* skip delimiters that are sequential*/
#define BIGSTRING     2000        /* size of long strings */
#define MAXC     2048        /* max characters in strings */
#define MISSING	-999.0		/* Missing data value */

#define IsEmptyFieldChar(c)   (((c)=='\0' || (c)==EMPTY_FIELD_CHAR) ? 1:0)
#define IsEmptyField(s)   (((*s)=='\0' || (*s)==EMPTY_FIELD_CHAR) ? 1:0)
#define MAX(a,b)	(((a)>(b))?(a) : (b))	/* maximum of 2 #s */
#define MIN(a,b)	(((a)<(b))?(a) : (b))	/* minimum of 2 #s */
#define IsMissing(v)  ((v==MISSING) ? 1:0)

#define STATUS_FAILURE		1	/* return codes for routines */
#define STATUS_SUCCESS		0

#define STATUS_CANNOT_READ_FILE	2

#define SORT_QUICK            3       /* quick sort */
#define SORT_ASCENDING        1       /* sort orders */
#define SORT_DESCENDING       2

#define HOST_SHORT				0x1     /* short host names */
#define HOST_LONG				0x2     /* short host names */
#define HOST_IP					0x4     /* IP address */

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#ifndef ESPMAXC
#define ESPMAXC 1024
#endif

#ifndef MAX_MESSAGE_FILE
#define MAX_MESSAGE_FILE  5 /* max FILEs for each type of message routine */
#endif

#ifndef PAD_BACK
#define PAD_BACK	0x1
#define PAD_FRONT	0x2
#define PAD_FRONT_BACK	(PAD_FRONT|PAD_BACK)
#endif

#define UNIT_DIR			1	/* units of measure */
#define UNIT_CONST			2
#define UNIT_ENERGY			3
#define UNIT_ENERGY_PER_AREA		4
#define UNIT_POWER			5
#define UNIT_LENGTH			6
#define UNIT_SPEED			7
#define UNIT_AREA			8
#define UNIT_VOLUME			9
#define UNIT_DISCHARGE			10
#define UNIT_PRESSURE			11
#define UNIT_TEMP			12
#define UNIT_TIME			13

#define	RETURN_KEYWORDS 0 
#define	RETURN_NO_KEYWORDS 1

#define	DATA_AS_ROWS 1 
#define	DATA_AS_COLUMNS 2 

#define JULIAN1900DAYS        693960 /* Julian days as of Dec 31, 1899,
                                           relative to Jan 1, O A.D. */


#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif /* C++ */

static int      MonthDays[] = {       31, 28, 31, 30, 31, 30,
                                        31, 31, 30, 31, 30, 31 };

static int      MonthYearDays[] = {     0,  31,  59,  90, 120, 151,
                                        181, 212, 243, 273, 304, 334 };

typedef struct _MsgData {
	int level;
	FILE    *fp;
	int (*func)(int, char *, char *);
} MsgData;

void	PrintDebug (		int, char *, char *, ... ),
	PrintError (		char *, char *, ... ),
	PrintStatus (		int, char *, char *, ... ),
	PrintWarning (		int, char *, char *, ... );
char *MonthAbbreviation (      int );
int
	SetDebugLevel (		int, int ),
	SetDebugFILE (		FILE *, int ),
	SetErrorFILE (		FILE *, int),
	SetProgramData (    char *, char *, int, char *[] , char **),
	SetStatusLevel (	int, int ),
	SetStatusFILE (		FILE *, int ),
	SetWarningLevel (	int, int ),
	SetWarningFILE (	FILE *, int ),
	SetOutputFILEs (  FILE *, int ),
	SetOutputFunctions (  int (*)(int, char *, char *), int, char * ),

	PrintNWSError (		int el, char *routine, char* message ),
	PrintNWSWarning (	int wl, char *routine, char* message );

int	Basename (		char *, char * ),
	Dirname (		char *, char * ),
	ESPstrcasecmp (		char *, char * ),
	ESPstrncasecmp (	char *, char *, int ),
	FileReadable(		char *),
	GetConversion (		char *, char *, float *, float *, float *,
                                char * ),
	GetConversionSame (	char *, char *, float *, float * ),
	GetDataTypeInfo(	char *,char *,char *,char *, char *),
	GetDef(			char *, char * ),
	GetFileParts (		char *, char *, char *, char *,
				char *, char * ),
	GetUnitsDimension (	char * ,char *),
	GetUnitsFormat (	char * , int, int *, char * ),
	GetUnitsType (		char *, int * ),
	GetUser (		char * ),
	HRAPToLatLon (		float, float, float *, float * ),
	LatLonToHRAP (		float, float, float *, float * ),
	LinearRegression(	int,float*,float*,float*,float*,float*),
	SpawnProcess (		char *, FILE *, FILE *, unsigned int ),
	StringListLength (	char **, int * ),
	StringMatchesRegExp (	char *, char * ),
	StringReadToDelim( char*, char, char* ),
	FileReadable ( char *),
	FindMatch ( char*, char**, char** ),
	GetDateFromJulianHour1900 ( int, int *, int *, int *, int * ),
	GetHostname ( char *, int *, unsigned int ),
	GetJulianHour1900FromDate ( int, int, int, int, int * ),
	GetMax ( int n, float *x, float *max , int *ndmx),
	GetMean ( int n, float *x, float *mean ),
	GetMeanFromInst ( int n, float *x, float *mean),
	GetMin ( int n, float *x, float *min, int *ndmn ),
	GetProgramData ( char *, char *),
	GetSum ( int n, float *x, float *sum ),
	GetSystemTimeString ( char *, char *),
	GetNdto ( int n, float criteria, int direction, float *x, int *ndto ),
	GetNdis ( int n, float criteria, int direction, float *x, int *ndis ),
	GetStdDev ( int, float *, float *),
	GetVar	( int, float *, float * ),
	GetWorkingDir ( char *, int *),
	MonthFromAbbreviation (   char * ),
	Normalize (int n, float *data),
	PrintCreatorHeader ( FILE *, char *, int, unsigned int ),
	RemoveNewline ( char * ),
	ReverseArrayF ( float *data, int ndata ),
	ReverseArrayI ( int *data, int ndata ),
	UnsortF ( float *data, int ndata, int *sort_order ),
	UnpadString ( char *, char *, unsigned long int ),
	SetProgramCommandFile(char * ),
	SortF ( float *, int , int , int , int *, int ),
	CumNormDistInv (float Prob, float *Quant),
	GetJulianDay1900FromDate(int,int,int,int *),
	NumDaysInMonth(int,int),
	NumDaysInMonthS(int,int,int),		/* return number of days in more than 1 month */
	NumDaysInYear(int),
	ReplaceChar( char*, char, char ),
	ToUpper( char* ),
	IsInteger( char* ),
	IsLeapYear(int ),
	IsValidDay(int,int,int),
	IsValidMonth(int),
	IsValidHour(int),

	IsDouble( char* );

float Interp ( float x, float xmin, float xmax, float ymin, float ymax );

double TableInterp( double x_value, double** table, int table_length, int );

char	
	**AddFormattedStringToStringList( char **, int*, char*, ... ),
	**AddListToStringList ( char **, char **, int * ),
	**AddToStringList (	char **, char *, int * ),
	**ArrayToStringList ( int, char *[] ),
	**BreakStringList ( char *, char *, unsigned int, int *),
	**FreeStringList (	char ** ),
	**GetDirList (		char *, char **, char **, unsigned, int * ),
	**GetFilesFromPathList ( char**, char*, int* ),
	**GetFilesUsingRegExp (	char *, char **, unsigned int, int * ),
	**GetSubStringList (	char**, int, char*, int*, int ),
	**SortStringList (	char **, unsigned long int );

int rmresjfs5files( float *pold, int *iseg );
int mvresjfilestoperm( float *p, int *iseg );

#ifdef LINX
  void generateresjfilename_( float*, char*, int*, int* );
#else
  void generateresjfilename( float*, char*, int*, int* );
#endif
#if defined(__cplusplus) || defined(c_plusplus)
}
#endif /* C++ */

#endif

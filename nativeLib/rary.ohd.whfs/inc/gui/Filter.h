/*
	File:		Filter.h
	Date:		7/25/94
	Author:		Dale Shelton
	
	Purpose:
	
*/

#ifndef Filter_h
#define Filter_h


#ifdef __cplusplus
extern "C" {
#endif

/*
	Filters to perform type checking for
	base types.
*/
int	isAlpha(char c);
int	isAlphaNum(char c);
int	isFloat(char c);
int	isNumeric(char c);
int	isDate(char c);
int	isTime(char c);


/*
	Helper functions.
*/
int	isApostrophe(char c);
int	isColon(char c);
int	isHyphen(char c);
int	isDot(char c);
int	isMinus(char c);
int	isSlash(char c);
int	isSpace(char c);


/*
	Character checks.
*/
int	isUpper(char c);
int	isLower(char c);

#ifdef __cplusplus
}
#endif

#endif

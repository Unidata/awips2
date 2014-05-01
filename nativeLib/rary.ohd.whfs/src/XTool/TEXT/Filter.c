/*
	File:		Filter.c
	Date:		7/25/94
	Author:		Dale Shelton
	
	Purpose:	
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "Filter.h"


int isAlpha(char c)
{
	if (isalpha((int) c))
		return(1);
	return(0);
}


int isAlphaNum(char c)
{
	if (isAlpha(c) || isNumeric(c))
		return(1);
	return(0);
}


int isFloat(char c)
{
	if (isNumeric(c) || isDot(c))
		return(1);
	return(0);
}


int isNumeric(char c)
{
	if (isdigit((int) c))
		return(1);
	return(0);
}


int isDate(char c)
{
	if (isNumeric(c) || isSlash(c))
		return(1);
	return(0);
}


int isTime(char c)
{
	if (isNumeric(c) || isColon(c))
		return(1);
	return(0);
}




/*************************************************/


int isApostrophe(char c)
{
	if (c == '\'')
		return(1);
	return(0);
}


int isColon(char c)
{
	if (c == ':')
		return(1);
	return(0);
}


int isDot(char c)
{
	if (c == '.')
		return(1);
	return(0);
}


int isHyphen(char c)
{
	if (c == '-')
		return(1);
	return(0);
}


int isMinus(char c)
{
	if (c == '-')
		return(1);
	return(0);
}


int isSlash(char c)
{
	if (c == '/')
		return(1);
	return(0);
}


int isSpace(char c)
{
	if (c == ' ')
	   	return(1);
	return(0);
}



/*******************************************/



int isUpper(char c)
{
	if (isupper(c))
		return(1);
	return(0);
}


int isLower(char c)
{
	if (islower(c))
		return(1);
	return(0);
}




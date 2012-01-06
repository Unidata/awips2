/*
	File:		cvt_to_latlon.c
	Date:		August 1994
	Authors:	Dale Shelton, Chip Gobs, Paul Taylor (4/1/97)
	
	Purpose:	Provide support for converting lat/lon double values
			to & from regular string variables.
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include "DbmsDefs.h"
#include "cvt_latlon.h"

#define STR_LEN 4
#define NUM_CHARS_IN_SEG

/*****************************************************************************/

double  cvt_to_latlon(char *str, int negate)
{
   double coord;
	
	if  (isPackedLatLon(str))
	{
		coord = cvt_packed_format(str, negate);
	}   
	
	else    /* spaced format */
	{
		coord = cvt_spaced_format(str, negate);
	}   
	
	return (coord);
}


/*****************************************************************************/


int	isPackedLatLon(char *str)
{
	if (  abs(atof(str)) > 999 )  /* contains 4 digits in a row */   
        {
		return 1;   
	}
	else
	{
		return 0;   
	}   
}   


/*****************************************************************************/


double	cvt_spaced_format(char *str, int negate)
{ 
	char		*s1 = NULL ,
			temp[LAT_LON_LEN];
	
	double		coord,
			deg,
			min,
			sec;	
	
	
        if ( str == NULL )
        {
           return MAX_DBL ;
        }

	strcpy(temp, str);
	
	/*
		Convert value.
	*/
	s1  = strtok(temp, " \t");
	deg = atof(s1);	
	s1  = strtok(NULL, " \t");

        if ( s1 == NULL )
        {
           min = 0.0 ;
           sec = 0.0 ;
        } 
        else
        {
	   min = atof ( s1 ) ;
	   s1  = strtok ( NULL , " \t");

	   if ( s1 == NULL )
           {
	      sec = 0.0;
           }
	   else
           {
	      sec = atof ( s1 ) ;
           }

	   min += sec / 60.00;
        }

	coord = (fabs(deg) + (min / 60.00));

	if ( deg < 0 ) coord *= -1.0 ;

	if ( negate )
        {
	   coord *= -1.0 ;
        }
	
	return ( coord ) ;
}


/*****************************************************************************/


double	cvt_packed_format(char *str, int negate)
{
	char		temp[LAT_LON_LEN],
			deg_c[STR_LEN],
			min_c[STR_LEN],
			sec_c[STR_LEN];
		
	double		coord,
			deg,
			min,
			sec;
	
	int		use3digits = 0;
	
	
	/*
		Make sure that for "strlen(str) > 6"
		we use 3 characters for the degree-part.
	*/
	if (strlen(str) > 6)
	{   
		use3digits = 1;
	}
	
	
	/*
		copy string so don't corrupt original
	*/
	strncpy(temp, str, sizeof(temp));

	
	/*
		copy three characters over from  str to the sub string
		vars
	*/
	memset(deg_c, '\0', sizeof(deg_c));
	memset(min_c, '\0', sizeof(min_c));
       	memset(sec_c, '\0', sizeof(sec_c));
	       

	if (use3digits)  /* degree-part is 3 characters long */
	{
		strncpy(deg_c, temp, 3);
		strncpy(min_c, &temp[3], 2);
		strncpy(sec_c, &temp[5], 2);
	}
	
	else  /* degree-part is 2 characters long */
	{
	 	strncpy(deg_c, temp, 2);
		strncpy(min_c, &temp[2], 2);
		strncpy(sec_c, &temp[4], 2);  	   
	}   
		
	
	/*
		Convert values to numbers
	*/
	deg = atof(deg_c);	
	min = atof(min_c);
	sec = atof(sec_c);
	
	
	/*
		Convert to decimal
	*/
	min += sec / 60.00;
	coord  = deg + (min / 60.00);
	
	
	if (negate)
		coord *= -1.0;
	
	return(coord);   
}


/*****************************************************************************/


double  cvt_to_lat(char *str)
{

	return(cvt_to_latlon(str, 0));
}


/*****************************************************************************/


double  cvt_to_lon(char *str)
{
 
	return(cvt_to_latlon(str, 1));
}


/*****************************************************************************/



double	bcvt_to_lat(char *str)
{
	char	buf[3],
		*s1;
	
	double	deg,
		min,
		sec;
		
	s1  = strtok(str, ".");
	str = (char *) NULL;
	deg = atof(s1);
	
	strncpy(buf, str, 2);
	min = atof(buf);
	
	buf[0] = str[2];
	buf[1] = str[3];
	buf[2] = '\0';
	sec = atof(buf);
	
	printf("DEG: %f  MIN %f  SEC %f\n", deg, min, sec);
	return(min);	
}


/*****************************************************************************/


double  ncvt_to_latlon(char *s)
{
	char	buf[LAT_LON_LEN];
	char	*sPtr, 
		*tPtr;
	
	double	latlon;
	int	first;
	
	
	sPtr   = s;
	first  = 1;
	latlon = 0.0;
	memset(buf, '\0', LAT_LON_LEN);

	
	/*
		Convert value.
	*/	
	while ((tPtr = strtok(sPtr, " \t")) != (char *)NULL)
	{
		if (first)
		{
			strcpy(buf, tPtr);
			strcat(buf, ".");
			first = 0;
		}
		else
		{
			strcat(buf, tPtr);
		}
		sPtr = (char *)NULL;
	}
	
	latlon = atof(buf);
	return(latlon);
}


/****************************************************************************/


char*	cvt_latlon_from_double (double x)
{
   static char	buf[10];
   
   long		deg, min, sec;

   double	temp, tmpx;

   tmpx = x;
   x = fabs(x);

#ifdef DEBUG
printf("x == <%4.5lf>\n", x);
#endif

   /*
   	Take the whole part of double and use as degrees.
	Use the fractional part of double as minutes & seconds.
   */
   deg = (long) x;	/* keep the integer part as degrees */
#ifdef DEBUG
   printf("deg == <%ld>\n", deg);
#endif
   

   temp = (x - ((double)deg)) * 60.0;
   min = (long) temp;	/* compute the minutes */
#ifdef DEBUG
   printf("temp == <%4.5lf>, min == <%ld>\n", temp, min);
#endif
   

   temp = floor(((temp - ((double)min)) * 60.0) + 0.5);
   sec = (long) temp;	/* compute the seconds */
#ifdef DEBUG
   printf("temp == <%4.5lf>, sec == <%ld>\n", temp, sec);
#endif

   /*
	Check for boundary errors and correct them.
   */
   if (sec == 60)
   {
      if (min == 59)
      {
	 deg += 1;
	 min  = 0;
	 sec  = 0;
      }
      else if (min < 59)
      {
	 min += 1;
	 sec  = 0;
      }
   }

   
   /*
   	store the latlon value.
   */
   memset(&buf, '\0', sizeof(buf));
   if ( tmpx >= 0.0)
   sprintf(buf, "%02ld %02ld %02ld", deg, min, sec);
   else
   sprintf(buf, "-%02ld %02ld %02ld", deg, min, sec);

   return(buf);
}




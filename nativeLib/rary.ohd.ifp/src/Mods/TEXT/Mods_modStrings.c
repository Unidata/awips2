/****************************************************************************************/
/*											*/
/*	Mods_modStrings.c								*/
/*											*/
/*		Coded by:	Tom Adams - NWS/Hydrologic Research Laboratory		*/
/*		Date:		10/30/90						*/
/*		Modified:	11/23/94                                                */
/*                              05/19/95 - D. Page					*/
/*                              04/17/01 - A. Vo 					*/
/*                              09/07/02 - A. Vo fixed bug r21-36        		*/
/*				         ZERODIFF mod was not created correctly  	*/
/*											*/
/*		This file contains functions, callbacks & otherwise, to			*/
/*		get data from the mods_popup window (Run-time Modifications)		*/
/*		and put the data into the appropriate mods data structure, used		*/
/*		to generate ASCII data files in FORTRAN card image format for		*/
/*		the coupled NWSRFS ver. 5 FORTRAN program.				*/
/*											*/
/*		( see: "Mods_info.h" - formats A1, A2, B1, B2, or B3)			*/
/*											*/
/*											*/
/****************************************************************************************/

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "Mods_info.h"


#define MAX_KEYWORDS    9

extern char *get_timeZone_code(Display *);
extern int      selected_Mods_type;
extern char *get_fgroup_name();
extern void get_month_day_year_hour_tzc(int*,int*,int*,int*,int*,int*,int*,
                                 int*, int*, char*);
/* **************************************************************************

	make_mod_A1_string()

   ************************************************************************** */

char *make_mod_A1_string(Mods_everythingStruct *data, int whichMod, Display *display)
{

	int     i, j, returnString_length;
	char    *returnString, *cardString, string[50], str[50];
	char    *tempString;
	char    *blank = " ";
	char    *first_blank;
	char    *forward_slash = "/";
	char    *continuation_code = "&";
	int     cardString_length;
	int     julda, julhr;    /* Julian day and hour */
	int     month, day, year, hour;
	int     zondum;  /* time zone value */
	int     dlsdum;  /* Daylight savings time flag
			    1 for daylight savings time,
			    0 for standard time */
	char    *tz_code;   /* time zone code */
	char    tz_code_str[5]; /* time zone code string */



returnString = (char *)malloc(1000);
memset(returnString, '\0', 1000);

cardString = (char *)malloc(74);
memset(cardString, '\0', 74);

memset(string, '\0', 50);
memset(str, '\0', 50);


 tz_code = get_timeZone_code(display);

/* ------------------------------ CARD 1 ------------------------------ */

strcpy(returnString, ".");
strcat(returnString, data->ModArray[whichMod]->a1.command);
strcat(returnString, blank);

/********************************************************************************/
/*                                                                              */
/*      There are no dates set for type A1 Mods, but for future                 */
/*      changes, the possibility is allowed for them; there is a                */
/*      check for their presence...                                             */
/*                                                                              */
/********************************************************************************/


if(data->ModArray[whichMod]->a1.start_date != 0)     /* If it's there, Date #1...                            */
	{
	get_month_day_year_hour_tzc((int*)data->ModArray[whichMod]->a1.start_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	strcpy(tz_code_str, tz_code);
	first_blank = strstr(tz_code_str, " ");
	if(first_blank != NULL) *first_blank = '\0';

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code_str);
	}

strcat(returnString, blank);                    /* 'Space' separator between dates...                   */

if(data->ModArray[whichMod]->a1.end_date != 0)       /* If it's there, Date #2...                            */
	{
	get_month_day_year_hour_tzc((int*)data->ModArray[whichMod]->a1.end_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	strcpy(tz_code_str, tz_code);
	first_blank = strstr(tz_code_str, " ");
	if(first_blank != NULL) *first_blank = '\0';

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code_str);
	}

strcat(returnString, blank);                    /* 'Space' separator between dates...                   */

if(data->ModArray[whichMod]->a1.valid_date != 0)       /* If it's there, Date #3...                            */
	{
	get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->a1.valid_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code);
	}

strcat(returnString, "\n");                     /* End CARD 1...                                        */

/* ------------------------------ CARD 2 ------------------------------ */

if(data->ModArray[whichMod]->a1.number_of_ts != 0)
	{
	for(i = 0; i < data->ModArray[whichMod]->a1.number_of_ts; i++)
		{
		strcpy(cardString, data->ModArray[whichMod]->a1.segment_id); /* Segment ID (name)...                 */
		cardString_length = strlen(cardString);

		/* Time-series ID...                                                                            */
		if(cardString_length + strlen(data->ModArray[whichMod]->a1.info[i]->ts_id) + 1 > MAX_CARD_LENGTH)
			{                                       /* A continuation is needed...                  */
			strcat(cardString, blank);
			strcat(cardString, continuation_code);
			strcat(cardString, "\n");
			strcat(returnString, cardString);

			strcpy(cardString, data->ModArray[whichMod]->a1.info[i]->ts_id);    /* Start a new card...   */
			cardString_length = strlen(cardString);
			}
		else    {       /* There's enough room on the card for the value...                             */
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->a1.info[i]->ts_id);
			}

		/* Time-series data type...                                                                     */
		if(cardString_length + strlen(data->ModArray[whichMod]->a1.info[i]->data_type) + 1 > MAX_CARD_LENGTH)
			{                                       /* A continuation is needed...                  */
			strcat(cardString, blank);
			strcat(cardString, continuation_code);
			strcat(cardString, "\n");
			strcat(returnString, cardString);

			strcpy(cardString, data->ModArray[whichMod]->a1.info[i]->data_type); /* Start a new card...  */
			cardString_length = strlen(cardString);
			}
		else    {       /* There's enough room on the card for the value...                             */
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->a1.info[i]->data_type);
			}

		sprintf(string, "%d", data->ModArray[whichMod]->a1.info[i]->delta_t);        /* Time-series Delta_t  */
		if(cardString_length + strlen(string) + 1 > MAX_CARD_LENGTH)
			{                                       /* A continuation is needed...                  */
			strcat(cardString, blank);
			strcat(cardString, continuation_code);
			strcat(cardString, "\n");
			strcat(returnString, cardString);

			strcpy(cardString, string);                                     /* Start a new card...  */
			cardString_length = strlen(cardString);
			}
		else    {       /* There's enough room on the card for the value...                             */
			strcat(cardString, blank);
			strcat(cardString, string);
			}

		j = 0;
		while(data->ModArray[whichMod]->a1.dates[j].date != 0)
			{
			if(data->ModArray[whichMod]->a1.dates[j].type_of_date == 0)
				{                       /* The date is a SINGLE date...                         */
				strcat(cardString, blank);

				get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->a1.dates[j].date, &julda, &julhr,
							    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
				year = year%100;        /* We only need the last two digits of the 'year'...    */

				strcpy(tz_code_str, tz_code);
				first_blank = strstr(tz_code_str, " ");
				if(first_blank != NULL) *first_blank = '\0';

				sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
				strcat(string, tz_code_str);

				cardString_length = strlen(cardString);
				if(cardString_length + strlen(string) > MAX_CARD_LENGTH)
					{
					strcat(cardString, continuation_code);
					strcat(cardString, "\n");
					strcat(returnString, cardString);
					strcpy(cardString, string);
					cardString_length = strlen(cardString);
					}
				else    {
					strcat(cardString, string);
					}
				}
			else    {                       /* The date is a date RANGE...                          */
				strcat(cardString, blank);

				/*      First date...           */
				get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->a1.dates[j].date, &julda, &julhr,
							    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
				year = year%100;        /* We only need the last two digits of the 'year'...    */

				strcpy(tz_code_str, tz_code);
				first_blank = strstr(tz_code_str, " ");
				if(first_blank != NULL) *first_blank = '\0';

				sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
				strcat(string, tz_code_str);

				strcat(string, "-");

				/*      Second date...          */
				j++;    /* We incremented 'j' because these two dates need to be connected...   */
				get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->a1.dates[j].date, &julda, &julhr,
							    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);

				year = year%100;        /* We only need the last two digits of the 'year'...    */

				strcpy(tz_code_str, tz_code);
				first_blank = strstr(tz_code_str, " ");
				if(first_blank != NULL) *first_blank = '\0';

				sprintf(str, "%02d%02d%02d%02d", month, day, year, hour);
				strcat(string, str);
				strcat(string, tz_code_str);

				cardString_length = strlen(cardString);
				if(cardString_length + strlen(string) > MAX_CARD_LENGTH)
					{
					strcat(cardString, continuation_code);
					strcat(cardString, "\n");
					strcat(returnString, cardString);
					strcpy(cardString, string);
					cardString_length = strlen(cardString);
					}
				else    {
					strcat(cardString, string);
					}
				}
			j++;
			if( j == MAX_NUMBER_OF_SELECTABLE_DATES) break;
			}

		strcat(cardString, "\n");
		strcat(returnString, cardString);
		returnString_length = strlen(returnString);
		cardString_length = strlen(cardString);
		if(returnString_length + cardString_length > 500)
			{
			returnString = (char *) realloc(returnString, returnString_length + cardString_length);
			returnString_length = strlen(returnString);
			}

		memset(cardString, '\0', 74);
		}
	}
else    {       /* All Operations have been selected ==> we don't need to handle the Operation names... */
	strcpy(cardString, data->ModArray[whichMod]->a1.segment_id);       /* Segment ID (name)...           */
	cardString_length = strlen(cardString);

	/* Time-series ID...                                                                            */
	if(cardString_length + strlen(data->ModArray[whichMod]->a1.info[i]->ts_id) + 1 > MAX_CARD_LENGTH)
		{                                       /* A continuation is needed...                  */
		strcat(cardString, blank);
		strcat(cardString, continuation_code);
		strcat(cardString, "\n");
		strcat(returnString, cardString);

		strcpy(cardString, data->ModArray[whichMod]->a1.info[i]->ts_id);    /* Start a new card...   */
		cardString_length = strlen(cardString);
		}
	else    {       /* There's enough room on the card for the value...                             */
		strcat(cardString, blank);
		strcat(cardString, data->ModArray[whichMod]->a1.info[i]->ts_id);
		}

	/* Time-series data type...                                                                     */
	if(cardString_length + strlen(data->ModArray[whichMod]->a1.info[i]->data_type) + 1 > MAX_CARD_LENGTH)
		{                                       /* A continuation is needed...                  */
		strcat(cardString, blank);
		strcat(cardString, continuation_code);
		strcat(cardString, "\n");
		strcat(returnString, cardString);

		strcpy(cardString, data->ModArray[whichMod]->a1.info[i]->data_type); /* Start a new card...  */
		cardString_length = strlen(cardString);
		}
	else    {       /* There's enough room on the card for the value...                             */
		strcat(cardString, blank);
		strcat(cardString, data->ModArray[whichMod]->a1.info[i]->data_type);
		}

	sprintf(string, "%d", data->ModArray[whichMod]->a1.info[i]->delta_t);        /* Time-series Delta_t  */
	if(cardString_length + strlen(string) + 1 > MAX_CARD_LENGTH)
		{                                       /* A continuation is needed...                  */
		strcat(cardString, blank);
		strcat(cardString, continuation_code);
		strcat(cardString, "\n");
		strcat(returnString, cardString);

		strcpy(cardString, string);                                     /* Start a new card...  */
		cardString_length = strlen(cardString);
		}
	else    {       /* There's enough room on the card for the value...                             */
		strcat(cardString, blank);
		strcat(cardString, string);
		}

	j = 0;
	while(data->ModArray[whichMod]->a1.dates[j].date != 0)
		{
		if(data->ModArray[whichMod]->a1.dates[j].type_of_date == 0)
			{                       /* The date is a SINGLE date...                         */
			strcat(cardString, blank);

			get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->a1.dates[j].date, &julda, &julhr,
						    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
			year = year%100;        /* We only need the last two digits of the 'year'...    */

			strcpy(tz_code_str, tz_code);
			first_blank = strstr(tz_code_str, " ");
			if(first_blank != NULL) *first_blank = '\0';

			sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
			strcat(string, tz_code_str);

			cardString_length = strlen(cardString);
			if(cardString_length + strlen(string) > MAX_CARD_LENGTH)
				{
				strcat(cardString, continuation_code);
				strcat(cardString, "\n");
				strcat(returnString, cardString);
				strcpy(cardString, string);
				cardString_length = strlen(cardString);
				}
			else strcat(cardString, string);
			}
		else    {                       /* The date is a date RANGE...                          */
			strcat(cardString, blank);

			/*      First date...           */
			get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->a1.dates[j].date, &julda, &julhr,
						    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
			year = year%100;        /* We only need the last two digits of the 'year'...    */

			strcpy(tz_code_str, tz_code);
			first_blank = strstr(tz_code_str, " ");
			if(first_blank != NULL) *first_blank = '\0';

			sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
			strcat(string, tz_code_str);

			strcat(string, "-");

			/*      Second date...          */
			j++;    /* We incremented 'j' because these two dates need to be connected...   */
			get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->a1.dates[j].date, &julda, &julhr,
						    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);

			year = year%100;        /* We only need the last two digits of the 'year'...    */

			strcpy(tz_code_str, tz_code);
			first_blank = strstr(tz_code_str, " ");
			if(first_blank != NULL) *first_blank = '\0';

			sprintf(str, "%02d%02d%02d%02d", month, day, year, hour);
			strcat(string, str);
			strcat(string, tz_code_str);

			cardString_length = strlen(cardString);
			if(cardString_length + strlen(string) > MAX_CARD_LENGTH)
				{
				strcat(cardString, continuation_code);
				strcat(cardString, "\n");
				strcat(returnString, cardString);
				strcpy(cardString, string);
				cardString_length = strlen(cardString);
				}
			else strcat(cardString, string);
			}
		j++;
		if( j == MAX_NUMBER_OF_SELECTABLE_DATES) break;
		}

	strcat(cardString, "\n");
	strcat(returnString, cardString);
	memset(cardString, '\0', 74);
	}



returnString_length = strlen(returnString) + 1;
tempString = (char *) realloc(returnString, returnString_length);
return(tempString);

}



/* **************************************************************************

	make_mod_A2_string()

   ************************************************************************** */

char *make_mod_A2_string(Mods_everythingStruct *data, int whichMod, Display *display)
{

    return NULL;

}



/* **************************************************************************

	make_mod_B1_string()

   ************************************************************************** */

char *make_mod_B1_string(Mods_everythingStruct *data, int whichMod, Display *display)
{

	int     i, j, returnString_length,segstr_len;
	char    *returnString, *cardString, string[50], str[50], *tempString;
	char    *blank = " ";
	char    *first_blank;
	char    *forward_slash = "/";
	char    *continuation_code = "&";
	int     cardString_length;
	int     julda, julhr;      /* Julian day and hour */
	int     month, day, year, hour;
	int     zondum;            /* time zone value */
	int     dlsdum;            /* Daylight savings time flag
				      1 for daylight savings time,
				      0 for standard time */
	char    *tz_code;          /* time zone code */
	char    tz_code_str[5];    /* time zone code string */
	char    *tmpfg;     
	char    *tmp;     



returnString = (char *)malloc(1000);
memset(returnString, '\0', 1000);

cardString = (char *)malloc(74);
memset(cardString, '\0', 74);

memset(string, '\0', 50);
memset(str, '\0', 50);


 tz_code = get_timeZone_code(display);

/* ------------------------------ CARD 1 ------------------------------ */

strcpy(returnString, ".");
strcat(returnString, data->ModArray[whichMod]->b1.command);
strcat(returnString, blank);


if(data->ModArray[whichMod]->b1.start_date != 0)     /* If it's there, Date #1...                            */
	{
	get_month_day_year_hour_tzc((int*)data->ModArray[whichMod]->b1.start_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code);
	}
	
strcat(returnString, blank);                    /* 'Space' separator between dates...                   */

if(data->ModArray[whichMod]->b1.end_date != 0)       /* If it's there, Date #2...                            */
	{
	get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b1.end_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code);
	}

strcat(returnString, blank);                    /* 'Space' separator between dates...                   */


if(data->ModArray[whichMod]->b1.valid_date != 0)       /* If it's there, Date #3...                            */
	{
	get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b1.valid_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code);
	}

/* add FGROUP id toward the end of card 1*/

tmpfg = get_fgroup_name();
if(tmpfg == NULL)
      {
      printf("No Forecast ID found !!\n");
      return 0;
      }
tmp = strstr(data->ModArray[whichMod]->b1.id,tmpfg);

if(tmp != NULL)
	{
	strcat(returnString, "  FGROUP");
	}

strcat(returnString, "\n");                     /* End CARD 1...                                        */


/* ------------------------------ CARD 2 ------------------------------ */


if(data->ModArray[whichMod]->b1.number_of_opers != 0)
	{
	for(i = 0; i < data->ModArray[whichMod]->b1.number_of_opers; i++)
		{
			strcpy(cardString, data->ModArray[whichMod]->b1.id);       /* Segment ID (name)...           */

		cardString_length = strlen(cardString);
		if(cardString_length + strlen(data->ModArray[whichMod]->b1.keyword) + 1 > MAX_CARD_LENGTH)
			{                                       /* A continuation is needed...          */
			strcat(cardString, blank);
			strcat(cardString, continuation_code);
			strcat(cardString, "\n");
			strcat(returnString, cardString);

			strcpy(cardString, data->ModArray[whichMod]->b1.keyword);    /* Start a new card...  */
			cardString_length = strlen(cardString);
			}
		else    {
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->b1.keyword);
			}

		j = 0;
		while(data->ModArray[whichMod]->b1.dates[j].date != 0)
			{
			if(data->ModArray[whichMod]->b1.dates[j].type_of_date == 0)
				{                       /* The date is a SINGLE date...                         */
				strcat(cardString, blank);

				get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b1.dates[j].date, &julda, &julhr,
							    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
				year = year%100;        /* We only need the last two digits of the 'year'...    */

				strcpy(tz_code_str, tz_code);
				first_blank = strstr(tz_code_str, " ");
				if(first_blank != NULL) *first_blank = '\0';

				sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
				strcat(string, tz_code_str);

				cardString_length = strlen(cardString);
				if(cardString_length + strlen(string) > MAX_CARD_LENGTH)
					{
					strcat(cardString, continuation_code);
					strcat(cardString, "\n");
					strcat(returnString, cardString);
					strcpy(cardString, string);
					cardString_length = strlen(cardString);
					}
				else strcat(cardString, string);
				}
			else    {                       /* The date is a date RANGE...                          */
				strcat(cardString, blank);

				/*      First date...           */
				get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b1.dates[j].date, &julda, &julhr,
							    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
				year = year%100;        /* We only need the last two digits of the 'year'...    */

				strcpy(tz_code_str, tz_code);
				first_blank = strstr(tz_code_str, " ");
				if(first_blank != NULL) *first_blank = '\0';

				sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
				strcat(string, tz_code_str);

				strcat(string, "-");

				/*      Second date...          */
				j++;    /* We incremented 'j' because these two dates need to be connected...   */
				get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b1.dates[j].date, &julda, &julhr,
							    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);

				year = year%100;        /* We only need the last two digits of the 'year'...    */

				strcpy(tz_code_str, tz_code);
				first_blank = strstr(tz_code_str, " ");
				if(first_blank != NULL) *first_blank = '\0';

				sprintf(str, "%02d%02d%02d%02d", month, day, year, hour);
				strcat(string, str);
				strcat(string, tz_code_str);

				cardString_length = strlen(cardString);
				if(cardString_length + strlen(string) > MAX_CARD_LENGTH)
					{
					strcat(cardString, continuation_code);
					strcat(cardString, "\n");
					strcat(returnString, cardString);
					strcpy(cardString, string);
					cardString_length = strlen(cardString);
					}
				else 
				{
				   strcat(cardString, string);
				   /* need to update cardString_length - dp 24 June 1997 */
				   cardString_length = strlen(cardString);
				}
				}
			j++;
			if( j == MAX_NUMBER_OF_SELECTABLE_DATES) break;
			}


		/* Operation name...                                                                    */

		if(cardString_length + strlen(data->ModArray[whichMod]->b1.opname[i]) + 3 > MAX_CARD_LENGTH)
			{                                       /* A continuation is needed...          */
			strcat(cardString, blank);
			strcat(cardString, continuation_code);
			strcat(cardString, "\n");
			strcat(returnString, cardString);

			strcpy(cardString, forward_slash);      /* Start a new card...                  */
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->b1.opname[i]);
			strcat(cardString, "\n");
			}
		else    {                       /* The current card has room for the Operation name...  */
			strcat(cardString, blank);
			strcat(cardString, forward_slash);
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->b1.opname[i]);
			strcat(cardString, "\n");
			}

		strcat(returnString, cardString);
		memset(cardString, '\0', 74);
		}
	}
else    {       /* All Operations have been selected ==> we don't need to handle the Operation names... */
		/*
		if (selected_Mods_type == 2)
		{
        		strcpy(cardString, range_selected.seg_str[range_selected.first]);	
			strcat(cardString, "-");
        		strcat(cardString, range_selected.seg_str[range_selected.last]);	
		}
		else 
		*/
		strcpy(cardString, data->ModArray[whichMod]->b1.id);       /* Segment ID (name)...           */

	cardString_length = strlen(cardString);

	j = 0;
	if(cardString_length + strlen(data->ModArray[whichMod]->b1.keyword) + 1 > MAX_CARD_LENGTH)
		{                                       /* A continuation is needed...          */
		strcat(cardString, blank);
		strcat(cardString, continuation_code);
		strcat(cardString, "\n");
		strcat(returnString, cardString);

		strcpy(cardString, data->ModArray[whichMod]->b1.keyword);    /* Start a new card...  */
		cardString_length = strlen(cardString);
		}
	else    {
		strcat(cardString, blank);
		strcat(cardString, data->ModArray[whichMod]->b1.keyword);
		}

	while(data->ModArray[whichMod]->b1.dates[j].date != 0)
		{
		if(data->ModArray[whichMod]->b1.dates[j].type_of_date == 0)
			{                       /* The date is a SINGLE date...                         */
			strcat(cardString, blank);

			get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b1.dates[j].date, &julda, &julhr,
						    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
			year = year%100;        /* We only need the last two digits of the 'year'...    */

			strcpy(tz_code_str, tz_code);
			first_blank = strstr(tz_code_str, " ");
			if(first_blank != NULL) *first_blank = '\0';

			sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
			strcat(string, tz_code_str);

			cardString_length = strlen(cardString);
			if(cardString_length + strlen(string) > MAX_CARD_LENGTH)
				{
				strcat(cardString, continuation_code);
				strcat(cardString, "\n");
				strcat(returnString, cardString);
				strcpy(cardString, string);
				cardString_length = strlen(cardString);
				}
			else strcat(cardString, string);
			}
		else    {                       /* The date is a date RANGE...                          */
			strcat(cardString, blank);

			/*      First date...           */
			get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b1.dates[j].date, &julda, &julhr,
						    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
			year = year%100;        /* We only need the last two digits of the 'year'...    */

			strcpy(tz_code_str, tz_code);
			first_blank = strstr(tz_code_str, " ");
			if(first_blank != NULL) *first_blank = '\0';

			sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
			strcat(string, tz_code_str);

			strcat(string, "-");

			/*      Second date...          */
			j++;    /* We incremented 'j' because these two dates need to be connected...   */
			get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b1.dates[j].date, &julda, &julhr,
						    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);

			year = year%100;        /* We only need the last two digits of the 'year'...    */

			strcpy(tz_code_str, tz_code);
			first_blank = strstr(tz_code_str, " ");
			if(first_blank != NULL) *first_blank = '\0';

			sprintf(str, "%02d%02d%02d%02d", month, day, year, hour);
			strcat(string, str);
			strcat(string, tz_code_str);

			cardString_length = strlen(cardString);
			if(cardString_length + strlen(string) > MAX_CARD_LENGTH)
				{
				strcat(cardString, continuation_code);
				strcat(cardString, "\n");
				strcat(returnString, cardString);
				strcpy(cardString, string);
				cardString_length = strlen(cardString);
				}
			else strcat(cardString, string);
			}
		j++;
		if( j == MAX_NUMBER_OF_SELECTABLE_DATES) break;
		}

	strcat(cardString, "\n");
	strcat(returnString, cardString);
	memset(cardString, '\0', 74);
	}


returnString_length = strlen(returnString) + 1;
tempString = (char *) realloc(returnString, returnString_length);

return(tempString);

}



/* **************************************************************************

	make_mod_B2_string()

   ************************************************************************** */

char *make_mod_B2_string(Mods_everythingStruct *data, int whichMod, Display *display)
{

	int     i, j, returnString_length,segstr_len;
	char    *returnString, *cardString, string[50], *tempString, tmpstring[50];
	char    *blank = " ";
	char    *first_blank;
	char    *forward_slash = "/";
	char    *continuation_code = "&";
	int     cardString_length;
	int     julda, julhr;   /* Julian day and hour */
	int     month, day, year, hour;
	int     zondum;  /* time zone value */
	int     dlsdum;  /* Daylight savings time flag
			    1 for daylight savings time,
			    0 for standard time */
	char    *tz_code; /* time zone code */
	char    tz_code_str[5]; /* time zone code string */

	char    *tmpfg,*tmp;
       
        int     ii;
returnString = (char *)malloc(500);
memset(returnString, '\0', 500);

cardString = (char *)malloc(74);
memset(cardString, '\0', 74);

memset(string, '\0', 50);

 tz_code = get_timeZone_code(display);
     
/* ------------------------------ CARD 1 ------------------------------ */

strcpy(returnString, ".");
strcat(returnString, data->ModArray[whichMod]->b2.command);
strcat(returnString, blank);

if(data->ModArray[whichMod]->b2.start_date != 0)     /* If it's there, Date #1...                            */
	{
	get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b2.start_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	strcpy(tz_code_str, tz_code);
	first_blank = strstr(tz_code_str, " ");
	if(first_blank != NULL) *first_blank = '\0';

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code_str);
	}

strcat(returnString, blank);                    /* 'Space' separator between dates...                   */

if(data->ModArray[whichMod]->b2.end_date != 0)       /* If it's there, Date #2...                            */
	{
	get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b2.end_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	strcpy(tz_code_str, tz_code);
	first_blank = strstr(tz_code_str, " ");
	if(first_blank != NULL) *first_blank = '\0';

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code_str);
	}

strcat(returnString, blank);                    /* 'Space' separator between dates...                   */



if(data->ModArray[whichMod]->b2.valid_date != 0)       /* If it's there, Date #3...                            */
	{
	get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b2.valid_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code);
	}

/* add FGROUP id toward the end of card 1*/

tmpfg= get_fgroup_name(); 

tmp = strstr(data->ModArray[whichMod]->b2.id,tmpfg); 
/* FGroup mods type ,add FGROUP id toward the end of card1*/
if(tmp != NULL )
	{
	strcat(returnString, "  FGROUP");
	}

strcat(returnString, "\n");                     /* End CARD 1...                                        */

/* ------------------------------ CARD 2 ------------------------------ */

if(data->ModArray[whichMod]->b2.number_of_opers != 0)

	{  
                                   /* At least one of the operations has not been selected...      */
        
	for(i = 0; i < data->ModArray[whichMod]->b2.number_of_opers; i++)
		{
		j = 0;

		strcpy(cardString, data->ModArray[whichMod]->b2.id);       /* Segment ID (name)...           */
		cardString_length = strlen(cardString);
                
		/*while(strlen(data->ModArray[whichMod]->b2.keyword[j]) != 0)*/
                for(j=0;j<8;j++)
                {
                        if(strlen(data->ModArray[whichMod]->b2.keyword[j]) == 0) continue;
                        memset(tmpstring,'\0',50);
                        sprintf(tmpstring, "%.2f", data->ModArray[whichMod]->b2.values[j]);
                     
                        cardString_length = strlen(cardString);/* AV added 4/17/01 */
			if((cardString_length + strlen(data->ModArray[whichMod]->b2.keyword[j]) + strlen(tmpstring) + 3) > MAX_CARD_LENGTH)
				{                                       /* A continuation is needed...          */
                                
				strcat(cardString, blank);
				strcat(cardString, continuation_code);
				strcat(cardString, "\n");
				strcat(returnString, cardString);
                                
				strcpy(cardString, data->ModArray[whichMod]->b2.keyword[j]);    /* Start a new card...  */
				cardString_length = strlen(cardString);
                                
				}
			else    {
				strcat(cardString, blank);
				strcat(cardString, data->ModArray[whichMod]->b2.keyword[j]);
                                
                                
				}

			if(data->ModArray[whichMod]->b2.values[j] > -998)
				{
				strcat(cardString, blank);
				sprintf(string, "%.2f", data->ModArray[whichMod]->b2.values[j]);
                                
				cardString_length = strlen(cardString);
                                
				if(cardString_length + strlen(string) > MAX_CARD_LENGTH)
					{
					strcat(cardString, continuation_code);
                                        
					strcat(cardString, "\n");
					strcat(returnString, cardString);
					strcpy(cardString, string);
					cardString_length = strlen(cardString);
					}
				else 
                                
                                    strcat(cardString, string);
                                   
                                
				}
			/*j++;*/
		}


		/* Operation name...                                                                    */

		if(cardString_length + strlen(data->ModArray[whichMod]->b2.opname[i]) + 3 > MAX_CARD_LENGTH)
			{                                       /* A continuation is needed...          */
			strcat(cardString, blank);
			strcat(cardString, continuation_code);
			strcat(cardString, "\n");
			strcat(returnString, cardString);

			strcpy(cardString, forward_slash);      /* Start a new card...                  */
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->b2.opname[i]);
			strcat(cardString, "\n");
			}
		else    {                       /* The current card has room for the Operation name...  */
			strcat(cardString, blank);
			strcat(cardString, forward_slash);
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->b2.opname[i]);
			strcat(cardString, "\n");
			}

		strcat(returnString, cardString);
		returnString_length = strlen(returnString);
		cardString_length = strlen(cardString);
		if(returnString_length + cardString_length > 500)
			{
			returnString = (char *) realloc(returnString, returnString_length + cardString_length);
			returnString_length = strlen(returnString);
			/* printf("Return string length = %s\n", returnString_length); */
			}

		memset(cardString, '\0', 74);
		}
	}
else    {       /* All Operations have been selected ==> we don't need to handle the Operation names... */
	strcpy(cardString, data->ModArray[whichMod]->b2.id);       /* Segment ID (name)...           */
	cardString_length = strlen(cardString);
	/*j = 0; MR 1991*/
        
        /*keyword[9][7] defined in mods_info.h - Loop thru max of num keyword */
        /*store new value to modArray if keyword is not NULL */
        for(j=0;j<8;j++)
        {
                if(strlen(data->ModArray[whichMod]->b2.keyword[j])==0) continue;
                memset(tmpstring,'\0',50);
                sprintf(tmpstring, "%.2f", data->ModArray[whichMod]->b2.values[j]);
                cardString_length = strlen(cardString);
   
		if((cardString_length + strlen(data->ModArray[whichMod]->b2.keyword[j]) + strlen(tmpstring) + 3) > MAX_CARD_LENGTH)
		
		{                                       /* A continuation is needed...          */
			strcat(cardString, blank);
			strcat(cardString, continuation_code);
			strcat(cardString, "\n");
			strcat(returnString, cardString);

			strcpy(cardString, data->ModArray[whichMod]->b2.keyword[j]);    /* Start a new card...  */
			cardString_length = strlen(cardString);
		}
		else    
                {
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->b2.keyword[j]);
                        
		}

		if(data->ModArray[whichMod]->b2.values[j] > -998)
		{
			strcat(cardString, blank);
			sprintf(string, "%.2f", data->ModArray[whichMod]->b2.values[j]);

			cardString_length = strlen(cardString);
			if(cardString_length + strlen(string) > MAX_CARD_LENGTH)
			{
				strcat(cardString, continuation_code);
				strcat(cardString, "\n");
				strcat(returnString, cardString);
				strcpy(cardString, string);
				cardString_length = strlen(cardString);
			}
			else strcat(cardString, string);
                                
                        
                        
		 }
		/*j++;*/
	}

	strcat(cardString, "\n");
	strcat(returnString, cardString);
	memset(cardString, '\0', 74);
}


returnString_length = strlen(returnString) + 1;
tempString = (char *) realloc(returnString, returnString_length);
return(tempString);

}



/* **************************************************************************

	make_mod_B3_string()

   ************************************************************************** */

char *make_mod_B3_string(Mods_everythingStruct *data, int whichMod, Display *display)
{

	int     i, j, returnString_length,segstr_len;
	char    *returnString, *cardString, string[50], *tempString;
	char    *blank = " ";
	char    *first_blank;
	char    *forward_slash = "/";
	char    *continuation_code = "&";
	int     cardString_length;
	int     julda, julhr;  /* Julian day and hour */
	int     month, day, year, hour;
	int     zondum;  /* time zone value */
	int     dlsdum;  /* Daylight savings time flag
			    1 for daylight savings time,
			    0 for standard time */
	char    *tz_code;  /* time zone code */
	char    tz_code_str[5];  /* time zone code string */
	char    *tmpfg,*tmp;



returnString = (char *)malloc(1000);
memset(returnString, '\0', 1000);

cardString = (char *)malloc(74);
memset(cardString, '\0', 74);

memset(string, '\0', 50);


tz_code = get_timeZone_code(display);

/* ------------------------------ CARD 1 ------------------------------ */

strcpy(returnString, ".");
strcat(returnString, data->ModArray[whichMod]->b3.command);
strcat(returnString, blank);
if(data->ModArray[whichMod]->b3.start_date != 0)     /* If it's there, Date #1...                            */
	{
	get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b3.start_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	strcpy(tz_code_str, tz_code);
	first_blank = strstr(tz_code_str, " ");
	if(first_blank != NULL) *first_blank = '\0';

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code_str);
	}

strcat(returnString, blank);                    /* 'Space' separator between dates...                   */

if(data->ModArray[whichMod]->b3.end_date != 0)       /* If it's there, Date #2...                            */
	{
	get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b3.end_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	strcpy(tz_code_str, tz_code);
	first_blank = strstr(tz_code_str, " ");
	if(first_blank != NULL) *first_blank = '\0';

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code_str);
	}

strcat(returnString, blank);                    /* 'Space' separator between dates...                   */

if(data->ModArray[whichMod]->b3.valid_date != 0)       /* If it's there, Date #3...                            */
	{
	get_month_day_year_hour_tzc((int *)data->ModArray[whichMod]->b3.valid_date, &julda, &julhr,
			    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
	year = year%100;                        /* We only need the last two digits of the 'year'...    */

	sprintf(string, "%02d%02d%02d%02d", month, day, year, hour);
	strcat(returnString, string);
	strcat(returnString, tz_code);
	}

/* add FGROUP id toward the end of card 1*/

tmpfg= get_fgroup_name();
tmp = strstr(data->ModArray[whichMod]->b3.id,tmpfg);
 
if(tmp != NULL)
	{
	strcat(returnString, "  FGROUP");
	}

strcat(returnString, "\n");                     /* End CARD 1...                                        */

/* ------------------------------ CARD 2 ------------------------------ */

if(data->ModArray[whichMod]->b3.number_of_opers != 0)
	{
	for(i = 0; i < data->ModArray[whichMod]->b3.number_of_opers; i++)
		{
		strcpy(cardString, data->ModArray[whichMod]->b3.id);       /* Segment ID (name)...           */
                cardString_length = strlen(cardString);
		for(j = 0; j < data->ModArray[whichMod]->b3.num_values; j++){
			strcat(cardString, blank);
			if(strcmp( data->ModArray[whichMod]->b3.command, "CHGBLEND") == 0)
			   sprintf(string, "%d", (int)data->ModArray[whichMod]->b3.values[j]);
                        else			 
			   sprintf(string, "%.2f", data->ModArray[whichMod]->b3.values[j]);
                        
			/*
                         Added to fix the ZERODIFF mod format (hsd_bug r21-36)
                         it should be: .ZERODIFF mmddyyhhZ 
                                        DRNI4
                         There is no values for this mod.  set b3.values to blanks.
                        */
                        if(strcmp( data->ModArray[whichMod]->b3.command, "ZERODIFF") == 0)
			{ 
			
	                   sprintf(string, "%d",data->ModArray[whichMod]->b3.values[j]);
			}
			cardString_length = strlen(cardString);
			if(cardString_length + strlen(string) > MAX_CARD_LENGTH){
				strcat(cardString, continuation_code);
				strcat(cardString, "\n");
				strcat(returnString, cardString);
				strcpy(cardString, string);
				cardString_length = strlen(cardString);
			}
			else{
			   strcat(cardString, string);
			   /* need to update cardString_length - dp 24 June 1997 */
			   cardString_length = strlen(cardString);
			}
                }
                /* AV  this strcmp create a coredump on linux  */
		/*if(strcmp(data->ModArray[whichMod]->b3.keyword, NULL) != 0) */ /* If a Keyword is present...           */
		if(strcmp(data->ModArray[whichMod]->b3.keyword , "\0") == 0){
			if(cardString_length + strlen(data->ModArray[whichMod]->b3.keyword) + 1 > MAX_CARD_LENGTH){                                       /* A continuation is needed...          */
				strcat(cardString, blank);
				strcat(cardString, continuation_code);
				strcat(cardString, "\n");
				strcat(returnString, cardString);

				strcpy(cardString, data->ModArray[whichMod]->b3.keyword);    /* Start a new card...  */
				cardString_length = strlen(cardString);
			}
			else{
				strcat(cardString, blank);
				strcat(cardString, data->ModArray[whichMod]->b3.keyword);
				cardString_length = strlen(cardString);
			}
		}

		/* Operation name...                                                                    */

		if(cardString_length + strlen(data->ModArray[whichMod]->b3.opname[i]) + 3 > MAX_CARD_LENGTH)
			{                                       /* A continuation is needed...          */
			strcat(cardString, blank);
			strcat(cardString, continuation_code);
			strcat(cardString, "\n");
			strcat(returnString, cardString);

			strcpy(cardString, forward_slash);      /* Start a new card...                  */
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->b3.opname[i]);
			strcat(cardString, "\n");
			}
		else    {                       /* The current card has room for the Operation name...  */
			strcat(cardString, blank);
			strcat(cardString, forward_slash);
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->b3.opname[i]);
			strcat(cardString, "\n");
			}

		strcat(returnString, cardString);
		memset(cardString, '\0', 74);
		}
	}
else    {       /* All Operations have been selected ==> we don't need to handle the Operation names... */
        strcpy(cardString, data->ModArray[whichMod]->b3.id);       /* Segment ID (name)...           */
	cardString_length = strlen(cardString);        
	for(j = 0; j < data->ModArray[whichMod]->b3.num_values; j++){
		strcat(cardString, blank);
		if(strcmp( data->ModArray[whichMod]->b3.command, "CHGBLEND") == 0)
	           sprintf(string, "%d", (int)data->ModArray[whichMod]->b3.values[j]);
                else{                   
		   sprintf(string, "%.2f", data->ModArray[whichMod]->b3.values[j]);
                }
                /*
                 Added to fix the ZERODIFF mod format (hsd_bug r21-36)
                 it should be: .ZERODIFF mmddyyhhZ 
                                DRNI4
                 There is no values for this mod.  set b3.values to blanks.
                */
                if(strcmp( data->ModArray[whichMod]->b3.command, "ZERODIFF") == 0)
	            sprintf(string, "%s", " ");
		cardString_length = strlen(cardString);
		if(cardString_length + strlen(string) > MAX_CARD_LENGTH){
			strcat(cardString, continuation_code);
			strcat(cardString, "\n");
			strcat(returnString, cardString);
			strcpy(cardString, string);
			cardString_length = strlen(cardString);
		}
		else 
		{
		   strcat(cardString, string);
		   /* need to update cardString_length - dp 24 June 1997 */
		   cardString_length = strlen(cardString);
		}
        }
        /* AV  this strcmp create a coredump on linux*/
	/*if(strcmp(data->ModArray[whichMod]->b3.keyword, NULL) != 0)  *//* If a Keyword is present...           */
	if(strcmp(data->ModArray[whichMod]->b3.keyword , "\0") != 0){
		if(cardString_length + strlen(data->ModArray[whichMod]->b3.keyword) + 1 > MAX_CARD_LENGTH)
			{                                       /* A continuation is needed...          */
			strcat(cardString, blank);
			strcat(cardString, continuation_code);
			strcat(cardString, "\n");
			strcat(returnString, cardString);

			strcpy(cardString, data->ModArray[whichMod]->b3.keyword);    /* Start a new card...  */
			cardString_length = strlen(cardString);
			}
		else    {
			strcat(cardString, blank);
			strcat(cardString, data->ModArray[whichMod]->b3.keyword);
			}
	}

	strcat(cardString, "\n");
	strcat(returnString, cardString);
	memset(cardString, '\0', 74);
	}



 /* We want to free any unused space by 'returnString'; ideally we'd like to use        */
 /*     'realloc()', but it does'nt seem to do the trick; so, do it this way:           */

 returnString_length = strlen(returnString) + 1;
 tempString = (char *) malloc(returnString_length);
 memset(tempString, '\0', returnString_length);
 strcpy(tempString, returnString);
 return(tempString);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_modStrings.c,v $";
 static char rcs_id2[] = "$Id: Mods_modStrings.c,v 1.10 2006/04/18 15:27:56 aivo Exp $";}
/*  ===================================================  */
}



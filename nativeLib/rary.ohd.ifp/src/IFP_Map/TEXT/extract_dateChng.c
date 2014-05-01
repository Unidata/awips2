

  typedef struct
	{
	int     time_change;
	int     time;
	char    time_zone[5];
	}       date_data;



/* ************************************************************************************

	extract_date_changes()

   ************************************************************************************ */

void extract_date_changes(next_line, data)
	char            *next_line;
	date_data       *data;
{

	char    *asterisk;
	char    *time_string;
	char    *slash;
	char    *date_increment_str;
	char    *the_zone;

	int     i, length;
	int     date_increment;
	int     the_time;



 time_string = (char *) malloc(5);
 memset(time_string, '\0', 5);

 the_zone = (char *) malloc(5);
 memset(the_zone, '\0', 5);

 date_increment_str = (char *) malloc(15);
 memset(date_increment_str, '\0', 15);

 asterisk = strstr(next_line, "*");
 asterisk++;
 switch(*asterisk)
	 {
	 case '/':       /* The string following '*' has the form: "/1200Z   " or "/1200Z"      */
		 asterisk++;
		 strncpy(time_string, asterisk, 2);
		 the_time = atoi(time_string);
		 for(i = 1; i <= 4; i++) asterisk++;    /* Increment past 'time_string'...      */
		 strcpy(the_zone, asterisk);

		 length = strlen(the_zone);
		 if(length > 0)
			 {
			 for(i = 1; i <= 4 - length; i++) strcat(the_zone, " ");
			 }

		 data->time_change = 0;
		 data->time = the_time;
		 strcpy(data->time_zone, the_zone);

		 break;

	 case '+':       /* The string following '*' has the form: "+05/1200Z   " or "+05/1200Z" */
		 asterisk++;
		 slash = strstr(asterisk, "/");
		 strncpy(date_increment_str, asterisk, slash - asterisk);
		 date_increment = atoi(date_increment_str);
		 strcpy(asterisk, slash);

		 asterisk++;
		 strncpy(time_string, asterisk, 2);
		 the_time = atoi(time_string);
		 for(i = 1; i <= 4; i++) asterisk++;    /* Increment past 'time_string'...      */
		 strcpy(the_zone, asterisk);

		 length = strlen(the_zone);
		 if(length > 0)
			 {
			 for(i = 1; i <= 4 - length; i++) strcat(the_zone, " ");
			 }

		 data->time_change = date_increment;
		 data->time = the_time;
		 strcpy(data->time_zone, the_zone);

		 break;

	 case '-':       /* The string following '*' has the form: "-05/1200Z   " or "-05/1200Z" */
		 slash = strstr(asterisk, "/");
		 strncpy(date_increment_str, asterisk, slash - asterisk);
		 date_increment = atoi(date_increment_str);
		 strcpy(asterisk, slash);

		 asterisk++;
		 strncpy(time_string, asterisk, 2);
		 the_time = atoi(time_string);
		 for(i = 1; i <= 4; i++) asterisk++;
		 strcpy(the_zone, asterisk);

		 length = strlen(the_zone);
		 if(length > 0)
			 {
			 for(i = 1; i <= 4 - length; i++) strcat(the_zone, " ");
			 }

		 data->time_change = date_increment;
		 data->time = the_time;
		 strcpy(data->time_zone, the_zone);


		 break;

	 }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/extract_dateChng.c,v $";
 static char rcs_id2[] = "$Id: extract_dateChng.c,v 1.1 1995/09/08 14:55:20 page Exp $";}
/*  ===================================================  */

}




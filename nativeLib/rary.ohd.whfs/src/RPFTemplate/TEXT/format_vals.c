/*********************************************************************
   format_vals.c
      
   format_vals()
   get_format_index()
   format_datetime()
   format_date()
   format_timephrase()  
   setdate_mixedcase()
   create_MND_datetime()
   format_wwa_timephrase()
   
   
   MODIFICATION HISTORY:
   FUNCTION              DATE       PERSON      DESCRIPTION
   format_timephrase()  04/28/2004  Jingtao  se TIMEPHRASE_NUM and
                                             TIMEPHRASE_DAY_NUM 
   format_timephrase()  05/13/2004  Jingtao  For time variables in HEADER section,
                                             only use office-wide timzone
					     which is saved in set_hydro_env.
                                             A new argument - varindex.
   create_MND_datetime() 05/11/2004 Jingtao  MND date/time format
                                                
   
*********************************************************************/

#include <time.h>   
#include <stdio.h>  
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>

#include "format_vals.h"    /* function prototypes */
#include "Location.h"
#include "buf_template.h"

extern char paramdir[];

/*********************************************************************
   
   format_vals()
   
   PURPOSE
   Formats the value of a single variable into a character string and
   returns its length.
   
   NOTES   
   The var_dat variables use formats defined as var_tim formats.
   This is unlike all the other variable types, which have formats
   of the same type as their own type.
   
   This function handles the possibility for missing values for integer
   and float value.  String values are assumed to never
   be missing.  Date type values are also assumed to be never be missing
   since they are only used for historical crest values.
   Time values that are missing are also labelled as missing.
   
   The pad_switch is used to indicate that the data must be padded
   during the format - it is also used (cheating) to indicate that the
   section whose data is being formatted is the tabular section.
   
   The var_inded references the variable in terms of the full list of
   RiverPro variables, NOT the list of variables defined in the template.
   The f(ormat_)index references the format in terms of the format list
   defined in the template.
   
   *******************************************************************/

void format_vals(const values		 rawvalue,
		 const char		 dqcode[],
		 const int		 var_index,
		 const format_struct 	 *format,
		 const int		 findex,
		       misc_struct 	 *misc,
		 const int		 pad_switch,
		       char		 msg_str[],
		       char		 valuestr[],
		       int 		 *valuelen,
		 const pcc_struct	 *pcc,
		 const char		 local_lid[])
   
{
   extern template_variable_struct TEMPLATE_VARIABLES_TABLE[];
   
   char	formatstr[MAXLEN_STRING];
   char tempstr[MAXLEN_STRING];
   int type, rawlen, width, right_of_decimal;
   int k, mm, nn;
   float ff;   
   char	missing_val_str[MISS_LEN + 1];
   
   
   /* initialize */
   
   *valuelen = 0;
   memset(valuestr, 0, 1);
   memset(formatstr, 0, MAXLEN_STRING);
   type = TEMPLATE_VARIABLES_TABLE[var_index].type;
        
      
   /* set the string to use for missing numeric data as either
      the specially designated string or the default missing string. */
   
   if (strlen(msg_str) > 0)
      strcpy(missing_val_str, msg_str);
   else
      strcpy(missing_val_str, misc->miss_val);
   
         
   /*-----------------------------------------------------------------*/
   /* define the format for numerics by using either the user specified
      format that was found above or by defining a default format */
   
   /* if integer value */
   
   if (type == VAR_INT)
   {
      if (findex != MISSINGVAL)
      {
	 if (format->type_size[findex] == VAR_INT)
	    sprintf(formatstr, "%s%d%s", "%", format->size[findex].i, "d");
	 else
	    sprintf(formatstr, "%s%d%s", "%",
		    (int )format->size[findex].f, "d");		  
      }
      else
	 strcpy(formatstr, "%d");
   }
   
   
   /* if float then use width specified in format;
      for a format of the style "%5.2f"; allow format
      of type F5.0 to use an integer format */
   
   else if (type == VAR_FLT)
   {
      /* use this ugly method to get the decimal portion of the value 
	 since the "%" operator does not work properly on some machines */
      
      if (findex != MISSINGVAL)
      {
	 ff = (format->size[findex].f * 10.0);
	 mm = (int)(ff + 0.2);		
	 nn = mm / 10;	 
	 right_of_decimal =  mm - (nn *  10);         
	 
	 sprintf(formatstr, "%s%d%s%d%s",
	         "%", (int )format->size[findex].f, ".",
		 right_of_decimal, "f");		 	 
	 	 
      }
      
      else
      {	 	 
	 if (rawvalue.f < -99.999)
	    strcpy(formatstr, "%f");
	 else if (rawvalue.f < -9.999)
	    strcpy(formatstr, "%5.1f");
	 else if (rawvalue.f < 0)
	    strcpy(formatstr, "%4.1f");
	 else if (rawvalue.f < 10)
	    strcpy(formatstr, "%3.1f");
	 else if (rawvalue.f < 100)
	    strcpy(formatstr, "%4.1f");
	 else if (rawvalue.f < 1000)
	    strcpy(formatstr, "%5.1f");
	 else
	    strcpy(formatstr, "%.0f");
      }
      
      
   }
   
   /*--------------------------------------------------------------*/
   /* now that we have a format, either user-supplied or a default,
      load numeric data using the format; if missing then load in
      defined string */
   
   if (type == VAR_INT)
   {
      if (rawvalue.i != MISSINGVAL)
	 sprintf(valuestr, formatstr, rawvalue.i);
   }
   
   else if (type == VAR_FLT)
   {
      if ((int )rawvalue.f != MISSINGVAL)
      {      
	 sprintf(valuestr, formatstr, rawvalue.f);
      }
      
      
      /* if the data values has a data qualifier code
	 of E for estimated, then indicate this in the formatted
	 floating value.  use the pad switch to note that the 
	 section being handled is in the tabular section */
      
      if (strcmp(dqcode, "E") == 0)
      {
         /* for the tabular section put the E in front of the number.
            if there is no room, put it in anyway, which will throw off
            the lining up of the numbers. otherwise, abut the E
            against the left side of the number */
	 
	 if (pad_switch == 1)
	 {
	    if (valuestr[0] != ' ')
	    {
	       strcpy(tempstr, valuestr);
	       sprintf(valuestr, "E%s", tempstr);
	    }
	    else
	    {
	       for (k = 1; k < (int )format->size[findex].f; k++)
	       {
		  if (valuestr[k] != ' ')
		  {
		     valuestr[k - 1] = 'E';
		     break;
		  }
	       }
	    }
	 }
	 
	 else	 
	 {
	    strcat(valuestr, " (Estimated)");
	 }
      }
   }
   
   
   /* if a format was specified for numeric values, and the value is 
      missing, then adjust the missing label string so that its has
      a length equal to the specified width. this is needed particularly
      for the tabular section, where columns must align properly */
   
   if (((type == VAR_INT &&       rawvalue.i == MISSINGVAL) ||
	(type == VAR_FLT && (int )rawvalue.f == MISSINGVAL)))
   {
      if (findex != MISSINGVAL)
      {
	 /* get the width of the missing label, and get the
	    width of the specified format. */
	 
	 rawlen = strlen(missing_val_str);
	 
	 if (format->type_size[findex] == RPF_INT)
	    width = format->size[findex].i;
	 else
	    width = format->size[findex].f;
	 
	 
	 /* if label length less than format width, then copy the label
	    and precede it with extra blanks.  if label length greater
	    than format width, then copy the label but truncate it.
	    if they're the same length, then copy label */
	 
	 if (rawlen < width)
	 {
	    for (k = 0; k < (width - rawlen); k++)
	       valuestr[k] = ' ';
	    strcpy(&valuestr[width - rawlen], missing_val_str);
	       	       	       
	    valuestr[width] = '\0';
	 }      
	 
	 else if (rawlen > width)
	 {
	    strncpy(valuestr, missing_val_str, width);
	    valuestr[width] = '\0';
	 }      
	 
	 else
	    strcpy(valuestr, missing_val_str);
      }
      
      
      /* if numeric missing and no format specified, simply copy the 
	 missing string */
      
      else
	 strcpy(valuestr, missing_val_str);
   }
   
   /*------------------------------------------------------------------*/      
   /* format non-numeric data into the output */  
   
   /* if string, adjust the raw string so that it has a 
      length identical to desired string length */
   
   if (type == VAR_STR)
   {
      if (findex != MISSINGVAL)
      {
	 
	 rawlen = strlen(rawvalue.s);
	 width = format->size[findex].i;
	 
	 if (rawlen < width)
	 {
	    strcpy(valuestr, rawvalue.s);
	    for (k = rawlen; k < width; k++)
	       valuestr[k] = ' ';
	    valuestr[width] = '\0';
	 }
	 
	 else if (rawlen == width)
	    strcpy(valuestr, rawvalue.s);
	 
	 else if (rawlen > width)
	 {
	    strncpy(valuestr, rawvalue.s, width);
	    valuestr[width] = '\0';
	 }
      }
      else
	 sprintf(valuestr, "%s", rawvalue.s);
   }
   
   
   /* if a time, then pass the index to the specific format type to use
      and format the string. the usertime format string is also passed
      in case we are using a user-specified time format. */
   
   else if (type == VAR_TIM)
   {
      if (findex != MISSINGVAL)
	 format_datetime(rawvalue.t, format->size[findex].i, 
			 format->usertime[findex], misc, pad_switch,
			 valuestr, pcc, local_lid, var_index, missing_val_str);
      else
	 format_datetime(rawvalue.t, T_DEFAULT, "", misc,
			 pad_switch,
			 valuestr, pcc, local_lid, var_index, missing_val_str);
      
      setdate_mixedcase(valuestr);
   }
   
   
   /* if a date, then simply pass the index to the specific
      format type to use and actually format the string here */
   
   else if (type == VAR_DAT)
   {
      if (findex != MISSINGVAL)
	 format_date(rawvalue.d, format->size[findex].i, valuestr);
      else
	 format_date(rawvalue.d, T_DEFAULT, valuestr);
      
      setdate_mixedcase(valuestr);
   }   

   /*------------------------------------------------------------------*/
   /* set and check the value string length */   
   /* set the value length based on the string length and check the length */
   
   *valuelen = strlen(valuestr);
   if (*valuelen > MAXLEN_LONGSTR) log_msg(EXCEED_LONGSTR, "");
   
   return;
}


/*********************************************************************
   get_format_index()
   
   PURPOSE
   To get the index to the entry in the format list that applies
   to the variable.
   
   NOTES
   This is not used for the tabular section since it uses the exact 
   format order provided in the template format record.  I.E. if it used
   this function, and the same variable was specified twice in the
   template variable record with DIFFERENT formats in the corresponding
   template format record, then the first instance of the format in the
   template format record would ALWAYS be used for that variable.   
   
   *******************************************************************/
int get_format_index(const int		 	var_index,
		     const variable_struct 	*variable,
		     const format_struct 	*format,
		     const int			type)
{
   int varfound, varformat_found;
   int findex, vindex;
   int i;
   
   
   /* initialize */
   
   findex = MISSINGVAL;

   
   /* check if any user specified formats, if so get the index to it.
      this check is done for both numerics and non-numerics */

   if (format->num_of_formats >= variable->num_of_variables &&
       variable->num_of_variables > 0)
   {
      varfound = FALSE;
      
      
      /* loop on the number of variables, which should have formats defined */
      
      for (i = 0; i < variable->num_of_variables && varfound == FALSE; i++)	 
      {	 
	 /* check if the given variable matches that in the variable list */
	 
	 if (variable->varinfo[i].varindex == var_index)
	 {
	    varfound = TRUE;
    
	    
	    /* find the index to the format for this variable by looping
	       on the formats and looking for formats for variables. e.g. if
	       this is the second variable in the variable list, find the
	       second format in the format list that is a format for a
	       variable; this is needed for since the format may contain
	       formats that are not for variables and are therefore ignored */
	    
	    findex = 0;      /* format index */
	    vindex = 0;      /* variable index */
	    varformat_found = FALSE;
	    
	    do
	    {
	       if (isvar(format->type[findex]) == TRUE) 
	       {
		  if (vindex == i) 
		  {
		     varformat_found = TRUE;
		     break;
		  }
		  vindex++;
	       }
	       findex++;
	    } while (findex < format->num_of_formats);
	    
	    
	    /* check if the basic types (i.e. float format for float
	       variable) match.  allow time formats for date variables */
	    
	    if (varformat_found)
	    {
	       if ((format->type[findex] != type && type != VAR_DAT) || 
		   (format->type[findex] != VAR_TIM && type == VAR_DAT))
	       {
		  log_msg(MISMATCH_FORMVAR, "");
		  findex = MISSINGVAL;
	       }
	    }
	    else
	       findex = MISSINGVAL;
	 }
      }
   }
   
   return(findex);
}


/*********************************************************************
   format_datetime()
   
   PURPOSE
   To format a time value into the specified format.
   This function is used for formatting time values expressed
   in the time_t type representation.
   For segmentation as point, could use local timezone, for 
   segmentation as group and county, could use office-wide
   time zone.
    
   NOTES
   
   *******************************************************************/
void format_datetime(const time_t	rawtime,
		     const int     	index,
		     const char		*usertime,
		           misc_struct	*misc,
		     const int		pad_switch,
		     	   char 	valuestr[],
		     const pcc_struct	*pcc,
		     const char 	local_lid[],
		           int          varindex,
			   char         missing_val_str[])
{
   extern template_variable_struct TEMPLATE_VARIABLES_TABLE[];
   struct tm 	*time_struct;
   time_t	rounded_timet;
   char 	dtstr[MAXLEN_STRING];
   char 	tempstr[MAXLEN_STRING];
   int 		numwrote;
   int 		k, width, msglen;
   char		formatstr[40];
   Location	*locPtr = NULL;
   char		where[25];    
   static char  lastTZset[TZ_LEN +1] = "";
   static char	setTZcmd[25] =" ";
   char		dude[100]; 
   int		putenv_rc;
   char         missing_str[MISS_LEN + 1];
   

   /* If using individual location's time zone flag  */
   
   if (pcc->product.timeZoneFlag == 1)
   {    
        /* if the time format is for the HEADER section, 
	   then use the default time zone */
      
        if (check_var_access(TEMPLATE_VARIABLES_TABLE[varindex].accesslist,
		             HEADER) == TRUE)
        {
            memset( setTZcmd, 0, 25);		
	    sprintf(setTZcmd, "TZ=%s", misc->defaultTZ);
	    putenv_rc = putenv( setTZcmd);	
        }  
	
	else
	{
	  /* Retrieve the location record from the DBMS and use its
	     tzone field to set the TZ variable. */	

	   sprintf(where, " WHERE lid = '%s' ", local_lid);
	
	   if ( (locPtr = GetLocation(where)) != (Location *) NULL )
	   {
		if (strcmp(lastTZset, locPtr->tzone) != 0)
		{			
		    sprintf(setTZcmd, "TZ=%s", locPtr->tzone);
		    putenv_rc = putenv(setTZcmd);
		    
		    memset( lastTZset, 0, TZ_LEN + 1);
		    strncpy(lastTZset, locPtr->tzone, TZ_LEN);
		    
                    sprintf(dude, "In Format DateTime setTZcmd='%s' for lid '%s'",
		            setTZcmd, local_lid);
                    log_msg("", dude);
		}
		
		
                /* Free the Location table pointer if not null */

   	        FreeLocation(locPtr);
   	        locPtr = NULL;
	   }
	}  
   }
   
       	 		   	 	 
   /* break down the time value into its components */	 
   
   time_struct = localtime(&rawtime);  
  
   
   /* null the datetime string */
   
   memset(dtstr, 0, MAXLEN_STRING);
   
   
   /* make default format mm/dd hh:mm as in 2/14 22:00 */
   
   if (index == T_DEFAULT)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%m/%d %H:%M", time_struct);

   
   /* use the user specified time format, which follows the strftime 
      format method */
   
   else if (index == T_USER)
   {
      numwrote = strftime(dtstr, MAXLEN_STRING, usertime, time_struct);
   }
   
   /* make format hhmm tz www mmm dd yyyy, as in 
      1015 AM EDT MON AUG 15 1994, which is typically used in header;
      also substitute the local time zone into the time zone portion
      of the output format. don't include non-significant leading zero
      if applicable */
   
   else if (index == T_HEADER)
   {        
      if (misc->batch_mode == TRUE)
      {	          
         /* use this silly string to avoid problems with SCCS
	    messing up the %characters when doing a get */
      
         sprintf(formatstr, "%s", "%I");
         strcat(formatstr, "%M %p %Z %a %b %d %Y");
         numwrote = strftime(tempstr, MAXLEN_STRING, formatstr, time_struct);
         if (tempstr[0] == '0')
         {
	    if (pad_switch == FALSE)
	       strcpy(dtstr, &tempstr[1]);
	    else
	    {
	       strcpy(dtstr, tempstr);
	       dtstr[0] = ' ';
	    }
          }
      
         else
	    strcpy(dtstr, tempstr);
      }
      
      
      /* if in interactive mode, then place a code here that is later translated
         into the actual time, with the presumption that the T_HEADER format is 
	 used only for the current time */
	 
      else
      {
         strcpy(dtstr, TIMESTR_HDR);
      }
   }

   
   /* make format mm/dd/yyyy as in 04/20/1932 */
   
   else if (index == T_MMDDYYYY)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%m/%d/%Y", time_struct);
   
   
   /* make format mm/dd/yy as in 04/16/32 */
   
   else if (index == T_MMDDYY)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%m/%d/%y", time_struct);
   
   
   /* make format mm/dd as in 03/30 */
   
   else if (index == T_MMDD)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%m/%d", time_struct);
   
   
   /* make format mmdd as in 0330 */
   
   else if (index == T_MMDDS)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%m%d", time_struct);
   
   
   /* make format mm/dd as in 03/30 AM */
   
   else if (index == T_MMDDXM)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%m/%d %p", time_struct);
   
   
   /* make format mm/dd as in Thu AM */
   
   else if (index == T_AWXM)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%a %p", time_struct);
   
   
    /* make format mm/dd as in Thursday AM */
   
   else if (index == T_WXM)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%A %p", time_struct);
   
   
   /* make format hh as in 11 AM. if leading zero, then 
      replace with blank if in tabular, ir not in tabular,
      then remove zero.*/
   
   else if (index == T_HHXM)
   {
      numwrote = strftime(tempstr, MAXLEN_STRING, "%I %p", time_struct);
      if (tempstr[0] == '0')
      {
	 if (pad_switch == FALSE)
	    strcpy(dtstr, &tempstr[1]);
	 else
	 {
	    strcpy(dtstr, tempstr);
	    dtstr[0] = ' ';
	 }
      }
      else
         strcpy(dtstr, tempstr);
   }
   
   
   /* make format wwwww mmmmmm dd yyyy as in TUESDAY MAY 24 1994 */
   
   else if (index == T_WCMDDYYYY)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%A %B %d %Y", time_struct);
   
   
   /* make format wwwww mmmmmm dd yyyy as in FRIDAY MAR 24 2056 */
   
   else if (index == T_WCAMDDYYYY)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%A %b %d %Y", time_struct);

   
   /* make format ww mmmmmm dd yyyy as in FRI MAR 24 2056 */
   
   else if (index == T_AWCAMDDYYYY)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%a %b %d %Y", time_struct);
   
   
   /* make format mmmmmm dd yyyy as in AUGUST 18 1959 */
   
   else if (index == T_CMDDYYYY)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%B %d %Y", time_struct);
   
   
   /* make format mmm dd yyyy as in AUG 18 1959 */
   
   else if (index == T_CAMDDYYYY)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%b %d %Y", time_struct);
   
   
   /* make format mmmmmmm dd as in OCTOBER 4 */
   
   else if (index == T_CMDD)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%B %d", time_struct);
   
   
   /* make format mmmdd as in JUL 30 */
   
   else if (index == T_CAMDD)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%b %d", time_struct);
   
   
   /* make format wwwwww cmmmmm dd as in TUESDAY OCTOBER 6 */
   
   else if (index == T_WCMDD)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%A %B %d", time_struct);
   
   
   /* make format wwwwww cmmm dd as in FRIDAY NOV 12 */
   
   else if (index == T_WCAMDD)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%A %b %d", time_struct);
   
   
   /* make format www cmmm dd as in THU MAY 13 */
   
   else if (index == T_AWCAMDD)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%a %b %d", time_struct);
   
   
   /* make format wwwwww hh as in FRIDAY 10 AM */
   
   else if (index == T_WHH)
   {
      round_hour(rawtime, &rounded_timet);
      time_struct = localtime(&rounded_timet);  
      numwrote = strftime(dtstr, MAXLEN_STRING, "%A %I %p", time_struct);
   }
   
   
   /* make format www hh as in FRI 10 AM */
   
   else if (index == T_AWHH)
   {
      round_hour(rawtime, &rounded_timet);
      time_struct = localtime(&rounded_timet);  
      numwrote = strftime(dtstr, MAXLEN_STRING, "%a %I %p", time_struct);
   }
   
   
   /* make format www [h]h as in MON 2 PM;
      this format removes any leading zero if the hour is less than 10;
      to complicate things, if the pad switch is set and the time is valid,
      then replace the leading zero with a blank instead. */
   
   else if (index == T_AWH)
   {
      if ((int )rawtime != MISSINGVAL)
      {
	 round_hour(rawtime, &rounded_timet);
	 time_struct = localtime(&rounded_timet);  
	 numwrote = strftime(tempstr, MAXLEN_STRING, "%a %I %p", time_struct);
	 if (tempstr[4] == '0')
	 {
	    if (pad_switch == FALSE)
	    {
	       memset(dtstr, 0, MAXLEN_STRING);
	       strncpy(dtstr, tempstr, 4);
	       strcat(dtstr, &tempstr[5]);
	    }
	    else
	    {
	       memset(dtstr, 0, MAXLEN_STRING);
	       strncpy(dtstr, tempstr, 4);
	       strcat(dtstr, &tempstr[5]);
	       strcat(dtstr, " ");
	    }
	 }	 
	 else
	    strcpy(dtstr, tempstr);
      }
      
      else
	 numwrote = strftime(tempstr, MAXLEN_STRING, "%a %I %p", time_struct);
   }
   
   
   /* make format hh wwwwww as in 02 PM MONDAY */
   
   else if (index == T_HHW)
   {
      round_hour(rawtime, &rounded_timet);
      time_struct = localtime(&rounded_timet);  
      numwrote = strftime(dtstr, MAXLEN_STRING, "%I %p %A", time_struct);
   }

   
   /* make format [h]h wwwwww as in 2 PM MONDAY;
      this format removes any leading zero if the hour is less than 10;
      to complicate things, if the pad switch is set and the time is valid,
      then replace the leading zero with a blank instead. */
   
   else if (index == T_HW)
   {
      if ((int )rawtime != MISSINGVAL)
      {
	 round_hour(rawtime, &rounded_timet);
	 time_struct = localtime(&rounded_timet);  
	 numwrote = strftime(tempstr, MAXLEN_STRING, "%I %p %A", time_struct);
	 if (tempstr[0] == '0')
	 {
	    if (pad_switch == FALSE)
	       strcpy(dtstr, &tempstr[1]);
	    else
	    {
	       strcpy(dtstr, tempstr);
	       dtstr[0] = ' ';
	    }
	 }	 
	 else
	    strcpy(dtstr, tempstr);
      }
      
      else
	 numwrote = strftime(tempstr, MAXLEN_STRING, "%I %p %A", time_struct);
   }
   
   
   
   /* make format hh www as in 07 AM WED */
   
   else if (index == T_HHAW)
   {
      round_hour(rawtime, &rounded_timet);
      time_struct = localtime(&rounded_timet);  
      numwrote = strftime(dtstr, MAXLEN_STRING, "%I %p %a", time_struct);
   }

   
   /* make format hh mm/dd as in 11 AM 04/12 */
   
   else if (index == T_HHMMDD)
   {
      round_hour(rawtime, &rounded_timet);
      time_struct = localtime(&rounded_timet);  
      numwrote = strftime(dtstr, MAXLEN_STRING, "%I %p %m/%d", time_struct);
   }

   
   /* make format wwwwww as in THURSDAY */
   
   else if (index == T_W)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%A", time_struct);
   
   
   /* make format www as in FRI */
   
   else if (index == T_AW)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%a", time_struct);
   
   
   /* make format www hhnn as in FRI 11:30 PM */
   
   else if (index == T_AWHHNN)
      numwrote = strftime(dtstr, MAXLEN_STRING, "%a %I:%M %p", time_struct);
         
   
   /* make format ddhhnn as in 312359 */
   
   else if (index == T_DDHHNN)
   {
      /* use this silly string to avoid problems with SCCS
	 messing up the %characters when doing a get */
      
      sprintf(formatstr, "%s", "%d%H");
      strcat(formatstr, "%M");
      
      
      numwrote = strftime(dtstr, MAXLEN_STRING, formatstr, time_struct);
   }
   
   
   /* make format based on user defined phrase lookup table */
   
   else if (index == T_PHRASE)
      format_timephrase(misc, rawtime, dtstr);
      
         
   /* make format based on WWA time format rule */
   
   else if (index == T_WWA)
      format_wwa_timephrase(misc, rawtime, dtstr);
   
   
   /* if the value is valid, copy the created string into the
      returned string */
   
   if ((int )rawtime != MISSINGVAL)
   {
      strcpy(valuestr, dtstr);
   }

   
   /* if the time is missing, and processing for the tabular section,
      then represent missing time values and pad or truncate the 
      string as necessary so that its width is the same as that
      expected from. this check is done here, after the long if stack
      above, to take advantage of the fact that the length of the
      requested string is set, although the value is bad. this allows
      that tabular output to be lined up properly. */
   
   else 
   {
      if (pad_switch == TRUE)
      {
	 width = strlen(dtstr);
	 
	 /* use user specified missing_val_str if exists, otherwise use the
	 default misc->miss_tim from RpfParams table */
	 
	 if (strlen(missing_val_str) > 0)
	    strcpy(missing_str, missing_val_str);
	 else   
	    strcpy(missing_str, misc->miss_tim);
	    
	 msglen = strlen(missing_str);
	 
	 
	 /* if label length less than format width, then copy the label
	    and pad it with extra blanks; if label length greater than
	    format width, then copy the label but truncate it */
	 
	 if (msglen < width)
	 {
	    strcpy(valuestr, missing_str);
	    for (k = msglen; k < width; k++)
	       valuestr[k] = ' ';
	    valuestr[width] = '\0';
	 }
	 
	 
	 else if (msglen > width)
	 {
	    strncpy(valuestr, missing_str, width);
	    valuestr[width] = '\0';
	 }
	 
	 else 
	    strcpy(valuestr, missing_str);
      }
      
      else
	 strcpy(valuestr, missing_str);
   }
   
   
      
   /* if tzone for an individual location was used
      then set TZ back to original office wide value */
      
   if (pcc->product.timeZoneFlag == 1)
   {
	if (strcmp(lastTZset, misc->defaultTZ) != 0)
	{
	   memset(setTZcmd, 0, 25);
	   sprintf(setTZcmd, "TZ=%s", misc->defaultTZ);
	   putenv_rc = putenv( setTZcmd);
	   
	   memset( lastTZset, 0, TZ_LEN + 1);
	   strncpy(lastTZset, misc->defaultTZ, TZ_LEN);
	}
   }
   
  
   return;
}


/*********************************************************************
   round_hour()
   
   PURPOSE
   Rounds the time, represented in structure fashion, to the
   top of the nearest hour.
   
   *******************************************************************/

void round_hour(time_t rawtime, 
		time_t *rounded_timet)
{
   
   struct tm *time_struct;
   
   
   /* convert the timet value into its components */
   
   time_struct = gmtime(&rawtime);     
   
   
   /* check if the time is already on the hour. if it is
      don't adjust the time. if it is less or equal to 30 minutes
      past the hour, subtract out the minutes and seconds so that
      it is on the hour. if greater than 30 minutes, round up
      to the next hour by adding an hour, then subtracting out
      the minutes and seconds.  */
   
   if (time_struct->tm_min == 0)
      *rounded_timet = rawtime;
   
   else if (time_struct->tm_min <= 30)     
      *rounded_timet = rawtime -
	 ((time_struct->tm_min * 60) + time_struct->tm_sec);
   
   else if (time_struct->tm_min > 30)
      *rounded_timet = rawtime + (60 * 60) -
	 ((time_struct->tm_min * 60) + time_struct->tm_sec);
   
   
   return;
}


/*********************************************************************
   format_timephrase()
   
   PURPOSE
   To format a time value into the format specified in the data file
   timephra.dat.
   It allows precise phrase definitions for yesterday,
   today, tomorrow and other weekday.
   
   *******************************************************************/
void format_timephrase(misc_struct	*misc,
		       const time_t 		rawtime,
		       char 		dtstr[MAXLEN_STRING])
{
   static int    file_read = FALSE;
   static char   *timephrase[TIMEPHRASE_NUM];
   static int    readintbeg[TIMEPHRASE_NUM], readintend[TIMEPHRASE_NUM];
   struct tm 	*curtime;
   struct tm 	*valtime;
   FILE        *file_ptr;
   char 	*varptr, *endpos, *fgets_ptr, *readstrptr, *phraseptr;
   int 		yesterday, today, tomorrow;
   int 		firstday, lastday;
   int 		i, numwrote, cnt;
   size_t       phraselen;
   int          bdaylen, linestartnum;   
   char 	filename[MAXLEN_FILENAME];
   char 	weekday[7][10] = 
               {"SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY",
                "THURSDAY", "FRIDAY", "SATURDAY"};
   char         dtstrg[MAXLEN_STRING]="";  
   char         modify_dtstr[MAXLEN_STRING]="";
   char         readstr[MAXLEN_STRING]="",readstrbeg[3]="",readstrend[3]="";
   
   
   /* initialize */
   
   yesterday = today = tomorrow = FALSE;
   firstday = lastday = FALSE;
   
         
   /* malloc space for a copy of the valtime structure since the
      localtime call uses the same memory and therefore overwrites
      it; then copy the info from scratch var curtime into valtime */
   
   valtime = (struct tm *)malloc(sizeof(struct tm));
   if (valtime == NULL) 
      log_msg(FAILED_MALLOC, "for valtime tm structure in format_timephrase");
                  
   curtime = localtime(&rawtime);
   memcpy(valtime, curtime, sizeof(struct tm));           
   	      
   strftime(dtstrg, MAXLEN_STRING, "%m/%d %H:%M", valtime);
   
   
   /* break system time into its components */
   
   curtime = localtime(&misc->system_time);
   
   
   /* determine whether the time value day is for yesterday, today,
      or tomorrow; first check if the day is today */
   
   if (valtime->tm_year == curtime->tm_year &&
       valtime->tm_yday == curtime->tm_yday)
      today = TRUE;
   
   
   /* now check if time matches yesterday or today; 
      need to consider whether it current day is the first
      or last day of the year */
   
   else
   {
      if (curtime->tm_yday == 0)
	 firstday = TRUE;
      else if (curtime->tm_mon == 12 && curtime->tm_mday == 31) 
	 lastday = TRUE;
      
      
      /* if not the first or last day of the year, then simply check the
	 julian day */
      
      if (firstday == FALSE && lastday == FALSE)
      {
	 if (curtime->tm_yday - 1 == valtime->tm_yday &&
	     curtime->tm_year == valtime->tm_year)
	    yesterday = TRUE;
	 else if (curtime->tm_yday + 1 == valtime->tm_yday &&
		  curtime->tm_year == valtime->tm_year)
	    tomorrow = TRUE;
      }
      
      
      /* if today is the first day of the year */
      
      else if (firstday == TRUE)
      {
	 if (curtime->tm_year - 1 == valtime->tm_year &&
	     (valtime->tm_mon == 11 && valtime->tm_mday == 31))
	    yesterday = TRUE;
	 else if (valtime->tm_yday == 1)
	    tomorrow = TRUE;	    
      }
      
      else if (lastday == TRUE)
      {
	 if (curtime->tm_yday - 1 == valtime->tm_yday)
	    yesterday = TRUE;
	 else if (curtime->tm_year +1 == valtime->tm_year &&
		  (valtime->tm_mon == 0 && valtime->tm_mday == 1))
	    tomorrow = TRUE;
      }            
   }
   
   
   /* define the starting line number in the file that contains the time
       and the time phrases */

   if (yesterday == TRUE)
      linestartnum = 0;
   else if (today == TRUE)
      linestartnum = 8;
   else if (tomorrow == TRUE)
      linestartnum = 16;
   else
      linestartnum = 24;  
      
      
   /* open the phrase lookup file if necessary */
   
   if (file_read == FALSE)
   { 
      /* malloc space for timephrase array and initialize readintbeg[] and
         readintend[]*/
                 
      for (i=0; i< TIMEPHRASE_NUM; i++)
      {
         timephrase[i] = (char *)malloc(MAXLEN_STRING);
         if (timephrase[i] == NULL)
            log_msg(FAILED_MALLOC, "for timephrase in format_timephrase");
         memset(timephrase[i], 0, MAXLEN_STRING);	
	 
	 readintbeg[i] = MISSINGVAL;
	 readintend[i] = MISSINGVAL;
      } 
      
      sprintf(filename, "%s/%s", paramdir, TIMEPHRASE_FILE); 
      file_ptr = fopen(filename, "r");  
          

      /* if an error in opening file, do not abort;
	  use default time format instead */

      if(file_ptr == NULL)
      {
	 log_msg(FILE_OPENERR, filename);

	 /* make default format mm/dd hh:mm as in 2/14 22:00 */

	 numwrote = strftime(dtstr, MAXLEN_STRING, "%m/%d %H:%M",
				valtime);
	 if (valtime != NULL)
	    free(valtime);
	    
	 return;
      }
      
      
      /* load all time phrases in the timephra.dat.
	 assume that the first six characters are hh-hh:<space> ,
	 the number and the time phrases could be changed by the users, but
	 there must be 8 time phrases for today, yesterday, tomorrow or weekday*/
      
      cnt = 0;
      
      for (;;)
      {	 
	 fgets_ptr = fgets(readstr, MAXLEN_STRING, file_ptr);
	 
	 if (fgets_ptr == NULL)	 	   
	   break;
	 
	 	   
         if (readstr != NULL)
	 {   
	   phraselen = strlen(readstr);
	   if (phraselen <= 6 || phraselen >= MAXLEN_STRING)
	   {
	      log_msg(INVALID_TIMEPHRASE, "");	    
	      strcpy(dtstr, "*INVALID TIME PHRASE*");
	      return;
	   }
	   else
	   {
	      phraseptr = strpbrk(readstr, ":");
	      if (phraseptr == NULL)
		 log_msg("", "Invalid time phrase, assume as hh-hh:");
	      else   
		 strcpy(timephrase[cnt], (phraseptr+1));
           }
	 
	   /*split the begin time and the end time by "-"*/

	   readstrptr = strpbrk(readstr, "-");
	   if (readstrptr == NULL)
              log_msg("","Invalid string, assume as hh-hh:");
	   else
	   {
              strncpy(readstrbeg, readstr, 2); 	   
	      readintbeg[cnt] = atoi(readstrbeg);	 
	      strncpy(readstrend, readstrptr+1, 2);	    
	      readintend[cnt] = atoi(readstrend);
	   }	 
         }
	 
	 cnt++;
      }

      fclose(file_ptr);
      file_read = TRUE;
   }   
   
   
   /* determine which time phrase should be read depending on the day and 
      the time */   
   
   for (i=0; i< TIMEPHRASE_DAY_NUM; i++)
   {
      if (valtime->tm_hour >= readintbeg[linestartnum + i] && 
          valtime->tm_hour < readintend[linestartnum + i])
      {         	 	     	     
         /* build the phrase from the last line read; search for 
	   the weekday string variable and if not found, substitute text */      
         
	 if (timephrase[linestartnum + i] != NULL)
	 {
            varptr = strstr(timephrase[linestartnum + i], "<Weekday>");
            if (varptr == NULL)
	       strcpy(dtstr, timephrase[linestartnum + i]);

            /* insert the week day into the string; assumes that the weekday
		 string is only <> variable in the string. */

            else
            {	    
	       endpos = strstr(timephrase[linestartnum + i], ">");	   
	       bdaylen = strcspn(timephrase[linestartnum + i], "<");
	       strncpy(dtstr, timephrase[linestartnum + i], bdaylen);	       
	       strcat(dtstr, weekday[valtime->tm_wday]);	       
	       strcat(dtstr, (endpos + 1));	
	           		 
	    } 
	 }
	 else
	   log_msg("", "THERE SHOULD BE TIME PHRASE AFTER THE HOUR.");     
      }
    }  
    
    /*trail the spaces ahead of dtstr*/
    
    if (dtstr != NULL)
    {
       for (i=0; i< strlen(dtstr); i++)
       {
          if (!isspace(dtstr[i]))
	  {
	     strcpy(modify_dtstr, dtstr+i);
	     break;
	  }
       }
    }
    
    if (modify_dtstr != NULL)
       strcpy(dtstr, modify_dtstr);
           	     
    /* the range of valtime->tm_hour is 0~23, in the timephrase.dat,
       there might be no 0 defined, instead with 24*/
      
  /*  if (valtime->tm_hour == 0) 
    {
       varptr = strstr(timephrase[linestartnum + 7], "<Weekday>");
       if (varptr == NULL)
       {
	  if (tomorrow==TRUE)
	     linestartnum = 8;
	  else if (today == TRUE)
	     linestartnum = 0;

	  strcpy(dtstr, timephrase[linestartnum + 7]);
       }   
    */   
       /* insert the week day into the string; assumes that the weekday
	    string is only <> variable in the string. */

      /* else
       {	    
	  endpos = strstr(timephrase[linestartnum + 7], ">");	   
	  bdaylen = strcspn(timephrase[linestartnum + 7], "<");
	  strncpy(dtstr, timephrase[linestartnum + 7], bdaylen);
	  strcat(dtstr, " ");
	  if (weekday[valtime->tm_wday] == 0)
	     strcat(dtstr, "Saturday");
	  else   
	     strcat(dtstr, weekday[valtime->tm_wday-1]);
	  strcat(dtstr," ");
	  strcat(dtstr, (endpos + 1));	    		 
       }           	 
    }
        
  */	          
   /* free the memory */
   
   if (valtime != NULL)
      free(valtime);          
   
   return;
}


/*********************************************************************
   format_date()
   
   PURPOSE
   To format a date value into the specified format.
   This function is used for formatting time values expressed
   in the character string representation of mm/dd/yyyy.
   
   NOTES
   Only allow those time formats that include the year specification
   and that do not include a weekday, hour, or minute specification.
   
   *******************************************************************/
void format_date(const	char	dait[],
		 const	int     index,
			char 	valuestr[])
{

   int numread;
   int day, month, year;
   char dtstr[MAXLEN_STRING];
   char abbrevmon[4];
   char monstr[12][10] =
      {"JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
      "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"};
   
   
   /* initialize the date string */
   
   memset(dtstr, 0, MAXLEN_STRING);      
   
   
   /* extract the day, month, and year fields */
   
   numread = sscanf(dait, "%2d/%2d/%4d", &month, &day, &year);
   if (numread != 3)
   {
      log_msg(INVALID_DATE, dait);
      month = 1;
      day = 1;
      year = 1970;
   }
   
   else
   {
      if (month < 1 || month > 12)
      {
	 log_msg(INVALID_DATE, dait);
	 month = 1;
      }
      
      if (day < 1 || day > 31)
      {
	 log_msg(INVALID_DATE, dait);
	 day = 31;
      }
   }
   
   
   /* make format mm/dd/yyyy as in 04/20/1932 */
   
   if (index == T_MMDDYYYY)
      sprintf(dtstr, "%i/%i/%i", month, day, year);
   
   
   /* make format mm/dd/yy as in 04/16/32 */
   
   else if (index == T_MMDDYY)
   {
      if (year < 2000)
	 sprintf(dtstr, "%i/%i/%i", month, day, year - 1900);
      else 
	 sprintf(dtstr, "%i/%i/%i", day, month, year - 2000);
   }	 
   
   
   /* make format mmmmmm dd yyyy as in AUGUST 18 1959 */
   
   else if (index == T_CMDDYYYY)
      sprintf(dtstr, "%s %i %i", monstr[month - 1], day, year);
   
   
   /* make format mmm dd yyyy as in AUG 18 1959 */
   
   else if (index == T_CAMDDYYYY)
   {
      memset(abbrevmon, 0, 4);
      strncpy(abbrevmon, monstr[month - 1], 3);
      sprintf(dtstr, "%s %i %i", abbrevmon, day, year);
   }
   

   /* if other format is defined, then use a default format */
   
   else
   {
      sprintf(dtstr, "%s %i %i", monstr[month - 1], day, year);
   }
   
   
   /* copy the created string into the returned string */
   
   strcpy(valuestr, dtstr);
   
   
   return;
}



/*********************************************************************
   setdate_mixedcase()
   
   PURPOSE
   Sets a date time string to mixed case.
   
   NOTES
   Entire string is set to lower case except the beginning of month 
   and day-of-week names, and the strings AM and PM.
   
   *******************************************************************/
void setdate_mixedcase(char *dtstr)
{
#define NMON 12
#define NDAY  7
   
   char monstr[NMON][4] =
      {"jan", "feb", "mar", "apr", "may", "jun",
       "jul", "aug", "sep", "oct", "nov", "dec"};

   char daystr[NDAY][4] = 
      {"sun", "mon", "tue", "wed", "thu", "fri", "sat"};
   
   int i;
   char *first;
   int inlen, findlen, firstpos;

   
   /* get the input string length for use later */
   
   inlen = strlen(dtstr);
 
   /* do not change to lower case if the dtstr is defined in
      TIMESTR_HDR */
      
   if (strcmp(dtstr, TIMESTR_HDR) == 0)
     return;   
   
   /* set the entire string to lowercase */
   
   convert_str_to_lowcase(dtstr);
   
   
   /* search for any month strings and uppercase the first character */
   
   for (i = 0; i < NMON; i++)
   {
      if ((first = strstr(dtstr, monstr[i])) != NULL)
      {
	 findlen  = strlen(first);
	 firstpos = inlen - findlen;
	 dtstr[firstpos] = toupper(dtstr[firstpos]);
      }
   }
   
   
   /* search for any day of the week strings and uppercase the
      first character */
   
   for (i = 0; i < NDAY; i++)
   {
      if ((first = strstr(dtstr, daystr[i])) != NULL)
      {
	 findlen  = strlen(first);
	 firstpos = inlen - findlen;
	 dtstr[firstpos] = toupper(dtstr[firstpos]);
      }
   }
   
   
   /* search for AM or PM and uppercase both characters */
   
   if (((first = strstr(dtstr, "am")) != NULL) ||
       ((first = strstr(dtstr, "pm")) != NULL))
   {
      findlen  = strlen(first);
      firstpos = inlen - findlen;
      dtstr[firstpos]     = toupper(dtstr[firstpos]);
      dtstr[firstpos + 1] = toupper(dtstr[firstpos + 1]);
   }
   
   
   /* search for timezone strings and uppercase all three characters;
      this assumes that timezone ends with .dt, .st, or .mt */
   
   if (((first = strstr(dtstr, "dt")) != NULL) ||
       ((first = strstr(dtstr, "st")) != NULL) ||
       ((first = strstr(dtstr, "mt")) != NULL))
   {
      findlen  = strlen(first);
      firstpos = inlen - findlen - 1;
      if (firstpos < 0) firstpos = 0;
      dtstr[firstpos]     = toupper(dtstr[firstpos]);
      dtstr[firstpos + 1] = toupper(dtstr[firstpos + 1]);
      dtstr[firstpos + 2] = toupper(dtstr[firstpos + 2]);
   }
   
   
   return;  
}


/*********************************************************
  create_MND_datetime()

  PURPOSE
  Generate local current system time formatted as 
  "%I%M %p %Z %a %b %d %Y" in the product segment header.
  For example: 530 PM CDT FRI APR 6 2003
  
  May set timecode and define local time zone offset as needed.
  
**********************************************************/
void create_MND_datetime(const pcc_struct  *pcc,
                         fp_struct         *fp,
			 int               fpindex,
                         misc_struct 	   *misc,
                         char              *datetime_str)
{
   struct tm 	*time_struct;
   char 	formatstr[40];
   int  	numwrote, putenv_rc;
   char 	dtstr[MAXLEN_STRING], tempstr[MAXLEN_STRING];
   Location	*locPtr = NULL;
   char		where[25];    
   char		log_str[100];    
   static char  lastTZset[TZ_LEN +1] = "";
   static char	setTZcmd[35] = "";
      
      
   /* for segmented by point, MND date/time can be specified as office-wide 
      time zone and local time zone. for segmented by group and county,
      only use office-wide time zone */
           
   if (pcc->product.segment_mode == SEGMENT_MODE_POINT &&
       pcc->product.timeZoneFlag == 1)
   {   
      /* if using individual location's time zone flag,
         retrieve the location tzone value and set the TZ variable.
	 If record not found then set TZ to the default time zone */	

      sprintf(where, " WHERE lid = '%s' ", fp[fpindex].id);

      if ( (locPtr = GetLocation(where)) != (Location *) NULL )
      {
          if (strcmp(lastTZset, locPtr->tzone) != 0)
	  {
		memset(setTZcmd, 0, 25);
		sprintf(setTZcmd, "TZ=%s", locPtr->tzone);
		putenv_rc = putenv(setTZcmd);
		    		
                sprintf(log_str, "MND DateTime setTZcmd='%s' for lid '%s'", 
		        setTZcmd, fp[fpindex].id);
                log_msg("", log_str);
		    
		memset(lastTZset, 0, TZ_LEN + 1);
		strncpy(lastTZset, locPtr->tzone, TZ_LEN);		     
	   }
	   
   	   FreeLocation(locPtr);
   	   locPtr = NULL;
      }
   }        
   
   
   /* load in the time form as a value or a code to
      be filled in at issuance time */

   if (misc->batch_mode == TRUE)
   {
      /* break down the time value into its local components */

      time_struct = localtime(&(misc->system_time));
      
      
      /* define the datetime string */

      memset(dtstr, 0, MAXLEN_STRING);
            
      sprintf(formatstr, "%s", "%I");
      strcat(formatstr, "%M %p %Z %a %b %d %Y");
      numwrote = strftime(tempstr, MAXLEN_STRING, formatstr, time_struct);
   
      if (tempstr[0] == '0')   
        strcpy(dtstr, &tempstr[1]);            
      else   
        strcpy(dtstr, tempstr);
            
      if (datetime_str != NULL)
        strcpy(datetime_str, dtstr);
   }
   
   
   /* if the timezone is the standard one, set timecode as <MNDtime>;
      otherwise set timecode as <MND:TZcode>, in which case
      the TZcode will be used later when translating the product. */
      
   else
   {      
      if (pcc->product.segment_mode == SEGMENT_MODE_POINT &&
          pcc->product.timeZoneFlag == 1)
      {
         sprintf(datetime_str, "%s%s>", TIMESTR_MND, lastTZset);
      }
      else
         sprintf(datetime_str, "%sDEFAULT>", TIMESTR_MND);
   }
     
     
   /* if tzone for an individual location was used
      then set TZ back to original office wide value */
      
   if (pcc->product.segment_mode == SEGMENT_MODE_POINT &&
       pcc->product.timeZoneFlag == 1)
   {
    	if (strcmp(lastTZset, misc->defaultTZ) != 0)
    	{
    	   memset(setTZcmd, 0, 25);
    	   sprintf(setTZcmd, "TZ=%s", misc->defaultTZ);
    	   putenv_rc = putenv(setTZcmd);
		
    	   memset(lastTZset, 0, TZ_LEN + 1);
    	   strncpy(lastTZset, misc->defaultTZ, TZ_LEN);
    	}
   }
     
         
   return;
}   


/********************************************************************

 format_wwa_timephrase()

 PURPOSE
 Use WWA time phrase rule to format the date/time.
 
*********************************************************************/

void format_wwa_timephrase(misc_struct		*misc,
		           const time_t 	rawtime,
		    	   char 		dtstr[MAXLEN_STRING])
{
   struct tm 	*curtime;
   struct tm 	*valtime;
   int 	        today, tomorrow, yesterday;
   int 		firstday, lastday;
   char 	weekday[7][10] = 
               {"SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY",
                "THURSDAY", "FRIDAY", "SATURDAY"};
       
   
   /* initialize */
   
   today = tomorrow = yesterday = FALSE;
   firstday = lastday = FALSE; 
   
   /* set dtstrg as "further notice" if rawtime is missing */
   
   if (rawtime == MISSINGVAL)
      return; 
      
   else
   {
      /* malloc space for a copy of the valtime structure since the
	 localtime call uses the same memory and therefore overwrites
	 it; then copy the info from scratch var curtime into valtime */

      valtime = (struct tm *)malloc(sizeof(struct tm));
      if (valtime == NULL) 
	 log_msg(FAILED_MALLOC, "for valtime tm structure in format_wwa_timephrase");

      curtime = localtime(&rawtime);
      memcpy(valtime, curtime, sizeof(struct tm));

    /*  strftime(dtstrg, MAXLEN_STRING, "%m/%d %H:%M", valtime);*/

      /* break system time into its components */

      curtime = localtime(&misc->system_time);


      /* determine whether the time value day is for today,
	 or tomorrow; first check if the day is today */

      if (valtime->tm_year == curtime->tm_year &&
	  valtime->tm_yday == curtime->tm_yday)
	 today = TRUE;


      /* now check if time matches today; 
	 need to consider whether it current day is the first
	 or last day of the year */

      else
      {
	 if (curtime->tm_yday == 0)
	    firstday = TRUE;
	 else if (curtime->tm_mon == 12 && curtime->tm_mday == 31) 
	    lastday = TRUE;


	 /* if not the first or last day of the year, then simply check the
	    julian day */

	 if (firstday == FALSE && lastday == FALSE)
	 {
	    if (curtime->tm_yday - 1 == valtime->tm_yday &&
		curtime->tm_year == valtime->tm_year)
	       yesterday = TRUE;
	    else if (curtime->tm_yday + 1 == valtime->tm_yday &&
		     curtime->tm_year == valtime->tm_year)
	       tomorrow = TRUE;
	 }


	 /* if today is the first day of the year */

	 else if (firstday == TRUE)
	 {
	    if (curtime->tm_year - 1 == valtime->tm_year &&
		(valtime->tm_mon == 11 && valtime->tm_mday == 31))
	       yesterday = TRUE;
	    else if (valtime->tm_yday == 1)
	       tomorrow = TRUE;	    
	 }

	 else if (lastday == TRUE)
	 {
	    if (curtime->tm_yday - 1 == valtime->tm_yday)
	       yesterday = TRUE;
	    else if (curtime->tm_year +1 == valtime->tm_year &&
		     (valtime->tm_mon == 0 && valtime->tm_mday == 1))
	       tomorrow = TRUE;
	 }            
      }


      /* The time will be identified as being in one of four time slots in a day
        (0-6,6-12, 12-18,18-23)*/           
			     
      if (today == TRUE)
      {
           if (valtime->tm_hour >= 0 && valtime->tm_hour < 6)	 	 	
	      strcpy(dtstr, "EARLY THIS MORNING");

	   else if (valtime->tm_hour >= 6 && valtime->tm_hour < 12)   
	      strcpy(dtstr, "THIS MORNING");

	   else if (valtime->tm_hour >= 12 && valtime->tm_hour < 18)
	      strcpy(dtstr, "THIS AFTERNOON");

	   else if (valtime->tm_hour >= 18 && valtime->tm_hour < 24)
	      strcpy(dtstr, "THIS EVENING");
       }
       else if (tomorrow == TRUE)
       {
           if (valtime->tm_hour >= 0 && valtime->tm_hour < 6)
	      strcpy(dtstr, "LATE TONIGHT");

	   else if (valtime->tm_hour >= 6 && valtime->tm_hour < 12)
	   {  	            
	      strcpy(dtstr, weekday[valtime->tm_wday]);
	      strcat(dtstr, " MORNING");
	   }
	   else if (valtime->tm_hour >= 12 && valtime->tm_hour < 18)
	   {
	      strcpy(dtstr, weekday[valtime->tm_wday]);
	      strcat(dtstr, " AFTERNOON");
	   }
	   else if (valtime->tm_hour >= 18 && valtime->tm_hour < 24)
	   {
	      strcpy(dtstr, weekday[valtime->tm_wday]);
	      strcat(dtstr, " EVENING");
	   }
	 
       }
       else
       {
       	  /* Other than today and tomorrow, the week day within one week from now*/
       	  
       	  if ((valtime->tm_yday - curtime->tm_yday) < 7 )
       	  {
       	  	
	          if (valtime->tm_hour >= 0 && valtime->tm_hour < 6)
		  {  
		     strcpy(dtstr, "LATE ");
			      
		     /*  if valtime->tm_wday is 0 which means SUNDAY, 
			 then force to use Saturday*/
			      
	             if (valtime->tm_wday == 0)
			strcat(dtstr, weekday[6]);
		     else	 
		        strcat(dtstr, weekday[valtime->tm_wday -1]);
			      
		     strcat(dtstr, " NIGHT");
	          }
		     	  
		  else if (valtime->tm_hour >= 6 && valtime->tm_hour < 12)
		  {  	            
		     strcpy(dtstr, weekday[valtime->tm_wday]);
		     strcat(dtstr, " MORNING");
	          }
	          else if (valtime->tm_hour >= 12 && valtime->tm_hour < 18)
	          {
		     strcpy(dtstr, weekday[valtime->tm_wday]);
		     strcat(dtstr, " AFTERNOON");
	          }
	          else if (valtime->tm_hour >= 18 && valtime->tm_hour < 24)
		  {
		     strcpy(dtstr, weekday[valtime->tm_wday]);
		     strcat(dtstr, " EVENING");
	          }
       	  }	
       	  else
       	  {
       	  	strftime(dtstr, MAXLEN_STRING, "%A %B %d", valtime);
       	  }	
       	  	   
        }

      /* free the memory */

      free(valtime);
  }    

   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}


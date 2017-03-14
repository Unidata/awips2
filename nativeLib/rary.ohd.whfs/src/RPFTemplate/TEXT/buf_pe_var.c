/************************************************************************
   buf_pe_var.c
   
   check_if_PEvar()
   read_petime()
   check_if_derived()
   check_if_timeflag()

   **********************************************************************/

#include <string.h>                  /* string library functions */
#include <stdio.h>                   /* standard io library functions */
#include <stdlib.h>                  /* standard library functions */
#include <ctype.h>                   /* character type library functions */
#include <time.h>                    /* time library functions */

#include "buf_pe_var.h"       /* function protos */


/***********************************************************************
   check_if_PEvar()
   
   PURPOSE
   To check if the string token is the special variable used
   for specifying specific obs pe values, and if so, to load 
   the info about the request.
   
   <[lid,]pe,dur,ts,extr|extr/prob,time[,timeflag|,derivemode(s)[,timeflag]]>
   
   where time is either LATEST or relday|abshr:absmin|hrwindow
   
   and derivemode is either MIN##, MAX##, CHG##, GZ, MSL, FLOW, STG,
   MET, PV#
   where ## is the number of hours associated with the derivation
   the #after PV is the number of reference value such as depth of the
   soil
   the timeflag is the keyword TIME.
   
   ***********************************************************************/
int check_if_PEvar(char 		*fullfield,
		   varinfo_struct	*varinfo)
{
   int 			status, found;
   char			item[80];
   int			slen;
   char			*token;
   int			pe_read;
   int			durval;
   time_t		tnow;
   char			timestr[50];
   petime_struct	pe_time;
   char			msgstr[120];
   
   
   /* initialize status to bad */
   
   status = -1;
   
   
   /* make a local copy and do some simple checks. 
      check if the item is bounded by <>, as required. */
   
   slen = strlen(fullfield);
   
   if (slen < 70)
   {
      if (fullfield[0] != '<' || fullfield[slen - 1] != '>')
	 return(status);
      else
      {
	 strcpy(item, &fullfield[1]);
      }
   }
   else
      return(status);
   
   
   /* at this point, we have the token without its left bracket.
      parse thru a comma separated list and get the atrributes */
   
   token = strtok(item, ",>");
   
   
   /* the first field can be either a location id or the 
      physical element code */
   
   memset(varinfo->lid, 0, LOC_ID_LEN + 1);
   pe_read = 0;
   
   if (token != NULL)
   {  
     if (strlen(token) > 2 && strlen(token) <= LOC_ID_LEN)
	strcpy(varinfo->lid, token);

     else if (strlen(token) == 2)
     {
	strcpy(varinfo->pe, token);
	pe_read = 1;
     }

     else
	return(status);
   }
   else
      return(status);
   
   /* now get the pe unless it was already gotten. 
      if we have reached this point, then we assume that the
      user intended this to be a PE variable, so start issuing
      error messages if there are any problems. */   
   
   token = strtok(NULL, ",>");
   
   if (token != NULL)
   {
     if (!pe_read)
     {
       if (strlen(token) == 2)
	 strcpy(varinfo->pe, token);
      
       else
       {
	 sprintf(msgstr, " SHEF PE length != 2; %s", token);
	 log_msg(INVALID_PE_VAR, msgstr);
	 return(status);
       }
      
      
      /* get the next token now */
      
      token = strtok(NULL, ",>");
     }  
   }
   else
      return(status);
   
   /* now get the duration value as an integer */
   
   if (token != NULL)
   {
     if (strlen(token) > 0 && strlen(token) < 5)
     {
	durval = atoi(token);
	if ((durval >= 0 && durval < 5000) || durval == 5004)
	   varinfo->dur = durval;
	else
	{
	   sprintf(msgstr, "SHEF duration out of range; %d", durval);
	   log_msg(INVALID_PE_VAR, msgstr);
	   return(status);
	}

     }
     else
     {
	sprintf(msgstr, "SHEF duration invalid; %s", token);
	log_msg(INVALID_PE_VAR, msgstr);
	return(status);
     }
   
   }
   else
      return(status);
      
   /* now get the type source code */
   
   token = strtok(NULL, ",>");
   
   if (token != NULL)
   {
     if (strlen(token) == 2 &&
	 (token[0] == 'R' || token[0] == 'P' ||
	  token[0] == 'F' || token[0] == 'C')) 
	strcpy(varinfo->ts, token);

     else
     {
	sprintf(msgstr, "SHEF type-source invalid; %s", token);
	log_msg(INVALID_PE_VAR, msgstr);
	return(status);
     }
   
   }
   else
      return(status);
      
   /* now get the SHEF extremum and/or probability code, if token consists
      one character, consider it as extremum code; if two characters, 
      consider the second char as probability code*/
   
   token = strtok(NULL, ",>");
   
   memset(varinfo->extremum, 0, SHEF_EX_LEN + 1);
   memset(varinfo->probcode, 0, SHEF_PROB_LEN + 1);
   
   if (token != NULL)
   {
      if (strlen(token) == 1)
      {
	 strcpy(varinfo->extremum, token);
	 strcpy(varinfo->probcode, "Z");
      }
      else if (strlen(token) == 2)
      {
         strncpy(varinfo->extremum, token, 1);
	 strncpy(varinfo->probcode, token+1, 1);
      }	 
      else
      {
	 sprintf(msgstr, "SHEF extremum invalid; %s", token);
	 log_msg(INVALID_PE_VAR, msgstr);
	 return(status);
      }
   
   }
   else
      return(status);
      
   /* now get the time information. NOTE which 
      tokens are checked against!!! this is needed to get the
      three comma-separated time fields together for parsing */
   
   token = strtok(NULL, ",>");
   if (token != NULL)
   {
      slen = strlen(token);

      if (slen > 0)
      {
	 time(&tnow);

	 strcpy(timestr, token);

	 status = read_petime(tnow, timestr, &pe_time);


	 /* now load the information into the proper attributes.
	    the latest flag is only supported for observed type  data
	    while the next flag is only supported for forecast data. */

	 if (status >= 0)
	 {
	    if (pe_time.latest_flag == 1 &&
		(varinfo->ts[0] == 'R' || varinfo->ts[0] == 'P' ||
		 varinfo->ts[0] == 'C'))
	    {
	       varinfo->datatime  = 0;
	       varinfo->hr_window = 0;
	       varinfo->latest_flag = TRUE;
	       varinfo->next_flag   = FALSE;
	    }

	    else if (pe_time.next_flag == 1 &&
		     varinfo->ts[0] == 'F')
	    {
	       varinfo->datatime  = 0;
	       varinfo->hr_window = 0;
	       varinfo->latest_flag = FALSE;
	       varinfo->next_flag   = TRUE;
	    }

	    else if (pe_time.basetime != 0)
	    {
	       varinfo->datatime  = pe_time.basetime;
	       varinfo->hr_window = pe_time.hr_window;
	       varinfo->latest_flag = FALSE;
	       varinfo->next_flag   = FALSE;
	    }

	    else
	    {
	       sprintf(msgstr, "specified time invalid; %s", token);
	       log_msg(INVALID_PE_VAR, msgstr);
	       return(-1);
	    }
	 }
	 else 
	    return(status);
      }

      else
      {
	 sprintf(msgstr, "SHEF time specifier missing");
	 log_msg(INVALID_PE_VAR, msgstr);
	 return(status);
      }
   }
   else
      return(status);
      
   /* the remainder of the variable specification contains either
      none, one, or two items.  if the first field can be either
      the time (instead of value) indicator OR it can be the derived 
      data info.  the second field, if there is one, can only be 
      the time indictor, with the presumption that the first field
      is the derived info. */
   
   /* initialize the remaining info. */
   
   varinfo->time_flag     = 0;
   varinfo->derive_flag   = 0;
   
   varinfo->chg_flag = varinfo->min_flag = varinfo->max_flag = 0;
   varinfo->accum_flag = 0;
   varinfo->gagezero_flag = varinfo->msl_flag = 0;
   varinfo->flow_flag = varinfo->stage_flag = 0;
   varinfo->derive_numhrs = 0;
   varinfo->metric_flag = 0;
   varinfo->pv_flag = 0;
   varinfo->pv_value = 0;
   
   
   /* get the next token */
      
   token = strtok(NULL, ",>");
   if (token == NULL)
   	slen = 0;
   else
   	slen  = strlen(token);
   
   
   /* continue until there are no more tokens */
   
   while (slen > 0)
   {      
      /* check if the time flag is specified.  don't set the status
	 variable from the returned argumnent since it is perfectly
	 ok if this field is not the timeflag */
      
      found = check_if_timeflag(token);
      if (found)
	 varinfo->time_flag = 1;
      
      
      /* check if derived info is given and load it if it is. */
      
      else
      {
	 status = check_if_derived(token, varinfo);
	 if (status < 0)
	    return(status);
      } 
      
      
      /* get the next token if one exists */
      
      token = strtok(NULL, ",>");
      if (token == NULL)
	slen = 0;
      else
	slen = strlen(token);
   }

   
   /* now check that various invalid combinations of derived
      instructions have not been requested. */
   
   if (varinfo->derive_flag)
   {
      if (varinfo->ts[0] == 'C')
      {
	 sprintf(msgstr, "derived not allowed for contingency data");
	 log_msg(INVALID_PE_VAR, msgstr);
	 return(-1);
      }
      
      if (varinfo->ts[0] == 'F' && varinfo->chg_flag)
      {
	 sprintf(msgstr, "change not allowed for forecast data");
	 log_msg(INVALID_PE_VAR, msgstr);
	 return(-1);
      }
      
      if (varinfo->chg_flag &&
          (varinfo->gagezero_flag || varinfo->msl_flag ||
	   varinfo->stage_flag    || varinfo->flow_flag))
      {
	 sprintf(msgstr, "change not allowed with gagezero, msl, flow, or stage");
	 log_msg(INVALID_PE_VAR, msgstr);
	 return(-1);
      }
      
      if ((varinfo->min_flag + varinfo->max_flag + varinfo->chg_flag) > 1)
      {
	 sprintf(msgstr, "specify either min, max, or chg only");
	 log_msg(INVALID_PE_VAR, msgstr);
	 return(-1);
      }
      
      if ((varinfo->stage_flag    + varinfo->flow_flag +
           varinfo->gagezero_flag + varinfo->msl_flag) > 1)
      {
	 sprintf(msgstr, "specify only one of stage, flow, gagezero, or msl");
	 log_msg(INVALID_PE_VAR, msgstr);
	 return(-1);
      }
      
  }
   
   
   /* if it made all the way to here, then the attributes
      were read properly, so set the status */
   
   status = 1;   
   return(status);   
}


/*********************************************************************
   read_petime()
   
   PURPOSE
   Loads in the stage times specified for the observed and
   forecast stages in the template.
   
   format: refday|basehourminute|hrwindow
   or:	LATEST
   
   *******************************************************************/
int read_petime(time_t		curtime,
		char 		*timestr,
		petime_struct	*petime)
{
   int		num_items;
   struct tm 	*basetime;
   time_t 	timeval;
   char 	daystr[MAXLEN_STRING];
   char		hrminstr[MAXLEN_STRING];
   int		status;
   int		dayval, hrval, minval, windowval;
   char		msgstr[80];
   
   
   /* initialize status to bad */
   
   status = -1;
        
   
   /* check the string to see if it is the keyword LATEST. */
   
   if (strcmp(timestr, "LATEST") == 0)
   {
      petime->latest_flag = TRUE;
      petime->basetime  = 0;
      petime->hr_window = 0;
      status = 1;
   }
   
   
   /* check the string to see if it is the keyword NEXT. */
   
   else if (strcmp(timestr, "NEXT") == 0)
   {
      petime->next_flag = TRUE;
      petime->basetime  = 0;
      petime->hr_window = 0;
      status = 1;
   }
   
   
   /* the string must have the day value, hour:min value,
      and the hour window value */
   
   else
   {
      petime->latest_flag = FALSE;
      
      
      /* now grab the three fields */
      
      num_items = sscanf(timestr, "%d|%d:%d|%d",
			 &dayval, &hrval, &minval, &windowval);  
      
      
      /* check the relative day value */
      
      if (num_items != 4 ||
	  dayval < -60  || dayval    > 60 ||
	  hrval  < 0    || hrval     > 23 ||
	  minval < 0    || minval    > 59 || 
	  windowval < 0 || windowval > 48)
      {	 
	 sprintf(msgstr, "invalid time; %s", timestr);
	 log_msg(INVALID_PE_VAR, msgstr);
	 status = -1;
      }
      
      else
      {
	 timeval  = curtime + (dayval * 3600 * 24);
	 basetime = gmtime(&timeval);
	 strftime(daystr, MAXLEN_STRING, "%m/%d/%Y", basetime);
	 
	 sprintf(hrminstr, "%2.2d:%2.2d", hrval, minval);
	 
	 timeval = convert_gmdatetime_to_time(daystr, hrminstr);
	 if (timeval != 0)
	    status = 1;
      }
      
            
      if (status >= 0)
      {
	 petime->basetime  = timeval;
	 petime->hr_window = windowval;
      }
   }
   
    
   return(status);
}


/***********************************************************************

   check_if_derived()
   
   Check if the given string field specifies a derived value
   instruction.
      
   ***********************************************************************/
int check_if_derived(char 		*fullfield,
		     varinfo_struct	*varinfo)
{
   int 	badstatus, goodstatus;
   int 	durval, pvval;
   int 	slen;
   char	msgstr[100];
   
   
   /* initialize.  */
   
   badstatus  = -1;
   goodstatus = 1;
   
   durval = 0;
   pvval = 0;     
   
   /* check on the allowable values */
   
   slen = strlen(fullfield);
   
   if      (strncmp(fullfield, "CHG", 3) == 0)
   {
      if (slen > 3) durval = atoi(&fullfield[3]);
      varinfo->chg_flag = 1;
   }
   
   else if (strncmp(fullfield, "MIN", 3) == 0)
   {
      if (slen > 3) durval = atoi(&fullfield[3]);
      varinfo->min_flag = 1;
   }
   
   else if (strncmp(fullfield, "MAX", 3) == 0)
   {
      if (slen > 3) durval = atoi(&fullfield[3]);
      varinfo->max_flag = 1;
   }
   
   else if (strcmp(fullfield, "ACC") == 0)
      varinfo->accum_flag   = 1;
   
   else if (strcmp(fullfield, "GZ") == 0)
      varinfo->gagezero_flag = 1;
      
   else if (strcmp(fullfield, "MSL") == 0)
      varinfo->msl_flag = 1;
   
   else if (strcmp(fullfield, "FLOW") == 0)
      varinfo->flow_flag = 1;
   
   else if (strcmp(fullfield, "STG") == 0)
      varinfo->stage_flag = 1;
   
   else if (strcmp(fullfield, "MET") == 0)
      varinfo->metric_flag = 1;   
      
   else if (strncmp(fullfield, "PV", 2) == 0)
   {
      if (slen > 2) pvval = atoi(&fullfield[2]);
      varinfo->pv_flag = 1;
   }   
         
   else
   {
      sprintf(msgstr, "invalid field; %s", fullfield);
      log_msg(INVALID_PE_VAR, msgstr);
      return(badstatus);
   }
   
   
   /* check if the duration is okay */
   
   if (durval >= 0 && durval <= 720)
      varinfo->derive_numhrs = durval;
   else
   {
      sprintf(msgstr, "invalid derive duration; %d", durval);
      log_msg(INVALID_PE_VAR, msgstr);
      return(badstatus);
   }   
      
   
   if (pvval >= 0 )
      varinfo->pv_value = pvval;
      
   else
   {
      sprintf(msgstr, "invalid paired reference value; %d", pvval);
      log_msg(INVALID_PE_VAR, msgstr);
      return(badstatus);
   }   
   
   
   /* if got to here, then all ok */
   
   varinfo->derive_flag = 1;
   return(goodstatus);
}


/***********************************************************************
   
   check_if_timeflag()
   
   Check if the given string field is a string that specifies the
   time (instead of value) instruction.
   
   *********************************************************************/
int check_if_timeflag(char 		*fullfield)
{
   int	slen;
   int found;

   
   /* initialize */
   
   slen   = strlen(fullfield);
   
   
   /* check on the allowable values */
   
   if (slen == 4 && strncmp(fullfield, "TIME", 4) == 0)
      found = 1;
   
   else   
      found = 0;
   
   
   return(found);
}

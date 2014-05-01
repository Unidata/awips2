/*********************************************************************
   create_final_propduct.c 
   
   
   create_final_product()
   translate_timecode()
   
   *******************************************************************/

#include <stdio.h>             /* standard io library functions */
#include <string.h>            /* library string functions */
#include <stdlib.h>            /* standard library functions */
#include <ctype.h>             /* for isalpha() */
#include <time.h>

#include "create_final_product.h" 
 

/************************************************************************
   create_final_product()
   
   PURPOSE   
   Read a product file and insert the final/actual ETNs
   and insert any time codes based on issuance time.
   Opening and closing of the files are handled externally.
   
   *********************************************************************/
void  create_final_product(FILE 	*infile_ptr, 
		           FILE 	*outfile_ptr,
			   misc_struct  *misc, 
			   int		max_buflen,
			   char		*returned_msg,
			   int		numfps,
			   fp_struct	*fp,
			   vtecinfo_struct *vtecinfo,
			   char		*event_signif,
			   pcc_struct   *pcc)
{
   char 		fileline[MAXLEN_STRING];
   char 		fileline2[MAXLEN_STRING];
   char			dummytime[40];
   char 		*fgets_ptr;
   int			status, num_read, i, lid_len = 0;
   int			actual_etn;
   char			where[200];
   VTECevent		*eventHead = NULL;
   int			retrieved_data = FALSE;
   int			new_event_count, max_prev_etn;
   int			active;
   prev_vtecinfo_struct prev_vtecinfo;
   char			local_msg[240];
   char			*token;
   int			fpindex, timecode_index;
   time_t               actual_time, expire_time, current_time;
   struct tm    	*tm_struct;
   
   /* P-VTEC line */
   
   char         vtec_cat[VTEC_PRODMODE_LEN + 1] = ""; 
   char         action[VTEC_ACTION_LEN + 1] = "";   
   char		office[RFC_LEN + 1] = "";
   char         phenom[VTEC_PHENOM_LEN + 1] = "";         
   char         signif[VTEC_SIGNIF_LEN + 1] = "";         
   int          etn;                     
   char         begintime[VTEC_TIMECODE_LEN + 1] = "";      
   char         endtime[VTEC_TIMECODE_LEN + 1] = "";
            
   /* H-vtec line info */
      
   char         lid[VTEC_GEOID_LEN + 1] = "";
   char         severity[VTEC_SEVER_LEN + 1] = "";         
   char         immed_cause[VTEC_CAUSE_LEN + 1] = "";         
   char         risetime[VTEC_TIMECODE_LEN + 1] = "";      
   char         cresttime[VTEC_TIMECODE_LEN + 1] = "";      
   char         falltime[VTEC_TIMECODE_LEN + 1] = "";      
   char         record[VTEC_RECORD_LEN + 1] = ""; 
   
   
   /* initialize */
   
   new_event_count = 0;
   max_prev_etn    = 0;
   actual_time  = MISSINGVAL;
   expire_time  = MISSINGVAL;
   current_time = misc->system_time;
      
   strcpy(returned_msg, "");

   
   /* loop on the input file */
   
   for (;;)
   {      
      fgets_ptr = fgets(fileline, MAXLEN_STRING, infile_ptr);
      if (fgets_ptr == NULL) break;
      
      
      /* check if this is the beginning of the P-VTEC, H-VTEC
         combination of lines, and process them together. */
      
      if (pcc->product.vtec_flag == TRUE)
      {
	if (fileline[0] == '/')
	{
	
	   /* check if there are time codes in the P-VTEC line */
	   	 
	   translate_timecode(outfile_ptr, fileline, misc,
	                      &timecode_index, &actual_time);
	
	
	   /* scan the fields in the line.  
	      note that the lengths are hardcoded!!!
	      if the vtec times need translating, load the times into dummy
	      variables and then substitute the actual time. */
	      		      
	   if (timecode_index == TIMECODE_VTEC_BEGIN)
	   {
	      num_read = sscanf(&fileline[1], 
	                        "%1s.%3s.%4s.%2s.%1s.%d.%12s-%12s",
		     	        vtec_cat, action, office, phenom, signif, &etn, 
			        dummytime, endtime);
				
              tm_struct = gmtime(&actual_time);
              strftime(begintime, VTEC_TIMECODE_LEN+1, 
	               "%y%m%dT%H%MZ", tm_struct); 	    
           }
	   
	   
	   else if (timecode_index == TIMECODE_VTEC_END)
	   {
	      num_read = sscanf(&fileline[1], 
	                        "%1s.%3s.%4s.%2s.%1s.%d.%12s-%12s",
		     	        vtec_cat, action, office, phenom, signif, &etn, 
			        begintime, dummytime);
				
              tm_struct = gmtime(&actual_time);
              strftime(endtime, VTEC_TIMECODE_LEN+1, 
	               "%y%m%dT%H%MZ", tm_struct); 	     	    
	   } 

	      
           else
	   {
	      num_read = sscanf(&fileline[1], 
	                        "%1s.%3s.%4s.%2s.%1s.%d.%12s-%12s",
		     	        vtec_cat, action, office, phenom, signif, &etn, 
			        begintime, endtime);
	   }
	   
	   
	   /* check for error reading information */

	   if (num_read != 8)
	   {
	      sprintf(local_msg, 
	              "ERROR - too few fields (%d) in P-VTEC for:\n%s", 
		      num_read, fileline);
	      fprintf(stderr, local_msg);
	      log_msg("", local_msg);
	      strcat(returned_msg, local_msg);		  
	   }


	   /* now read the H-VTEC line */

	   fgets_ptr = fgets(fileline2, MAXLEN_STRING, infile_ptr);
	   if (fgets_ptr == NULL) 
	   {
	      sprintf(local_msg, "ERROR - invalid H-VTEC:\n%s", fileline2);
	      fprintf(stderr, local_msg);
	      log_msg("", local_msg);
	      strcat(returned_msg, local_msg);

	      break;
	   }


	   /* get the lid in a special way since it can be of variable length.
	      restore the stripped off period after getting the token. */

	   token = strtok(&fileline2[1], ".\n");
	   if (token == NULL)
	   {
	      sprintf(local_msg, "ERROR - error reading lid from H-VTEC.\n");
	      fprintf(stderr, local_msg);
	      log_msg("", local_msg);
	      strcat(returned_msg, local_msg);

	      break;
	   }
	 
	   else
	   {
	      strcpy(lid, token);
	      lid_len = strlen(lid);

	      fileline2[1+lid_len] = '.';
	   }


	   /* skip over the lid portion of the line and get the
	      remaining fields */

	   num_read = sscanf(&fileline2[1+lid_len+1], 
			     "%1s.%2s.%12s.%12s.%12s.%2s",
			     severity, immed_cause, 
			     risetime, cresttime, falltime, record);
	   if (num_read != 6)
	   {
	      sprintf(local_msg, "ERROR - too few fields (%d) after lid"
	                         " in H-VTEC:\n%s", 
		                 num_read, fileline2);
	      fprintf(stderr, local_msg);
	      log_msg("", local_msg);
	      strcat(returned_msg, local_msg);		  
	   }
	   
	   
	   /* initialize */
	   
	   fpindex    = MISSINGVAL;
	   actual_etn = MISSINGVAL;


           /* determine the index to the forecast point, for use when
	      updating the vtec info. */
	      
	   for (i = 0; i < numfps; i++)
	   {
              if (strcmp(fp[i].id, lid) == 0)
              {
		 fpindex = i;
		 break;
	      }
	   }
	   
	   if (fpindex == MISSINGVAL)
	   {
	      sprintf(local_msg, "ERROR - VTEC location %s unknown.\n", lid);
	      fprintf(stderr, local_msg);
	      log_msg("", local_msg);
	      strcat(returned_msg, local_msg);		  
	   }
	   

	   /* if this is a ROU.HY.S, the do nothing, simply echo the lines. */

	   if (strcmp(action, "ROU") == 0)
	   {
	      status = fputs(fileline,  outfile_ptr);      
	      status = fputs(fileline2, outfile_ptr); 
	      
	      actual_etn = 0;     	    
	   }

	 
	   /* non-ROU events need an actual ETN. set the value and then 
	      write the VTEC lines. */

	   else
	   {	    
	      /* get the previous VTECevent info now and use it later. 
		 use the significance code read from the product.  
		 note that a given product can not have mixed significance codes,
		 except that ROU.HY.S can be mixed in. so get only those events
		 that match the first non-ROU event. */

	      if (retrieved_data == FALSE)
	      {
		 retrieved_data = TRUE;

		 sprintf(where, " WHERE signif = '%s' AND office_id = '%s' ", 
			 event_signif, &office[1]);  

			 
	         /* use VTECpractice table if workstation mode is "PRACTICE" */
   
                 if (misc->workstation_mode == PRACTICE_MODE)
                    eventHead = (VTECevent *)GetVTECpractice(where);
		 else   
		    eventHead = GetVTECevent(where); 


		 /* find the max_etn for use in finding what etn to use for 
		    NEW events below. */ 

		 max_prev_etn = find_max_prev_etn(eventHead, "", current_time);
		 printf("max prev ETN = %d\n", max_prev_etn);
	      }


	      /* load the previous event info for this location into a 
	         convenient local structure. */

	      active    = FALSE;
	      
	      if (fpindex != MISSINGVAL)
	      {				
                 if (strcmp(event_signif, SIGNIF_WARNING) == 0)
                    copy_previous_event(vtecinfo[fpindex].prev_flw, 
		                        &prev_vtecinfo);
		       
                 else if (strcmp(event_signif, SIGNIF_WATCH) == 0)
                    copy_previous_event(vtecinfo[fpindex].prev_fla, 
		                        &prev_vtecinfo);
		       
                 else if (strcmp(event_signif, SIGNIF_ADVISORY) == 0)
                    copy_previous_event(vtecinfo[fpindex].prev_fly, 
		                        &prev_vtecinfo);


		 if (prev_vtecinfo.event_found == TRUE)
		    active = check_if_event_active(prev_vtecinfo.action, 
				                   prev_vtecinfo.endtime,
				                   current_time);
		 else 
		    active = FALSE;
	      }
	      

	      /* define the next available ETN for NEW events. */

	      if (strcmp(action, "NEW") == 0)
	      {
		 new_event_count++;
		 actual_etn = max_prev_etn + new_event_count;	       


		 if (active == TRUE)
		 {
		    sprintf(local_msg, 
			    "ERROR - NEW event has active event for location %s. \n",
			    lid);
		    fprintf(stderr, local_msg);
		    log_msg("", local_msg);
		    strcat(returned_msg, local_msg);		  
		 }
	      }


	      /* for EXP events, use the ETN from previous, inactive event */

	      else if (strcmp(action, "EXP") == 0)
	      {
		 if (prev_vtecinfo.event_found == TRUE)
		    actual_etn = prev_vtecinfo.etn;


		 /* check for error conditions */

		 if (vtecinfo[fpindex].endtime > (current_time  + (3600/2)) ||
		     vtecinfo[fpindex].endtime < (current_time  - (3600/2)))
		 {
		    sprintf(local_msg, 
			    "ERROR - EXP event outside 30 minutes of endtime"
			    " for location %s\n", lid);
		    fprintf(stderr, local_msg);
		    log_msg("", local_msg);
		    strcat(returned_msg, local_msg);
		 }	       
	      }


	      /* for non-NEW, non-EXP events, use the ETN from the 
	         previous event */

	      else
	      {
		 if (prev_vtecinfo.event_found == TRUE)
		    actual_etn = prev_vtecinfo.etn;


		 /* check for error conditions */

		 if (active == FALSE)
		 {
		    sprintf(local_msg, 
			    "ERROR - non-NEW event has no active event for %s."
			    "  ETN invalid!!!\n", lid);
		    fprintf(stderr, local_msg);
		    log_msg("", local_msg);
		    strcat(returned_msg, local_msg);
		 }	       
	      }


	      /* if no previous ETN exists, then use 0 instead of
		 missing (-9999). */

	      if (actual_etn == MISSINGVAL) 
	      {
		 sprintf(local_msg, 
			 "ERROR - missing ETN for %s!!!\n", lid);
		 fprintf(stderr, local_msg);
		 log_msg("", local_msg);
		 strcat(returned_msg, local_msg);
		 
		 actual_etn = 0;		  
	      }


	      /* output the two lines, with the new ETN and any substituted 
	         times into the P-VTEC line */

	      fprintf(outfile_ptr, "/%s.%s.%s.%s.%s.%.4d.%s-%s/\n", 
		      vtec_cat, action, office, phenom, signif, actual_etn,
		      begintime, endtime);	    
	      status = fputs(fileline2, outfile_ptr); 

	   }  /* end of if check for action wrt non-ROU event */
	   

	   /* update the etn info so that when the event info is later saved
	      to the database after issuance, it has the actual etn value.
	      also load in the previously defined actual_time for the UGC
	      now that we have the lid/fpindex known. */

	   if (fpindex != MISSINGVAL)
	   {	      
	      if (expire_time == MISSINGVAL)
	      {
		 sprintf(local_msg, 
			 "ERROR - Undefined UGC expire time "
			 " for location %s\n", lid);
		 fprintf(stderr, local_msg);
		 log_msg("", local_msg);
		 strcat(returned_msg, local_msg);
	      }
	      
	      vtecinfo[fpindex].etn        = actual_etn;
	      vtecinfo[fpindex].expiretime = expire_time;
	      
	      /* note that the begin time and end time values for 
	         the vtecinfo structure are NOT (re-)assigned here
		 based of any time translation needs.  that is because
		 the only translation of the begin and end times are
		 to use the current time, and for these cases the structure's
		 begin and end time values use a special value to indicate
		 the CURRENT time, in addition to using the special 
		 time coding feature.  */ 
	   }
	   
	}  /* end of if block for slash in first column indicating VTEC line */
	
	
	/* if not a line with VTEC, check if timecode translation needed. */
	   	
        else
	{
	   expire_time = MISSINGVAL;
	   
	   translate_timecode(outfile_ptr, fileline, misc,
	                      &timecode_index, &actual_time);
			      
			      
	   /* if there are any timecodes, in these non-VTEC lines, then do not 
	      write the output here since the translate function does the 
	      writing to the product in that case. */
	   
	   if (timecode_index == TIMECODE_NONE)
  	      status = fputs(fileline, outfile_ptr); 
	      
	      
	   /* important: if translating the UGC expire time for vtec events,
	      the actual_time needs to be saved later/above in the vtecinfo
	      structure after the location id (lid) is read.  this value is 
	      even later written to the database upon issuance. */
	   
	   else if (timecode_index == TIMECODE_UGC_OFFSET ||
	            timecode_index == TIMECODE_UGC_RELATIVE ||
	            timecode_index == TIMECODE_UGC_ABSOLUTE)
           {
	      expire_time = actual_time;
	   }
	}	 
      } /* end of if vtec product check*/
      
              	   
      /* if not a VTEC product, there is still a possibility that time 
         translation is needed. */
      
      else
      {
	 translate_timecode(outfile_ptr, fileline, misc,
	                      &timecode_index, &actual_time);
			      
	 if (timecode_index == TIMECODE_NONE)
  	    status = fputs(fileline, outfile_ptr); 
      }
   }  /* end of for loop on reading file */
   
   
   /* free memory */
   
   if (eventHead != NULL)
   {
      FreeVTECevent(eventHead);
      eventHead = NULL;
   }
   
   
   return;
}


/************************************************************************
   translate_timecode
   
   PURPOSE 
   Process the four possible time codes subject to translation.
   -- UGC expiration time (3 different modes)
   -- segement MND time
   -- product MND header time
   -- P-VTEC begin/end time
   
   For three of the four, insert the translated actual time in place of the
   time code and write the output.  For the P-VTEC substitution, return the 
   actual time but not do write the output.  This special case is needed
   because the P-VTEC line is translated for its ETN also, so the calling
   function takes care of the output generation.  To do this, this function
   returns the actual time and an index noting which code was found. This 
   time is also used for the expire time, which is needed to update memory
   since this field is later written to the database.
   
   *********************************************************************/
void translate_timecode(FILE 		*outfile_ptr,
                        char		*fileline, 
			misc_struct	*misc,
	                int		*timecode_index, 
			time_t		*actual_time)
{
   char 	*left_ptr = NULL, *right_ptr = NULL;
   char		local_msg[240];
   char 	formatstr[40];
   char         TZcode[TZ_LEN +1];   
   int		len_num = 0, len_before = 0;
   int		putenv_rc, numwrote, code_index, rv;
   char		temp_line[MAXLEN_STRING+MAXLEN_STRING]= "";
   char		temp_str[MAXLEN_STRING]= "";
   char		*token = NULL;   
   char		setTZcmd[35] = "";
   time_t	adjust_mins;
   time_t 	expire_time, endtime, current_time;
   time_t 	translate_time;
   struct tm 	*translate_tm = NULL;
   
   
   /* initialize */
   
   translate_time = MISSINGVAL;
   
   
   /* set local variable */
   
   current_time = misc->system_time;
   

   /* check if line includes any of the time codes.
      note the location of the left and right angle brackets
      for possible later use. */
   
   if      ((left_ptr = strstr(fileline, TIMESTR_UGC_OFFSET))  != NULL)
      code_index = TIMECODE_UGC_OFFSET; 
           
   else if ((left_ptr = strstr(fileline, TIMESTR_UGC_ABSOLUTE))!= NULL)
      code_index = TIMECODE_UGC_ABSOLUTE;
        
   else if ((left_ptr = strstr(fileline, TIMESTR_UGC_RELATIVE))!= NULL)
      code_index = TIMECODE_UGC_RELATIVE;  
     
   else if ((left_ptr = strstr(fileline, TIMESTR_MND))         != NULL)
      code_index = TIMECODE_MND;  
          
   else if ((left_ptr = strstr(fileline, TIMESTR_VTEC_BEGIN))  != NULL)
      code_index = TIMECODE_VTEC_BEGIN;  
      
   else if ((left_ptr = strstr(fileline, TIMESTR_VTEC_END))    != NULL)
      code_index = TIMECODE_VTEC_END;     
          
   else if ((left_ptr = strstr(fileline, TIMESTR_HDR))         != NULL)
      code_index = TIMECODE_HDR;  
          
   else 
      code_index = TIMECODE_NONE;
       
      
   if (left_ptr != NULL)
      right_ptr = strstr(left_ptr, ">");   
                  
		  
   /* ----------------------------------------------- */
   /* adjust the UGC related codes first */
   /* for standard UGC offset, read the offset in minutes
      and adjust the time. assume the form of <UGC+###> */
      
   if (code_index == TIMECODE_UGC_OFFSET   ||
       code_index == TIMECODE_UGC_RELATIVE || 
       code_index == TIMECODE_UGC_ABSOLUTE)
   {
     if (code_index == TIMECODE_UGC_OFFSET)
     {
        len_num = strspn((left_ptr+5), "1234567890"); 
    
        if (len_num == 0 || len_num >= 20) 
        {
           adjust_mins = 0;
      
           sprintf(local_msg, "ERROR - invalid minutes in time code\n");
           fprintf(stderr, local_msg);
	   log_msg("", local_msg);
        } 
      
        else
        {
           memset(temp_str, '\0', MAXLEN_STRING);
           strncpy(temp_str, (left_ptr+5), len_num);
           sscanf(temp_str, "%ld", &adjust_mins);
        }
     
   
        /* define the adjusted time */
      
        translate_time = current_time + (adjust_mins * 60);      


       /* fprintf(outfile_ptr, 
         "numstr,translate,adjust,len_num,before,after = :%s:%ld:%ld:%d:%d:%s\n",
          temp_str, translate_time, adjust_mins, len_num, len_before, right_ptr);
       */
     }
   
   
     /* special adjustments for UGC expire time based on vtec info */
   
     else if (code_index == TIMECODE_UGC_RELATIVE || 
              code_index == TIMECODE_UGC_ABSOLUTE)
     {     
        /* first parse out the info in the field, in order to get the 
           proposed expire time and vtec end time.
	   begin by making copy of string starting after initial code,
	   to be used by token functions. 
	   any actual times in the code must be 16 chars and of the form
	   "yyyy-mm-dd mm:dd"; the ":00" seconds must be appended. 
	   a missing end time is shown as 000000000000 (12 zeroes)*/

        strcpy(temp_line, left_ptr+6);
      	   
        if (code_index == TIMECODE_UGC_RELATIVE)
        {
          token = strtok(temp_line, ",");
	  if (token == NULL)
	  {
             sprintf(local_msg, "ERROR - invalid fld1 in UGC relative timecode.\n");
             fprintf(stderr, local_msg);
             log_msg("", local_msg);
	  }
	  
	  else
	  {
	     adjust_mins = atoi(token);
	     expire_time = current_time + (adjust_mins * 60);
	    
	     token = strtok(NULL, ">");
/* printf("reltoken=%s\n", token); */
	     if (token == NULL || (strlen(token) > 20))
	     {
               sprintf(local_msg, "ERROR - invalid fld2 in UGC relative timecode.\n");
               fprintf(stderr, local_msg);
               log_msg("", local_msg);
	     }	
	     
	     else
	     { 
	       if (strcmp(token, TIMESTR_MSG_ENDTIME) == 0)
  	          endtime = MISSINGVAL;
	       else
	       {
	          sprintf(temp_str, "%s%s", token, ":00"); 
                  rv = yearsec_ansi_to_timet(temp_str, &endtime);
	       }
	     }
	   }      
        }
   
        else if (code_index == TIMECODE_UGC_ABSOLUTE)
        {
          token = strtok(temp_line, ",");
	  if (token == NULL || (strlen(token) > 20))
	  {
             sprintf(local_msg, "ERROR - invalid fld1 in UGC absolute timecode.\n");
             fprintf(stderr, local_msg);
             log_msg("", local_msg);
	  }
	 
	  else
	  {
	     sprintf(temp_str, "%s%s", token, ":00"); 
             rv = yearsec_ansi_to_timet(temp_str, &expire_time);
	       	    
	     token = strtok(NULL, ">");
	     if (token == NULL || (strlen(token) > 20))
	     {
                sprintf(local_msg, "ERROR - invalid fld2 in UGC absolute timecode.\n");
                fprintf(stderr, local_msg);
                log_msg("", local_msg);
	     }	
	     else
	     { 
	        if (strcmp(token, TIMESTR_MSG_ENDTIME) == 0)
  	           endtime = MISSINGVAL;
		   
	        else
		{
	           sprintf(temp_str, "%s%s", token, ":00"); 
                   rv = yearsec_ansi_to_timet(temp_str, &endtime);
		}
	     }
	   }
      
        }
      
        if (expire_time == 0 || endtime == 0)
	{
          sprintf(local_msg, "ERROR - invalid expire or endtime in UGC timecode.\n");
          fprintf(stderr, local_msg);
          log_msg("", local_msg);
	}
      
      
        /* now adjust the expire time to make sure it is not beyond the 
           vtec end time but still within 60 minutes. */
/* printf("exp=%ld; end=%ld; cur=%ld; ", 
   expire_time, endtime, current_time); */
	 
        translate_time = expire_time;
	    
        if (endtime != MISSINGVAL)
        {
	  if (expire_time > endtime)
	     translate_time = endtime;
        }   
			
        if (translate_time < current_time + 3600)
          translate_time = current_time + 3600;		
     }
   

     /* enter in the final UGC expire time using a method
        common for all codes */
printf("trans=%ld\n", translate_time);	 

     translate_tm   = gmtime(&translate_time); 
      
                 
     /* get the length of the string before the time code,
        knowing the format of the code. */
   
     len_before = (left_ptr - fileline);
   
     if (len_before <= 0 || len_before > 80)
     {
        sprintf(local_msg, "ERROR - time code line length invalid.\n");
        fprintf(stderr, local_msg);
        log_msg("", local_msg);
     }
      
      
     /* now insert the translated time in the proper format.
        note that the newline character is already in the line. */             	   
	       	       
     sprintf(temp_str, "%2.2d%2.2d%2.2d", 
             translate_tm->tm_mday, translate_tm->tm_hour, translate_tm->tm_min);
	      	      
     memset(temp_line, '\0', MAXLEN_STRING + MAXLEN_STRING);
     strncpy(temp_line, fileline, len_before);
     strcat(temp_line, temp_str);
      
      
     /* add the final dash to the string */
      
     if (right_ptr != NULL && (strlen(right_ptr + 1) > 0)) 
        strcat(temp_line, (right_ptr+1));
     else
     {   
         sprintf(local_msg, "ERROR - missing end of UGC time code info.\n");
         fprintf(stderr, local_msg);
         log_msg("", local_msg);
     }
 
     fprintf(outfile_ptr, "%s", temp_line);      
   }



   /* -------------------------------------------------- */
   /* adjust the non-UGC time fields */
   /* if the local time zone code could be specified, then
      use it.  otherwise, use the default time zone to set the 
      output time */
      
   else if (code_index == TIMECODE_MND)
   {
      if (strstr(fileline, "<MND:DEFAULT>") == NULL)
      {
          /* grab the time zone code located after the colon */
	  
          token = strtok((left_ptr+5), ">");
	  strcpy(TZcode, token);
  
          /* set the TZ variable for use when setting the local time if it
	     differs from the local time. */
	  
	  if (strcmp(misc->defaultTZ, TZcode) != 0)
	  {
	     memset(setTZcmd, 0, 25);
	     sprintf(setTZcmd, "TZ=%s", TZcode);
	     putenv_rc = putenv(setTZcmd);
	  }
      }
	
      
      /* get the local time parts */
      	
      translate_time = current_time;
      translate_tm = localtime(&current_time);
      
      
      /* define the actual string. strip off any leading zero */

      memset(temp_line, '\0', MAXLEN_STRING + MAXLEN_STRING);
            
      sprintf(formatstr, "%s", "%I");
      strcat(formatstr, "%M %p %Z %a %b %d %Y");
      numwrote = strftime(temp_str, MAXLEN_STRING, formatstr, translate_tm);
      
      
      /* always convert to upper case, regardless of user setting */
      
      convert_str_to_upcase(temp_str);
   
      if (temp_str[0] == '0')   
         strcpy(temp_line, &temp_str[1]);            
      else   
         strcpy(temp_line, temp_str);
      
           
      /* output the resulting string */
          
      fprintf(outfile_ptr, "%s\n", temp_line);
      
      
      /* reset the time code as needed */
       
      if (strstr(fileline, "<MND:DEFAULT>") == NULL)
      {  
	  if (strcmp(misc->defaultTZ, TZcode) != 0)
	  {
	     memset(setTZcmd, 0, 25);
	     sprintf(setTZcmd, "TZ=%s", misc->defaultTZ);
	     putenv_rc = putenv(setTZcmd);
	  }
      }
   }
   
   
   /* if the header time is being specified, no adjustment is necessary. */
      
   else if (code_index == TIMECODE_HDR)
   {
      /* get the local time parts */
      	
      translate_time = current_time;
      translate_tm = localtime(&current_time);
      
      
      /* define the actual string. strip off any leading zero */

      memset(temp_line, '\0', MAXLEN_STRING + MAXLEN_STRING);
            
      sprintf(formatstr, "%s", "%I");
      strcat(formatstr, "%M %p %Z %a %b %d %Y");
      numwrote = strftime(temp_str, MAXLEN_STRING, formatstr, translate_tm);
      
     
      /* always convert to upper case, regardless of user setting */
     
      convert_str_to_upcase(temp_str);
   
      if (temp_str[0] == '0')   
         strcpy(temp_line, &temp_str[1]);            
      else   
         strcpy(temp_line, temp_str);
      
           
      /* output the resulting string */
          
      fprintf(outfile_ptr, "%s\n", temp_line);
   }
   
   
   /* the VTEC line is not translated here because it also needs
      to have its VTEC ETN translated.  therefore, the calling
      function generates the output.  nonetheless, the translated 
      time needs to be returned for use in the calling function. */
                      
   else if (code_index == TIMECODE_VTEC_BEGIN || 
            code_index == TIMECODE_VTEC_END)
   {     
       translate_time = current_time;
   }  
          
            	
   /* set return variables */
   
   *timecode_index = code_index;
   *actual_time    = translate_time;
   
   		
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

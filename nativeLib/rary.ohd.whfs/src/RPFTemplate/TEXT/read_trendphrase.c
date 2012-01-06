/************************************************************
     read_trendphrase()
     
     PURPOSE: The file fcst_trend_data.xxx (xxx stands for
     the site id) contains the forecast trend phrases which
     could be costumized by users. 
*************************************************************/

#include <stdio.h>             /* standard io library functions */
#include <string.h>            /* library string functions */
#include <stdlib.h>            /* standard library functions */     
#include "read_trendphrase.h"
#include "cat_and_product_defs.h"

extern char paramdir[];
fcsttrend_info_struct   *fcsttrend_info;


void read_trendphrase(char  *selected_office)
{
   FILE *file_ptr;
   char *fgets_ptr;
   char fcsttrend_file[MAXLEN_FILENAME];
   char msgstr[300];
   char rawline[MAXLEN_TEMPLATEREC];
   char fileline[MAXLEN_TEMPLATEREC];
   char *case_read;
   int  i, k, cnt = 0;
   char *trendphrase_value;
   int  ifoverride; 
   int	linenum;
   
   /* open fcst_trend_data.xxx which includes the detailed trend phrases */
   
   sprintf(fcsttrend_file, "%s/%s.%s", 
	   paramdir, FCSTTREND_FILE, selected_office);
   file_ptr = fopen(fcsttrend_file, "r");
   if (file_ptr == NULL) log_msg(FILE_OPENERR, fcsttrend_file);
   
   
   /* write informational message in log message */
   
   sprintf(msgstr, "Reading trend phrases from file %s...",
	   fcsttrend_file);
   log_msg("", msgstr);	
   
   
   /* find the number (cnt) of the phrases in the file */
   
   for (;;)
   {  
      /* get a line from the input  file, if end-of-file, then exit
	 the loop */
      
      fgets_ptr = fgets(rawline, MAXLEN_TEMPLATEREC, file_ptr);
      if (fgets_ptr == NULL) break;	
      
      cnt +=1;
   }
   
   /* check that the number of allowablw phrases in the file has not been
      exceeded */
   
   if (cnt >= MAX_NUMPHRASES)
   {
      log_msg(EXCEED_MAXNUM_OF_PHRASES, fcsttrend_file);
      fclose(file_ptr);
      return;
   } 
   
   
   /* malloc structure fcsttrend_info */
   
   if (cnt == 0)
      fcsttrend_info = (fcsttrend_info_struct *)malloc(sizeof (fcsttrend_info_struct) * 1);
   else
      fcsttrend_info = (fcsttrend_info_struct *)malloc(sizeof(fcsttrend_info_struct) * cnt);
   
   if (fcsttrend_info == NULL)
   {
      log_msg(FAILED_MALLOC, "of fcsttrend_info in read_trendphrase");
      exit(-1);
   }    
     
   
   /* allocate memory for structure fcsttrend_info fields */
              
   for (k = 0; k < cnt; k++)
   {  
      fcsttrend_info->trend_phrase[k] = (char *) malloc(MAXLEN_TEMPLATEREC);
      if (fcsttrend_info->trend_phrase[k] == NULL)
         log_msg(FAILED_MALLOC, "of fcsttrend_info->trend_phrase[k] in read_trendphrase");
      
      fcsttrend_info->trend_name[k] = (char *) malloc(MAXLEN_FILENAME);
      if (fcsttrend_info->trend_name[k] == NULL)
         log_msg(FAILED_MALLOC, "of fcsttrend_info->trend_name[k] in read_trendphrase");
   }	   	   
   
   if (cnt == 0)
      fcsttrend_info->trend_index = (int *) malloc(sizeof(int) * 1);
   else
   {  
      fcsttrend_info->trend_index = (int *) malloc(sizeof(int) * cnt);  
      if (fcsttrend_info->trend_index == NULL)
      {
         log_msg(FAILED_MALLOC, "of fcsttrend->info->trend_index in read_trendphrase");
	 exit(-1);
      }
   }
   
      	    
   /* initialize each fields in fcsttrend_info structure */
   
   fcsttrend_info->num_phrase = 0;
   for (k=0; k < cnt; k++)
   {
      strcpy(fcsttrend_info->trend_name[k],   "");
      strcpy(fcsttrend_info->trend_phrase[k], "");
      fcsttrend_info->trend_index[k] = MISSINGVAL;
   }     
   
   /* put the pointer back to the begining of the file */   
   
   rewind(file_ptr);
   
   
   /* get a line from the input file, if end-of-file, then exit the loop */
   
   linenum = 0;
   for (;;)   
   { 
      ifoverride = FALSE;     
      fgets_ptr = fgets(rawline, MAXLEN_TEMPLATEREC, file_ptr);
      if (fgets_ptr == NULL) break;
      linenum++;
      
      /* read any continuation lines to get them out of the way of subsequent
	 calls to determine the record type */
      
      get_continuation_lines(rawline, file_ptr, &linenum, fileline);  
      
      /* get the first token in the file line */       
      
      if ((fileline[0] != COMMENT_SYMBOL) && (strstr(fileline,":") != NULL))
      {
	 if ((case_read = strtok(fileline, ":\n")) != NULL)
	 {
	    /* convert type read to upper case */
	    
	    convert_str_to_upcase(case_read);
	    
	    if ((strcmp(case_read, "RISE_STEADY") == 0) ||
		(strcmp(case_read, "RISE_ABOVEWS") ==0) ||
		(strcmp(case_read, "RISE_ABOVEFS") == 0) ||	      
		(strcmp(case_read, "RISE_CREST") == 0) ||
		(strcmp(case_read, "FLUCTUATE_NEAR") == 0) ||
		(strcmp(case_read, "FALL_STEADY") == 0) ||
		(strcmp(case_read, "FALL_BELOWFS") == 0) ||
		(strcmp(case_read, "FALL_BELOWWS") == 0) ||
		(strcmp(case_read, "WEIR_NO_OVERFLOW") == 0) ||
		(strcmp(case_read, "WEIR_SLIGHT_OVERFLOW") == 0) ||
		(strcmp(case_read, "WEIR_BEGIN_OVERFLOW") == 0) ||
		(strcmp(case_read, "WEIR_OVERFLOW_INC") == 0) ||
		(strcmp(case_read, "WEIR_PRESENT_OVERFLOW") == 0) ||
		(strcmp(case_read, "WEIR_REMAIN_OVERFLOW") == 0) ||
		(strcmp(case_read, "WEIR_OVERFLOW_DEC") == 0) ||
		(strcmp(case_read, "WEIR_END_OVERFLOW") == 0) ||
		(strcmp(case_read, "WEIR_STOP_OVERFLOW") == 0) ||
		(strcmp(case_read, "WEIR_RENEW_OVERFLOW") == 0))
	    {   
	       
	       /*  for each type_read which matches the valid trend phrase 
		  (RISE_STEADY...), use the latest trend phrase loaded from
		  fcst_trend_data.xxx), it means override the previous trend
		  phrase with the new thrend phrase for the same trend name */
	       
	       for (i= 0; i < cnt; i++)
	       {
		  if (strcmp(case_read, fcsttrend_info->trend_name[i]) == 0)
		  {
		     trendphrase_value = strtok(NULL, "\t\n");
		     
		     if (trendphrase_value != NULL && 
			 (!IsBlankStr(trendphrase_value)))
		     {
			strcpy(fcsttrend_info->trend_phrase[i], 
			       trendphrase_value);
			ifoverride= TRUE;
			break;
		     }   
		  }   
	       }
	       
	       /* if the index ifoverride is FALSE */
	       
	       if (ifoverride == FALSE)
	       {	         		     
		  trendphrase_value = strtok(NULL,"\t\n");
		  
		  if (trendphrase_value != NULL)
		  {
		     fcsttrend_info->num_phrase +=1;
		     strcpy(fcsttrend_info->trend_name[fcsttrend_info->num_phrase -1],
			    case_read);
		     strcpy(fcsttrend_info->trend_phrase[fcsttrend_info->num_phrase -1],
			    trendphrase_value);
		  } 
		  else
		  { 
		     sprintf(msgstr, "Warning: No trend phrase for %s in file %s",
			     case_read,fcsttrend_file);
		     log_msg("", msgstr);
		  }		  
	       }  
	    } 
	    
	    else
	    {
	       /* if the type_read is not valid then issue error message */
	       
	       log_msg(BADPRIMARYKEYWORD_VALUE, case_read); 
	    }   
	 }	
      }
      
   }  /* end reading file */
   
   
   /* close the FcstDetailTrend_data.xxx file */
   
   fclose(file_ptr);
   
   
   /* check how many trend phrases defined in the fcst_trend_data.xxx file */
   
   if (fcsttrend_info->num_phrase > MAXNUM_OF_TRENDPHRASE) 
      log_msg(EXCEED_MAXNUM_OF_TRENDPHRASES, FCSTTREND_FILE);     
   else
   {
      if (fcsttrend_info->num_phrase != 0)
      {
	 for (k= 0; k < fcsttrend_info->num_phrase; k++)
	 {
	    if (strcmp(fcsttrend_info->trend_name[k], "RISE_STEADY") == 0)
	       fcsttrend_info->trend_index[k] = RISE_STEADY;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "RISE_ABOVEWS") == 0)
	       fcsttrend_info->trend_index[k] = RISE_ABOVEWS;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "RISE_ABOVEFS") == 0)
	       fcsttrend_info->trend_index[k] = RISE_ABOVEFS;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "RISE_CREST") == 0)
	       fcsttrend_info->trend_index[k] = RISE_CREST;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "FLUCTUATE_NEAR") == 0)
	       fcsttrend_info->trend_index[k] = FLUCTUATE_NEAR;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "FALL_STEADY") == 0)
	       fcsttrend_info->trend_index[k] = FALL_STEADY;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "FALL_BELOWFS") == 0)
	       fcsttrend_info->trend_index[k] = FALL_BELOWFS;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "FALL_BELOWWS") == 0)
	       fcsttrend_info->trend_index[k] = FALL_BELOWWS;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "WEIR_NO_OVERFLOW") == 0)
	       fcsttrend_info->trend_index[k] = WEIR_NO_OVERFLOW;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "WEIR_SLIGHT_OVERFLOW") == 0)
	       fcsttrend_info->trend_index[k] = WEIR_SLIGHT_OVERFLOW;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "WEIR_BEGIN_OVERFLOW") == 0)
	       fcsttrend_info->trend_index[k] = WEIR_BEGIN_OVERFLOW;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "WEIR_OVERFLOW_INC") == 0)
	       fcsttrend_info->trend_index[k] = WEIR_OVERFLOW_INC;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "WEIR_PRESENT_OVERFLOW") == 0)
	       fcsttrend_info->trend_index[k] = WEIR_PRESENT_OVERFLOW;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "WEIR_REMAIN_OVERFLOW") == 0)
	       fcsttrend_info->trend_index[k] = WEIR_REMAIN_OVERFLOW;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "WEIR_OVERFLOW_DEC") == 0)
	       fcsttrend_info->trend_index[k] = WEIR_OVERFLOW_DEC;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "WEIR_END_OVERFLOW") == 0)
	       fcsttrend_info->trend_index[k] = WEIR_END_OVERFLOW;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "WEIR_STOP_OVERFLOW") == 0)
	       fcsttrend_info->trend_index[k] = WEIR_STOP_OVERFLOW;
	    
	    else if (strcmp(fcsttrend_info->trend_name[k], "WEIR_RENEW_OVERFLOW") == 0)
	       fcsttrend_info->trend_index[k] = WEIR_RENEW_OVERFLOW;
	 }
      }	     	 
   } 	     	     	      	                      
   return;
}    



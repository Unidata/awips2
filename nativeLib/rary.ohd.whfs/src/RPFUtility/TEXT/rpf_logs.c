/********************************************************************
   rpf_logs.c

   get_envdirs()
   log_msg()
   close_msglog()
   Modification:   04/05/2006  Add check if addmsg is NULL in the log_msg().
   
   ********************************************************************/

#include <stdio.h>                  /* standard io library */
#include <stdlib.h>                 /* standard libarary */
#include <string.h>                 /* string library */
#include <time.h>                   /* time library */
#include <unistd.h>

#include "rpf_logs.h"               /* protypes for functions */

static int msgopen = FALSE;
static FILE *msgfile_ptr;

char paramdir[128];
char outputdir[128];
char productdir[128];
char bindir[128];
char file_suffix[10];

/*********************************************************************
   get_envdirs()
   
   PURPOSE
   Gets the directory names used in RiverPro, as defined 
   by environment variables.
   
*********************************************************************/
void get_envdirs(char	*user_suffix)
{
   long  pidval;

    int  len=0, rlen=0, istatus=0;

   len = strlen("rpf_template_dir");
   istatus = get_apps_defaults("rpf_template_dir", &len, paramdir, &rlen);


   if (istatus != 0)
   {
      fprintf(stderr, "rpf_template_dir undefined.  Program aborting.\n");
      exit(-1);
   }
   
   len = strlen("rpf_log_dir");
   istatus = get_apps_defaults("rpf_log_dir", &len, outputdir, &rlen);


   if (istatus != 0)
   {
      fprintf(stderr, "rpf_log_dir undefined.  Program aborting.\n");
      exit(-1);
   }

   len = strlen("whfs_product_dir");
   istatus = get_apps_defaults("whfs_product_dir", &len, productdir, &rlen);


   if (istatus != 0)
   {
      fprintf(stderr, "whfs_product_dir undefined.  Program aborting.\n");
      exit(-1);
   }
   
   len = strlen("whfs_bin_dir");
   istatus = get_apps_defaults("whfs_bin_dir", &len, bindir, &rlen);


   if (istatus != 0)
   {
      fprintf(stderr, "whfs_bin_dir undefined.  Program aborting.\n");
      exit(-1);
   }
   
   
   /* also user the user specified suffix or
      get the process id for use in uniquely
      identifying the files. the getpid returns a pid_t */
   
   if (strlen(user_suffix) == 0)
   {
      pidval = (long )getpid();
      sprintf(file_suffix, "%-ld", pidval);
   }
   else
      strcpy(file_suffix, user_suffix);
   
   return;
}


/*********************************************************************
   log_msg()
   
   PURPOSE
   Processes messages for the River Product Formatter program, including
   informational message, warning messages, and fatal messages. 
   
   NOTES
   The approach for building messages is that there can be two parts
   to each message - the generic part and the specific part.  This always the 
   same condition, which may be triggered in any number of functions, to be 
   labelled generically, and then have specific info describing the situation.

   Three classes of message are logged: informational, product logging, and 
   warning/error messages, into two possible locations: standard output and
   the message file.
     
   A call to this function with the first argument null implies that
   the message is an informational message.  If the letter "p" is passed
   as the first argument, then the message is assumed to be product logging; 
   otherwise the message is assumed to be a warning, error, or fatal message.
   If the letter "f" is the first character in this first argument,
   the message is assumed to be an order to abort the program processing.
  
   *******************************************************************/

void log_msg(const char *msg,
	     const char *addmsg)
{
   static int first = TRUE;
   time_t logstart_time;
   char outmsg[MAXLEN_LONGSTR];
   static int numwarnings = 0, numerrors = 0;
   extern int daylight;
   extern long timezone;
   extern char *tzname[2];
   char filename[MAXLEN_FILENAME];
   
   /* initialize */
   
   memset(outmsg, 0, MAXLEN_LONGSTR);
   time(&logstart_time);
         
   /* open the output files if they are not open already; if opening
      subsequent times in the session, open in append mode.
      if an error opening then display message - don't call this function
      recursively */
   
   if (msgopen == FALSE)
   {
      sprintf(filename, "%s/%s.%s", outputdir, MESSAGE_LOGFILE, file_suffix);
      if (first)
	 msgfile_ptr = fopen(filename, "w");
      else
	 msgfile_ptr = fopen(filename, "a");
	 
	 
      
      if(msgfile_ptr == NULL) 
      {
	 printf("FATAL CONDITION... could not open output message file %s\n",
		filename);
	 exit(0);
      }
      else
      {
	 fprintf(msgfile_ptr, "RIVERPRO message log file opened at %s",
		 asctime(gmtime(&logstart_time)));
	 fprintf(msgfile_ptr, "Unique file_suffix for filenames: %s\n",
		 file_suffix);
	 msgopen = TRUE;
	 
	 
	 /* set the extern variables based on the TZ environment
	    variable and log for info */
	 
	 tzset();
	 fprintf(msgfile_ptr,
		 "[TZ info (daylt flag/secs offset/names) = %d %ld %s %s]\n",
		 daylight, timezone, tzname[0], tzname[1]);
      }
   }
   
   
   first = FALSE;
   
   /* if the generic message string is undefined, simply copy the string */
   
   if (strlen(msg) == 0 && addmsg != NULL)
      strcpy(outmsg, addmsg);  


   /* if the generic message string indicates product logging, then
      simply copy the string */
   
   else if (strcmp(msg, "p") == 0 && addmsg != NULL)
      strcpy(outmsg, addmsg);
   
   else if (strcmp(msg, "warncnt") == 0)
   {
	 sprintf(outmsg, "\n%i warnings; %i errors generated.",
		 numwarnings, numerrors);
   }

   
   /* otherwise need to build the message */
   
   else
   {  
      if (msg[0] == 'f')
      {
	 strcpy(outmsg, "FATAL CONDITION...");
	 strcat(outmsg, &msg[1]);
      }
      else if (msg[0] == 'w') 
      {
	 strcpy(outmsg, "WARNING...");
	 strcat(outmsg, &msg[1]);
	 numwarnings++;
      }
      else 
      {
	 strcpy(outmsg, "ERROR...");
	 strcat(outmsg, msg);
	 numerrors++;
      }
      
      strcat(outmsg, " ");
      if (addmsg != NULL)
         strcat(outmsg, addmsg);
   }
   
   /* log the message to the standard output if not a product logging!!! 
      
   if (strcmp(msg, "p") != 0) printf("%s\n", outmsg);
   */
   
   /* log the message to the message file always; use newline as needed */
   
   if (strcmp(msg, "p") == 0)
      fprintf(msgfile_ptr, "->%s", outmsg);
   else
      fprintf(msgfile_ptr, "%s\n", outmsg);
   fflush(msgfile_ptr);   
   
   
   /* if fatal error then abort;
      there are two different versions of this function, one for the
      interactive program, one for the non-interactive program! */
   
   if (msg[0] == 'f') abort_rpf(outmsg);
   
   return;
}

/********************************************************************
  close_msglog()
  
  PURPOSE 
  Close the message log file.
  This function is coordinated with the log_msg function.
      
   ********************************************************************/
void close_msglog()
{
   int status;
   
   if (msgopen == TRUE)
   {
      status = fclose(msgfile_ptr);
      if (status != 0) log_msg(FILE_CLOSEWARN, "RiverPro message file");
      msgfile_ptr = NULL;
      msgopen = FALSE;
   }
   
   return;
}

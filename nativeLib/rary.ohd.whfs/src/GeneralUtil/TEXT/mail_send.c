/*#############################################################################

  Name 		: mail_send.c
  
  Description 	: Function to send emails.
  Args          : char *mail_from ---- from id
		  char *rcpt_list ---- to id (if multiple ids are used then ',' should be used 
                                       as delimiter
                  char  *subject  ---- subject of the mail
                  char  *filename ---- file containing the message part of the mail
                  FILE  *logfp    ---- file ptr to log the o/p or errors of the function
                                       if logfp is NULL then the o/p or errors are send to stderr
  Author        : Varalakshmi Rajaram
  Created       : April 7th 2005

#############################################################################*/
#include<stdio.h>
#include<errno.h>
#include<unistd.h>
#include<time.h>

#include "GeneralUtil.h"

int mail_send(char* mail_from, char *rcpt_list, char * subject, char * filename, FILE* logfp)
{
  FILE* fd;                   /* socket descriptor */
  int ret=0;                /* To hold the return value from calls to read / write */
  FILE * fp_cont_file;      /* File pointer to read the content file */
  char *rcpt_to;            /* To hold the individual rcpt id's which are extracted from rcpt_list 
                               with ',' as delimiter */
  char *rcpt_list_copy;     /* To hold the rcpt list for strtok manip */
  char buff[1000];          /* To hold to string that has to be written to socket ,
                               Also to hold the contents read from the content file and 
                               write it to socket */
  char tempString[50];

  if (logfp == (FILE*)NULL)
   logfp = stderr;

  memset(tempString, '\0', sizeof(tempString));
  sprintf(tempString, "/usr/sbin/sendmail %s", rcpt_list);

  if((fd = popen(tempString, "w")) != (FILE*) NULL)
  {

  if((fp_cont_file=fopen(filename,"r")) == (FILE *)NULL)
  {
    fprintf(logfp,"\n[%s]: Error in opening content_file [%s] error :[%s]",log_time(), filename, strerror(errno));
    return -1;
  }

  /* Send the mail from address and confirm */
  memset(buff, 0, sizeof(buff));
  sprintf(buff, "From: %s\r\n", mail_from);
  ret = fprintf(fd, buff);
  if (ret < 0)
  {
    fprintf (logfp,"\n[%s]:Error in write to socket buff:[%s] error:[%s] \n", log_time(), buff, strerror(errno));
    pclose (fd);
    return -1;
  }

  /* send to multiple recipients */
  
  rcpt_list_copy = (char*) calloc(strlen(rcpt_list+1), sizeof(char));
  strcpy(rcpt_list_copy, rcpt_list);
  rcpt_to= (char*)strtok(rcpt_list_copy, ",");
  while(1)
  {
    if (rcpt_to != (char*)NULL)
    {
  	/* Send the rcpt to address and confirm */
  	memset(buff, 0, sizeof(buff));
  	sprintf(buff, "to: %s\r\n", rcpt_to);
  	ret = fprintf(fd, buff);
  	if (ret < 0)
  	{
            fprintf (logfp,"\n[%s]:Error in write to socket buff:[%s] error:[%s] \n", log_time(), buff, strerror(errno));
            pclose (fd);
            return -1;
        }
     }
     else
       break;

     rcpt_to = (char*) strtok(NULL, ",");
  }
  /* send to multiple recipients */

  memset(buff, 0, sizeof(buff));
  sprintf(buff,"subject:%s\n", subject);
  ret = fprintf(fd, buff);
  if (ret < 0)
  {
    fprintf (logfp,"\n[%s]:Error in write buff [%s] within data part to socket [%s] \n", log_time(), buff, strerror(errno));
    pclose (fd);
    fclose(fp_cont_file);
    return -1;
  }

#ifdef CHECK
  memset(buff, 0, sizeof(buff));
  sprintf(buff,"reply-to:%s\n", rcpt_list);
  ret = fprintf(fd, buff);
  if (ret < 0)
  {
    fprintf (logfp,"\n[%s]:Error in write buff [%s] within data part to socket [%s] \n", log_time(), buff, strerror(errno));
    pclose (fd);
    fclose(fp_cont_file);
    return -1;
  }
#endif

  /* Read the cont_file file */
  memset(buff, 0, sizeof(buff));
  while(fgets(buff, 200, fp_cont_file) != (char *)NULL)
  {
    ret = fprintf(fd, buff);
    if (ret < 0)
    {
     fprintf (logfp,"\n[%s]:Error in write the file's [%s] content to socket [%s] \n", log_time(), filename, strerror(errno));
     pclose (fd);
     fclose(fp_cont_file);
     return -1;
    }
   memset(buff, 0, sizeof(buff));
  }

  memset(buff, 0, sizeof(buff));
  sprintf(buff, ".\r\n");
  ret = fprintf(fd, buff);
  if (ret < 0)
  {
    fprintf (logfp,"\n[%s]:Error in write to buff [%s] denoting end to server [%s] \n", log_time(), buff, strerror(errno));
    pclose (fd);
    return -1;
  }

  fprintf(logfp,"\n[%s]:Written the infm to mail", log_time());
  fflush(logfp);
  fclose(fp_cont_file);
  pclose(fd);
  }
  else
  {
   fprintf(logfp, "\n[%s]:Popen failed... error[%s]", log_time(),strerror(errno));
   fflush(logfp);
  }
  return 0;
}

/*****************************************************************************
  Name        : build_time.c Author   : Varalakshmi Rajaram  
  Created : April 7th 2005
  Description : Function to get the current gmtime in string format
                  for ex: Wed Apr 13 15:05:27 2005
  Args : void
  Return : char * --- contains the time in string format
*******************************************************************************/
    
char * log_time()
{
  static char time_str[30];
  time_t cur_time;

  cur_time = time(&cur_time);
  strcpy(time_str, asctime(gmtime(&cur_time)));

  time_str[24] = '\0';
  return time_str;
}

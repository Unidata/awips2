#define _GNU_SOURCE
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <stdarg.h>
#include <Xm/Xm.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <stdio.h>

#include "GeneralUtil.h"
#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

static FILE *logFile = NULL;

void openLogFile ()
{
   static char *logToken = "mpe_editor_logs_dir";
   char tokenValue[TOKEN_VALUE_LEN], strFileName[FNAME_LEN];
   char tmpTime[12];
   int length;
   int reply_len;
   int status;
   struct tm *pDate = NULL;
   time_t currentTime;

   currentTime = time(NULL);

   length = strlen (logToken);
   status = get_apps_defaults (logToken, &length, tokenValue, &reply_len);

   if (reply_len <= 0)
   {
      printf ("ERROR: Token value for token \"%s\" is not available."
	      "\n\t", logToken);
   }

   pDate = gmtime (&currentTime);

   strftime (tmpTime, YYYYMMDDHH_LEN + 1, "%m%d%H%M", pDate);

   sprintf (strFileName, "%s/mpe_editor%s.%d", tokenValue, tmpTime, getpid ());

   if ((logFile = fopen (strFileName, "w")) == NULL)
   {
      printf ("ERROR: can't open log file:\"%s\"." "\n\tProgram exiting.",
	      strFileName);
      exit (0);
   }
   else
   {
      printf ("Write log information to:\n%s\n", strFileName);
   }

}

void closeLogFile ()
{
   if (logFile != NULL)
   {
      fclose (logFile);
      logFile = NULL;
   }
}

void logMessage (const char * messageFormat, ...)
{
   const char * pBeginChar = NULL;
   char * pEndChar = NULL;
   char * pFormatString = NULL;
   char message[1024];
   char valueString[1024];
   int i;
   va_list ap;
   va_start (ap, messageFormat );
   
   pBeginChar = messageFormat;

   memset ( message, '\0', 1024 );
 
   /* Recognized format characters: s,d,f,c */
   pEndChar = strchr ( pBeginChar, '%' );

   while ( pEndChar != NULL  )
   {

      strncat ( message, pBeginChar, pEndChar - pBeginChar ); 

      i = 1;

      while ( pEndChar[i] != '\0')
      {
         switch ( pEndChar[i] )
         {
            case 'c':

               pFormatString = strndup ( pEndChar, i + 2 ); 
               pFormatString[i + 1]='\0';
               sprintf ( valueString, pFormatString, (char) va_arg (ap, int ) ); 
               strcat ( message, valueString );
               break;

            case 's':

               pFormatString = strndup ( pEndChar, i + 2 ); 
               pFormatString[i + 1]='\0';
               sprintf ( valueString, pFormatString, va_arg (ap, char * ) ); 
               strcat ( message, valueString );
               break;

            case 'd':

               pFormatString = strndup ( pEndChar, i + 2 ); 
               pFormatString[i + 1]='\0';
               sprintf ( valueString, pFormatString, va_arg (ap, int ) ); 
               strcat ( message, valueString );
               break;

            case 'f':

               pFormatString = strndup ( pEndChar, i + 2 ); 
               pFormatString[i + 1]='\0';
               sprintf ( valueString, pFormatString, va_arg (ap, double ) ); 
               strcat ( message, valueString );
               break;

            default:
 
               ++i;
               continue;
         } 

         free ( pFormatString );
         pFormatString = NULL;
         break;

      }

      if ( pEndChar[i] == '\0' )
      {
         va_end(ap);
         return;
      }

      pBeginChar = pEndChar + i + 1;
      pEndChar = strchr ( pBeginChar, '%' );
   }

   strcat (message, pBeginChar );

   if (logFile != NULL)
   {
      /* Output the message to the log file. */
      fprintf (logFile, "%s", message);
      fflush (logFile);

      /* Output the message to STDOUT. */
      printf ("%s", message);
   }
   else
   {
      /* Output the message to STDOUT. */
      printf ("%s", message);
   }

   va_end(ap);
   return;
}

void flogMessage (FILE * stream, const char * messageFormat, ...)
{
   const char * pBeginChar = NULL;
   char * pEndChar = NULL;
   char * pFormatString = NULL;
   char message[1024];
   char valueString[1024];
   int i;
   va_list ap;
   va_start (ap, messageFormat );
   
   pBeginChar = messageFormat;

   message[0] = '\0';;
 
   /* Recognized format characters: s,d,f,c */
   pEndChar = strchr ( pBeginChar, '%' );

   while ( pEndChar != NULL  )
   {

      strncat ( message, pBeginChar, pEndChar - pBeginChar ); 

      i = 1;

      while ( pEndChar[i] != '\0')
      {
         switch ( pEndChar[i] )
         {
            case 'c':

               pFormatString = strndup ( pEndChar, i + 2 ); 
               pFormatString[i + 1]='\0';
               sprintf ( valueString, pFormatString, (char) va_arg (ap, int ) ); 
               strcat ( message, valueString );
               break;

            case 's':

               pFormatString = strndup ( pEndChar, i + 2 ); 
               pFormatString[i + 1]='\0';
               sprintf ( valueString, pFormatString, va_arg (ap, char * ) ); 
               strcat ( message, valueString );
               break;

            case 'd':

               pFormatString = strndup ( pEndChar, i + 2 ); 
               pFormatString[i + 1]='\0';
               sprintf ( valueString, pFormatString, va_arg (ap, int ) ); 
               strcat ( message, valueString );
               break;

            case 'f':

               pFormatString = strndup ( pEndChar, i + 2 ); 
               pFormatString[i + 1]='\0';
               sprintf ( valueString, pFormatString, va_arg (ap, double ) ); 
               strcat ( message, valueString );
               break;

            default:
 
               ++i;
               continue;
         } 

         free ( pFormatString );
         pFormatString = NULL;
         break;

      }

      if ( pEndChar[i] == '\0' )
      {
         va_end(ap);
         return;
      }

      pBeginChar = pEndChar + i + 1;
      pEndChar = strchr ( pBeginChar, '%' );
   }

   strcat (message, pBeginChar );

   if (logFile != NULL)
   {
      /* Output the message to the log file. */
      fprintf (logFile, "%s", message);
      fflush (logFile);

      /* Output the message to STDOUT. */
      fprintf ( stream, "%s", message);
   }
   else
   {
      /* Output the message to STDOUT. */
      fprintf ( stream, "%s", message);
   }

   va_end(ap);
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

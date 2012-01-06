/* Function to read the numeric dates in the date_files_copied file and 
 * in the mods_sent_to_ofs file and compare them.  If the date files were
 * copied is greater (newer) than the date mods were copied, it returns
 * a TRUE.  If not, it makes a further check to see if the mods file for the
 * Forecast Group exists - if it does then it returns a TRUE, if not a FALSE. 
 * Written by D. Page - 23 Sept. 1997
 */
 
#include <stdio.h>
#include <sys/stat.h>
#include <sys/errno.h>

#define MAXCHAR 30
#define TRUE 1
#define FALSE 0

int ok_use_previous_files(char *fgroup_name)
{
   FILE         *fp;
   char         fname[180];
   char         line[MAXCHAR];
   int          len;
   double       files_date, mods_date;
   int          status;
   struct stat  buffer;
   
  /*----------------------------------------------------------------*/   
   memset(fname, '\0', 180);
   memset(line, '\0', MAXCHAR);
   
   strcpy(fname, (char *)getenv("HOME"));
   strcat(fname, "/.ifp_files/local/date_files_copied");
   
   /* Open the file if possible */
   if((fp = fopen(fname, "r")) == NULL)
      printf("Problem opening file: %s\n", fname);
      
   /* Process the 2 line file, if file exists */
   if (fp != NULL)
   {
      fgets(line, MAXCHAR, fp);
      status = fscanf(fp, "%lf", &files_date);
   }
   if ((fp == NULL) || (status == EOF))
   {
      strcpy(line, " ");
      files_date = 0.0;
   }
         
   if(fp != NULL) fclose(fp);
   
   /* Get the second date */
   memset(fname, '\0', 180);
   memset(line, '\0', MAXCHAR);
   
   strcpy(fname, (char *)getenv("HOME"));
   strcat(fname, "/.ifp_files/local/mods_sent_to_ofs");
   
   /* Open the file if possible */
   if( (fp = fopen(fname, "r")) == NULL)
      printf("Problem opening file: %s\n", fname);
      
   /* Process the 3 line file, if file exists */
   if (fp != NULL)
   {
      fgets(line, MAXCHAR, fp);
      fgets(line, MAXCHAR, fp);
      status = fscanf(fp, "%lf", &mods_date);
   }
   if ((fp == NULL) || (status == EOF))
   {
      strcpy(line, " ");
      mods_date = 0.0;
   }
         
   if(fp != NULL) fclose(fp);
   
   /* Decide which value to return */
   if (files_date > mods_date)
      return(TRUE);
   else
   {
      memset(fname, '\0', 180);
      strcpy(fname, (char *)getenv("HOME"));
      strcat(fname, "/.ifp_files/mods_from_ofs/FGroups/");
      strcat(fname, fgroup_name);
      
      /* see if the file exists */
      if(stat(fname, &buffer) != -1)
         return(TRUE);
      else
         return(FALSE);      
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/ok_use_previous_files.c,v $";
 static char rcs_id2[] = "$Id: ok_use_previous_files.c,v 1.3 2006/04/07 13:30:18 aivo Exp $";}
/*  ===================================================  */

}

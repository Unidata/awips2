/* Function to read the mods_sent_to_ofs file and fill the
 * label variable
 *
 * Written by D. Page - 10 Oct. 1995
 */

#include <stdio.h>

#define MAXCHAR 30

void get_mods_copied_date(char *label)
{
   FILE         *fp;
   char         fname[100];
   char         line[MAXCHAR];
   int          len;
   
  /*----------------------------------------------------------------*/   
   memset(fname, '\0', 100);
   memset(line, '\0', MAXCHAR);
   
   strcpy(fname, (char *)getenv("HOME"));
   strcat(fname, "/.ifp_files/local/mods_sent_to_ofs");
   
   /* Open the file if possible */
   if( (fp = fopen(fname, "r")) == NULL)
      printf("Problem opening file: %s\n", fname);
      
   /* start to fill the label variable */
   strcpy(label, "Mods last copied to OFS:  ");
  
   /* Process first line (first of 2 lines) if file exists */
   if(fp != NULL)
      fgets(line, MAXCHAR, fp);
   else
      strcpy(line, "  ");
      
   /* add line to label - use strncat so don't copy the new line character */
   len = strlen(line);
   strncat(label, line, len-1);
   

   /* Process second line (last line) if file exists */
   if(fp!= NULL)
      fgets(line, 8, fp);
   else
      strcpy(line, "  "); 
   
   /* add line to label - use strncat so don't copy the new line character */
   strcat(label, "  ");
   len = strlen(line);
   strncat(label, line, len-1);
      
   if( fp != NULL){
	fclose(fp);
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/get_mods_copied_date.c,v $";
 static char rcs_id2[] = "$Id: get_mods_copied_date.c,v 1.3 2006/04/07 13:29:59 aivo Exp $";}
/*  ===================================================  */

}
         
        
   
   

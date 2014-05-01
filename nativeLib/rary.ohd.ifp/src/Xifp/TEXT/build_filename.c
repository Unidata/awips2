/**************************************************************************/
/* FILE PATH/NAME:   (ifp_source_dir/Xifp/build_filename.c            */
/*  FUNCTION NAME:   build_filename                                       */
/*       FUNCTION:                                                        */
/**************************************************************************

Function type:
   char *

Called by function:
   initialize_data  (in read_write_data)

Local variables:
   path - stack deref character;
   file - stack deref character;
   filename - stack deref (array) character; dimensioned 80;

******************************************** BEGIN build_filename ********/

/* This routine takes two strings, combines them together, and returns a
   pointer the result.
   Written by D. Page - HRL - 21 Nov. 1992
*/

char *build_filename(char *path, char *file)
{
   char   *filename;
   int    length;

   length = strlen(path) + strlen(file) + 1;
   filename = (char *)malloc(length * sizeof(char));
   memset(filename, '\0', length);

   strcpy(filename, path);
   strcat(filename, file);

   return (filename);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/build_filename.c,v $";
 static char rcs_id2[] = "$Id: build_filename.c,v 1.1 1995/09/08 15:00:05 page Exp $";}
/*  ===================================================  */

}

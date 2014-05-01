#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int getfname(int *istatus, char file_name[20])
{

   FILE  *fnamefp;

   char  path_name[80];

   *istatus = 0;

   strcpy(path_name,getenv("HOME"));

   strcat(path_name,"/.ifp_files/STORE_FG_NAME");

   if( (fnamefp = fopen(path_name, "r")) == NULL)
   {
   	printf("File not found or error in open %s\n",path_name);
	*istatus = 0;
   }
   else
   {
	memset(file_name,'\0',20);
   	fscanf(fnamefp,"%s",file_name);
        /* printf("filename %s\n",file_name); */
   	fclose(fnamefp);
	*istatus = 1;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/getfname.c,v $";
 static char rcs_id2[] = "$Id: getfname.c,v 1.1 2000/03/16 12:10:58 page Exp $";}
/*  ===================================================  */

   return 0;

}

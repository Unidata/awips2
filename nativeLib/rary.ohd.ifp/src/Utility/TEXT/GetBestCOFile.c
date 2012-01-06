#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>

getbestcofile(segment_name, julian_day, internal_hour,
	       found_file, ireturn)

 char   segment_name[8], found_file[16];
 int    *julian_day, *internal_hour, *ireturn;
{
 char   system_input_string[80], segname_noblanks[9];
 char   co_files[30][50], file_wanted[50];
 char   input_file_name[50], date_wanted[30];
 int    i, num_co_files, julian_hour;
 int    difference_from_wanted_date[30];
 int    closest_date;
 int    new_julian_hour, new_julian_day, new_internal_hour;
 char   *loc;
 FILE   *input_file;

 memset(system_input_string, '\0', 80);
 memset(segname_noblanks, '\0', 9);
 for(i = 0; i < 30; i++)
     memset(co_files[i], '\0', 50);
 memset(file_wanted, '\0', 50);
 memset(date_wanted, '\0', 30);
 memset(found_file, ' ', 8);
 *ireturn = 0;

 closest_date = -1;

 i = 0;
 while(segment_name[i] != ' ' && i < 8)
    {
     segname_noblanks[i] = segment_name[i];
     i++;
    }

 julian_hour = (((*julian_day) - 1) * 24) + *internal_hour;

 memset(file_wanted, '\0', 50);
 sprintf(file_wanted, "%s_%d", segname_noblanks, julian_hour);

 strcpy(system_input_string, "ls /u/nwsrfs/RFC_data/carryover/");
 strcat(system_input_string, segname_noblanks);
 strcat(system_input_string, "_* > $HOME/.ifp_files/local/CO_out");

 system(system_input_string);

 strcpy(input_file_name, getenv("HOME"));
 strcat(input_file_name, "/.ifp_files/local/CO_out");
 input_file = fopen(input_file_name, "r");

 if(input_file == NULL)
   {
    printf("Problem getting best co file for segment %s\n",
	    segname_noblanks);
    return;
   }
 else
   {
    i = 0;
    while (fscanf(input_file, "%s", co_files[i++]) != EOF )
	  {
	   num_co_files = i;
	   if(num_co_files == 30)
	     {
	      printf("Number of carryover files too large in ");
	      printf("GetBestCOFile\n");
	      break;
	     }
	  }
    if(num_co_files < 1)
      {
       printf("Problem getting best co file for segment %s\n",
	       segname_noblanks);
       return;
      }
    if(num_co_files == 1)
      {
    /*
     * Only one date of carryover, see if it matches one wanted.
     */
       loc = strrchr(co_files[0], '/');
       if(loc == NULL)
	 {
	  printf("Problem getting best co file for segment %s\n",
		  segname_noblanks);
	  return;
	 }
       strcpy(found_file, ++loc);
      /*
       * If date of one carryover file does not match wanted one
       *  must post atom and set return code.
       */
       loc = strrchr(co_files[0], '_');
       new_julian_hour = atoi(++loc);
       if(new_julian_hour - julian_hour != 0)
	 {
	  new_julian_day = new_julian_hour/24 + 1;
	  new_internal_hour = new_julian_hour - (new_julian_day-1)*24;

	  set_start_date_atom(&new_julian_day, &new_internal_hour);

	  *julian_day = new_julian_day;
	  *internal_hour = new_internal_hour;
	  *ireturn = 1;
	 }
       return;
       }
    /*
     * More than one file with carryover.
     * See if any files match the date wanted exactly.
     */
    for(i = 0; i < num_co_files; i++)
       {
	loc = strrchr(co_files[i], '/');
	if(loc == NULL)
	  {
	   printf("Problem getting best co file for segment %s\n",
		   segname_noblanks);
	   return;
	  }
	if(strcmp(file_wanted, ++loc) == 0)
	  {
	   strcpy(found_file, file_wanted);
	   return;
	  }
       }
    /*
     * Didn't find an exact match if we get here.
     * Now look for the closest date after the requested one.
     */
    for(i = 0; i < num_co_files; i++)
       {
	loc = strrchr(co_files[i], '_');
	if(loc == NULL)
	  {
	   printf("Problem getting best co file for segment %s\n",
		   segname_noblanks);
	   return;
	  }
	difference_from_wanted_date[i] = atoi(++loc) - julian_hour;
       }
   /*
    * Return the closest date after wanted one.
    * If no dates after, return the closest one before.
    */
    for(i = 0; i < num_co_files; i++)
       {
	if(closest_date < 0)
		closest_date = i;
	else
	  {
	   if(difference_from_wanted_date[i] > 0 &&
	      difference_from_wanted_date[closest_date] < 0)
		closest_date = i;
	   if(closest_date != i)
	     {
	      if(difference_from_wanted_date[i] > 0)
		{
		 if(difference_from_wanted_date[closest_date] >
		    difference_from_wanted_date[i])
		      closest_date = i;
		}
	      else
		{
		 if(difference_from_wanted_date[closest_date] <
		    difference_from_wanted_date[i])
		      closest_date = i;
		}
	     }
	  }
       }
    if(closest_date > -1)
      {
       loc = strrchr(co_files[closest_date], '/');
       strcpy(found_file, ++loc);
       loc = strrchr(co_files[closest_date], '_');
       new_julian_hour = atoi(++loc);
       new_julian_day = new_julian_hour/24 + 1;
       new_internal_hour = new_julian_hour - (new_julian_day-1)*24;

       set_start_date_atom(&new_julian_day, &new_internal_hour);

       *julian_day = new_julian_day;
       *internal_hour = new_internal_hour;
       *ireturn = 1;
       return;
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/GetBestCOFile.c,v $";
 static char rcs_id2[] = "$Id: GetBestCOFile.c,v 1.1 1995/09/08 14:59:21 page Exp $";}
/*  ===================================================  */

}

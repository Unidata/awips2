/* ============================================================================
**
**  Varible names kept the same as those in efaze2 
**  in: ncstor -- number of carryover days
**  in: icday -- array of carryover dates in julian day
**  in: ichour -- array of hours of carryover day
**  in: segname -- name of current segment
**  in: nlstz -- number of hours difference between Local Standard and Z time
**  
** out: ierr -- error flag
**
** Written by Edwin Welles 5/15/98 HRL
**
** Modified by Scott Townsend RTi July 2003
**   Modified code to memcopy the contents of a strtok function call
**   to a character pointer instead of assigning the strtok memory location
**   to the character pointer. This was needed because on Linux free'ing the
**   strtok memory location cause a SEG Fault.
** ============================================================================
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "c_call_f/mdyh1.h"


#define  MAX_PATH  256

extern int  get_apps_defaults(char *, int *, char *, int *);
extern void getpid2(int*);
extern void gethostid2(long*);


void ecopy( int *ncstor, int *icday, int *ichour, char *segname, int *lsegname, int *nlstz,
int *ierr)
{

 char token[15], ofs_path[MAX_PATH],  esp_path[MAX_PATH];
 char esp_file[MAX_PATH], ofs_file[MAX_PATH];
 char  sys_command[MAX_PATH+MAX_PATH+10], *tzc_token, *seg_token;
 char  month[3], day[3],  hour[3], tzc[5], num_temp[3];
 char dot[] = ".", name_temp[] = "/TEMP.", blank[] = " ", cp_string[] = "cp ";
 int ofs_length, esp_length, token_length;
 int i, idsav, int_month, int_day, int_year, int_hour, int_tzc, exit_status=0;
 char procidstr[16], hostidstr[32];
 int procidnum;
 long hostidnum;

 union
  {
    int  int_utzc;
    char chr_utzc[4];
  } utzc;

 *ierr=0;
 
 /* Added by Hank Herr (2004-06-07).  Acquire the process id and host id
    as strings.  This function performs stuff that upnofi (util -- now)
    performs, and is somewhat redundant.*/
 getpid2(&procidnum);
 gethostid2(&hostidnum);
 sprintf(procidstr, "%5d", procidnum); 
 sprintf(hostidstr, "%10d", hostidnum);
 
 /* Fill in the spaces preceding the right justified number with 0's. */
 i = 0;
 while ((procidstr[i] == ' ') && (i < 5))
 {
     procidstr[i] = '0';
     i ++;
 }
 i = 0;
 while ((hostidstr[i] == ' ') && (i < 10))
 {
     hostidstr[i] = '0';
     i ++;
 }

 /* Changed by Hank Herr (2004-06-07).  The TEMP files are now in the
    /tmp directory. */
 strcpy(ofs_path, "/tmp");


 strcpy(token,"ens_files");

 token_length=10;
 get_apps_defaults(token, &token_length, esp_path, &esp_length);

/* must save 50 spaces for rest of path and filename */
 if(esp_length >= MAX_PATH-50) 
	{ printf("ERROR: The name returned for the esp_dir directory is too long.\n",
		   "ERROR: The maximum length is 206 characters.\n");
	*ierr=2;
	return;
	}
 else if(esp_length==0) 
	{ printf("ERROR: No esp_dir token found.\n");
	*ierr=2;
	return;
	}

/* cutoff any blanks from the end of segname  for later use in esp filename*/

/* 
 * Sat RTi, Sept 2003
 * Since segment names can contain any character must pass the character size from the
 * fortran routine (efaze2.f) calling this C funtion. This is necessary to allocate enough
 * memory space for the seg_token variable while not loading in garbage from the Fortran
 * program memory locations.
 */
 seg_token = (char *)malloc((*lsegname+1)*sizeof(char*));
 memcpy(seg_token,segname,(*lsegname)*sizeof(char*));
 seg_token[*lsegname] = '\0'; 

 /* create name of esp directory */
 strcat(esp_path,"/carryover/");

 /* make sure esp directory exists */
 if(access(esp_path,02) == 0 ) 
	{
 	/* loop through the carryover dates */
 	for(i=0; i<*ncstor; i++)
		{
 		/* convert julian day to m/d/y/h */
 		/* nlstz is already a pointer passed in from efaze2 */
	
 		idsav=0;
                MDYH1(&icday[i], &ichour[i], &int_month, &int_day, &int_year,
                &int_hour, nlstz, &idsav, &int_tzc);
	
 		/* convert ints to chars */
 		sprintf(month,"%2.2d",int_month);
 		sprintf(day,"%2.2d",int_day);
 		sprintf(hour,"%2.2d",int_hour);
                  utzc.int_utzc = int_tzc;
                  strncpy(tzc,utzc.chr_utzc,4);
                  tzc[4] = '\0';
		
 		/* cutoff any blanks from the end of tzc */
		/* Edited by RTi. Edited for the same reason as above although */
		/* no segmentation fault is happening during the free. */
 		tzc_token = (char *)malloc(5*sizeof(char));
 		memcpy(tzc_token,strtok(tzc," "),5*sizeof(char));

		/* create esp filename */
		strcpy(esp_file,seg_token);
		strcat(esp_file,dot);
 		strcat(esp_file,month);
		strcat(esp_file,dot);
 		strcat(esp_file,day);
		strcat(esp_file,dot);
 		strcat(esp_file,hour);
		strcat(esp_file,dot);
 		strcat(esp_file,tzc_token);
	
		/* make ofs file name */
		strcpy(ofs_file,name_temp);
		sprintf(num_temp,"%2d",70+i);
		strcat(ofs_file, num_temp); 
	
			
		/* and make sure it exists */
		if(access(ofs_path,04) !=0)

			{ printf("ERROR: Trouble accessing OFS files.\n");
			*ierr=3;
			break; 
			}
		/* ofs_file exists and is readable so do copy */
        /* Changed by Hank Herr (2004-06-07).  Uses sprintf instead
           of a sequence of strcpy and strcat. */
        sprintf(sys_command, "%s %s%s.%s.%s %s%s", cp_string, ofs_path, ofs_file,
            procidstr, hostidstr, esp_path, esp_file);
        printf("####>> Executing command, \"%s\".\n", sys_command);       

/*		strcpy(sys_command,cp_string);
		strcat(sys_command, blank);
		strcat(sys_command, ofs_path);
		strcat(sys_command, ofs_file);
		strcat(sys_command, blank);
		strcat(sys_command, esp_path);
		strcat(sys_command, esp_file);
*/

		*ierr = system(sys_command);
		if(*ierr != 0)
			{ printf("ERROR: Trouble copying files with cp command.\n");
			*ierr=4;
			break;
			}
		

		/* end ncstor for loop */
		}

        free(tzc_token);
	}
 else 
	{ printf("ERROR: No access to esp carryover directory.\n");
	*ierr=5;
		}

  free(seg_token);
  return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ecopy.c,v $";
 static char rcs_id2[] = "$Id: ecopy.c,v 1.8 2006/04/20 14:35:52 xfan Exp $";}
/*  ===================================================  */

}

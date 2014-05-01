/************************************************************************
   load_offices.c
      
   PURPOSE   
   Reads the different candidate office information and related functions.
    
   *********************************************************************/
#ifdef  __HPUX__
#define _INCLUDE_POSIX_SOURCE
#endif
 
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include "load_offices.h"

extern char paramdir[];

/************************************************************************
   load_offices()
      
   PURPOSE   
   Reads the different candidate office information and 
   loads them into the program for permanent use.
    
   *********************************************************************/

void load_offices(misc_struct *misc)
{
   UniqueList   *ul_hsaHead = NULL, *ul_fpHead = NULL, *ulPtr = NULL;
   int		cnt;
   char		where[120];
   int		office_ok;
   int		office_cnt;
   int 		num_tpl_files;
   int 		num_default_pcc_files; 
   int 		num_pcc_files;
   char		check_office[HYD_SERV_LEN + 1];
   

   /* read the list of unique offices from the database view. */
   
   strcpy(where, " ");
   ul_hsaHead = LoadUnique("hsa", "FpInfo", where, &cnt);
   
   if (cnt == 0) log_msg(NO_FPS, "");
   

   /* loop on the number of offices and check that 
      there are pcc and template files for the office */
      
   office_cnt = 0;
   if (ul_hsaHead != NULL)
	ulPtr = (UniqueList *) ListFirst(&ul_hsaHead->list);
   
   while (ulPtr)
   {
      /* trim the blank padded uchar string */
      
      memset(check_office, 0, HYD_SERV_LEN + 1);
      strncpy(check_office, ulPtr->uchar, HYD_SERV_LEN);
      
      office_ok = check_office_files(check_office,
				     &num_tpl_files,
				     &num_default_pcc_files, 
				     &num_pcc_files);
      
      if (office_ok)
      {
	 strcpy(misc->offices[office_cnt].id, check_office);
	 
	 sprintf(where, "Identified office %s as candidate office...",
		 check_office);
	 log_msg("", where);
	 
	 misc->offices[office_cnt].num_default_pcc = num_default_pcc_files;
	 misc->offices[office_cnt].num_total_pcc   = num_pcc_files;

	 
	 /* load in the number of forecast points for general
	    info purposes */
	 
	 sprintf(where, " WHERE hsa = '%s' ", check_office);
	 ul_fpHead = LoadUnique("lid", "FpInfo", where, &cnt);
	 
	 FreeUnique(ul_fpHead);
	 
	 misc->offices[office_cnt].num_fps = cnt;
	 
	 office_cnt++;
      }
      
      
      /* if not all files there, issue warning message */
      
      else
      {
	 log_msg(INCOMPLETE_OFFICE_FILES, ulPtr->uchar);
      }
      
      
      /* skip to next item */
      
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);
   }
   
   
   misc->num_offices = office_cnt;
 
   
   FreeUnique(ul_hsaHead);
   
   
  return;
}


/************************************************************************
   check_office_files.c
   
   
   PURPOSE 
   Look for the necessary files for the given office. 
    
   *********************************************************************/

int check_office_files(char		*check_office,
		       int		*num_tpl_files,
		       int		*num_default_pcc_files,
		       int		*num_total_pcc_files)
{
   DIR 			*dirp;
   struct dirent 	*dp;
   int 	tpl_file_cnt;
   int 	pcc_file_cnt;
   int  default_pcc_file_cnt;
   char	*pcc_loc     = NULL;
   char *tpl_loc     = NULL;
   char *lastdot_loc = NULL;
   int	suffix_len = 0;
   char	office_suffix[HYD_SERV_LEN + 1];
   int	status;
   
   
   /* open the directory containing the pcc and template files;
      if an error occurs, the log_msg aborts the program */
   
   dirp = opendir(paramdir);
   if (dirp == NULL)
      log_msg(PARAMFILEACCESS_ERR, paramdir);
   
   
   /* initialize */
   
   tpl_file_cnt = 0;
   pcc_file_cnt = 0;
   default_pcc_file_cnt = 0;
   

   /* loop on the entries in the directory */
   /* only consider *.pcc.* and *.tpl.* files */
   
   while ((dp = readdir(dirp)) != NULL)
   {      
      /* only consider *.pcc.* and *.tpl.* files */
      
      pcc_loc = strstr(dp->d_name, ".pcc.");
      tpl_loc = strstr(dp->d_name, ".tpl.");
      
      if (pcc_loc != NULL || tpl_loc != NULL)
      {
	 lastdot_loc = strrchr(dp->d_name, '.');
	 suffix_len = strlen(lastdot_loc) - 1;
      }
      
      
      /* process pcc files */
      
      if (pcc_loc != NULL)
      {
	 /* check that the suffix is defined */
	 	 
	 if (suffix_len > 0 && suffix_len <= HYD_SERV_LEN)
	 {	    
	    /* check if the office matches.  the files can be
	       any case, but the selected office must be uppercase. */
	    
	    strcpy(office_suffix, lastdot_loc + 1);
	    convert_str_to_upcase(office_suffix);

	    if (strcmp(check_office, office_suffix) == 0)
	    {
	       pcc_file_cnt++;
	       
	       
	       /* check if the special default pcc files are defined */
	       
	       if (strstr(dp->d_name, RVS_PCCFILE) != NULL)
		  default_pcc_file_cnt++;
	       else if (strstr(dp->d_name, FLS_PCCFILE) != NULL)
		  default_pcc_file_cnt++;
	       else if (strstr(dp->d_name, FLW_PCCFILE) != NULL)
		  default_pcc_file_cnt++;
	    }
	 }	 	 
      }
      
      else if (tpl_loc != NULL)
      {
	 /* check that the suffix is defined */
	 
	 if (suffix_len > 0 && suffix_len <= HYD_SERV_LEN)
	 {	    
	    /* check if the office matches */
	    
	    strcpy(office_suffix, lastdot_loc + 1);
	    convert_str_to_upcase(office_suffix);
	    
	    if (strcmp(check_office, office_suffix) == 0)
	    {	
	       
	       /* check each of the required template files */
	       
	       if (strstr(dp->d_name, HEADER_TEMPLATEFILE)          != NULL)
		  tpl_file_cnt++;
	       else if (strstr(dp->d_name, SUMMARY_TEMPLATEFILE)    != NULL)
		  tpl_file_cnt++;
	       else if (strstr(dp->d_name, HEADLINE_TEMPLATEFILE)   != NULL)
		  tpl_file_cnt++;  		  
	       else if (strstr(dp->d_name, BASIS_TEMPLATEFILE)      != NULL)
		  tpl_file_cnt++;
	       else if (strstr(dp->d_name, TABULAR_TEMPLATEFILE)    != NULL)
		  tpl_file_cnt++;
	       else if (strstr(dp->d_name, ROUNDUP_TEMPLATEFILE)    != NULL)
		  tpl_file_cnt++;
	       else if (strstr(dp->d_name, COMPARISON_TEMPLATEFILE) != NULL)
		  tpl_file_cnt++;
	       else if (strstr(dp->d_name, IMPACT_TEMPLATEFILE)     != NULL)
		  tpl_file_cnt++;
	       else if (strstr(dp->d_name, CTA_TEMPLATEFILE)        != NULL)
		  tpl_file_cnt++;	       
	    }
	 }
      }  /* end of loop on templare files */
   }     /* end of while loop on reading more files */
  
   
   /* check that the necessary files are indeed there */
   
   if (default_pcc_file_cnt >= NUM_DEFAULT_PCC_FILES &&
       tpl_file_cnt         >= NUM_TEMPLATE_FILES)
      status = 1;
   else
      status = 0;
   
   
   /* load the returned variables */
   
   *num_tpl_files         = tpl_file_cnt;
   *num_default_pcc_files = default_pcc_file_cnt;
   *num_total_pcc_files   = pcc_file_cnt;
      
   
   /* close the directory */
   
   closedir(dirp);
   
   
   /* return with the status */
   
   return(status);
}
	 

#include <string.h>

#include "rpf_protos.h"           /* rpf program prototypes */

/*********************************************************************
   get_misc() 
   
   PURPOSE
   Collects various control settings and stores them in the 
   miscellaneous structure.  Some of these settings are in the 
   relational database.  The initial time is also defined here.
      

   ********************************************************************/

void get_misc(misc_struct *misc)
{  
   RpfParams 	*rpfPtr;
   Admin 	*adminPtr;
   char		log_str[100];


   /* get the RiverPro parameteric data */
   
   rpfPtr = GetRpfParams(" ");
   
   if (rpfPtr != NULL)
   {            
      strcpy(misc->miss_val, rpfPtr->missval);
      strcpy(misc->miss_cat, rpfPtr->misscat);
      strcpy(misc->miss_tim, rpfPtr->misstim);
      
      misc->rvs_expirehrs = rpfPtr->rvsexphrs;
      misc->fls_expirehrs = rpfPtr->flsexphrs;
      misc->flw_expirehrs = rpfPtr->flwexphrs;
            
      FreeRpfParams(rpfPtr);     
   }
   
   else
      log_msg(NO_RPFPARAMS, "");
   
   
   /* load in the hsa from the Admin table. the admin table hsa field 
      length (5) is larger than the hsa defined for each location (3) */
   
   adminPtr = GetAdmin("");
   if (adminPtr != NULL)
   {
      if (strlen(adminPtr->hsa) <= HYD_SERV_LEN)
	 strcpy(misc->hsa, adminPtr->hsa);
      else
      {
	 memset(misc->hsa, 0, HYD_SERV_LEN + 1);
	 strncpy(misc->hsa, adminPtr->hsa, HYD_SERV_LEN);
      }
      
      FreeAdmin(adminPtr);
   }
   
   else
      log_msg(NO_ADMIN, "");
   
   
   /* get the system time */
   
   time(&misc->system_time);
   
   
   /* must initialize startup_pcc_file */
   
   memset(misc->startup_pcc_file, 0, MAXLEN_FILENAME);
   
   
   /* also initialize the issuance number and UGC expire time info */
   
   misc->issnum = 0;
   
   misc->expire_set = MISSINGVAL;
   
   
   /* get and store the current TZ setting */
	 
   memset(misc->defaultTZ, 0, TZ_LEN + 1);
   strncpy(misc->defaultTZ, getenv("TZ"), TZ_LEN);
	 
   sprintf(log_str, "Default timezone TZ='%s'", misc->defaultTZ);
   log_msg("", log_str);
   
   
   /* initilize cor_prevprod struct for correction info */
   
   memset(misc->cor_prevprod.prodid,    0, PRODUCT_LEN + 1);
   memset(misc->cor_prevprod.prodtype,  0, BOOL_LEN + 1);
   memset(misc->cor_prevprod.prodcateg, 0, PROD_CATEG_LEN + 1);
   memset(misc->cor_prevprod.prod_ansi_time, 0, ANSI_YEARSEC_TIME_LEN + 1);
   memset(misc->cor_prevprod.post_ansi_time, 0, ANSI_YEARSEC_TIME_LEN + 1);
   
   misc->cor_prevprod.corevent_cnt = 0;
   misc->cor_prevprod.event_cnt    = 0;
   misc->cor_prevprod.cor_flag       = FALSE;
   misc->cor_prevprod.issue_cor_flag = FALSE;
   
   strcpy(misc->cor_prevprod.ccx_str, "");  
   
   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

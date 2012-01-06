/**********************************************************************/

/* Routine to get the value of an apps_default that is an integer.

*/

#include <stdlib.h>

void get_apps_intval (char *apps_var, int *iapps_dflt, int iapps_req, 
                      int *iapps_val, int iprint, int ldebug, int *istat)
{

int  i;
int  lapps_var, lapps_val;
int  ifound, iretrn, ierror;
char capps_val[25], capps_valo[25];
char strng;

  ldebug=0;

   if (ldebug > 1) printf ("enter get_apps_intval\n");
 
   *istat=0;
   
   ifound=0;

/* set value to default */
   *iapps_val=*iapps_dflt;
   
/* check if apps_default specified */
   lapps_var=strlen(apps_var);
   if (get_apps_defaults(apps_var,&lapps_var,capps_val,&lapps_val)) {
      if (iapps_req == 1) {
         printf ("ERROR: apps_default %s not specified.\n",apps_var);
         *istat=1;
	 }
	 else {
	    if (iprint == 1)
               printf ("NOTE: apps_default %s not specified. The default (%i) will be used.\n",
                       apps_var,*iapps_dflt);
            }
      }
      else {
         ifound=1;
      /* check for valid value */
         if (ldebug > 1) printf ("capps_val is %s\n",capps_val);
	 strcpy (capps_valo," ");
         if (strcmp(capps_val,"yes") == 0 ||
             strcmp(capps_val,"YES") == 0 ||
             strcmp(capps_val,"on") == 0 ||
             strcmp(capps_val,"ON") == 0) {
	    strcpy (capps_valo,capps_val);
            strcpy (capps_val,"1");
	    lapps_val=strlen(capps_val);
	    }
         if (strcmp(capps_val,"no") == 0 ||
             strcmp(capps_val,"NO") == 0 ||
             strcmp(capps_val,"off") == 0 ||
             strcmp(capps_val,"OFF") == 0) {
	     strcpy (capps_valo,capps_val);
            strcpy (capps_val,"0"); 
	    lapps_val=strlen(capps_val);
	    }
         ierror=0;
         strcpy (&strng," ");
         for (i=0; i<lapps_val; i++) {
            strncpy (&strng,&capps_val[i],1);
            iretrn=isdigit(strng);
            if (iretrn != 0 || strng == '-')
               continue;
            printf ("ERROR: in get_apps_intval - the character '%s' ",&strng);
            printf ("found at position %i ",i+1);
            printf ("of '%s' is invalid for apps_default %s.",capps_val,apps_var);
            printf ("\n");
            ierror=1;
            }
         if (ierror == 0) {
         /* set value */
            *iapps_val=atoi(capps_val);
            if (ldebug > 1) printf ("iapps_val is %i\n",*iapps_val);
            if (iprint == 1) {
	       if (strcmp(capps_valo," ") == 0)
	          printf ("NOTE: apps_default %s is %i.\n",apps_var,*iapps_val);
		  else
	             printf ("NOTE: apps_default %s is %i (specified as '%s').\n",
		             apps_var,*iapps_val,capps_valo);
	       }
            }
         }

   if ((ldebug > 0) || (ldebug == -1 && ifound == 1))
       printf ("in get_apps_intval - %s=%i\n",apps_var,*iapps_val);
   
   return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/get_apps_intval.c,v $";
 static char rcs_id2[] = "$Id: get_apps_intval.c,v 1.5 2002/02/20 10:21:14 michaelo Exp $";}
/*  ===================================================  */

}

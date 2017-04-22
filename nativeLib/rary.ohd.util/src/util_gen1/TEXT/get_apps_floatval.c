/**********************************************************************/

/* Routine to get the real value of an apps_default.

*/

#include <stdlib.h>

void get_apps_floatval (char *apps_var, float *fapps_dflt, int iapps_req, 
                        float *fapps_val, int iprint, int ldebug, int *istat) {

int  i;
int  lapps_var, lapps_val;
int  iretrn, ierror;
char capps_val[25];
char strng;
 

   if (ldebug > 1) printf ("enter get_apps_floatval\n");

   *istat=0;

/* set value to default */
   *fapps_val=*fapps_dflt;
   
/* check if apps_default specified */
   lapps_var=strlen(apps_var);
   if (get_apps_defaults(apps_var,&lapps_var,capps_val,&lapps_val)) {
      if (iapps_req == 1) {
         printf ("ERROR: apps_default %s not specified.\n",apps_var);
         *istat=1;
         }
      }
      else {
      /* check for valid value */
         if (ldebug > 1) printf ("capps_val=%s\n",capps_val);
         ierror=0;
         strng=' ';
         for (i=0; i<lapps_val; i++) {
            strncpy (&strng,&capps_val[i],1);
            iretrn=isdigit(strng);
            if (iretrn != 0 || strng == '.' || strng == '-') 
               continue;
            printf ("ERROR: in get_apps_floatval - the character '%s' ",&strng);
            printf ("found at position %i ",i+1);
            printf ("of '%s' is invalid for apps_default %s.",
                    capps_val,apps_var);
            printf ("\n");
            ierror=1;
            }
         if (ierror == 0) {
         /* set value of apps_default */
	    *fapps_val=atof(capps_val);
            if (ldebug > 1) printf ("fapps_val=%f\n",*fapps_val);
            if (iprint == 1) 
               printf ("NOTE: apps_default %s is %.2f.\n",apps_var,*fapps_val);
            }
         }

   if (ldebug > 0) printf ("in get_apps_floatval - %s=%f\n",apps_var,*fapps_val);
   
   return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/get_apps_floatval.c,v $";
 static char rcs_id2[] = "$Id: get_apps_floatval.c,v 1.3 2001/06/13 14:56:09 dws Exp $";}
/*  ===================================================  */

}

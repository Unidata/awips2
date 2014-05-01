/**********************************************************************/

/* Routine to get the character value of an apps_default.

*/

void get_apps_charval (char *apps_var, char *capps_dflt, int iapps_req, 
                       char *capps_val, int iprint, int ldebug, int *istat) {

int lapps_var, lapps_val;
 

   if (ldebug > 1) printf ("enter get_apps_charval apps_var=%s capps_dflt=%s\n",apps_var ,capps_dflt);

   *istat=0;
   
/* check if apps_default specified */
   lapps_var=strlen(apps_var);
   if (get_apps_defaults(apps_var,&lapps_var,capps_val,&lapps_val)) {
      if (iapps_req == 1) {
         printf ("ERROR: apps_default %s not specified.\n",apps_var);
         *istat=1;
         }
   /* set value to default */
      strcpy (capps_val,capps_dflt);
      if (ldebug > 1)
         printf ("capps_dflt=%s capps_val=%s\n",capps_dflt,capps_val);
      }
      else
      if (iprint == 1) 
         printf ("NOTE: apps_default %s is %s.\n",apps_var,capps_val);
      
   if (ldebug > 0) printf ("in get_apps_charval - %s=%s\n",apps_var,capps_val);
   
   return;
   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/get_apps_charval.c,v $";
 static char rcs_id2[] = "$Id: get_apps_charval.c,v 1.4 2002/02/20 10:20:50 michaelo Exp $";}
/*  ===================================================  */

}

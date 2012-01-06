/* Routine to get the database version from token db_name.
*  Logic: get upto 4 characters from db_name where the last one must be a
* number for db_version.  If no number found, set db_version to be empty.
*/

#include <stdlib.h>

void get_db_version (char *apps_var, int iapps_req, char *db_name,
                     char *db_version, int ldb_version,
                     int iprint,int ldebug, int *istat) {

	int  i, iofset;

   if (ldebug > 1) printf ("enter get_db_version\n");
    iprint=1;

   *istat=0;

	/* find last number in db_name */
	for (i=strlen(db_name)-1;i>=0;i--){
		if (db_name[i] >= '0' && db_name[i] <= '9')
		break;
	}

	/* get db_version from db_name */
	i++;
	if (i<1) /*no number found*/
		strcpy(db_version,"");
	else if(i < (ldb_version)) {
		strncpy (db_version,db_name,i);
		db_version[i]=0;
	}
	else {
		iofset=i-ldb_version+1;
		strncpy (db_version,&db_name[iofset],ldb_version-1);
		db_version[ldb_version-1]=0;
	}

	if (strcmp(db_version,"") == 0 || strcmp(db_version," ") == 0) {
		printf ("WARNING in get_db_version: cannot apps_default get %s from %s.\n",
			apps_var,db_name);
		*istat=1;
	}
	else {
		if (iprint == 1) 
			printf ("NOTE: %s (%s) obtained from db_name (%s).\n", apps_var,db_version,db_name);
	}

	if (ldebug > 0) printf ("in get_dbversion - %s=%s\n",apps_var,db_version);

	if (ldebug > 1) printf ("exit get_db_version\n");
   
	return;
   
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/get_db_version.c,v $";
 static char rcs_id2[] = "$Id: get_db_version.c,v 1.6 2005/03/17 16:59:41 wkwock Exp $";}
/*  ===================================================  */

}

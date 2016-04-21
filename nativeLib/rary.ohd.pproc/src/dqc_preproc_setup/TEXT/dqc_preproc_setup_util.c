#include "dqc_preproc_setup.h"

const char * dqc_preproc_setup_MPE_STATION_LIST_DIR_TOKEN	= "mpe_station_list_dir";
const char * MPE_CLIMO_DIR_TOKEN		= "mpe_climo_dir";
const char * dqc_preproc_setup_MPE_POINT_PRECIP_DIR_TOKEN = "mpe_point_precip_dir";
const char * dqc_preproc_setup_MPE_SITE_ID_TOKEN			= "mpe_site_id" ;
const char * MPE_PRISM_DIR_TOKEN		= "mpe_prism_dir" ;

/**********************************************************************
 * getAppsDefaults ( )
 *
 * This function loads token value.
 * If token is not available, return -1; otherwise return 0.
 *********************************************************************/

#if 0

int getAppsDefaults(const char* strToken, char* strTokenValue)
{
    int tokenLen, tokenValueLen;

    tokenLen=strlen(strToken);
    get_apps_defaults( ( char *) strToken, &tokenLen,
                        strTokenValue, &tokenValueLen);

    if (tokenValueLen == 0)
        return -1;

    return 0;
}

#endif

/**********************************************************************
 * loadAppsDefaults ( )
 *
 * This function loads required token value.
 *********************************************************************/

void loadDqcAppsDefaults()
{
	int status = 0;

	status = getAppsDefaults ( dqc_preproc_setup_MPE_STATION_LIST_DIR_TOKEN, dqc_preproc_setup_mpe_station_list_dir );
	if ( status == -1 )
	{
		fprintf ( stderr, "Token %s is not defined.	Program exit.\n",
				dqc_preproc_setup_MPE_STATION_LIST_DIR_TOKEN );
		exit(-1);
	}

	status = getAppsDefaults ( MPE_CLIMO_DIR_TOKEN, mpe_climo_dir );
	if ( status == -1 )
	{
		fprintf ( stderr, "Token %s is not defined.	Program exit.\n",
				MPE_CLIMO_DIR_TOKEN );
		exit(-1);
	}

	status = getAppsDefaults(MPE_PRISM_DIR_TOKEN, mpe_prism_dir);
	if ( status == -1 )
	{
		fprintf ( stderr, "Token %s is not defined.	Program exit.\n",
				MPE_PRISM_DIR_TOKEN );
		exit(-1);
	}

	status = getAppsDefaults(dqc_preproc_setup_MPE_SITE_ID_TOKEN, dqc_preproc_setup_mpe_site_id);
	if ( status == -1 )
	{
		fprintf ( stderr, "Token %s is not defined.	Program exit.\n",
				dqc_preproc_setup_MPE_SITE_ID_TOKEN );
		exit(-1);
	}

	return ;
}


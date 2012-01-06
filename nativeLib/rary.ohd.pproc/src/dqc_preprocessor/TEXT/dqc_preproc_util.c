
/**********************************************************************
 * getAppsDefaults ( )
 * 
 * This function loads token value.
 * If token is not available, return -1; otherwise return 0.
 *********************************************************************/
#include "dqc_preproc.h"

const char * MPE_STATION_LIST_DIR_TOKEN      = "mpe_station_list_dir";
const char * MPE_POINT_PRECIP_DIR_TOKEN      = "mpe_point_precip_dir"; 
const char * MPE_SITE_ID_TOKEN               = "mpe_site_id" ;
const char * MPE_AREA_NAMES_TOKEN            = "mpe_area_names" ;
const char * DB_NAME_TOKEN                   = "db_name"; 
const char * MPE_POINT_TEMPERATURE_DIR_TOKEN = "mpe_point_temperature_dir"; 

int dqc_preprocessor_getAppsDefaults(const char* strToken, char* strTokenValue)
{
    int tokenLen, tokenValueLen;

    tokenLen=strlen(strToken);
    get_apps_defaults( ( char *) strToken, &tokenLen,
                        strTokenValue, &tokenValueLen);

    if (tokenValueLen == 0)
        return -1;

    return 0;
}


/**********************************************************************
 * loadAppsDefaults ( )
 * 
 * This function loads required token value.
 *********************************************************************/

void loadAppsDefaults()
{
    int status = 0;

    status = dqc_preprocessor_getAppsDefaults(DB_NAME_TOKEN, db_name);
    if ( status == -1 )
    {
        printf("Token \"%s\" is not defined. Program exit.\n",
                DB_NAME_TOKEN );
        exit(-1);
    }

    status = dqc_preprocessor_getAppsDefaults ( MPE_STATION_LIST_DIR_TOKEN, mpe_station_list_dir );
    if ( status == -1 )
    {
        printf( "Token \"%s\" is not defined. Program exit.\n",
                MPE_STATION_LIST_DIR_TOKEN );
        exit(-1);
    }

    status = dqc_preprocessor_getAppsDefaults ( MPE_POINT_TEMPERATURE_DIR_TOKEN, mpe_point_temperature_dir );
    if ( status == -1 )
    {
        printf( "Token \"%s\" is not defined. Program exit.\n",
                MPE_POINT_TEMPERATURE_DIR_TOKEN );
        exit(-1);
    }

    status = dqc_preprocessor_getAppsDefaults(MPE_POINT_PRECIP_DIR_TOKEN, mpe_point_precip_dir);
    if ( status == -1 )
    {
        printf( "Token \"%s\" is not defined. Program exit.\n",
                MPE_POINT_PRECIP_DIR_TOKEN );
        exit(-1);
    }

    status = dqc_preprocessor_getAppsDefaults(MPE_SITE_ID_TOKEN, mpe_site_id);
    if ( status == -1 )
    {
        printf( "Token \"%s\" is not defined. Program exit.\n",
                MPE_SITE_ID_TOKEN );
        exit(-1);
    }

    status = dqc_preprocessor_getAppsDefaults(MPE_AREA_NAMES_TOKEN, mpe_area_names);
    if ( status == -1 )
    {
        printf( "Token \"%s\" is not defined. Program exit.\n",
                MPE_AREA_NAMES_TOKEN );
        exit(-1);
    }

    return ;
}


char** stringTokenize(char* strValue, char delims[], int * listNum)
{
    char *result = NULL;
    char ** resultList = NULL;
    int index = 0;

    resultList = (char **)malloc(MAX_TOKEN_NUMBER * sizeof(char *)); 
    if(resultList == NULL)
    {
        printf( "ERROR: memory allocation failure"
            " in stringTokenize function."
            "\n\tProgram exit.\n") ;
        exit(-1) ;
    }

    result = strtok( strValue, delims );
    while( result != NULL )
    {
        resultList[index] = (char *)malloc( strlen(result) * sizeof(char )); 
        if(resultList[index] == NULL)
        {
            printf( "ERROR: memory allocation failure"
                " in stringTokenize function."
                "\n\tProgram exit.\n") ;
            exit(-1) ;
        }

        memset(resultList[index], '\0', strlen(result));
        strcpy(resultList[index], result);
        index ++;

        if(index > MAX_TOKEN_NUMBER)
        {
            printf( "ERROR: Token number great than %d."
                "\n\tProgram exit.\n", MAX_TOKEN_NUMBER) ;
            exit(-1) ;            
        }

        result = strtok( NULL, delims );
    }

    *listNum = index;

    return resultList ;

}

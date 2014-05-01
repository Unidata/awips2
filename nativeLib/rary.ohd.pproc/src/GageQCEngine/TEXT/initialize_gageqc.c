/*******************************************************************************
* FILENAME:            initialize_gageqc.c
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       March 3, 2006
* ORGANIZATION:        OHD HSEB
* MACHINE:             Linux Workstation
* MODIFICATION HISTORY:
*     DATE         PROGRAMMER        DESCRIPTION/REASON
*     3/6/2006     Bryon Lawrence    Original Coding     
********************************************************************************
*/

#include <limits.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "GeneralUtil.h"
#include "mpe_log_utils.h"

/*******************************************************************************
* MODULE NAME:         initialize_gageqc
* PURPOSE:             Initialize variables needed by MPE Editor Gage QC
*                      tools.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
Widget awidget [ MAX_GAGEQC_DAYS ];
Widget bwidget [ MAX_GAGEQC_DAYS ];
Widget gpbutton;
static double rmesh = 0;

/* Function which associates the Gage QC edit levels with a value. */
int funct[]={8,0,6,2,3,4,5,1,7,9};

static void hrapsize(const double flat)
{
    const double pi = 3.141592654 ;
    const double dtorad = pi / 180.0 ;

    rmesh = 4.7625 * (1.0 + sin(flat * dtorad))
             / (1.0 + sin(60.0 * dtorad)) ;
    return ;
}

void initialize_gageqc ( )
{
   static char message [ GAGEQC_MESSAGE_LEN ];
   static const char mpe_type_source_token [ ] = "mpe_type_source";
   char * pColon = NULL;
   char * pToken = NULL;
   char type_sources [ TOKEN_VALUE_LEN ];
   int string_len;
   int reply_len;
   extern int tsmax;
   extern struct ts ts[];
   point hrap_point;
   struct HRAP hrap_lat_lon;
   extern int init_maxmin;
   extern struct maxmin *maxmin;

   /* Initialize the maximum number of available typesources. */
   tsmax = 0;

   /* Set up the fonts. */

   /* Set up the typesources. */ 
   string_len = strlen ( mpe_type_source_token );


   get_apps_defaults ( ( char * ) mpe_type_source_token,
                       & string_len,
                       type_sources,
                       & reply_len );

   if ( reply_len <= 0 )
   {
      sprintf ( message, "%s token not defined.", mpe_type_source_token );
      flogMessage ( stdout, "%s\n", message );
      logMessage ( message );
   }
   else
   {
     pToken = strtok ( type_sources, "," );

     while ( pToken != NULL )
     {
        pColon = strchr ( pToken, ':' );

        if ( pColon != NULL )
        {
           strncpy ( ts[tsmax].abr, pToken, ( pColon - pToken ) );
           strcpy ( ts[tsmax].name, pColon + 1);
           ++ tsmax;
        }

        pToken = strtok ( NULL, "," );
     }

     if ( tsmax == 0 )
     {
        sprintf ( message, "Could not parse any typesources from %s token.",
                           mpe_type_source_token ); 
        flogMessage ( stdout, "%s\n", message );
        logMessage ( message );
     }
   }

   /* Initialize the factors used to compute distances on the map. */
   /* Determine the maximum latitude, minimum latitude and center
      longitude of the MPE forecast area. */
   
   /* Southwest corner. */
   hrap_point.x = XOR;
   hrap_point.y = YOR + ( MAXY / 2 );
   hrap_lat_lon = HrapToLatLongMpe ( hrap_point );

   hrapsize ( hrap_lat_lon.x );
 
   return;
}

double get_rmesh ( )
{
   return rmesh;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

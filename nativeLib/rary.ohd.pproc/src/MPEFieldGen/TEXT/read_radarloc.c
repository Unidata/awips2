#include "DbmsDefs.h"
#include "RadarLoc.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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
#include "mpe_fieldgen.h"

void MPEFieldGen_readRadarLoc ( radarLoc_table_struct * pRadarLocTable )
{
    static const char where [ ] = "WHERE use_radar = 'T' ORDER BY radid";
    int counter = 0;
    int radarSize = RADAR_NUMBER ;

    radarLoc_record_struct *returnPtr;

    RadarLoc * pRadarLocHead = NULL;
    RadarLoc * pRadarLocNode = NULL;

    /* Retrieve the data from the RadarLoc IHFS table. */
    pRadarLocHead = GetRadarLoc ( where );

    if ( pRadarLocHead != NULL )
    {
        pRadarLocNode = ( RadarLoc *) ListFirst ( & pRadarLocHead->list ) ;

        while ( pRadarLocNode != NULL )
        {
            /**
              * dynamically allocate memory
              * when the radar size exceeds the current limit.
              **/
            if ( counter >= radarSize )
            {
                radarSize += RADAR_NUMBER ;
                returnPtr = (radarLoc_record_struct *)realloc(pRadarLocTable->ptrRadarLocRecords,
                (size_t)(radarSize * sizeof(radarLoc_record_struct)));

                if(pRadarLocTable->ptrRadarLocRecords == NULL)
                {
                    sprintf ( message , "ERROR: Memory allocation failure"
                        " in getRadarLoc function.\n"
                        "\tProgram exit");
                    shutDownMPE( message, logFile );
                }
            }

            strncpy ( pRadarLocTable->ptrRadarLocRecords[counter].radarID,
                pRadarLocNode->radid, RADAR_ID_LEN );
            pRadarLocTable->ptrRadarLocRecords[counter].radarID[RADAR_ID_LEN]
                 = '\0' ;
            pRadarLocTable->ptrRadarLocRecords[counter].latitude
                 = (double)pRadarLocNode->lat;
            pRadarLocTable->ptrRadarLocRecords[counter].longitude
                 = (double)pRadarLocNode->lon;
            pRadarLocTable->ptrRadarLocRecords[counter].elevation
                 = ((double)pRadarLocNode->elev +
                    (double)pRadarLocNode->tower_ht) * 0.3048;
            memset ( pRadarLocTable->ptrRadarLocRecords[counter].officeID, '\0', RFC_LEN + 1 );
            strncpy ( pRadarLocTable->ptrRadarLocRecords[counter].officeID, pRadarLocNode->office_id, RFC_LEN );

            ++ counter;

            pRadarLocNode = ( RadarLoc * ) ListNext ( & pRadarLocNode->node );
        }

        pRadarLocNode = NULL;
        FreeRadarLoc ( pRadarLocHead );
        pRadarLocHead = NULL;
    } /* if ( pRadarLocHead != NULL ) */

    pRadarLocTable->radarNum = counter;

} /* end MPEFieldGen_readRadarLoc */

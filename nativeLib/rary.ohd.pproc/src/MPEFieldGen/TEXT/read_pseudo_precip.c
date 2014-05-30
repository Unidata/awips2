/*******************************************************************************
* FILENAME:             read_pseudo_precip.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*   MODULE 1:           readPseudoPrecip
* DESCRIPTION:          For a given hour, this routine retrieves the 
*                       pseudogage data from the PseudoGageVal table.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        September 10, 2004
* ORGANIZATION:         OHD-11 HSEB
* MACHINE:              Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        09/10/2004    Bryon Lawrence    Original Coding
*          1        01/31/2005    Guoxian Zhou      Using struct data type
*          1        03/21/2005    Guoxian Zhou      retrieve multiple hours data
*          1        06/28/2005    Guoxian Zhou      finish component testing
********************************************************************************
*/

#include <stdio.h>
#include <string.h>

#include "PseudoGageVal.h"       /* DbGen PseudoGageVal utilities */
#include "time_convert.h"        /* Time conversion utilities. */
#include "time_defs.h"           /* Time constant utilities. */
#include "get_loc_latlon.h"
#include "get_total_precip.h"
#include "mpe_fieldgen.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   readPseudoPrecip
* PURPOSE:       Read pseudo gage data from the PseudoGageVal table for 
*                the given hour.
*
*                This routine was created to introduce DbGen code for reading
*                the PseudoGageVal table into MPE FieldGen in place of the
*                hand coded ESQL/C.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME           DESCRIPTION/UNITS
*   Input  int *       iunit          Contains the logical unit number
*                                     of the log file.
*   Input  char *      datetime       The datetime to retrieve the PseudoGage
*                                     data for.
*   Input  int *       max_num_gages  The maximum number of gages that can
*                                     be stored in the values, x_coords, 
*                                     and y_coords arrays.
*   Input  int *       hrap_x_orig    The x coordinate of the local MPE hrap
*                                     grid with respect to the national grid.
*                                      
*   Input  int *       hrap_y_orig    The y coordinate of the local MPE hrap
*                                     grid with respect to the national grid.
*   Input  int *       hrap_num_cols  The number of columns in the local hrap
*                                     grid.
*   Input  int *       hrap_num_rows  The number of rows in the local hrap
*                                     grid.
*   Output float *     gage_values    The array of pseudo gage values read
*                                     from the PseudoGageVal table.
*   Output int *       x_hrap_coords  The array of HRAP x coords for the
*                                     processed pseudo gage values.
*   Output int *       y_hrap_coords  The array of HRAP y coords for the 
*                                     processed pseudo gage values.
*   Output int *       num_gages      The number of pseudo gage values read.
*
*   Modified by guoxian zhou 01/25/2005
* 
*   Input  geo_data_struct *pGeoData        local MPE hrap grid struct data
*
*   Input  FILE *       logFile                    log file
*
*   Output gage_table_struct * pGageTable    The array of pseudo gage struct data
*                                            read from the PseudoGageVal table.
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME               HEADER FILE      DESCRIPTION
*   GetPseudoGageVal   PseudoGageVal.h  Retrieves a linked list containing
*                                       data read from the PseudoGageVal
*                                       table.  The user supplies the
*                                       where clause.
*   LatLongToHrap      convert_hrap.h   Converts from Lat/Lon to Hrap
*                                       coordinates.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE       NAME              DESCRIPTION
*   char [ ]        datetime_cstr     The C-style string representing the
*                                     the time to retrieve pseudo gage
*                                     data for.
*   char [ ]        message           Contains messages to be written to the
*                                     log file.
*   char [ ]        where_clause      Contains the where clause for 
*                                     retrieving pseudo gage data.
*   double          col               The HRAP column of the pseudo gage.
*   double          row               The HRAP row of the pseudo gage.
*   PseudoGageVal * pPseudoNode       Points to a node in the linked list
*                                     of PseudoGageVal data. 
*   PseudoGageVal * pPseudoGageVal    Points to the linked list of
*                                     PseudoGageVal data.
*   int             icol              The HRAP column of the pseudo gage
*                                     as truncated to a whole number.
*   int             irow              The HRAP row of the pseudo gage
*                                     as truncated to a whole number.
*
* DATA FILES AND/OR DATABASE:
*   
*   This routine reads the PseudoGageVal table in the IHFS table.  A
*   connection to the IHFS database must be established before this
*   routine is called.
*
* ERROR HANDLING:
*   If the number of pseudo gages exceeds the maximum allowable number of
*   gages, then this routine will send an error message to standard output
*   and shut MPE FieldGen down.
*
********************************************************************************
*/

void MPEFieldGen_readPseudoPrecip( const int runHours ,
                       char ** datetimes ,
                       const geo_data_struct * pGeoData,
                       gage_table_struct ** pGageTable,
                       gage_table_struct ** pGageTableP3,
                       int * gageNumber,
                       int * gageNumberP3)
{
    char where_clause [ 100 ] ;
    PseudoGageVal *pPseudoNode = NULL ;
    PseudoGageVal * pPseudoGageVal = NULL ;
    double col, row ;
    int i, j, icol, irow, total_gages = 0;
    int total_gages_p3 = 0;
    int * num_gages  = NULL;
    int * num_gages_p3  = NULL;
    char datetime_cstr [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
    int oldIndex = 0 ;    

    num_gages = (int *)malloc(runHours * sizeof(int));
    if(num_gages == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
                        " in readPseudoPrecip function."
                        "\n\tProgram exit.");
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < runHours; i++)
        num_gages[i] = 0 ;


    num_gages_p3 = (int *)malloc(runHours * sizeof(int));
    if(num_gages_p3 == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
                        " in readPseudoPrecip function."
                        "\n\tProgram exit.");
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < runHours; i++)
        num_gages_p3[i] = 0 ;




    /* Construct the where clause for the query to retrieve 
      the pseudo gage data. */
    sprintf ( where_clause , " where obstime >= '%s' and obstime <= '%s'"
               " order by obstime"    , 
               datetimes[0], datetimes[runHours - 1] ) ;

    pPseudoGageVal = GetPseudoGageVal ( where_clause ) ;

    if ( pPseudoGageVal != NULL )
    {
        pPseudoNode = ( PseudoGageVal * ) ListFirst(&pPseudoGageVal->list ) ; 
    }

    while ( pPseudoNode != NULL )
    {

        /* For each station with a precipitation total greater than or equal 
         to 0.0, process the gage's value. */
        if ( ( pPseudoNode->gage_value >= 0.0 ) && ( pPseudoNode->gage_value != MISSING_PRECIP ))
        {
	    /* Retrieve the HRAP coordinates for this pseudo gage. */
            LatLongToHrapByReference ( pPseudoNode->lat , pPseudoNode->lon , 
                         & row , & col ) ;

            /* Truncate to find integer HRAP coordinates which gage is in.
            Translate the origin to the lowerleft corner of the MPE 
            estimation domain, that is, convert from the global to the 
            local HRAP grid. */
            irow = ( int )row ;
            icol = ( int )col ;

            irow -= pGeoData->hrap_y ;
            icol -= pGeoData->hrap_x ;

    
   
	//Added by Ram
	//code added to read in all the pseudo gages which are outside the HRAP area
	//into the p3 gage structure (ABRFC requirement). This is just for the 
	//p3lmosaic routine. All other mosaics use gages inside the HRAP area
	//--------------------------------------------------------------------

	{
               dtimet_to_ansi( pPseudoNode->obstime, datetime_cstr );

                i = 0 ;
                for(j = 0; j < runHours; j++)
                {
                    if ( strcmp(datetime_cstr, datetimes[j]) == 0 )
                    {
                        i = j ;
                        if(oldIndex != i)
                        {
                            oldIndex = i ;
                           total_gages_p3 = 0 ;
                        }
                        break ;
                    }
                }

                /* Print gage information to standard output. */
                if ( total_gages_p3 == 0 )
                {
                    // Write out the header only once. 
                    sprintf( message, "\nSTATUS: loading pseudo precip for %s\n"
                         "    #        ID      X      Y  VALUE(mm)",
                        datetimes[oldIndex]) ; 
                    printMessage( message, logFile );
                }

                ++total_gages_p3;
		//sprintf ( message , "%5d  %8s  %5d  %5d    %7.2f" ,
                   //             ++total_gages_p3 , 
                     //           pPseudoNode->pseudo_gage_id ,
                       //         icol , irow , pPseudoNode->gage_value ) ; 
                //printMessage( message, logFile  );

                /**
                 * Test to see if the number of pseudo gages has exceeded
                 * the capacity of the gage array.
                 **/ 
                if ( num_gages_p3[i] >= gageNumberP3[i] )
                {
                    sprintf ( message , "in read_pseudo_precip ... number of "
                                       "gages exceeds capacity of gage "
                                       "array. memory reallocation." ) ;
                    shutDownMPE( message, logFile );
                }

                pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageValue
                    = pPseudoNode->gage_value ;
                pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].hrap_x = icol ;
                pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].hrap_y = irow ; 
                pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].latitude
                    = pPseudoNode->lat ;
                pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].longitude
                    = pPseudoNode->lon ; 
                strncpy(pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageID,
                        pPseudoNode->pseudo_gage_id, LOC_ID_LEN) ;
                pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageID[LOC_ID_LEN] = '\0' ;
                strcpy(pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageTS, "") ;

                num_gages_p3[i] ++ ;

    	}
   //----------------------------------------------------------- 
    
    
    /* Test to determine whether or not this gage is inside the
                office's MPE area. */ 
            if ( ( irow >= 0 ) && ( irow < pGeoData->num_rows ) &&
                 ( icol >= 0 ) && ( icol < pGeoData->num_cols ) )
            {
                /* Store the HRAP coordinates and gage values in the 
                   arrays which will be passed back to the caller of this
                   routine. */

                //dtimet_to_ansi( pPseudoNode->obstime, datetime_cstr );

                i = 0 ;
                for(j = 0; j < runHours; j++)
                {
                    if ( strcmp(datetime_cstr, datetimes[j]) == 0 )
                    {
                        i = j ;
                        if(oldIndex != i)
                        {
                            oldIndex = i ;
                            total_gages = 0 ;
                        }
                        break ;
                    }
                }

                /* Print gage information to standard output. */
                if ( total_gages == 0 )
                {
                    /* Write out the header only once. */
                    sprintf( message, "\nSTATUS: loading pseudo precip for %s\n"
                         "    #        ID      X      Y  VALUE(mm)",
                        datetimes[oldIndex]) ; 
                    printMessage( message, logFile );
                }

                sprintf ( message , "%5d  %8s  %5d  %5d    %7.2f" ,
                                ++ total_gages , 
                                pPseudoNode->pseudo_gage_id ,
                                icol , irow , pPseudoNode->gage_value ) ; 
                printMessage( message, logFile  );

                /**
                 * Test to see if the number of pseudo gages has exceeded
                 * the capacity of the gage array.
                 **/ 
                if ( num_gages[i] >= gageNumber[i] )
                {
                    sprintf ( message , "in read_pseudo_precip ... number of "
                                       "gages exceeds capacity of gage "
                                       "array. memory reallocation." ) ;
                    shutDownMPE( message, logFile );
                }

                pGageTable[i]->ptrGageRecords[num_gages[i]].gageValue
                    = pPseudoNode->gage_value ;
                pGageTable[i]->ptrGageRecords[num_gages[i]].hrap_x = icol ;
                pGageTable[i]->ptrGageRecords[num_gages[i]].hrap_y = irow ; 
                pGageTable[i]->ptrGageRecords[num_gages[i]].latitude
                    = pPseudoNode->lat ;
                pGageTable[i]->ptrGageRecords[num_gages[i]].longitude
                    = pPseudoNode->lon ; 
                strncpy(pGageTable[i]->ptrGageRecords[num_gages[i]].gageID,
                        pPseudoNode->pseudo_gage_id, LOC_ID_LEN) ;
                pGageTable[i]->ptrGageRecords[num_gages[i]].gageID[LOC_ID_LEN] = '\0' ;
                strcpy(pGageTable[i]->ptrGageRecords[num_gages[i]].gageTS, "") ;

                num_gages[i] ++ ;
            }
            else
            {
                sprintf ( message , "WARNING -- gage %s value= %6.1f "
                                "ix/iy=%d %d is beyond the edge of the field.",
                                pPseudoNode->pseudo_gage_id ,
                                pPseudoNode->gage_value , icol , irow ) ; 
                printMessage( message, logFile );
            }
        }      

        pPseudoNode = ( PseudoGageVal * ) ListNext ( & pPseudoNode->node ) ;
    } /* while ( pPseudoNode != NULL ) */

    for(i = 0; i < runHours; i++)
    {
        pGageTable[i]->totalGageNum = num_gages[i] ;
        pGageTable[i]->pseudoGageNum = num_gages[i] ;
        pGageTableP3[i]->totalGageNum = num_gages_p3[i] ;
        pGageTableP3[i]->pseudoGageNum = num_gages_p3[i] ;
    }

    if(num_gages != NULL)
    {
       free(num_gages);
       num_gages = NULL ;
    }
    if(num_gages_p3 != NULL)
    {
       free(num_gages_p3);
       num_gages_p3 = NULL ;
    }

} /* end MPEFieldGen_readPseudoPrecip */

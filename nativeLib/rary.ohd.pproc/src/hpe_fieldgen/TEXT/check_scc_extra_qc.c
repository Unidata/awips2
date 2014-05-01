/*******************************************************************************
* FILENAME:            check_scc_extra_qc.c
*
* Purpose:
* This function is converted from FORTRAN code: mlts_qc.f.
* it performs QCs gage data using satellite precip, radar
* precip and lightning data.
*
* calling function: runERMosaic
* functions called: writeGageQC
*
* input variables
*
* datetime - YYYY-MM-DD HH:MM:SS formatted datetime
*
* mosaic - two-dimensional raw radar data
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain 
*
* pGageArray - array of gage data
*
*
* output variables
*
* qcGage  : Quality controlled gage data of dimension gage number 
*           double qcGage[gages_num]
*
*         :  qcGage code is -1 for multi sensor test (radar)
*         :  qcGage code is -2 for multi sensor test (satellite)
*         :  qcGage code is -3 for multi sensor test (lightning)
*         :  qcGage code is -9 for 8 point neighbor check
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   02/13/2001   C. R. Kondragunta first version
*   Sept. 2003   Feng Ding         modification
*   March 2005   Guoxian Zhou      finish conversion to C Language 
*   08/08/2005   Guoxian Zhou      finish component testing
*
********************************************************************************
*/

#include "empe_fieldgen.h"

void checkSCCExtraQC(const char * datetime ,
                        double ** mosaic ,
                        const geo_data_struct *pGeoData,                    
                        const gage_table_struct * pGageArray)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;
    const int pseudoSize = pGageArray->pseudoGageNum ;
    const int gageSize   = pGageArray->totalGageNum ;

    char tokenvalue[TOKEN_VALUE_LEN] = {'\0'} ;
    int i, j, k, i1, i2, j1, j2 ;
    static double precip_factor = 4.0 ;
    static int first = 1 ;
    
    double max_mosaic;
    
    if(first == 1)
    {
        sprintf( message , "\n Start of SCC Extra Check") ;
        hpe_fieldgen_printMessage( message );

        hpe_fieldgen_getAppsDefaults("mpe_scc_precip_factor", tokenvalue) ;

        precip_factor = atof(tokenvalue);

        if((precip_factor < 1.0) || (precip_factor > 100.0)) 
            precip_factor = 4.0 ;

        sprintf( message , "Precipitation factor of SCC Extra Check"
                " is: %f ",    precip_factor ) ;
        hpe_fieldgen_printMessage( message );
        
        first = 0 ;
    }


    for(k = pseudoSize; k < gageSize; k++)
    {
        i = pGageArray->ptrGageRecords[k].hrap_y ;
        j = pGageArray->ptrGageRecords[k].hrap_x ;
	
	if( pGageArray->ptrGageRecords[k].latitude > 10000.0) 
	{
            pGageArray->ptrGageRecords[k].latitude =
	    pGageArray->ptrGageRecords[k].latitude - 10000.0;
	    
            if ((i == 0) ||
                (j == 0) ||
                (i == colSize - 1) ||
                (j == rowSize - 1) )
            {
                if ( pGageArray->ptrGageRecords[k].gageValue 
		     > (precip_factor*mosaic[j][i]) )
		{     		    
            sprintf ( message , "  %7s %10.4f %10.4f  Failed"
            " SCC Extra Check",
            pGageArray->ptrGageRecords[k].gageID,
            pGageArray->ptrGageRecords[k].gageValue,
		    mosaic[j][i] ) ;
            hpe_fieldgen_printMessage( message );

            pGageArray->ptrGageRecords[k].gageValue = -10000.0 ;

	        }
		else
	    {

            sprintf ( message , "  %7s %10.4f %10.4f  Passed"
            " SCC Extra Check",
            pGageArray->ptrGageRecords[k].gageID,
            pGageArray->ptrGageRecords[k].gageValue,
            mosaic[j][i] ) ;
            hpe_fieldgen_printMessage( message );
        
        }

            }
            else
            {
                i1 = i - 1 ;
                i2 = i + 1 ;
                j1 = j - 1 ;
                j2 = j + 1 ;
        max_mosaic = 0.0;
         
                if( mosaic[j1][i1] > max_mosaic ) max_mosaic = mosaic[j1][i1];  
                if( mosaic[j][i1] > max_mosaic ) max_mosaic = mosaic[j][i1];  
                if( mosaic[j2][i1] > max_mosaic ) max_mosaic = mosaic[j2][i1];  
                if( mosaic[j1][i] > max_mosaic ) max_mosaic = mosaic[j1][i];  
                if( mosaic[j][i] > max_mosaic ) max_mosaic = mosaic[j][i];  
                if( mosaic[j2][i] > max_mosaic ) max_mosaic = mosaic[j2][i];  
                if( mosaic[j1][i2] > max_mosaic ) max_mosaic = mosaic[j1][i2];  
                if( mosaic[j][i2] > max_mosaic ) max_mosaic = mosaic[j][i2];  
                if( mosaic[j2][i2] > max_mosaic ) max_mosaic = mosaic[j2][i2];  

                if ( pGageArray->ptrGageRecords[k].gageValue 
             > (precip_factor*mosaic[j][i]) )            
                {
                    sprintf ( message , "  %7s %10.4f %10.4f  Failed SCC Extra Check",
                    pGageArray->ptrGageRecords[k].gageID,
                    pGageArray->ptrGageRecords[k].gageValue,
            max_mosaic ) ;
                    hpe_fieldgen_printMessage( message );

                    pGageArray->ptrGageRecords[k].gageValue = -10000.0 ;

                }
        else
        {
                    sprintf ( message , "  %7s %10.4f %10.4f  Passed SCC Extra Check",
                    pGageArray->ptrGageRecords[k].gageID,
                    pGageArray->ptrGageRecords[k].gageValue,
            max_mosaic ) ;
                    hpe_fieldgen_printMessage( message );        
        }
        

            }

        }       
    
    }

}

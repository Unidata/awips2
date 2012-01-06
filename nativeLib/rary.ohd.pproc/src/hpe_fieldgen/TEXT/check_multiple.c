/*******************************************************************************
* FILENAME:            check_multiple.c
*
* Purpose:
* This function is converted from FORTRAN code: chk_multiple.f.
* it checks for multiple gages in a single HRAP box.
* if found average values are returned.
*
* calling function: readGageData
* functions called: none
*
* input variables
*
* pGageTable - array of gage data.
*
* output variables
*
* pGageTable - array of gage data after check.
*
*   HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   08/12/1998     D.-J. Seo         Original FORTRAN code
*   May  2005    Guoxian Zhou      finish conversion to C Language 
*   06/27/2005   Guoxian Zhou      finish testing
*
********************************************************************************
*/

#include "empe_fieldgen.h"

void checkMultiple( gage_table_struct * pGageTable)
{
    const int gageSize = pGageTable->totalGageNum ;

    int i, j ;
    int ndupl[gageSize];
    int count ;

    for (i = 0; i < gageSize; ++i)
        ndupl[i] = 1;

    for (i = 0; i < gageSize - 1; ++i)
    {
        if(ndupl[i] == 0)
            continue ;

        for (j = i + 1; j < gageSize; ++j)
        {
            if(ndupl[j] == 0)
                continue ;

            if ((pGageTable->ptrGageRecords[i].hrap_x ==
                 pGageTable->ptrGageRecords[j].hrap_x) &&
                (pGageTable->ptrGageRecords[i].hrap_y ==
                 pGageTable->ptrGageRecords[j].hrap_y))
            {
                ndupl[i] ++ ;
                ndupl[j] = 0 ;
                pGageTable->ptrGageRecords[i].gageValue += 
                pGageTable->ptrGageRecords[j].gageValue;
            }
        }
    }

    for (i = 0; i < gageSize; ++i)
    {
        if(ndupl[i] > 1)
        {
            pGageTable->ptrGageRecords[i].gageValue /=
                (double)ndupl[i] ;
        }
    }

    count = 0 ;

    for (i = 0; i < gageSize; ++i)
    {
        if(ndupl[i] == 0)
        {
            if (pGageTable->ptrGageRecords[i].gageTS[0] == '\0')
                    --(pGageTable->pseudoGageNum);
        }
        else
        {
            strcpy(pGageTable->ptrGageRecords[count].gageID,
                pGageTable->ptrGageRecords[i].gageID) ;

            strcpy(pGageTable->ptrGageRecords[count].gageTS,
                pGageTable->ptrGageRecords[i].gageTS) ;

            pGageTable->ptrGageRecords[count].gageValue =
                pGageTable->ptrGageRecords[i].gageValue ;

            pGageTable->ptrGageRecords[count].hrap_x = 
                pGageTable->ptrGageRecords[i].hrap_x ;

            pGageTable->ptrGageRecords[count].hrap_y = 
                pGageTable->ptrGageRecords[i].hrap_y ; 

            pGageTable->ptrGageRecords[count].latitude  = 
                pGageTable->ptrGageRecords[i].latitude ;

            pGageTable->ptrGageRecords[count].longitude = 
                pGageTable->ptrGageRecords[i].longitude ; 

            count ++ ;
        
        }
    }

    pGageTable->totalGageNum = count ;

    return ;
}

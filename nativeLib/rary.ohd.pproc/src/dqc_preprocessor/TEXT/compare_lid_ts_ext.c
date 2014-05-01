
/**********************************************************************
 * compare_XXX()
 * 
 * This set of functions compare function used for the binary search of
 * the id list done when checking if the location is a valid one.
 *
 * Modified Nov 12, 2009 Dave Miller Wyle IS, OHD/HSEB
 * Reason: When doing a string compare, comparison on unequal lengths is
 * allowed.  However, if this is in conjunction with a binary search and
 * on two strings that have added characters, the comparison can't be
 * be conducted with comparison strings in mind.  The reason is that
 * the comparison is made character by character and not according to length.
 * So, for example, BVO < BVOT2 in a comparison.  But, when the source character
 * for a station is added, BVOZ > BVOT2Z and therefore, if in conjunction with
 * a binary search, BVOZ will not be found as the list is now out of order.
 * As this is copying precipitation values from the database, not finding the
 * station will flag it as missing precip in daily QC rather than the values
 * in the database.
 *
 * Therefore, we have to normalize the lengths by filling in the lid string
 * with some character such as 0 or space that is going to be less than
 * the other possible characters in lids with greater lengths.  So, now
 * BVO0000Z it less than BVOT200Z and the binary search will find
 * that successfully.
 *
 * Note that this should be done in the last function at some point as well.
 * However, it isn't used in the dqc_preprocessor functions/routine.  So,
 * won't do that at this time.  The strCompare length looks incorrect in
 * any case if lid is 8.  Shouldn't this be strCompare[11] instead of [12]?
 *
 *********************************************************************/

#include "dqc_preproc.h"

int compare_lid ( void * search_value,
                  void * array_value )
{
    /*
     * declare and define the values for two variables
     * mapped against the input arguments.
     */
    char * pSearchValue = ( char * ) search_value ;
    struct precip_info * pArrayValue = ( struct precip_info * ) array_value ;

    /*
     * return the result of the simple string comparison
     */
    return ( strcmp ( pSearchValue , pArrayValue->lid) ) ;
}

int compare_lid_source ( void * search_value,
					     void * array_value )
{
    /*
     * declare and define the values for two variables
     * mapped against the input arguments.
     */
    char * pSearchValue = ( char * ) search_value ;
    struct precip_info * pArrayValue = ( struct precip_info * ) array_value ;
    char strCompare[10] = {'\0'};

    /* Must normalize lengths of strings before comparing.  This is also done
       in the linked list value within process_dailyPP and process_hourlyPPPC
    */

    strcpy(strCompare,pArrayValue->lid);
    while(strlen(strCompare)<8)
       strcat(strCompare,"0");
    sprintf(strCompare, "%s%c",strCompare,pArrayValue->source);

    /*
     * return the result of the simple string comparison
     */

    return ( strcmp ( pSearchValue , strCompare) ) ;
}


int compare_lid_source_ext ( void * search_value,
					         void * array_value )
{
    /*
     * declare and define the values for two variables
     * mapped against the input arguments.
     */
    char * pSearchValue = ( char * ) search_value ;
    struct temperature_info * pArrayValue = ( struct temperature_info * ) array_value ;
    char strCompare[12] = {'\0'};

    /*
     * return the result of the simple string comparison
     */
    sprintf(strCompare, "%s%c%c", pArrayValue->lid, pArrayValue->source, pArrayValue->extremum);
    return ( strcmp ( pSearchValue , strCompare) ) ;
}

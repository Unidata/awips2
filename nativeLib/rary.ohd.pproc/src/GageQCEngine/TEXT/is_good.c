
/*******************************************************************************
* FILENAME:            is_good.c
* DESCRIPTION:         Checks to determine if a given month number
*                      is in the range of months bounding the DailyQC period.                  
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       October 3, 2006
* ORGANIZATION:        OHD 11, HSEB
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   DATE             PROGRAMMER        DESCRIPTION/REASON
*   October 3, 2006  B. Lawrence       Added Documentation
********************************************************************************
*/

/*******************************************************************************
* MODULE NAME:  is_good
* PURPOSE:       Checks to determine if a given month is within the range of 
*                months bounding the DailyQC edit period.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  int         k                    The number of the month being
*                                           checked.
*   Input  int         smonth               The number of the month marking the
*                                           start of the DailyQC period.
*   Input  int         emonth               The number of the month marking the 
*                                           end of the DailyQC period.
*
* RETURNS:
*   DATA TYPE                               DESCRIPTION
*   int                                     1 -> the month is in range
*   int                                    -1 -> the month is out of range
* 
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
* 
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*   None
********************************************************************************
*/
int is_good (int k, int smonth, int emonth)
{

   if ( ( smonth <= emonth ) && ( k >= smonth ) && ( k <= emonth ) )
   {
      return (1);
   }

   if (smonth > emonth)
   {
      if (k <= emonth)
      {
         return (1);
      }

      if (k >= smonth)
      {
         return (1);
      }
   }
   return (-1);
}

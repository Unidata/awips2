#include "cex25.h"

/* File: FindDecimalPlaces.c
 *
 * Searches the decimal_place array to match datatype names then gets the
 * number of decimal places to print out for that datatype.
 *
 *          Output Variable
 *
 *  Type/Name                      Description
 *  ---------                      -----------
 *  int decimal_places[i].num_dp   Number of decimal places
 *
 */
int FindDecimalPlaces(data_type)
   char     data_type[5];  /* Data type array */
{
   int      i;     /* counter */

   for(i=0; i<NUM_DATATYPES; i++)
   {
      if(strlen(data_type) == strlen(decimal_places[i].datatype))
      {
	 if(strcmp(data_type, decimal_places[i].datatype) == 0)
	    return (decimal_places[i].num_dp);
	
      }
      else
      {
	 if(decimal_places[i].datatype[3] == '\0' &&
	    strncmp(data_type, decimal_places[i].datatype, 3) == 0)
	    return (decimal_places[i].num_dp);
	 
      }
      
   }
   return 0;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/FindDecimalPlaces.c,v $";
 static char rcs_id2[] = "$Id: FindDecimalPlaces.c,v 1.1 1995/09/08 14:56:49 page Exp $";}
/*  ===================================================  */

}  /* end of FindDecimalPlaces */


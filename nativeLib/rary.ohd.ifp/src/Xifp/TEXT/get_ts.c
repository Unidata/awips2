/* File: get_ts.c
 *
 * Given a type of modification and its associated parameters a
 * search is made for the related forcast points, the time series data.
 *
 *   following inputs: mod_name
 *                     operation_name
 *                     operation_locp
 *                     p_float
 *                     p_char
 *
 *            outputs: time_series_id
 *                     time_series_datatype
 *                     time_series_delta_t
 *
 */

#include <stdio.h>
#define MAX_RF_RO_OPERS 8
#define MAX_UNIT_HG_OPERS 1

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

void get_time_series(mod_name,
		     operation_name,
		     operation_type,
		     operation_locp,
		     time_series_id,
		     time_series_datatype,
		     time_series_delta_t,
		     p_float,
		     p_char)

char    *mod_name;    /* modification name */
char    *operation_name;
char    *operation_type;
int     *operation_locp;  /* pointer to the location of the beginning of
			     the operation parameter array. */
char    *time_series_id, *time_series_datatype;
int     *time_series_delta_t;  /* time series sample time interval */
float   p_float[];             /* parameter floating point data */
char    p_char[][4];           /* parameter character data */

{
   int     ioper,imatch;
   char    *unit_hg_opers[MAX_UNIT_HG_OPERS] = {"UNIT-HG"};
   char    *rf_ro_opers[MAX_RF_RO_OPERS] = {"SAC-SMA","API-CIN","API-HAR",
                         		    "API-MKC","API-SLC","API-CONT",
                                            "API-HAR2","API-HFD"};
   memset(time_series_id, '\0', 8);
   memset(time_series_datatype, '\0', 8);
   *time_series_delta_t = 0;

   if(*(operation_locp)-1 < 1)
     {
      printf("You must pass a location in the p array for the operation",
	     " %s, %s\n", operation_name, operation_type);
      printf("Returning from get_time_series with nothing filled.\n");
      return;
     }
    /*  locp defined if we get here - look in p array at locp for operation     */

   if(strcmp(mod_name, "RRICHNG") == 0)         /* if current mod is RRICHNG    */
     {
      /*  want a rainfall/runoff operation for RRICHNG mod
	     that is:
	 SAC-SMA, API-CIN, API-HAR, API-MKC, API-SLC, API-CONT, API-HAR2 or API-HFD*/

      imatch=-1;
      ioper=0;
      while (ioper<MAX_RF_RO_OPERS && imatch==-1)
         {
	    if (strcmp(operation_type,rf_ro_opers[ioper])==0)
		  imatch=ioper;
            ioper++;
	 }
      switch (imatch)
         {
	    case 0:     
               strncpy(time_series_id, p_char[*(operation_locp)-1 + 7], 8);
	       strncpy(time_series_datatype, p_char[*(operation_locp)-1 + 9], 4);
	               *time_series_delta_t = (int) p_float[*(operation_locp)-1 + 1];
               break;

            case 1:
               strncpy(time_series_id, p_char[*(operation_locp)-1 + 13], 8);
	       strncpy(time_series_datatype, p_char[*(operation_locp)-1 + 15], 4);
	               *time_series_delta_t = (int) p_float[*(operation_locp)-1 + 12];
               break;

            case 2:
	       strncpy(time_series_id, p_char[*(operation_locp)-1 + 25], 8);
	       strncpy(time_series_datatype, p_char[*(operation_locp)-1 + 27], 4);
	               *time_series_delta_t = (int) p_float[*(operation_locp)-1 + 19];
               break;

	    case 3:
	       strncpy(time_series_id, p_char[*(operation_locp)-1 + 13], 8);
	       strncpy(time_series_datatype, p_char[*(operation_locp)-1 + 15], 4);
	               *time_series_delta_t = (int) p_float[*(operation_locp)-1 + 12];
               break;

	    case 4:
	       strncpy(time_series_id, p_char[*(operation_locp)-1 + 11], 8);
	       strncpy(time_series_datatype, p_char[*(operation_locp)-1 + 13], 4);
	               *time_series_delta_t = (int) p_float[*(operation_locp)-1 + 14];
               break;

	    case 5:
	       strncpy(time_series_id, p_char[*(operation_locp)-1 + 7], 8);
	       strncpy(time_series_datatype, p_char[*(operation_locp)-1 + 9], 4);
                       *time_series_delta_t = (int) p_float[*(operation_locp)-1 + 6];
               break;

            case 6:
	       strncpy(time_series_id, p_char[*(operation_locp)-1 + 31], 8);
	       strncpy(time_series_datatype, p_char[*(operation_locp)-1 + 33], 4);
                       *time_series_delta_t = (int) p_float[*(operation_locp)-1 + 25];
               break;

            case 7:
	       strncpy(time_series_id, p_char[*(operation_locp)-1 + 36], 8);
               strncpy(time_series_datatype, p_char[*(operation_locp)-1 + 38], 4);
                       *time_series_delta_t = (int) p_float[*(operation_locp)-1 + 23];
               break;

            /*  if get here have invalid operation type for RRICHNG mod       */

	    default:
	       printf("The RRICHNG mod cannot be used for operation %s, %s\n",
		       operation_name, operation_type);
	 return;
         }
     }

   else if(strcmp(mod_name, "ROCHNG") == 0)     /* if current mod is ROCHNG     */
     {
      /*  want the UNIT-HG operation for ROCHNG mod                             */

     imatch=-1;
     ioper=0;
     while (ioper<MAX_UNIT_HG_OPERS && imatch==-1)
	{
	   if (strcmp(operation_type,unit_hg_opers[ioper])==0)
                 imatch=ioper;
           ioper++;
        }
     switch (imatch) 
        {
           case 0:
	      strncpy(time_series_id, p_char[*(operation_locp)-1 + 12], 8);
	      strncpy(time_series_datatype, p_char[*(operation_locp)-1 + 14], 4);
		     *time_series_delta_t = (int) p_float[*(operation_locp)-1 + 15];
              break;

      /*  if get here have invalid operation type for ROCHNG mod        */

	   default:
	      printf("The ROCHNG mod cannot be used for operation %s, %s\n",
	 	      operation_name, operation_type);
	      return;
	}
     }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/get_ts.c,v $";
 static char rcs_id2[] = "$Id: get_ts.c,v 1.2 1996/03/21 15:40:06 page Exp $";}
/*  ===================================================  */

}

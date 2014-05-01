/*Added underscore to fserch ---kwz*/
/* checkOperInSegment.c							*/
/*									*/
/* Scan p array to find any operations that apply to the specified mod. */
/*									*/
/* Originally written by George Smith - HRL - 941119                    */

#include "Mods_operMods_struct.h"
#include "c_call_f/fserch.h"

void checkOperInSegment(int mod_number,             /* number of the modification */
                        mod_oper_struct * mos,      /* pointer to the mod_oper_info for this mod */
                        int mp,                     /* length of parameter array */
                        float p_float[],            /* parameter float data */
                        char p_char[][4],           /* parameter character data */
                        char oper_names_found[][8], /* operation names found */
                        char oper_types_found[][8], /* operation types found */
                        int oper_locp[],            /* location of the beginning of */
                                                    /*  operations parameter array */
                        int ts_delta_t[],           /* sample time interval array */                                                    
                        int * number_in_list)       /* operation and parameter */
                                                    /*  array index pointer */
{
 int i;                        
 int locp;
 char nameout[8]; 

 if(mos->number_of_opers > 0)
   {
    for (i = 0; i < mos->number_of_opers; i++)
      {
       locp = 1;

       while(locp > 0)
         {
          memset(nameout, '\0', 8);
          FSERCH(&(mos->oper_numbers[i]), nameout, &locp, p_float, p_char, &mp);
          if(locp > 0)
            {
             strncpy(oper_names_found[*number_in_list], nameout, 8);
	     strncpy(oper_types_found[*number_in_list], mos->oper_types[i], 8);
	     oper_locp[*number_in_list] = locp;
      /*     
       * The following are exceptions and additions to the above logic
       * of adding the an operation name, type, and location in the p array
       * to the list passed forward for further mods processing based on
       * the entries in the opers_per_mod file.     
       *
       * If the current operation type if SNOW-17 (oper number 19),
       * store the delta_t for precipitation time series input to
       * the SNOW-17 operation from tenth position of the
       * second portion of the p array into the ts_delta_t array
       * for use by RAINSNOW mod.  Store value any time we find
       * a SNOW-17 operation to simplify logic.
       */
             if(mos->oper_numbers[i] == 19)
               {
                ts_delta_t[*number_in_list] = p_float[locp-1 + 9];
               }
      /*
       * If the current operation type if SNOW-43 (oper number 31),
       * store the delta_t for precipitation time series input to
       * the SNOW-43 operation from tenth position of the
       * second portion of the p array into the ts_delta_t array
       * for use by RAINSNOW mod.  Store value any time we find
       * a SNOW-43 operation to simplify logic.
       */
             if(mos->oper_numbers[i] == 31)
               {
                ts_delta_t[*number_in_list] = p_float[locp-1 + 9];
               }
      /*
       * If the current operation type is RES-SNGL (oper number 26),
       * store delta_t for time series (actually for entire
       * RES-SNGL operation) from seventh position of the
       * second portion of the p array into the ts_delta_t array
       * used by the SETQMEAN mod.  Store value any time we find
       * a RES-SNGL operation to simplify logic.
       */
             if(mos->oper_numbers[i] == 26)
               {
                ts_delta_t[*number_in_list] = p_float[locp-1 + 6];
               }
      /*         
       * The following if statement is to account for an exception
       * to the above logic of storing an operation name, type, and
       * location in the p array whenever it is found for a specified
       * Mod type.  The AEICQN mod is valid for the API-CONT operation
       * only when the AEI option is on in the API-CONT operation.
       * If the current processing is to store an API-CONT operation
       * (operation number 24) for use by an AEICQN mod (mod number 1)
       * and the AEI option is not turned on (14th value in the API-CONT
       * p array equal to 1) the value of *number_in_list will not be 
       * incremented.  This will cancel the entry of name, type, and locp
       * made above.
       *
       * So, in summary, increment *number_in_list unless:
       *      the operation number is 24 AND
       *      the mod number is 1        AND
       *      the AEI option is not 1 
       */
              if((mos->oper_numbers[i] != 24) || 
                 (mod_number != 1) ||
                 ((int)(p_float[locp-1 + 13]) == 1)
                )
                {
   	         (*number_in_list)++;
   	        }
	    }
         }
      }   
    }        

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/checkOperInSegment.c,v $";
 static char rcs_id2[] = "$Id: checkOperInSegment.c,v 1.3 2002/02/11 19:46:00 dws Exp $";}
/*  ===================================================  */

}

#include "GeneralUtil.h"

/*******************************************************************************
* NAME:    get_and_translate_duplicate_token
* PURPOSE: Reads the shef_duplicate token.  This value is translated from 
*          a string to one of five constant integer values.  The mapping is
*          as follows:
*          token value                 constant (defined in GeneralUtil.h)
*          "ALWAYS_OVERWRITE"          ALWAYS_OVERWRITE
*          "USE_REVCODE"               USE_REVCODE
*          "IF_DIFFERENT"              IF_DIFFERENT
*          "IF_DIFFERENT_OR_REVCODE"   IF DIFFERENT_OR_REVCODE
*          "IF_DIFFERENT_AND_REVCODE"  IF_DIFFERENT_AND_REVCODE 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   I/O    int *       shef_duplicate_value Contains the numeric representation
*                                           of the shef_duplicate token
*                                           value.
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME                     HEADER FILE    DESCRIPTION
*   get_apps_defaults        GeneralUtil.h  Returns the value of an apps
*                                           defaults token.
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME                  DESCRIPTION
*   char [ ]   reply                 Constains the value of the token as
*                                    returned by get_apps_defaults.
*   char *     shef_duplicate_token  Contains the name of the shef_duplicate
*                                    token.
*   int        first                 Makes sure that if this routine is 
*                                    called multiple times, get_apps_defaults
*                                    is called only the first time.
*   int        reply_len             The length of the token value string.
*   int        request_len           The length of the shef_duplicate token
*                                    name.
*   int        token_value           The value of the shef_duplicate token.
*
* DATA FILES AND/OR DATABASE:
*   Not Applicable.
*
* ERROR HANDLING:
*   None.  If there is a problem with retrieving the shef_duplicate token 
*   value, then it defaults to IF_DIFFERENT.
********************************************************************************
*/

void get_and_translate_duplicate_token ( int * shef_duplicate_value )
{
   char reply [ 100 ];
   static const char * shef_duplicate_token = "shef_duplicate";
   static int first = 1;
   int reply_len;
   int request_len;
   int status;
   static int token_value = IF_DIFFERENT;

   if ( first == 1 )
   {
      first = 0;

      /* Retrieve the value of the shef_duplicate token. */
      request_len = strlen ( shef_duplicate_token );
      get_apps_defaults ( ( char * ) shef_duplicate_token, 
                          &request_len,
                          reply, 
                          &reply_len );

      if ( reply_len > 0 )
      {
         /* Translate the reply string to upper case. */
         chgupper ( reply );
         
         /* Translate from a string to a numeric value. */
         status = strncmp ( reply, "ALWAYS_OVERWRITE", reply_len );   

         if ( status == 0 )
         {
            token_value = ALWAYS_OVERWRITE;
         }
         else
         {
            status = strncmp ( reply, "USE_REVCODE", reply_len );
            
            if ( status == 0 )
            {
               token_value = USE_REVCODE;
            }
            else
            {
               status = strncmp ( reply, "IF_DIFFERENT", reply_len );
           
               if ( status == 0 )
               {
                  token_value = IF_DIFFERENT;
               }
               else
               {
                  status = strncmp ( reply, "IF_DIFFERENT_OR_REVCODE", 
                                     reply_len );
                 
                  if ( status == 0 )
                  {
                     token_value = IF_DIFFERENT_OR_REVCODE;
                  }
                  else
                  {
                     status = strncmp ( reply, "IF_DIFFERENT_AND_REVCODE",
                                       reply_len );

                     if ( status == 0 )
                     {
                        token_value = IF_DIFFERENT_AND_REVCODE;
                     }
                  }
               }
            }
         }
      }
   }

   * shef_duplicate_value = token_value;
}

/************************************************
 Returns the value of upd_action

***********************************************/
int determine_update_action(int options_duplicate,
                            int shefrec_rev)
{
   int upd_action = DONT_UPDATE_ACTION;

   /* Check if the existing value should be overwritten. This
      occurs under any of the following conditions:

      1)if shef_duplicate = always_overwrite 
        ie., options_duplicate = ALWAYS_OVERWRITE
      2)if shef_duplicate = use_revcode and revisions flag is set in the
        SHEF record ie., options_duplicate = USE_REVCODE
      3)if shef_duplicate = if_different and the value is
        different ie., options_duplicate = IF_DIFFERENT
      4)if shef_duplicate = if_different_or_revcode and the revision flag is
        set in SHEF record or if value is different 
        ie., options_duplicate = IF_DIFFERENT_UPDATE_ACTION 
      5)if shef_duplicate = if_different_and_revcode and the revision flag is
        set in SHEF record and value is different 
        ie., options_duplicate = IF_DIFFERENT_UPDATE_ACTION */

   if   (options_duplicate == ALWAYS_OVERWRITE)
        upd_action = UPDATE_ACTION;
   else if (options_duplicate == USE_REVCODE)
           if (shefrec_rev == 1)
              upd_action = UPDATE_ACTION;
           else
              upd_action = DONT_UPDATE_ACTION;
   else if (options_duplicate  == IF_DIFFERENT_OR_REVCODE && shefrec_rev == 1)
           upd_action = UPDATE_ACTION;
   else if (options_duplicate  == IF_DIFFERENT_AND_REVCODE && shefrec_rev == 1)
           upd_action = IF_DIFFERENT_UPDATE_ACTION;
   else if (options_duplicate  != IF_DIFFERENT_AND_REVCODE)
   {
      /* This address the case where options_duplicate == IF_DIFFERENT or
         options_duplicate == IF_DIFFERENT_OR_REVCODE and shefrec_rev == 0 */
         upd_action = IF_DIFFERENT_UPDATE_ACTION;
   }

   return upd_action;
}


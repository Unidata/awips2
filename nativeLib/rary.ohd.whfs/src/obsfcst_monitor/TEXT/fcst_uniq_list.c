#include "obsfcst_monitor.h"

UniqueList*  get_unique_fcst_list(FILE* logfp,
                                  struct obsfcst_options obsfcst_opts)
{
   UniqueList *uni_lst_ptr        = NULL;

   char       where_clause  [BUFSIZ];
   int        cnt;
   time_t     cur_time;
   char       basis_time_ansi[20];
   char       valid_time_start_ansi[20];
   char       valid_time_end_ansi[20];
   char       cur_time_str[20];

   /* Find the unique lid , pe, ts combinations in the table for data
      between start time and end time */
   cur_time = time(NULL);
   memset(basis_time_ansi, '\0', 20);
   memset(valid_time_start_ansi, '\0', 20);
   memset(valid_time_end_ansi, '\0', 20);
   memset(cur_time_str, '\0', 20);

   timet_to_yearsec_ansi((cur_time-((obsfcst_opts.lb_basistime)*60*60)),
                         basis_time_ansi);
   timet_to_yearsec_ansi(cur_time, valid_time_end_ansi);
   timet_to_yearsec_ansi(cur_time-((obsfcst_opts.lb_validtime)*60*60),
                         valid_time_start_ansi);

   memset(where_clause, '\0', sizeof(where_clause));
   if (strcasecmp(obsfcst_opts.filter,"ALL") == 0)
   {
      sprintf(where_clause, " WHERE basistime>='%s' AND validtime>='%s' AND"
                            " validtime<='%s' AND value!=%8.1f",
                             basis_time_ansi, valid_time_start_ansi, 
                             valid_time_end_ansi, MISSING_VAL );
   }
   else /* if filter is HSA , this is the default too */
   {
       sprintf(where_clause, " WHERE basistime>='%s' AND validtime>='%s' AND"
                             " validtime<='%s' AND value!=%8.1f AND"
                             " lid IN (SELECT lid FROM location WHERE hsa"
                             " IN (SELECT DISTINCT(hsa) FROM admin)) ", 
                             basis_time_ansi, valid_time_start_ansi,
                             valid_time_end_ansi, MISSING_VAL );
   }

   timet_to_yearsec_ansi(cur_time, cur_time_str);
   fprintf(logfp,"\nBefore creating the unique list [%s]", cur_time_str);
   fprintf(logfp,"\nWhereclause for uniq fcst lid,pe,ts,basistime\n%s", 
                  where_clause);
   uni_lst_ptr = LoadUnique("lid||pe||ts||basistime", 
                 obsfcst_opts.fcsttablename, where_clause, &cnt);
   cur_time=time(NULL);
   memset(cur_time_str, '\0', 20);
   timet_to_yearsec_ansi(cur_time, cur_time_str);
   fprintf(logfp,"\nAfter creating the unique list [%s]", cur_time_str);
 
   return uni_lst_ptr;
}

struct parsed_uniq_rec get_parsed_uniq_rec(FILE* logfp,
                                    UniqueList* uniq_lst_ptr)
{
   char       **values            = NULL;
   struct parsed_uniq_rec uniq_rec;
   int cnt;

   /* extract each lid, pe,and ts from the unique combination string. */
   values = ParseUnique ( uniq_lst_ptr, &cnt );

   if ( ( values == NULL ) || ( cnt < 4 ) )
   {
         /* An error was ecountered parsing the unique string. */
         memset(&uniq_rec, 0, sizeof(struct parsed_uniq_rec));
         fprintf(logfp, "\nParse error ");
         return uniq_rec;
   }

   memset(&uniq_rec, 0, sizeof(struct parsed_uniq_rec));
   strncpy(uniq_rec.lid, values[0], LOC_ID_LEN);
   strncpy(uniq_rec.pe, values[1], SHEF_PE_LEN);
   strncpy(uniq_rec.ts, values[2], SHEF_TS_LEN);
   strncpy(uniq_rec.bt, values[3], 20);

   FreeParseUnique ( values );

   return uniq_rec;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}


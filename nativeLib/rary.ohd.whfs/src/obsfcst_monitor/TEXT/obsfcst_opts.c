#include "obsfcst_monitor.h"

int validate_fcsttablename(char* tablename)
{
  if( (strcasecmp(tablename, "FcstHeight") == 0) ||
      (strcasecmp(tablename, "FcstDischarge") == 0) )
    return SUCCESS;
  else
    return ERROR;
}
  
int get_obsfcst_monitor_options(FILE* logfp, 
                               int    argc, 
                               char** argv, 
                               struct obsfcst_options* obsfcst_opts)
{

  extern char *optarg;
  extern int  optind;
  extern int  optopt;
 
  int    ret;
  int    status   = SUCCESS;

  if (argc < 9)
  {
    status = ERROR;
    return status;
  }

  memset(obsfcst_opts, '\0', sizeof(struct obsfcst_options));

  /* Get the command line arguments */
  while ((ret = getopt(argc, argv, "f:d:t:b:v:w:")) != -1)
  {

    switch(ret)
    {
      case 'd':
            /* Get the dbname */
            strcpy(obsfcst_opts->dbname, optarg);
            break;

      case 'b':
            /* Get the look back hours for basistime*/
            obsfcst_opts->lb_basistime = atoi(optarg);
            break;

      case 't':
            /* Get the forecast tablename */
            strcpy(obsfcst_opts->fcsttablename,optarg);
            break;

      case 'f':
            /* Get the filter HSA/ALL */
            if(strlen(optarg) != 0)
               strcpy(obsfcst_opts->filter,optarg);
            break;

      case 'v':
            /* Get the lookback hours for valid time */
            obsfcst_opts->lb_validtime = atoi(optarg);
            break;

      case 'w':
            /* Get the matching valid time window */
            obsfcst_opts->tw_match_validtime = atoi(optarg);
            break;

      default:
            ret = ERROR; /* Invalid options */

    }  /* End of switch */

  } /* End of while getopt */

  return status;

}

int validate_obsfcst_opts(FILE* logfp,
                          struct obsfcst_options obsfcst_opts)
{
   int status =SUCCESS;
   if (strlen(obsfcst_opts.filter) == 0)
   {
        fprintf(logfp, "\nNo filters specified...Will assume HSA");
        strcpy(obsfcst_opts.filter, "HSA");
   }
   else if ( (strcasecmp(obsfcst_opts.filter, "HSA") != 0) &&
             (strcasecmp(obsfcst_opts.filter, "ALL") != 0) )
   {
         fprintf(logfp, "\nInvalid filter -- Use -f HSA/ALL");  
         status = ERROR;
   }
   else if (validate_fcsttablename(obsfcst_opts.fcsttablename) == ERROR)
   { 
        fprintf(logfp,"\nInvalid Fcst table name [%s]", 
                           obsfcst_opts.fcsttablename);
        fprintf(logfp,"\nValid entries are FcstHeight, FcstDischarge");
        status = ERROR;
   }
   return status;
}

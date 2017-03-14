/* ***************************************** */
/*      File:           SaveTSMod.c          */
/*      Date:           March 2002           */
/*      Author:         Wen Kwock            */
/*      Purpose:       Save tschng mods to   */
/*                     file                  */
/*                                           */
/* ***************************************** */

#include "SaveTSMod.h"
#include "TSPlots.h"
#include <stdio.h>
#include "ifp_atoms.h"
#include "c_call_f/mdyh2.h"

extern PLOTMGR    plotmgr;
extern Widget          global_toplevel;

void SaveTSMod(ModKeywords *TSMods,int NumTS)
{
	int             j = 0, tot_char = 0, num_char = 0;
	int             julda;  /* Julian day */
	int             julhr;  /* Julian hour */
	int             month, day, year, hour;
	int             zondum; /* time zone value */
	int             dlsdum; /* Daylight savings time flag, 1 for
				   daylight savings time, 0 for standard */
	char            tz_code[5]; /* time zone code */
	char            *control_str = "      ";
	char            text_val[10];

	FILE            *modfp;
	int             h,i,k;
	int             status;
	char            *fname;
	int     *metric, *iumgen, *iumsac, *iumapi;
	int     *modwrn, *ifpr, *noutz, *noutds;
	char    inptzc[4];
typedef struct
	{
	int             mod_units;                      /* English/Metric units...      */
	int             mod_sac_units;                  /* English/Metric units...      */
	int             mod_api_units;                  /* English/Metric units...      */
	int             metric_units;                   /* Gen. English/Metric units... */
	int             mod_warning;                    /* Show Mod warnings...         */
	int             future_precip;                  /* Quant. Precip. used:  Yes/No */
	char            input_time_zone_code[5];        /* ....NEW....                  */
	int             output_daylight_savings;        /* Output Daylight Sav.: Yes/No */
	int             output_time_zone;               /* Output Time Zone code        */
	char            mod_time_zone_code[5];
	}       univ_techniques_struct;
  univ_techniques_struct   *val_returned;
  int    type, format, nitems, left;

  if(XGetWindowProperty
	  (
	  XtDisplay(global_toplevel),
	  DefaultRootWindow(XtDisplay(global_toplevel)),
	  IFPA_univ_techniques,
	  (long) 0,
	  (long) sizeof(univ_techniques_struct),
	  FALSE,
	  IFPA_univ_techniques_type,
	  (Atom *)&type,
	  (int *)&format,
	  (unsigned long *)&nitems,
	  (unsigned long *)&left,
	  (unsigned char **)&val_returned
	  ) == Success && type == IFPA_univ_techniques_type
    )
     {
      strcpy(tz_code, val_returned->mod_time_zone_code);
     }
   else
     {
      printf("**Warning** in GetUniversalTechs, no techs posted\n");
      memset(tz_code, ' ', 4);
     }
  if( NumTS< 1) return;
  status = 1;
  fname = (char *) malloc(80);
  memset(fname, '\0', 80);
  strcpy(fname, getenv("HOME"));
  strcat(fname, "/.ifp_files/mods/");
  strcat(fname, plotmgr.SegName);
  if (index(fname, ' ') != NULL) memset(index(fname, ' '), '\0', 1);

  modfp = fopen(fname, "a");
  if(modfp == NULL)
  {
    printf("Permission to open %s was denied.\n", fname);
    return;
  }

  for (h=0;h<NumTS;h++)
  {
    for(i = 0; i < TSMods[h].ModIndex; i++)
    {
      for (k=0;k<TSMods[h].NumOps;k++)
      { 
        strcpy (TSMods[h].ModArray[i]->a2.optype,TSMods[h].OpTypes[k]) ;
        strcpy (TSMods[h].ModArray[i]->a2.opname,TSMods[h].OpNames[k]) ;
        fprintf(modfp, ".%s ", TSMods[h].ModArray[i]->a2.command);

/* Get appropriate dates and write them out to the file */
        if(TSMods[h].ModArray[i]->a2.start_date)
        {
          julda=TSMods[h].ModArray[i]->a2.start_date/24;
          julhr=TSMods[h].ModArray[i]->a2.start_date%24;
          MDYH2(&julda,&julhr,&month,&day,&year, &hour,&plotmgr.NOUTZ,&plotmgr.NOUTDS,tz_code) ;
          fprintf(modfp, "%02d%02d%02d%02d%s ", month, day, year%100, hour, tz_code);
        }
        if(TSMods[h].ModArray[i]->a2.end_date)
        {
          FindMDH(TSMods[h].ModArray[i]->a2.end_date, &julda, &julhr,
	      &month, &day, &year, &hour, &zondum, &dlsdum, &tz_code);
          fprintf(modfp, "%02d%02d%02d%02d%s ", month, day, year%100, hour, tz_code);
        }
        if(TSMods[h].ModArray[i]->a2.valid_date)
        {
          FindMDH(TSMods[h].ModArray[i]->a2.valid_date, &julda, &julhr,
	     &month, &day, &year, &hour, &zondum, &dlsdum, &tz_code);
          fprintf(modfp, "%02d%02d%02d%02d%s ", month, day, year%100, hour, tz_code);
        }

/* Put end of line on the first line in file */
        fprintf(modfp, "\n");

        tot_char = fprintf(modfp, "%s %s %s %d",
		   TSMods[h].ModArray[i]->a2.segment_id,
		   TSMods[h].ModArray[i]->a2.info->ts_id,
		   TSMods[h].ModArray[i]->a2.info->data_type,
		   TSMods[h].ModArray[i]->a2.info->delta_t);
		   
/* Temporary fix so mods always written out with 2 decimal places.
   Response to need for RAIM tschng in English units to have 2 sig. figs.
   Added by dp on 10-31-91.
*/
        control_str = " %.2f";
        while(j < TSMods[h].ModArray[i]->a2.num_values)
        {
           sprintf(text_val, "%.2f", TSMods[h].ModArray[i]->a2.values[j]);
           if(tot_char + strlen(text_val) < 68)
           {
             num_char = fprintf(modfp, control_str, TSMods[h].ModArray[i]->a2.values[j]);
             tot_char += num_char;
           }
           else
          {
            fprintf(modfp, " &\n");
            num_char = fprintf(modfp, control_str, TSMods[h].ModArray[i]->a2.values[j]);
            tot_char = num_char;
          }
          j++ ;
        } /*End of while loop j*/
        j = 0;

/* print out proper keyword/optype/opname info */
        if(strcmp(TSMods[h].ModArray[i]->a2.keyword, " ") != 0)
          if(tot_char + strlen(TSMods[h].ModArray[i]->a2.keyword) < 68)
            fprintf(modfp, " %s\n", TSMods[h].ModArray[i]->a2.keyword);
          else
          {
            fprintf(modfp, " &\n");
            fprintf(modfp, " %s\n" , TSMods[h].ModArray[i]->a2.keyword);
          }
        else
          if(strcmp(TSMods[h].ModArray[i]->a2.optype, " ") != 0)
            if(strcmp(TSMods[h].ModArray[i]->a2.opname, " ") != 0)
              if(tot_char + strlen(TSMods[h].ModArray[i]->a2.optype) +
		    strlen(TSMods[h].ModArray[i]->a2.opname) < 68)
	        fprintf(modfp, " %s %s\n", TSMods[h].ModArray[i]->a2.optype, TSMods[h].ModArray[i]->a2.opname);
              else
              {
	        fprintf(modfp, " &\n");
                fprintf(modfp, " %s %s\n", TSMods[h].ModArray[i]->a2.optype, TSMods[h].ModArray[i]->a2.opname);
              }
            else
	      if(tot_char + strlen(TSMods[h].ModArray[i]->a2.optype) < 68)
	        fprintf(modfp, " %s\n" , TSMods[h].ModArray[i]->a2.optype);
              else
              {
	        fprintf(modfp, " &\n");
	        fprintf(modfp, " %s\n" , TSMods[h].ModArray[i]->a2.optype);
              }
          else
            printf("ERROR: neither keyword or optype fields filled");
      }/*end 0f for k loop*/
    }/*End of for loop i*/
  }/*end of for h loop*/

  fclose(modfp);

  for (i=0;i<NumTS;i++)
  { free ((void *) TSMods[i].OpTypes) ;
    free ((void *) TSMods[i].OpNames) ;
  }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTS/RCS/SaveTSMod.c,v $";
 static char rcs_id2[] = "$Id: SaveTSMod.c,v 1.3 2006/04/07 15:59:58 aivo Exp $";}
/*  ===================================================  */

} /***End of subroutine SaveTSMod****/

#include <stdio.h>
#include "libXifp.h"
#include "ifp_atoms.h"

getuniversaltechs(iumgen, iumsac, iumapi, metric,
		  modwrn, ifpr,
		  inptzc, noutz, noutds, modtzc)
	int     *metric, *iumgen, *iumsac, *iumapi;
	int     *modwrn, *ifpr, *noutz, *noutds;
	char    inptzc[4], modtzc[4];
{
/*
 * univ_techniques_struct copied from techniques.h
 * make changes here, also, if change techniques.h
 */
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
  int   i, type, format, nitems, left;

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
/*
 *     printf("Inside GetUniversalTechs, val_returned = \n");
 *     printf("(mod_units, mod_sac_units, mod_api_units, metric_units) = ");
 *     printf("%d, %d, %d, %d\n", val_returned->mod_units,
 *                                val_returned->mod_sac_units,
 *                                val_returned->mod_api_units,
 *                                val_returned->metric_units);
 *     printf("(mod_warning, future_precip) = %d, %d\n",
 *                                val_returned->mod_warning,
 *                                val_returned->future_precip);
 *     printf("(input_time_zone_code, output_daylight_savings, ");
 *     printf("output_time_zone, mod_time_zone_code) =\n");
 *     printf("%s, %d, %d, %s.\n", val_returned->input_time_zone_code,
 *                                 val_returned->output_daylight_savings,
 *                                 val_returned->output_time_zone,
 *                                 val_returned->mod_time_zone_code);
 */
      *iumgen = val_returned->mod_units;
      *iumsac = val_returned->mod_sac_units;
      *iumapi = val_returned->mod_api_units;
      *metric = val_returned->metric_units;

      *modwrn = val_returned->mod_warning;
      *ifpr = val_returned->future_precip;

      strncpy(inptzc, val_returned->input_time_zone_code, 4);
      *noutz = val_returned->output_time_zone;
      *noutds = val_returned->output_daylight_savings;
      strncpy(modtzc, val_returned->mod_time_zone_code, 4);
     }
   else
     {
      printf("**Warning** in GetUniversalTechs, no techs posted\n");

      *iumgen = 0;
      *iumsac = 1;
      *iumapi = 0;
      *metric = 0;

      *modwrn = 0;
      *ifpr = 0;

      memset(inptzc, ' ', 4);
      *noutz = 0;
      *noutds = 0;
      memset(modtzc, ' ', 4);
     }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/GetUniversalTechs.c,v $";
 static char rcs_id2[] = "$Id: GetUniversalTechs.c,v 1.3 2006/04/07 16:59:17 aivo Exp $";}
/*  ===================================================  */

}

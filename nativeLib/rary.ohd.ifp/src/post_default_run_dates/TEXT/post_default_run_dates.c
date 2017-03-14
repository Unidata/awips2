/*
 *  post_default_run_dates.c program - reads NWSRFS OFS files and
 *   posts start, end_obs, and end run dates to Xwindow properties
 *  written by George Smith and Donna Page, HRL, Nov 17, 1992
 */

#include "ifp_dates.h"
#include "c_call_f/upinio.h"
#include "c_call_f/upchkd_wrapper.h"
#include "c_call_f/retrieve_hcl_techs_args.h"
#include "c_call_f/mdyh2.h"

post_default_run_dates_main (argc, argv)
	int      argc;
	char     **argv;
{
	Widget   toplevel;
	Display  *display;
	Window   root;

	int      type, format, nitems, left;
	date     *run_date;
	long     offset = 0;

	int      start_date_posted;
	int      end_obs_date_posted;
	int      end_date_posted;

/*----------------------------------------------------------------------*/
/* Assign directory pathnames for NWSRFS OFS file assignments           */
/*----------------------------------------------------------------------*/
 {
  char            global_dirs[] = "syst oper";
  int             lenstr = 9;
  int             dir_chk_status;

  (void) UPINIO();
  (void) UPCHKD_WRAPPER(global_dirs,&lenstr,&dir_chk_status);

  if(dir_chk_status != 0) exit(0);
 }

toplevel = XtInitialize(argv[0], "Set_dates", NULL, 0, &argc, argv);

display = XtDisplay(toplevel);
root = DefaultRootWindow(display);

/* ********************************** Intern the Atoms *********************************  */

 intern_the_atoms(toplevel);

 if(XGetWindowProperty
    (
    display,
    root,
    IFPA_run_start_date,
    offset,
    (long) sizeof(date),
    FALSE,
    (Atom)IFPA_run_start_date_type,
    (Atom *)&type,
    (int *)&format,
    (unsigned long *)&nitems,
    (unsigned long *)&left,
    (unsigned char **)&run_date
    ) == Success && type == IFPA_run_start_date_type
   )
    start_date_posted = TRUE;
 else
    start_date_posted = FALSE;

 if(XGetWindowProperty
    (
    display,
    root,
    IFPA_run_end_obs_date,
    offset,
    (long) sizeof(date),
    FALSE,
    (Atom)IFPA_run_end_obs_date_type,
    (Atom *)&type,
    (int *)&format,
    (unsigned long *)&nitems,
    (unsigned long *)&left,
    (unsigned char **)&run_date
    ) == Success && type == IFPA_run_end_obs_date_type
   )
    end_obs_date_posted = TRUE;
 else
    end_obs_date_posted = FALSE;

 if(XGetWindowProperty
    (
    display,
    root,
    IFPA_run_end_date,
    offset,
    (long) sizeof(date),
    FALSE,
    (Atom)IFPA_run_end_date_type,
    (Atom *)&type,
    (int *)&format,
    (unsigned long *)&nitems,
    (unsigned long *)&left,
    (unsigned char **)&run_date
    ) == Success && type == IFPA_run_end_date_type
   )
    end_date_posted = TRUE;
 else
    end_date_posted = FALSE;

 if(start_date_posted && end_obs_date_posted && end_date_posted)
      exit(0);

 else
   {
    int   a_in, a_ipr, a_ipu, a_ioerr, a_iodbug;
    int   a_metric, a_iumgen, a_iumsac, a_iumapi;
    int   a_nhopdb, a_nhocal, a_local, a_nlstz;
    char   a_inptzc[5], a_modtzc[5];
    int   a_noutz, a_noutds, a_modwrn;
    int   a_nosnow, a_nofrze, a_iupwe, a_iupsc, a_isac_snow;
    int   a_iprsac, a_iprsnw, a_icrtro, a_iprhy, a_ifpr;
    int   a_idarun, a_ihrrun, a_ldacpd, a_lhrcpd, a_ldarun, a_lhrrun;
    int   a_now[5];

    int   month, day, year, hour, dumzon, dumds;

    RETRIEVE_HCL_TECHS_ARGS(&a_in, &a_ipr, &a_ipu, &a_ioerr, &a_iodbug,
			    &a_metric, &a_iumgen, &a_iumsac, &a_iumapi,
			    &a_nhopdb, &a_nhocal, &a_local, &a_nlstz,
			    a_inptzc, a_modtzc,
			    &a_noutz, &a_noutds, &a_modwrn,
			    &a_nosnow, &a_nofrze, &a_iupwe, &a_isac_snow, &a_iupsc,
			    &a_iprsac, &a_iprsnw, &a_icrtro, &a_iprhy, &a_ifpr,
			    &a_idarun, &a_ihrrun, &a_ldacpd, &a_lhrcpd, &a_ldarun, &a_lhrrun,
			    a_now);

    a_inptzc[4] = '\0';
    a_modtzc[4] = '\0';

    run_date = (date *) malloc (sizeof(date));

    strcpy(run_date->time_zone, a_inptzc);

    if(!start_date_posted)
      {
       MDYH2(&a_idarun, &a_ihrrun,
	     &run_date->month, &run_date->day, &run_date->year, &run_date->hour,
	     &dumzon, &dumds, a_inptzc);

       XChangeProperty
	    (
	    display,
	    root,
	    IFPA_run_start_date,
	    IFPA_run_start_date_type,
	    8,
	    PropModeReplace,
	    (unsigned char *)run_date,
	    sizeof(date)
	    );
      }

    if(!end_obs_date_posted)
      {
       MDYH2(&a_ldacpd, &a_lhrcpd,
	     &run_date->month, &run_date->day, &run_date->year, &run_date->hour,
	     &dumzon, &dumds, a_inptzc);

       XChangeProperty
	    (
	    display,
	    root,
	    IFPA_run_end_obs_date,
	    IFPA_run_end_obs_date_type,
	    8,
	    PropModeReplace,
	    (unsigned char *)run_date,
	    sizeof(date)
	    );
      }

    if(!end_date_posted)
      {
       MDYH2(&a_ldarun, &a_lhrrun,
	     &run_date->month, &run_date->day, &run_date->year, &run_date->hour,
	     &dumzon, &dumds, a_inptzc);

       XChangeProperty
	    (
	    display,
	    root,
	    IFPA_run_end_date,
	    IFPA_run_end_date_type,
	    8,
	    PropModeReplace,
	    (unsigned char *)run_date,
	    sizeof(date)
	    );
      }
   }     /*  end else  */

 XFlush(display);

 exit(0);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/post_default_run_dates/RCS/post_default_run_dates.c,v $";
 static char rcs_id2[] = "$Id: post_default_run_dates.c,v 1.5 2006/03/29 13:47:12 aivo Exp $";}
/*  ===================================================  */

}

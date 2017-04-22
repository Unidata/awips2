#include "libXifp.h"
#include "ifp_atoms.h"

/*---------------------------------------------------------------------------
     get_plot_num(Widget)
     
     Gets the window properties for IFPA_number_of_TulPlots and
     IFPA_tot_num_TulPlots and calculates which plot in the
     segment is currently shown.  Returns the plot_num if it
     gets both atoms.  Returns 0 if it can't get one of the atoms.
---------------------------------------------------------------------------*/
     
int  get_plot_num(Widget w)
{
   int      *num_plots_left,        /* value of number_of_TulPlots atom */
            *tot_num_plots,         /* total number of plots in segment */
            plot_num,               /* return value - number of the plot */
            *first_plot;            /* flag for first plot in segment   */
   Atom     type;                   /* variables for XGetWindowProperty */  
   int      format;                 
   long     offset=0;
   long     nitems, left;
   
   
    if(XGetWindowProperty
        (
        XtDisplay(w),
        DefaultRootWindow(XtDisplay(w)),
        IFPA_number_of_TulPlots,
        offset,
        (long) sizeof(int),
        FALSE,
        IFPA_number_of_TulPlots_type,
        (Atom *)&type,
        (int *)&format,
        (unsigned long *)&nitems,
        (unsigned long *)&left,
        (unsigned char **)&num_plots_left
        ) == Success && type == IFPA_number_of_TulPlots_type)
    {
        if(XGetWindowProperty
              (
              XtDisplay(w),
              DefaultRootWindow(XtDisplay(w)),
              IFPA_tot_num_TulPlots,
              offset,
              (long) sizeof(int),
              FALSE,
              IFPA_tot_num_TulPlots_type,
              (Atom *)&type,
              (int *)&format,
              (unsigned long *)&nitems,
              (unsigned long *)&left,
              (unsigned char **)&tot_num_plots
              ) == Success && type == IFPA_tot_num_TulPlots_type)
        {
           plot_num = *tot_num_plots - *num_plots_left;
           return(plot_num); 
        }  
        else
        {
           printf(" tot_num_TulPlots atom not posted");
           return(0);
        }
                        
    }   
    else
    {
       printf(" number_of_TulPlots atom not posted");
       return(0);
    }   
        

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/get_plot_num.c,v $";
 static char rcs_id2[] = "$Id: get_plot_num.c,v 1.2 2006/04/07 16:59:23 aivo Exp $";}
/*  ===================================================  */

}          

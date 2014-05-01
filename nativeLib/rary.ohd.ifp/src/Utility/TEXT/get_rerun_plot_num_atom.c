#include "libXifp.h"
#include "ifp_atoms.h"

/*-------------------------------------------------------------------------
    get_rerun_plot_num_atom(Widget)
    
       gets and returns the value of the IFPA_rerun_plot_num atom
-------------------------------------------------------------------------*/       
       
int  get_rerun_plot_num_atom(Widget w)
{
   int      *rerun_plot_num;        /* value of rerun_plot_num atom */
   Atom     type;                   /* variables for XGetWindowProperty */  
   int      format;                 
   long     offset=0;
   long     nitems, left;
   
    if(XGetWindowProperty
        (
        XtDisplay(w),
        DefaultRootWindow(XtDisplay(w)),
        IFPA_rerun_plot_num,
        offset,
        (long) sizeof(int),
        FALSE,
        IFPA_rerun_plot_num_type,
        (Atom *)&type,
        (int *)&format,
        (unsigned long *)&nitems,
        (unsigned long *)&left,
        (unsigned char **)&rerun_plot_num
        ) == Success && type == IFPA_rerun_plot_num_type)
    {
       return (*rerun_plot_num);
    }   
    else
    {
       printf("rerun_plot_num_file atom not posted");
       *rerun_plot_num = 1;
       return(*rerun_plot_num);
    }
    

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/get_rerun_plot_num_atom.c,v $";
 static char rcs_id2[] = "$Id: get_rerun_plot_num_atom.c,v 1.2 2006/04/07 16:59:26 aivo Exp $";}
/*  ===================================================  */

}          

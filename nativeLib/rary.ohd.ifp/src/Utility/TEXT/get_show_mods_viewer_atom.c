#include "libXifp.h"
#include "ifp_atoms.h"

/*-------------------------------------------------------------------------
    get_show_mods_viewer_atom(Widget)
    
       gets and returns the value of the IFPA_show_mods_viewer atom
-------------------------------------------------------------------------*/       
       
int  get_show_mods_viewer_atom(Widget w)
{
   int      *flag_on;               /* value of show_mods_viewer atom */
   Atom     type;                   /* variables for XGetWindowProperty */  
   int      format;                 
   long     offset=0;
   long     nitems, left;
   
    if(XGetWindowProperty
        (
        XtDisplay(w),
        DefaultRootWindow(XtDisplay(w)),
        IFPA_show_mods_viewer,
        offset,
        (long) sizeof(int),
        FALSE,
        IFPA_show_mods_viewer_type,
        (Atom *)&type,
        (int *)&format,
        (unsigned long *)&nitems,
        (unsigned long *)&left,
        (unsigned char **)&flag_on
        ) == Success && type == IFPA_show_mods_viewer_type)
    {
       return (*flag_on);
    }   
    else
    {
       printf("show_mods_viewer atom not posted");
       *flag_on = FALSE;
       return(*flag_on);
    }
    
       

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/get_show_mods_viewer_atom.c,v $";
 static char rcs_id2[] = "$Id: get_show_mods_viewer_atom.c,v 1.2 2006/04/07 16:59:33 aivo Exp $";}
/*  ===================================================  */

}          

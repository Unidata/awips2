#include "libXifp.h"
#include "ifp_atoms.h"

/*-------------------------------------------------------------------------
    get_nts_mods_available_atom(Widget)
    
       gets and returns the value of the IFPA_nts_mods_available atom
-------------------------------------------------------------------------*/       
       
int  get_nts_mods_available_atom(Widget w)
{
   int      *flag_on;               /* value of nts_mods_available atom */
   Atom     type;                   /* variables for XGetWindowProperty */  
   int      format;                 
   long     offset=0;
   long     nitems, left;
   
    if(XGetWindowProperty
        (
        XtDisplay(w),
        DefaultRootWindow(XtDisplay(w)),
        IFPA_nts_mods_available,
        offset,
        (long) sizeof(int),
        FALSE,
        IFPA_nts_mods_available_type,
        (Atom *)&type,
        (int *)&format,
        (unsigned long *)&nitems,
        (unsigned long *)&left,
        (unsigned char **)&flag_on
        ) == Success && type == IFPA_nts_mods_available_type)
    {
       return (*flag_on);
    }   
    else
    {
       printf("nts_mods_available atom not posted");
       *flag_on = 0;
       return(*flag_on);
    }
    
       

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/get_nts_mods_available_atom.c,v $";
 static char rcs_id2[] = "$Id: get_nts_mods_available_atom.c,v 1.2 2006/04/07 16:59:20 aivo Exp $";}
/*  ===================================================  */

}          

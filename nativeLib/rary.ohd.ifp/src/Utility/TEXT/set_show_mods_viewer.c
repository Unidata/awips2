#include "globals.h"

/*----------------------------------------------------------------
    set_show_mods_viewer(Widget, caddr_t, XmToggleButtonCallbackStruct)
    
    Sets the show_mods_viewer local variable according to the setting
    of the toggle button then posts the IFPA_show_mods_viewer_file atom.
----------------------------------------------------------------*/
    
void set_show_mods_viewer(Widget w, caddr_t client_data,
		  XmToggleButtonCallbackStruct *call_data)
{
   int             showModsViewer;
   
   if(call_data->set)
      showModsViewer = TRUE;
   else               
      showModsViewer = FALSE;
   
   /*  call to routine to post the IFPA_show_mods_viewer_file atom */
   post_show_mods_viewer_atom(w, showModsViewer);  


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/set_show_mods_viewer.c,v $";
 static char rcs_id2[] = "$Id: set_show_mods_viewer.c,v 1.1 1997/06/24 15:54:59 page Exp $";}
/*  ===================================================  */

}          


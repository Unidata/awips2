#include "globals.h"

/*----------------------------------------------------------------
    set_save_gif(Widget, caddr_t, XmToggleButtonCallbackStruct)
    
    Sets the save_gif global variable according to the setting
    of the toggle button then posts the IFPA_save_gif_file atom.
----------------------------------------------------------------*/
    
void set_save_gif(Widget w, caddr_t client_data,
		  XmToggleButtonCallbackStruct *call_data)
{
   if(call_data->set)
      save_gif = 1;
   else               
      save_gif = 0;
   
   /*  call to routine to post the IFPA_save_gif_file atom */
   post_save_gif_atom(w, save_gif);  


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/set_save_gif.c,v $";
 static char rcs_id2[] = "$Id: set_save_gif.c,v 1.1 1995/09/08 14:55:54 page Exp $";}
/*  ===================================================  */

}          


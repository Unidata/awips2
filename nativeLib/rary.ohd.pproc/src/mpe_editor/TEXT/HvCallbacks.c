#include <unistd.h>
#include <Xm/Xm.h>


#include "color_threshold_show.h"
#include "map_defines.h"
#include "TSControl_show.h"
#include "mpe_field_names.h"
#include "map.h"
#include "map_resource.h"
#include "HvDisplayControlDefs.h"
#include "HvDisplayControlProto.h"
#include "gui_builder.h"
#include "get_mpe_colors.h"
#include "mpe_log_utils.h"
#include "NamedColorSetGroup.h"

/* Added these prototypes to get this code to successfully compile
   without warnings.  Bryon Lawrence  June 6, 2001. */
#include "HvDisplayControlDefs.h"

/* *************************************************************** */

void launchColorManager ( Widget w , XtPointer ptr , XtPointer cbs )
{
        const char * application_name = NULL;
        char * userid = NULL;
        static int first = 1;
        NamedColorSetGroup * pColorSetGroup = NULL;


        if ( first == 1 )
        {
           /* Initialize the color threshold window settings.  These will
              determine which colors it retrieves from the ColorValue
              IHFS database. */
           userid = getlogin ( );
           application_name = getApplicationName ( );
           
           /* Build the colors for the MPE products. */
           pColorSetGroup = get_mpe_default_colors ( );

           if ( pColorSetGroup == NULL )
           {
             logMessage ( "Could not create default color groups for MPE.\n" );
              return;
           }
           
           initialize_color_threshold_window ( pColorSetGroup, 
                                               application_name,
                                               userid,
                                               'E' );
       
           first = 0;
        }

	mSetCursor ( M_WATCH ) ;
	color_threshold_show ( w ) ;
	mSetCursor ( M_NORMAL ) ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

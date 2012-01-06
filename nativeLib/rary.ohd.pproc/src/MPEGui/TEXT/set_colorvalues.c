/*******************************************************************************
* FILENAME:             set_colorvalues.c
* NUMBER OF MODULES:    3
* GENERAL INFORMATION:
*   MODULE 1:           set_colorvalues
* DESCRIPTION:          Retrieves the colors thresholds from ColorValue for a
*                       MPE field or mosaic. Copies these colors into the
*                       central data structure which is read when
*                       drawing the MPE field on the Hydroview/MPE display.
*   MODULE 2:           get_closest_multihour_duration
* DESCRIPTION:          Retrieves the MultiHour QPE whose duration most closely
*                       matches the requested accumulation interval.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        Sept 2002
* ORGANIZATION:         OHD-11 HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*       1,2         9/2002       B. Lawrence       Original Coding
********************************************************************************
*/
#include <string.h> /* For BUFSIZE. */
#include <stdio.h>

#include "display_field.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "rfcwide.h"
#include "drawa.h"
#include "ColorValue.h"
#include "get_colorvalues.h"
#include "limits.h"
#include "LoadUnique.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "color_threshold_show.h"

static const int DEFAULT_PRECIP_DURATION = 3600 ;


/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   set_colorvalues
* PURPOSE:       Retrieves the color scheme for the user request MPE field.
*                First the ColorValue table is checked.  If a color scheme
*                cannot be found for the requested application name, user id,
*                product, and duration, the the ColorValue table is queried
*                again with a user id of "default".  If this fails, then
*                hard coded color lists are used for the product.
*
*                Once a color list has been found, it is stored in the
*                data structure for use in drawing the mpe product on the
*                screen.
* ARGUMENTS:
*   TYPE   DATA TYPE      NAME      DESCRIPTION/UNITS
*   I/O    draw_struct *  data      The retrieved color information is
*                                   stored in this structure.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME             HEADER FILE    DESCRIPTION
*   get_colorvalues  rfcwide.h      For the requested MPE product, user,
*                                   threshold unit, and duration, retrieve
*                                   the color scheme
*   ListFirst        List.h         Retrieves the first node in a linked list.
*   ListNext         List.h         Retrieves the next node in a linked list.
*
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME        DESCRIPTION
*   ColorValue * cvHead    Points to the head of the linked list of
*                          retrieved color information.
*   ColorValue * cvPtr     Points to a node in the linked list of color
*                          information.
*   float      mfactor     Used to convert the units specified by the
*                          entries in the Color Threshold table to the
*                          scaled metric values used internally by
*                          Hydroview/MPE.
*   int        i           A loop index variable.
*   int        numcol      The number of colors in the color scheme.
*   int        numlev      The number of color levels excluding the Missing
*                          and Minimum Threshold Colors.
*
* DATA FILES AND/OR DATABASE:
* Needs the ColorValue table in the IHFS database.
*
* ERROR HANDLING:
*  This routine will shutdown Hydroview/MPE if color information cannot be
*  found for the product.
*
********************************************************************************
*/
void MPEGui_set_colorvalues ( draw_struct * data,
                       const NamedColorSetGroup * pColorSetGroup )
{
   /* Global variables. */
   extern char LOGNAME [ ] ;

   const char * application_name = APPLICATION_NAME;

   Display * display = _get_map_display ( );
   float mfactor ;
   int i ;
   int mask = GCForeground;
   int numcol ;
   int numlev ;

   ColorValue * cvHead = NULL ;
   ColorValue * cvPtr = NULL ;
   XGCValues gcv;

   /*-----------------------------------------------*/
   /*  read levels and colors from ColorValue table */
   /*  or from hardcoded defaults.                  */
   /*-----------------------------------------------*/
   cvHead = get_colorvalues ( LOGNAME , application_name ,
                              data->cv_use , data->cv_duration , 'E' ,
                              & numcol , & numlev, pColorSetGroup ) ;

   numcol = 0;
   numlev = 0;

   if ( cvHead != NULL )
   {
      cvPtr = ( ColorValue * ) ListFirst ( & cvHead->list ) ;

      while ( cvPtr != NULL )
      {

         strcpy(color_list_levels[numcol],cvPtr->color_name);
         numcol ++ ;

         if(cvPtr->threshold_value >= 0.0)
         {
            level_value[numlev] = cvPtr->threshold_value;
            numlev++;
         }

         cvPtr = (ColorValue*) ListNext(&cvPtr->node);
      }

      FreeColorValue(cvHead);
     logMessage("%s levels,colors read from database\n",data->cv_use);

   }
   else
   {
      flogMessage ( stderr , "\nIn routine 'set_colorvalues':\n"
                         "Colors/levels not defined for application = %s\n"
                         "use name = %s logname = %s\n" , application_name ,
                         data->cv_use , LOGNAME ) ;
      exit(1);

   }

    /*---------------------------------------------*/
    /*  define mfactor value based on application  */
    /*---------------------------------------------*/

    units_factor = 25.4;
    scale_factor = 100;

    if(strcmp(data->cv_use,"LOCBIAS") == 0)
    {
       units_factor = 1.0;
       scale_factor = 100;
    }

    else if(strcmp(data->cv_use,"HEIGHT") == 0)
    {
       units_factor = .3048;
       scale_factor = 1;
    }
    else if(strcmp(data->cv_use,"INDEX") == 0 ||
            strcmp(data->cv_use,"LOCSPAN") == 0 )
    {
       units_factor = 1.0;
       scale_factor = 1;
    }
    else if ( strcmp (data->cv_use,"PRISM")==0 )
    {
       /* Display the PRISM data in inches.  This is new in OB72. */
       units_factor = 25.4;
       scale_factor = 1;
    }
    else if ( strcmp(data->cv_use,"MAX_TEMP_PRISM")==0 ||
              strcmp(data->cv_use,"MIN_TEMP_PRISM")==0 )
    {
       units_factor = 1.0;
       scale_factor = 10;
    }

    mfactor = units_factor * ( float ) scale_factor;

    /*---------------------------------------------*/
    /*  define data levels corresponding to colors */
    /*---------------------------------------------*/

    if ( data->levels != NULL )
    {
       free ( data->levels ) ;
       data->levels = NULL ;
    }

    data->levels = ( int * ) malloc ( numlev * sizeof ( int ) ) ;

    if ( data->levels == NULL )
    {
       flogMessage ( stderr , "In routine \"set_colorvalues\":\n"
                          "Cannot allocate memory for the levels member\n"
                          "of the draw_struct structure passed into this\n"
                          "routine.\n" ) ;
       return ;
    }

    for (i=0; i<numlev; i++)
       data->levels[i] = (int) (level_value[i]*mfactor);

    data->num_levels = numcol;

    /* Update the graphics context. */
    if ( data->gc != NULL )
    {
       for ( i = 0 ; i < data->previous_num_levels ; ++ i )
       {
          if ( data->gc [ i ] != NULL ) XFreeGC ( display ,
                                             data->gc [ i ] ) ;
       }

       free ( data->gc ) ;
       data->gc = NULL ;
    }

    data->gc = ( GC * ) malloc (
                        data->num_levels * sizeof ( GC ) );

    if ( data->gc == NULL )
    {
       flogMessage ( stderr , "In routine \"set_colorvalues\":\n"
                          "An error was encountered while attempting to\n"
                          "allocate memory for the graphics context data\n"
                          "array \"rad_data.gc\".  Aborting the display\n"
                          "of precipitation data.\n" ) ;
       display_field_free_memory ( ) ;
       return ;
    }

    /* Save the number of GC levels created so that the next call
       to "display_field" knows how many gc levels to free. */
    data->previous_num_levels = data->num_levels ;

    for ( i = 0 ; i < data->num_levels ; i++ )
    {
       gcv.foreground = get_pixel_by_name(data->w,
                                          color_list_levels[i]);
       data->gc[i] = XCreateGC(display,
                               DefaultRootWindow(display),
                               mask,
                               &gcv);
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

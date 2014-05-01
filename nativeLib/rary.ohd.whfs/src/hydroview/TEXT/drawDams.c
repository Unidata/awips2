#include "drawDams.h"
#include "HvDisplayControlProto.h"
#include "map.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_draw.h"
#include "map_library.h"
#include "map_resource.h"
#include "pointcontrol_mgr.h"
#include "damDisplayControl_show.h"
/***********************************************************************/

void drawAllDamDataSets ( )
{
   int x ;
   int y ;

   static char damColor [ MAX_COLOR_NAME_LENGTH ] ;
   static char * dam_color_token = "dam_icon_color" ;
   int reply_len ;
   int request_len ;
   int status = 0 ;

   extern DamReportList * damreportlistHead ;
   DamReportList * pDamReportlistNode = NULL ;

   /* Get the color for the dam icon. */
   request_len = strlen ( dam_color_token ) ;

   status = get_apps_defaults ( dam_color_token , & request_len ,
                                damColor , & reply_len ) ;
   if ( status != 0 )
      strcpy ( damColor, "BROWN" );

   /* Are Dams to be drawn? */
   if ( getDamDrawingState ( ) == 0 )
   {
      return ;
   }
   
   /* Are there any Dam icons to plot? */
   if ( damreportlistHead == NULL )
   {
      return ;
   }
   
   /* Draw the data for each dam */
   pDamReportlistNode = ( DamReportList  * ) ListFirst 
                                          ( & damreportlistHead->list ) ;

   while ( pDamReportlistNode != NULL )
   {
      if ( pDamReportlistNode->use == 1 )
      {
         /* This dam must be drawn. Convert its latitude and longitude
            into pixel coordinates. */
         mConvertLatLon2XY ( pDamReportlistNode->latitude ,
                             pDamReportlistNode->longitude ,
                             & x , & y ) ; 
         pDamReportlistNode->pos.x = x ;
         pDamReportlistNode->pos.y = y ;
         drawDamDataSet ( pDamReportlistNode ,x , y, damColor ) ;
      } 

      pDamReportlistNode = ( DamReportList * ) ListNext 
                                           ( & pDamReportlistNode->node ) ;
   }


   return ;
}

/***********************************************************************/

void drawDamDataSet( DamReportList * pDamReportList , int x , int y, char * color  )
{

   /* show the icon */
   if ( showDamIconSymbol() )
      drawDamIcon ( x , y, color ) ;
   
   /* draw labels such as id and name */
   drawDamIconLabels ( pDamReportList , x , y ) ;
   
   return;
}


/***********************************************************************/

void drawDamIcon ( int x , int y, char * color )
{

    draw_damcrest_point(M_EXPOSE, 0, x, y, color);	 
   //mDrawSymbol ( M_EXPOSE , 0 ,  x , y , color , M_DAMCREST_POINT ) ;
   
   return;   
}

/***********************************************************************/ 

void drawDamIconLabels( DamReportList * pDamReportList , int x , int y )
{   
   HvColorList *hcl = getHvColorList ( ) ;
   
   char idLabel[ NID_ID_LEN + 1 ];
   char nameLabel[ DAMREPORTLIST_NAME_LEN + 1 ];
   
   mSetColor ( hcl->labelColor ) ;

   if ( showDamIdLabel() )
   {      
      sprintf ( idLabel , "%s" , pDamReportList->nidid );
      mDrawText ( M_EXPOSE , 0 , x + 2 , y + 13 , idLabel ) ;
   }
   
   if ( showDamNameLabel() )
   {
      sprintf( nameLabel, "%s", pDamReportList->name );
      mDrawText ( M_EXPOSE , 0 , x + 2 , y , nameLabel ) ;
   }
  
   return;     
}

/*****************************************************************************
 *
 * Routine: draw_damcrest_point
 *
 * Description: this routine draws the icon for the damcrest point
 *
 ****************************************************************************/
void draw_damcrest_point (int area, int number, int x, int y, char * color )
{
  Pixmap pix;

  if (area == M_MAP)
    pix = _get_maps_pixmap(number);
  else if ( area == M_LEGEND )
    pix = _get_legend_pixmap(number) ;
  else if ( area == M_USER )
    pix = mGetUserPixmap ( ) ;
  else
    pix = _get_map_pixmap ( ) ;

  _set_foreground ( color ) ;
  _area_draw_fill_half_circle( pix, x-8, y-5, 10, 10);
  
  
}



/****************************************************************************
 *
 * Header: map.h
 *
 * Description: contains prototype definations needed for map 
 *
 ***************************************************************************/

#include "map_defines.h"

#ifndef MAP_H
#define MAP_H

#ifdef __cplusplus
extern "C" {
#endif

/* prototype definations */

extern void _clear_map(int);
extern MotionCallback _get_about_app_cb ( ) ;
extern CleanCallback _get_clean_routine ( ) ;
extern Pixmap _get_legend_pixmap(int);
extern Widget _get_legend_widget ( int map_index ) ;
extern int _get_map_height(int);
extern Pixmap _get_map_pixmap();
extern Widget _get_map_shell();
extern Widget _get_map_widget(int);
extern int _get_map_width(int);
extern Pixmap _get_maps_pixmap(int);
extern Pixmap _get_states_pixmap ( ) ;
extern int _get_num_of_maps();
extern void _set_num_of_maps ( int number );
extern void mSetUserPixmap ( Pixmap u_pixmap ) ;
extern Pixmap mGetUserPixmap ( ) ;
extern int _get_screen_width();
extern int _get_screen_height();
extern int _get_toolbar();
extern void _map_expose(Widget,XtPointer,XtPointer);
extern int _get_make_map( ) ;
extern void _set_make_map(int);
extern void _set_map_change(int);
extern void _set_legend_height ( int map_number , int new_legend_height ,
                                 int resize_pixmap ) ;
extern int _get_legend_height ( );
extern int _get_legend_height_index ( int map_index );
extern int _get_legend_width ( );
extern void _turn_legend_off();
extern void _turn_legend_on();
extern void _turn_toolbar_off();
extern void _turn_toolbar_on();
extern void mSetMapBackgroundColor(char *);
extern void mAddRecenterRoutine ( MotifCallback recenter_function ) ;

#ifdef __cplusplus
}
#endif

#endif /* #ifndef MAP_H */

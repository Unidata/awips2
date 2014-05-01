/****************************************************************************
 *
 * Header: map_menubar.h
 *
 * Description: contains the prototypes for map_menubar.c
 * Modification History:   March, 2006. Modified menu_items to add map_tool_pdsize
 *                         etc for "Set station Icon Size" under "Tools" menu.
 *
 ***************************************************************************/

#include "map_defines.h"

#ifndef MAP_MENUBAR_H
#define MAP_MENUBAR_H

enum menu_items {map_file_pane,map_file_cascade,map_file_save,map_file_print,
                 map_file_reverse_print,map_file_close,map_tool_pane,
                 map_tool_cascade,map_areal_zoom, map_toolbar_option,
                 map_tool_zoom,map_tool_zoom_cascade,map_tool_zoom_in,map_tool_zoom_out,
                 map_tool_pan,map_tool_pan_cascade,map_tool_pan_north,
                 map_tool_pan_south,map_tool_pan_east,map_tool_pan_west,
                 map_tool_recenter,map_tool_font,map_tool_font_cascade,
                 map_tool_font_miniscule,map_tool_font_very_small,
		         map_tool_font_small,map_tool_font_normal,
		         map_tool_font_large,map_tool_font_very_large,
                 map_tool_pdsize,map_tool_pdsize_cascade,map_tool_pdsize_small,
		 map_tool_pdsize_medium,map_tool_pdsize_large,map_tool_pdsize_vsmall,			 
                 map_projection_pane,map_projection_cascade,
                 map_projection_flat,map_projection_polar,map_projection_hrap,
                 map_overlay_pane,map_overlay_cascade,map_overlay_states,
                 map_overlay_basins,map_overlay_basin_names,map_overlay_city,
                 map_overlay_county,map_overlay_cwa,map_overlay_hsa,
                 map_overlay_highways_cascade,map_overlay_highways,
                 map_overlay_highways_and_roads,map_overlay_highways_only,
                 map_overlay_no_highways,map_hrap_grid,map_overlay_streams_lakes_cascade,map_overlay_streams_lakes,
                 map_overlay_rivers,map_overlay_streams,map_overlay_no_streams,
                 map_overlay_lat_lon,map_overlay_rfc,map_overlay_radar_locations,
                 map_overlay_radar_rings,map_overlay_timezones,
                 map_overlay_topography_cascade,map_overlay_topography,
                 map_overlay_topo,map_overlay_topo_contour,map_overlay_no_topo,
                 map_overlay_zones,map_overlay_foreground,
                 map_help_pane,map_help_cascade,
                 map_help_topics,map_help_screen,map_help_legend,
                 num_of_menu_items};

/*-------------------------------------------------------------------------
 
 Prototypes definitions

 --------------------------------------------------------------------------*/

extern Widget _get_menu_item_toggle_widget ( enum menu_items item) ;

extern void _create_cascade ( Widget menu , Widget * pane , Widget * cascade ,
                              char * name , char mnemonic ) ;
extern void _create_map_menubar(Widget form);
extern void _manage_menubar();
extern void _set_legend_state ( enum MapState state ) ;

extern void _create_menu_item_toggle ( Widget cascade,Widget *item, char *name ,
                                char mnemonic, char * accel, char * text_ac ) ;

#endif /* #ifndef MAP_MENUBAR_H */

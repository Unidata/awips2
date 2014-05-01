/****************************************************************/
/*								*/
/*	FILE:		Mods_config.h				*/
/*								*/
/*	Include file for configuring the Mods interface to	*/
/*	match the available Operations Table and Mods for	*/
/*	the Operations						*/
/*								*/
/*								*/
/*	Coded by:	Tom Adams (TEA)				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		06/01/94				*/
/*	Modified:	09/12/94 - (TEA)			*/
/*                      'menuItemsNames_struct'			*/
/*								*/
/****************************************************************/

#ifndef Mods_config_h
#define Mods_config_h

typedef struct
	{
	int			num;
	xs_menu_widget_struct	*widgetStruct;
	xs_menu_struct		*menuItems;
	}       pullDownMenu_struct;
 

typedef struct
	{
	int	num;
	char	**name;
	}       menuItemsNames_struct;
 
#endif

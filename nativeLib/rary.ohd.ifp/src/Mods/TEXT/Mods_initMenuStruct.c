/**************************************************************/
/*                                                            */
/*	FILE:		Mods_initializeMenuStruct.c           */
/*                                                            */
/*	Run-time Modifications menu initialization	      */
/*	module						      */
/*                                                            */
/*	Coded by:	Tom Adams                             */
/*			NWS * Office of Hydrology * HRL       */
/*	Date:		09/08/94                              */
/*                                                            */
/*      NOTE:	This file contains functions that complete    */
/*		the interface setup - these include GUI	      */
/*		elements that change dynamically and can not  */
/*		be created statically in a GUI builder,       */
/*		such as the available Operations or Mods for  */
/*		an individual Segment (basin).		      */
/*                                                            */
/**************************************************************/





#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_widgetStruct.h"
#include "Mods_config.h"






void genericCB(Widget w, XtPointer clientData, XtPointer callData)
{
        XmString     xmStringName;
        char         *name;
        
        
        
        
        XtVaGetValues(w, XmNlabelString, &xmStringName, NULL);
        XmStringGetLtoR(xmStringName, XmFONTLIST_DEFAULT_TAG, &name);
        
	/* printf("Inside the CALLBACK function for %s\n", name); */

}



pullDownMenu_struct *initializeMenuStruct(menuItemsNames_struct *data, char *lastName, 
						void (*callback_func) (), caddr_t callData)
 {
 
 	int		      i;
 	int		      numItems;
 	 	
	pullDownMenu_struct   *menuStruct;
	
	
	/* printf("Inside 'initializeMenuStruct()'...\n"); */
	
	/*-----------------------------------------------------------*/
	/* Check to see if there should be an 'ALL' item at the      */
	/* bottom of the pulldown/option menu; if there is we need   */
	/* to make space for two items including the separator       */
	/* widget in the menuItems struct array...                   */
	/*-----------------------------------------------------------*/
	if(lastName == NULL) numItems = data->num;
	else numItems = data->num + 2;
	
	menuStruct = (pullDownMenu_struct *)XtMalloc(sizeof(pullDownMenu_struct));
	
	menuStruct->num = numItems;
	menuStruct->menuItems = (xs_menu_struct *)XtMalloc(sizeof(xs_menu_struct)*numItems);
	
	for(i = 0; i < data->num; i++) {    		
    		menuStruct->menuItems[i].name           = (char *) XtMalloc((strlen(data->name[i])+1)*sizeof(char));
    		strcpy(menuStruct->menuItems[i].name, data->name[i]);
    		
     		menuStruct->menuItems[i].func           = callback_func;
    		menuStruct->menuItems[i].data           = callData;
     		menuStruct->menuItems[i].active         = TRUE;
     		menuStruct->menuItems[i].button_type    = PUSH_BUTTON;
     		menuStruct->menuItems[i].sub_menu       = NULL;
     		menuStruct->menuItems[i].n_sub_items    = 0;
      		menuStruct->menuItems[i].sub_menu_title = NULL;
       		}
	
	if(numItems > data->num) {
	          		
	        /* Do the separator 1st...                  */
    		menuStruct->menuItems[data->num].name           = (char *) NULL;    		
     		menuStruct->menuItems[data->num].func           = NULL;
    		menuStruct->menuItems[data->num].data           = (caddr_t) NULL;
     		menuStruct->menuItems[data->num].active         = TRUE;
     		menuStruct->menuItems[data->num].button_type    = PUSH_BUTTON;
     		menuStruct->menuItems[data->num].sub_menu       = NULL;
     		menuStruct->menuItems[data->num].n_sub_items    = 0;
      		menuStruct->menuItems[data->num].sub_menu_title = NULL;
	
	        /* Now do the last item...                  */
    		menuStruct->menuItems[data->num+1].name           = (char *) XtMalloc((strlen(lastName)+1)*sizeof(char));
    		strcpy(menuStruct->menuItems[data->num+1].name, lastName);
    		
     		menuStruct->menuItems[data->num+1].func           = callback_func;
    		menuStruct->menuItems[data->num+1].data           = callData;
     		menuStruct->menuItems[data->num+1].active         = TRUE;
     		menuStruct->menuItems[data->num+1].button_type    = PUSH_BUTTON;
     		menuStruct->menuItems[data->num+1].sub_menu       = NULL;
     		menuStruct->menuItems[data->num+1].n_sub_items    = 0;
      		menuStruct->menuItems[data->num+1].sub_menu_title = NULL;
	        }
	
	
 
 	return(menuStruct);
 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_initMenuStruct.c,v $";
 static char rcs_id2[] = "$Id: Mods_initMenuStruct.c,v 1.1 1995/11/14 12:19:23 page Exp $";}
/*  ===================================================  */

 }

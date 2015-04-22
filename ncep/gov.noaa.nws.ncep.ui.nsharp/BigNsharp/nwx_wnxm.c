#include "nwx_cmn.h"
#include "nwx_gui.h" 


/************************************************************************
 * nwx_wnxm.c                                                           *
 *                                                                      *
 * This module contains wrapper functions for the Nxm library calls     *
 * from the NWX functions.						*
 *                                                                      *
 * CONTENTS:                                                            *
 * 	wnxm_NxmWarn_show 						*	
 *      wnxm_NxmCursor_setCursor					*
 *      wnxm_NxmMenuPulldownBuild					*
 *      wnxm_NxmHelp_helpBtnCb						*
 *      wnxm_NxmExit_create						*
 *      wnxm_NxmGmpkRgstr						*
 *      wnxm_xxflsh    							*
 ***********************************************************************/

/*=====================================================================*/

void wnxm_NxmWarn_show( Widget parent, char *message )
/************************************************************************
 * wnxm_NxmWarn_show							*
 *									*
 * void wnxm_NxmWarn_show ( parent, message )				*
 *									*
 * Input parameters:							*
 *	parent		Widget	The top level widget			*
 *	*message	char	Warning message to be displayed		*
 *									*
 * Output parameters:							*
 * Return:								*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    NxmWarn_show( parent, message );	
}

/*=====================================================================*/

void wnxm_NxmCursor_setCursor( Widget parent, int ref )
/************************************************************************
 * wnxm_NxmCursor_setCursor						*
 *									*
 * void wnxm_NxmCursor_setCursor( parent, ref )				*
 *									*
 * Input parameters:							*
 *	parent		Widget	ID of parent widget			*
 *	ref		int	reference number of cursor		*
 *									*
 * Output parameters:							*
 * Return:								*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    NxmCursor_setCursor( parent, ref );	
}

/*=====================================================================*/

void wnxm_NxmClose_menuReset( Widget shell, 
			void( *func )( Widget, XtPointer, XtPointer), 
			XtPointer call )
/************************************************************************
 * wnxm_NxmClose_menuReset						*
 *									*
 * void wnxm_NxmClose_menuReset( shell, func, call )			*
 *									*
 * Input parameters:							*
 *	shell		Widget		ID of the window       		*
 *	func		XtCallbackProc	callback function for the menu 	*
 *	call		XtPointer	input dta to the cb function	*
 *									*
 * Output parameters:							*
 * Return:								*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    wnxm_NxmClose_menuReset( shell, func, call );	
}

/*=====================================================================*/

Widget wnxm_NxmMenuPulldownBuild( Widget parent, WidgetList return_item_w,
				char *menu_title, KeySym menu_mnemonic,
				_NXMmenuItem *items )
/************************************************************************
 * wnxm_NxmMenuPulldownBuild						*
 *									*
 * void wnxm_NxmMenuPulldownbuild( parent, return_item_w, menu_title,	*
 *				   menu_mnemonic, iterms ) 		*
 *									*
 * Input parameters:							*
 *  	parent		Widget		parent widget ID       		*
 *  	menu_title	char*		title name of pulldown menu    	*
 *  	menu_mnemonic	KeySym		mnemonic of the menu 		*
 *  	items`		*_NXMmenuItem   definition of eaqch menu item	*
 *									*
 * Output parameters:							*
 *	return_item_w	WidgetList	widget ID of each menu item	*
 *									*
 * Return:								*
 *			Widget		widget ID of the pulldown menu	*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    return(  NxmMenuPulldownBuild( parent, return_item_w, menu_title,  
    			       menu_mnemonic, items ) );
}

/*=====================================================================*/

void wnxm_NxmHelp_helpBtnCb( Widget wid, long fileid, XtPointer cbs )
/************************************************************************
 * wnxm_NxmHelp_helpBtnCb						*
 *									*
 * void wnxm_NxmHelp_helpBtnCb( wid, fileid, cbs )			*
 *									*
 * Input parameters:							*
 *  	wid		Widget		ID of calling widget   		*
 *  	fileid		long		id of help file                	*
 *  	cbs		XtPointer	callback struct (not used)	*
 *									*
 * Output parameters:							*
 *			None						*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    NxmHelp_helpBtnCb( wid, fileid, cbs );  
}

/*=====================================================================*/

Widget wnxm_NxmExit_create( Widget parent, char *title, char *message,
			void( *ok_cb )( Widget, XtPointer, XtPointer ),
			void( *cancel_cb )( Widget, XtPointer, XtPointer ) )
/************************************************************************
 * wnxm_NxmExit_create							*
 *									*
 * void wnxm_NxmExit_create( parent, title, message, ok_cb, cancel_cb ) *
 *									*
 * Input parameters:							*
 *  	parent		Widget	parent widget          			*
 *  	*title		char	dialog title                   		*
 *  	*message	char	message displayed in dialog		*
 *	*ok_cb()	void	callback function of OK button  	*
 *	*cancel_cb()	void	callback function of Cancel button	*
 *									*
 * Output parameters:							*
 *			None						*
 * Return:								*
 *			Widget	the dialog widget 			*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    return( NxmExit_create( parent, title, message, ok_cb, cancel_cb ) );  
}

/*=====================================================================*/

int wnxm_NxmGmpkInit( Widget wid, int mode, void( *init_func )(int *iret ) ) 
/************************************************************************
 * wnxm_NxmGmpkInit							*
 *									*
 * void wnxm_NxmGmpkInit( wid, mode, init_func )                 	*
 *									*
 * Input parameters:							*
 *  	wid		Widget	widget id              			*
 *  	mode		int	1 = map, 2 = graph			*
 *  	*init_func()	void	initialization function from appl	*
 *									*
 * Output parameters:							*
 *			None						*
 * Return:								*
 *			int	G_NORMAL = successful			*
 *				-1 = error				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    return( NxmGmpkInit( wid, mode, init_func ) );  
}

/*=====================================================================*/

void wnxm_NxmGmpkRgstr( Widget wid, char *name, void( *rgstr_func )( void ) ) 
/************************************************************************
 * wnxm_NxmGmpkRgstr							*
 *									*
 * Wrapper for NxmGmpkRgstr, which registers a window (drawing widget)  *
 * as a GEMPAK window.							*
 *									*
 * void wnxm_NxmGmpkRgstr( wid, name, rgstr_func )                 	*
 *									*
 * Input parameters:							*
 *  	wid		Widget	widget id              			*
 *  	*name		char	window name       			*
 *  	*rgstr_func()	void	initialization function          	*
 *									*
 * Output parameters:							*
 *			None						*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    printf("wnxm_NxmGmpkRgstr 1\n");
    printf("wnxm_NxmGmpkRgstr: %s\n", name );
    NxmGmpkRgstr( wid, name, rgstr_func );  
    printf("wnxm_NxmGmpkRgstr 2\n");
}

/*=====================================================================*/

void wnxm_xxflsh( int *raise, int *iret )
/************************************************************************
 * wnxm_xxflsh      							*
 *									*
 * Wrapper for xxflsh, which is not actually an nxm function, but it's  *
 * here because it is in proto_xw.h, and that is not included by 	*
 * nwx_wgem.c.  The full answer is that nwx should not be calling	*
 * xxflsh directly at all, but that change will have to wait.		*
 *									*
 * void wnxm_xxflsh( raise, iret )					*
 *									*
 * Input parameters:							*
 *  	*raise		int	raise flag             			*
 *									*
 * Output parameters:							*
 *  	*iret		int	return code            			*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    xxflsh( raise, iret );
}

/*=====================================================================*/

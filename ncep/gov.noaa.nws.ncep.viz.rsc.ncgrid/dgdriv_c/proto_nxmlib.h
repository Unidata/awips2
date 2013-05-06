/************************************************************************
 * proto_nxmlib.h                                                       *
 *									*
 * This include file contains prototypes for all the nxmlib libraries   *
 **                                                                     *
 * A. Hardy/GSC		12/00   Created					*
 * H. Zeng/EAI		02/01	modify NxmExit_create param list	*
 * T. Piper/SAIC	10/01	added full prototypes for callback fncs.*
 * H. Zeng/XTRIA        10/02   added new NxmVolcano_xxxx functions     *
 * R. Tian/SAIC         11/02   add NxmPushBtnMenu_create               *
 * R. Tian/SAIC    	01/03   add flag to NxmBxmBtn_create(Multi)	*
 * T. Piper/SAIC	05/03	added NxmGmpkInit & NxmGmpkRgstr	*
 * T. Piper/SAIC	07/03	removed NxmBusy_setBusyPid		*
 * H. Zeng/XTRIA	07/03   added more NxmVolcano_xxxx functions	*
 * T. Piper/SAIC	01/04	modified NxmLoadColorTable argument list*
 * T. Piper/SAIC	02/04	added NSHARP routines			*
 * E. Safford/SAIC	04/04	change call sequence to NxmGeneric_show *
 * T. Piper/SAIC	06/04	removed NxmColorDeleteSharedRWcells	*
 * H. Zeng/SAIC         05/04   added NxmPrt_txtPrtShow			*
 * H. Zeng/SAIC		08/04	added two NxmScaleA_xxxx functions	*
 * T. Piper/SAIC	10/04	added NxmScaleA_isUp			*
 * H. Zeng/SAIC		10/04	added NxmScaleA_updtLat			*
 * H. Zeng/SAIC		11/04	added NxmClrW_popdown2			*
 * C. Bailey/HPC        02/05   added NxmGif_create &  NxmGif_gifWPopup	*
 * T. Piper/SAIC	01/08	Added NxmWarn_show from proto_nmaplib	*
 ***********************************************************************/

#ifndef PROTO_NXMLIB
#define	PROTO_NXMLIB

/*
 *   Nui prototypes
 */

void 	NuiColorBarCreate ( 	Widget 		parent,
				Boolean	 	show_active_color );

void 	NuiColorBarReset ( 	Widget 		parent );

void 	NuiColorEditPopup (  	Widget 		parent );

/*
 *   Nxm prototypes
 */

void 	NxmBusy_createBtns ( 	Widget 		rc );

void 	NxmBusy_invoke ( 	Widget 		cursorw,
				char   		*sflag );

void 	NxmBusy_checkStopBtn (	void );

void 	NxmBusy_animateFinish (	void );

void 	NxmBusy_setStopBtn ( 	int		flag );


void 	NxmBxmBtn_addBxmLabel ( Widget         	button_id,
				unsigned int	width, 
				unsigned int	height,
				char            *button_fgcolor,
				char            *button_bgcolor,
				char		iconfile[],
				char		*text_label );

void	NxmBxmBtn_changeLabel ( char		*txt_label,
				char 		label_string[50] );

Widget 	NxmBxmBtn_create ( 	Widget         	parent,
				char           	button_name[],
				WidgetClass	button_class,
				unsigned int	width,
				unsigned int	height,
				char            *button_fgcolor,
				char            *button_bgcolor,
				char  		insensitive_bits[], 
				char		sensitive_bits[],
				char		*text_label,
				Boolean		press_flag,
				XtCallbackProc	callback,
				XtPointer      	callback_data );

Widget  NxmBxmBtn_createMulti ( Widget          parent,
                                char            *button_name,
                                WidgetClass     button_class,
                                unsigned int    width,
                                unsigned int    height,
                                struct  bxmInfo *bxm_info,
                                int             n_set,
                                char            *text_label,
				Boolean		press_flag,
                                XtCallbackProc	callback,
                                XtPointer       callback_data,
                                struct  pxmBuf  *pxm_buffer );

void	NxmBxmBtn_enableLabel ( int		flag );

void	NxmBxmBtn_setPxm ( 	Widget		button_id,
				Pixmap  	sensitive_map,
				Pixmap  	insensitive_map );


void 	NxmClose_menuReset ( 	Widget	   	shell,
				XtCallbackProc	func,
				XtPointer  	data );

void 	NxmClose_menuRmEntry ( 	Widget 		shell );

void 	NxmClose_popupCb ( 	Widget		w,
				Widget		popup,
				XtPointer	cbs );


void 	NxmClrW_create ( 	Widget  	parent_w );

void 	NxmClrW_popup ( 	Widget 		w,
				XtPointer 	clnt,
				XtPointer 	cbs );

void 	NxmClrW_popdown (	void );

void 	NxmClrW_popdown2 (	Widget		wid );

void 	NxmColorBarCreate ( 	Widget 		parent,
				int    		ncolors,
				Pixel  		color_pixels[],
				Boolean		show_active_color );

void 	NxmPopupColorEdit ( 	Widget    	w,
				long	  	color,
				XEvent	  	*event );

void 	NxmColorbarReset ( 	int 		type );

void 	NxmColorBarReload ( 	int		ncolors,
				Pixel		colorl[] );

void 	NxmColorbarSetBlack (	void );

void 	NxmDisplayCbColorcell ( int 		indx );


void	NxmColorBlinkSet ( 	int     	color_index,
				int     	type );
/*
 *   NxmColorEdit.c
 */
Widget NxmColorEditPopupCreate ( Widget parent, char *popup_name,
				 Pixel colr_pixels[], char *colrname_file,
				  int ncolors );

void 	NxmColorSetCurrent ( 	int      	indx );

void	NxmColorSetSliders (	void );

/*
 *
 */
void 	NxmColorNamelistsCreate(Widget 		parent,
				char   		*colrname_file );


void 	NxmColorPalettCreate ( 	Widget 		parent,
				int    		ncolors );


void  	NxmColorEditSlidersCreate ( Widget 	parent );


int  	NxmLoadColorTable ( 	Widget 		w,
				char   		*filename );

void  	NxmSetColorInTable ( 	int 		indx,
				float 		red, 
				float		green, 
				float		blue );

void  	NxmColorTablePanelCreate ( Widget 	parent );


NxmColrP_t *NxmColrP_create (   Widget          parent,
                                int             num_col,
                                int             orient,
                                XtEventHandler  func );

void 	NxmColrP_deselectAll (  NxmColrP_t      *attr_colr );

Pixel	NxmColrP_getColorPixel( int		which );

void    NxmColrP_setColor (     NxmColrP_t      *attr_colr,
                                int             which );


void 	NxmConfirm_show ( 	Widget		parent,
				char		*message,
				XtCallbackProc	func_ok,
				XtCallbackProc	func_cancel,
				XtPointer	data,
				int		*iret );

Widget 	NxmCtlBtn_create ( 	Widget		parent,
				char		spread_flag,
				char		*name,
				int		n,
				char		*btnstr[],
				XtCallbackProc	callback,
				WidgetList	btnw );


void 	NxmCursor_setCursor ( 	Widget		parent,
				int     	ref );

void 	NxmCursor_createCursor ( int     	ref );

currefTbl_t*  NxmCursor_getRefTbl ( void );

curtypTbl_t*  NxmCursor_getTypTbl ( void );


Widget 	NxmDwell_popupCreate( 	Widget 		parent,
				char   		*dialog_name );

int*   	NxmDwell_getDwellPtr (	void );


Widget 	NxmEnhw_create ( 	Widget 		parent,
				void   		(*func)(void),
				void   		(*func2)(char *) );

void 	NxmEnhw_popup ( 	int 		ityp );

void 	NxmEnhw_update ( 	int 		ityp );

void 	NxmEnhw_setLutfile ( 	int  		ityp,
				char 		*lutfile);

void 	NxmEnhw_getLutfile ( 	int  		ityp,
				char 		*lutfile );


Widget 	NxmErr_createPopup ( 	Widget 		parent );

Widget 	NxmErr_btCreate ( 	Widget		parent );

void	NxmErr_update (		void );


Widget 	NxmExit_create ( 	Widget		parent,
				char		*title,
				char		*message,
				XtCallbackProc	ok_cb,
				XtCallbackProc	cancel_cb );

Widget 	NxmGeneric_show (	Widget		parent,
				char		*title,
				char		*message,
				int		numBtns,
				char		*button[],
				XtCallbackProc	callback );

XmString NxmGeneric_Str2XmString ( char		*string );

Widget  NxmGif_create (		char		*wname,
				Widget		parent );

void    NxmGif_gifWPopup (	void );

int	NxmGmpkInit (		Widget		wid,
				int		mode,
				void		(*init_func)(int *iret) );

void	NxmGmpkRgstr (		Widget		wid,
				char		*name,
				void		(*rgstr_func)(void) );

Widget 	NxmHelp_create ( 	Widget		parent,
				char		*dialogw_name,
				char		*helpw_name,
				char		*hlp_table,
				int		rows,
				int		columns );

void 	NxmHelp_helpBtnCb ( 	Widget		w,
				long		fileid,
				XtPointer	cbs );


void 	NxmInitialize ( 	Widget 		widget );


Widget	NxmLabel_createFrameLbl(char		*label_str,
				Widget		parent_pane,
				Widget		frame_pane );

void 	NxmLabel_getStr ( 	Widget		w,
				char		label[] );

void 	NxmLabel_setStr ( 	Widget		w,
				char		*label );


Widget 	NxmLineA_create ( 	Widget 		parent );

void	NxmLineA_popUp (	_NXMattr 	*attr_copy,
                      		char     	*title_name,
                      		void    	(*apply_func)(void),
                      		Widget  	(*workarea_create)(Widget),
                      		void    	(*workarea_init)(void) );


Widget 	NxmAnimationPanelCreate(Widget 		parent,
				char		*panel_name,
				char		*bgcolr_name,
				char		*fgcolr_name,
				WidgetList	loop_insensitive,
				int		nloop_insensitive,
				XtCallbackProc	callback,
				void            (*display_image)(void) );


int 	NxmQueryAnimationStatus(void );

void 	NxmStopAnimation ( 	void );

void 	NxmRestartAnimation ( 	void );


void    NxmChangePixmapData ( 	int 		current,
				int 		total );

void	NxmLoopButtonCallback ( Widget          w,
				long		which,
				XtPointer	cbs );

void    NxmLoopbuttonSensitive( Boolean	 	state );


Widget 	NxmMarkA_create ( 	Widget 		parent );

void 	NxmMarkA_popup ( 	NxmMarkA_t 	*mk_info, 
				void 		(*apply_func)(void) );

Widget 	NxmMenuPulldownBuild ( 	Widget		parent,
				WidgetList	return_item_w,
				char		*menu_title,
				KeySym		menu_mnemonic,
				_NXMmenuItem	*items );


Widget	NxmPrompt_create ( 	Widget  	parent,
				char    	*title,
				char    	*prompt_string,
				XtCallbackProc	callback );


Widget	NxmPrt_create ( 	char		*wname,
				Widget		parent,
				void		(*print_func)(void) );

void	NxmPrt_prtWPopup ( 	void );

int	NxmPrt_isPrtFlgSet ( 	void );

int	NxmPrt_isPgFlgSet ( 	void );

void	NxmPrt_stopPrt ( 	void );

void    NxmPrt_txtPrtShow (	Widget		parent, 
				char*		fname );


void    NxmPushBtnMenu_create ( Widget          pushb, 
                                long            interval, 
                                char            *items[],
                                int             nitems, 
				XtCallbackProc callback );


void 	NxmRes_check ( 		Display		*dpy,
				char		*resfil,
				char    	*newfil );

void 	NxmScaleA_create ( 	Widget 		parent );

Boolean	NxmScaleA_isUp (	void );

void 	NxmScaleA_popup ( 	NxmScaleA_t 	*sc_info, 
				void 		(*apply_func)(void) );

void	NxmScaleA_updtLat (	void );


Widget	NxmTxtIn_create ( 	Widget  	parent,
				char    	*labelstr,
				int     	nc,
				Widget  	*textw );


void 	NxmVers_showTitle ( 	Widget 		topshell );


Widget  NxmVolcano_menuCreate ( Widget          parent, 
                                Widget          textwid, 
                                XtCallbackProc	push_cb );


int     NxmVolcano_getNum (     void );


void    NxmVolcano_getInfo(     int             indx, 
                                float           *lat, 
                                float           *lon, 
                                char            *name );

void    NxmVolcano_getArea(     int             indx, 
                                char            *area );

void	NxmVolcano_getSmNm(	int		indx, 
				char		*smnm );

void    NxmVolcano_getElev(     int             indx, 
                                float           *elev );

int     NxmVolcano_getIdx (     char            *name );

void    NxmWarn_show (          Widget          parent,
                                char            *message );


#endif	/* PROTO_NXMLIB */

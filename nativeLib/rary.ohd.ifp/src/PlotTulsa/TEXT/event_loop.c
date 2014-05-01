/* File: event_loop.c
 *
 *  Determines which window events need to be processed.
 */
/*      Modified by:    A. Vo                                            */
/*                      May 02 2004                                      */
/*                      Added resetDone() to destroy the main_plot_shell */ 
/*                      when user did not click on "done" to close the   */
/*                      the mod plot window                              */
/* ----------------------------------------------------------------------*/                     
#include "menus.h"
#include "cex25.h"
#include "ifp_struct.h"
#include "plot.h"

#include "libXifp.h"
#include "ctype.h"
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include "libXs.h"
#include "mods.h"
#include "mod_struct.h"
#include "ifp_atoms.h"
#include "ifp_help.h"
#include "ifp_globals.h"
#include "event_loop_exit_status.h"
#include "Mods_everythingStruct.h"

/* AV added  for TSPlot 2/2002*/
#include "TSPlots.h"
#include "TSWidget.h"
#include "TSCallbacks.h"
#include "TSUtils.h"
#include "dhm_mods.h"

Widget  create_opTable_viewer();
Widget  create_rc_plot();

extern char* create_dhm_mod_object();
extern int is_dhm_mod_available();

extern void Write_mods(Mods_everythingStruct *);
extern void post_gif_done_atom(int *);
extern int  get_show_mods_viewer_atom(Widget);
extern void save_dhm_mod(Mods_everythingStruct *);
extern void popdownModGui();
extern void show_MainTSDS();
extern void toggleBManageShellCB(Widget, Mods_everythingStruct *, XtPointer);

extern  PLOTMGR   *PlotMgr; 
extern int jvmTokenNotExists;
extern int dhmModsSelected;
void resetDone(Mods_everythingStruct *data);
int UhgFlag = 0;/*is UHGCDATE plot in display?*/
int event_loop(tables_data, plot_data, modShell,
	       p_float, p_char, t_int, ts_float, c_float, modsDataStruct_ptr)

   tables_cb_struct      *tables_data;                  /* call back table data pointer */
   plot_cb_struct        *plot_data;                    /* call back data structure  */
   Widget		 modShell;
   float                 *p_float;
   char                  p_char[][4];
   int                   *t_int;
   float                 *ts_float;
   float                 *c_float;
   Mods_everythingStruct *modsDataStruct_ptr;
{
   XEvent          event;
   Display         *display;
   Window          root;
   int             type;                                /* type of data stored in the window property   */
   int             format;                              /* format of the stored data                    */
   int             nitems;                              /* number of bytes retrieved                    */
   int             left;                                /* remaining bytes stored in the window         */
   long            offset = 0;                          /* property starting point data                 */
   int             *showTulsaPlot;                      /* Tulsa Plot window property data pointer      */
   int             *showTulsaTable;                     /* Tulsa Table window property data pointer     */
   int             *showPlotTS;                         /* TS Plot window property data pointer      */
   int             *showOperationsTable;                /* OperationsTable window property data pointer */
   int             *showRatingCurve;                    /* Rating Curve window property data pointer    */
   Widget          showOperationsTable_widget=NULL;
   Widget          showRatingCurve_widget=NULL;
   
   Widget          tmpmodsShell = NULL;

   int             stay_in_loop, exit_status;           /* flags */
   char            *rerun_segment, *next_segment, *goto_upstream_segment; /* segment pointers */
   char            *continue_to_next_operation;         /* continue_to_next_operation data pointer */
   int             *quit_NWSRFS;                        /* quit_NWSRFS data pointer */

   int             j;                     /* counters */
   int             *showMods;             /* showMods data pointer */
   int             showModsViewer;       /* Mods Viewer window property */
   int             *ok_to_quit_mods;      /* ok_to_quit_mods data pointer */
   int             *ok_to_write_mods;     /* ok_to_write_mods data pointer */
   int             entering_eventLoop;    /* entering_eventLoop flag */
   int             number_of_keycodes;

   KeySym          key_sym [] = {XK_F1, XK_F2, XK_F3, XK_F4, XK_F5, XK_F6,
				XK_F7, XK_F8, XK_F9, XK_F10, XK_F11, XK_F12,
				XK_Up, XK_Down, XK_Left, XK_Right,
				XK_Shift_L, XK_Shift_R, XK_Caps_Lock, XK_Alt_L,
				XK_Alt_R, XK_Control_L, XK_Control_R};
   KeyCode         key_code[XtNumber(key_sym)];   /* key code arrary */
   
   int             *first_plot;           /* flag for first plot in segment */
   int             save_gif_on;           /* flag for if saving gif files  */
   int             *gif_done;             /* flag for IFPA_gif_done atom */
/**********************************************************************************************/   
 
 entering_eventLoop = TRUE; 
 
 display = XtDisplay(modShell);
 root = DefaultRootWindow(display);

/*  set up things for saving gif files */
 first_plot = (int*)malloc(sizeof(int));
 gif_done   = (int*)malloc(sizeof(int));
  
 *gif_done = FALSE; 
 post_gif_done_atom(gif_done);

                        
/*      Convert the XString keycode names to X Keycodes...       */
 for(j = 0; j < XtNumber(key_sym); j++)
	key_code[j] = XKeysymToKeycode(display, key_sym[j]);

 number_of_keycodes = j;

 stay_in_loop = TRUE;

/* post_nwsrfs_eventloop_atom(); 
   need to check why this was not used in the new mods code
 */

 XChangeProperty(
		 display,
		 root,
		 IFPA_entering_NWSRFS_eventLoop,
		 IFPA_entering_NWSRFS_eventLoop_type,
		 8,
		 PropModeReplace,
		 (unsigned char *)&entering_eventLoop,
		 sizeof(int)
		);

 while(stay_in_loop)
 {
        /*save mod info. from dhm mod java gui if dhm mods (DPRECIP or DSAC)  */
	/*is selected from Mods Pull down menu and JVM is enabled*/
	
        if(jvmTokenNotExists == 1 && dhmModsSelected == 1)
        {
	    if (is_dhm_mod_available())
	    {
	        save_dhm_mod(modsDataStruct_ptr);
 	        popdownModGui();
		dhmModsSelected = 0;
            }
	}
        /*AiV 5/13/04  initialize doneclick if uhg,uhgd,rochng and rrichng 
        display is not activated */
        
        if(modsDataStruct_ptr->create_flag == 1){
           modsDataStruct_ptr->mp_doneClick = 0;
        }
        /*end Aiv  */
	XtNextEvent(&event);

	/* ---------------------------------------------------------------------------- */
	/* 'IsContextHelpWindowDown' is a global flag for Context Sensitive Help:       */
	/*      it's used to test if we've passed through 'destroy_help_message'        */
	/* for the destruction of the help_shell... this kludge is needed to avoid an   */
	/* XProtocol error for a bad windowID when a ButtonRelease event is spawned by  */
	/* releasing the mouse button outside of the widget we just got help on;        */
	/* it's a nasty thing that happens when you want context-sensitive help on      */
	/* Pulldown & Option menus...                                                   */

	if((event.type == ButtonRelease) &&
	   (IsContextHelpWindowDown == TRUE)) event.xbutton.window = root;
	IsContextHelpWindowDown = FALSE;
	/* ---------------------------------------------------------------------------- */



	switch(event.type)
	{
       
/* **************************************************************************************************** */
	   case PropertyNotify:

      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  rerun_segment  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
	      if(event.xproperty.window ==
		 root && event.xproperty.atom == IFPA_rerun_segment)
	      {
		 if(XGetWindowProperty(
				       display,
				       root,
				       IFPA_rerun_segment,
				       offset,
				       (long) 8,
				       FALSE,
				       (Atom)IFPA_rerun_segment_type,
				       (Atom *)&type,
				       (int *)&format,
				       (unsigned long *)&nitems,
				       (unsigned long *)&left,
				       (unsigned char **)&rerun_segment
				      ) == Success && type == IFPA_rerun_segment_type)
		    {
                      /* printf("rerun_segment = %s\n", rerun_segment); */
		       if(saccoScaleDS != NULL)
                       { 
                          
			  XtDestroyWidget(saccoScaleDS);
                          saccoScaleDS = NULL;
                       }
		       stay_in_loop = FALSE;
                       
		       exit_status = Control_RERUN;
                       resetDone(modsDataStruct_ptr);
                       if(jvmTokenNotExists == 1 && dhmModsSelected == 1)
                       {
                            popdownModGui();
		       }   
		      
		    }
	      }

      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<<  next_segment  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
	      else if(event.xproperty.window ==
		 root && event.xproperty.atom == IFPA_next_segment)
	      {
		 if(XGetWindowProperty(
				       display,
				       root,
				       IFPA_next_segment,
				       offset,
				       (long) 8,
				       FALSE,
				       (Atom)IFPA_next_segment_type,
				       (Atom *)&type,
				       (int *)&format,
				       (unsigned long *)&nitems,
				       (unsigned long *)&left,
				       (unsigned char **)&next_segment
				      ) == Success && type == IFPA_next_segment_type)
		    {
                       save_gif_on = get_save_gif_atom(modShell); 
                       if(save_gif_on) 
                       {
                          *gif_done = FALSE;
                          post_gif_done_atom(gif_done);
                          
                          save_ifp_gif_file(plot_data);
                          
                          *gif_done = TRUE;
                          post_gif_done_atom(gif_done);
                       }
                         
		       if(saccoScaleDS != NULL)
                       { 
                          
			  XtDestroyWidget(saccoScaleDS);
                          saccoScaleDS = NULL;
                       }
                                          
		       stay_in_loop = FALSE;
		       exit_status = Control_NEXT;
                       resetDone(modsDataStruct_ptr);
                       if(jvmTokenNotExists == 1 && dhmModsSelected == 1)
                       {
                            popdownModGui();
		       }
		       
		    }
	      }

      /*  <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  goto_upstream_segment  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
	      else if(event.xproperty.window ==
		 root && event.xproperty.atom == IFPA_goto_upstream_segment)
	      {
		 if(XGetWindowProperty(
				       display,
				       root,
				       IFPA_goto_upstream_segment,
				       offset,
				       (long) 8,
				       FALSE,
				       (Atom)IFPA_goto_upstream_segment_type,
				       (Atom *)&type,
				       (int *)&format,
				       (unsigned long *)&nitems,
				       (unsigned long *)&left,
				       (unsigned char **)&goto_upstream_segment
				      ) == Success && type == IFPA_goto_upstream_segment_type)
		    {
                       /* printf("goto_upstream_segment = %s\n", goto_upstream_segment); */
		       if(saccoScaleDS != NULL)
                       { 
                          
			  XtDestroyWidget(saccoScaleDS);
                          saccoScaleDS = NULL;
                       }
                     
		       stay_in_loop = FALSE;
		       exit_status = Control_GO_UPSTREAM;
                       resetDone(modsDataStruct_ptr);
                       if(jvmTokenNotExists == 1 && dhmModsSelected == 1)
                       {
                            popdownModGui();
		       }
		       
                       
		    }
	      }

      /*  <<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  continue_to_next_operation  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>> */
	      else if(event.xproperty.window ==
		 root && event.xproperty.atom == IFPA_continue_to_next_operation)
	      {
		 if(XGetWindowProperty(
				       display,
				       root,
				       IFPA_continue_to_next_operation,
				       offset,
				       (long) 8,
				       FALSE,
				       (Atom)IFPA_continue_to_next_operation_type,
				       (Atom *)&type,
				       (int *)&format,
				       (unsigned long *)&nitems,
				       (unsigned long *)&left,
				       (unsigned char **)&continue_to_next_operation
				      ) == Success && type == IFPA_continue_to_next_operation_type)
		    {
                       /* printf("continue_to_next_operation = %s\n", 
                                  continue_to_next_operation); */
                                  
                       /* move reset of first_plot atom outside of
                          the if(save_giv_on) so it will always
                          get reset - for use with rerun_plot_num
                          dp - 21 Feb. 1997
                       */                       
		       *first_plot = FALSE;
		       post_first_plot_atom(first_plot);
           
                       save_gif_on = get_save_gif_atom(modShell);                        
                       if(save_gif_on) 
                       {
                          *gif_done = FALSE;
                          post_gif_done_atom(gif_done);
                          
                          save_ifp_gif_file(plot_data);
                          
                          *gif_done = TRUE;
                          post_gif_done_atom(gif_done);                          
                       }
                                  
		       if(saccoScaleDS != NULL)
                       { 
                          
			  XtDestroyWidget(saccoScaleDS);
                          saccoScaleDS = NULL;
                       }
                      
		       stay_in_loop = FALSE;
		       exit_status = Control_CONTINUE;
                       resetDone(modsDataStruct_ptr);
                       if(jvmTokenNotExists == 1 && dhmModsSelected == 1)
                       {
                            popdownModGui();
		       }
		       
                       
		    }
	      }

      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  quit_NWSRFS  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>> */
	      else if(event.xproperty.window ==
		 root && event.xproperty.atom == IFPA_quit_NWSRFS)
	      {
		 if(XGetWindowProperty(
				       display,
				       root,
				       IFPA_quit_NWSRFS,
				       offset,
				       (long) sizeof(int),
				       FALSE,
				       (Atom)IFPA_quit_NWSRFS_type,
				       (Atom *)&type,
				       (int *)&format,
				       (unsigned long *)&nitems,
				       (unsigned long *)&left,
				       (unsigned char **)&quit_NWSRFS
				      ) == Success && type == IFPA_quit_NWSRFS_type)
		    {
                       /* printf("quit_NWSRFS = %d\n", *quit_NWSRFS); */
                       
                       save_gif_on = get_save_gif_atom(modShell); 
                       if(save_gif_on) 
                       {
                          *gif_done = FALSE;
                          post_gif_done_atom(gif_done);
                          
                          save_ifp_gif_file(plot_data);
                          
                          *gif_done = TRUE;
                          post_gif_done_atom(gif_done);
                       }
                       
                       if(saccoScaleDS != NULL)
                       { 
                          
			  XtDestroyWidget(saccoScaleDS);
                          saccoScaleDS = NULL;
                       }
                       
		       stay_in_loop = FALSE;
		       exit_status = Control_QUIT;
                       if(jvmTokenNotExists == 1 && dhmModsSelected == 1)
                       {
                            popdownModGui();
		       }
		       
		    }
	      }

      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  showTulsaTable  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>> */
	      else if(event.xproperty.window ==
		 root && event.xproperty.atom == IFPA_show_tables)
	      {
		 if(XGetWindowProperty(
				       display,
				       root,
				       IFPA_show_tables,
				       offset,
				       (long) sizeof(int),
				       FALSE,
				       (Atom)IFPA_show_tables_type,
				       (Atom *)&type,
				       (int *)&format,
				       (unsigned long *)&nitems,
				       (unsigned long *)&left,
				       (unsigned char  **)&showTulsaTable
				      ) == Success && type == IFPA_show_tables_type)
		    {
		       if(*showTulsaTable) XtMapWidget(tables_data->tultable);
		       else                XtUnmapWidget(tables_data->tultable);
		    }
	      }

      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  showTulsaPlot  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
	      else if(event.xproperty.window ==
		      root && event.xproperty.atom == IFPA_show_tulsa_plot)
	      {
		 if(XGetWindowProperty(
				       display,
				       root,
				       IFPA_show_tulsa_plot,
				       offset,
				       (long) sizeof(int),
				       FALSE,
				       (Atom)IFPA_show_tulsa_plot_type,
				       (Atom *)&type,
				       (int *)&format,
				       (unsigned long *)&nitems,
				       (unsigned long *)&left,
				       (unsigned char **)&showTulsaPlot
				      ) == Success && type == IFPA_show_tulsa_plot_type)
		    {
		       if(*showTulsaPlot) XtMapWidget(plot_data->main_plot_shell);
		       else               XtUnmapWidget(plot_data->main_plot_shell);
		    }
	      }

      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  showPlotTS  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
	      else if(event.xproperty.window ==
		      root && event.xproperty.atom == IFPA_show_plot_TS)
	      {
		 if(XGetWindowProperty(
				       display,
				       root,
				       IFPA_show_plot_TS,
				       offset,
				       (long) sizeof(int),
				       FALSE,
				       (Atom)IFPA_show_plot_TS_type,
				       (Atom *)&type,
				       (int *)&format,
				       (unsigned long *)&nitems,
				       (unsigned long *)&left,
				       (unsigned char **)&showPlotTS
				      ) == Success && type == IFPA_show_plot_TS_type)
		    {
                        /* Popup Plot TS window   AV 3/6/2002 */
                       if(*showPlotTS == 1)
                             show_MainTSDS(modShell);
                        
		        else
                            if (TSMainDS != NULL) XtUnmanageChild(TSMainDS);
                          
                        
	            }
               }

      /*  <<<<<<<<<<<<<<>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  showMods  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<>>>>>>>>>>>>>>>>> */
	       else if(event.xproperty.window ==
			root && event.xproperty.atom ==
			IFPA_display_selected && testForTSPopup != YES) break;

	       else if(event.xproperty.window == root &&
		       event.xproperty.atom == IFPA_show_mods)
		       {
		       if(XGetWindowProperty(
			       display,
			       root,
			       IFPA_show_mods,
			       offset,
			       (long) sizeof(int),
			       FALSE,
			       (Atom)IFPA_show_mods_type,
			       (Atom *)&type,
			       (int *)&format,
			       (unsigned long *)&nitems,
			       (unsigned long *)&left,
			       (unsigned char **)&showMods
			       ) == Success && type == IFPA_show_mods_type)
			   {
			    if(*showMods) XtManageChild(modShell);
			    else          XtUnmanageChild(modShell);
			   }
		       }

      /*  <<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<  showModsViewer  >>>>>>>>> */
      /*  added to handle events associated */
      /*  with the Mods Viewer being        */
      /*  available from the main mods menu */
      /*  dp - 1 May 1997                   */
      /*  <<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>> */
	       else if(event.xproperty.window == root &&
		       event.xproperty.atom == IFPA_show_mods_viewer)
		       {
		          showModsViewer = get_show_mods_viewer_atom(global_toplevel);
			  if(showModsViewer)
			     XmToggleButtonSetState(modsDataStruct_ptr->widgetData->newModsToggle, TRUE, FALSE);
			  else  
			     XmToggleButtonSetState(modsDataStruct_ptr->widgetData->newModsToggle, FALSE, FALSE);
		          toggleBManageShellCB(global_toplevel, modsDataStruct_ptr, NULL);
		       }

      /*  <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  show_operations_table  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */

	       else if(event.xproperty.window == root &&
		       event.xproperty.atom == IFPA_show_operations_table)
		       {
		       if(XGetWindowProperty(
			       display,
			       root,
			       IFPA_show_operations_table,
			       offset,
			       (long) sizeof(int),
			       FALSE,
			       (Atom)IFPA_show_operations_table_type,
			       (Atom *)&type,
			       (int *)&format,
			       (unsigned long *)&nitems,
			       (unsigned long *)&left,
			       (unsigned char **)&showOperationsTable
			       ) == Success && type == IFPA_show_operations_table_type)
			   {
			    if(*showOperationsTable)
			      {
			       showOperationsTable_widget =
					create_opTable_viewer(modShell, p_float,
							      p_char, t_int, ts_float, 
							      c_float);
			      }
			    else if(showOperationsTable_widget != NULL)
				   {
				    XtDestroyWidget(showOperationsTable_widget);
				    showOperationsTable_widget = NULL;
				   }
			   }
		       }

      /*  <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  show_rating_curve >>>>>>>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */

	       else if(event.xproperty.window == root &&
		       event.xproperty.atom == IFPA_show_rating_curve)
		       {
		       if(XGetWindowProperty(
			       display,
			       root,
			       IFPA_show_rating_curve,
			       offset,
			       (long) sizeof(int),
			       FALSE,
			       (Atom)IFPA_show_rating_curve_type,
			       (Atom *)&type,
			       (int *)&format,
			       (unsigned long *)&nitems,
			       (unsigned long *)&left,
			       (unsigned char **)&showRatingCurve
			       ) == Success && type == IFPA_show_rating_curve_type)
			   {
			    if(*showRatingCurve)
			      {
			       showRatingCurve_widget =
					     create_rc_plot(plot_data->rc_data);
			      }
			    else if(showRatingCurve_widget != NULL)
				   {
				    XtDestroyWidget(showRatingCurve_widget);
				    showRatingCurve_widget = NULL;
				   }
			   }
		       }

      /*  <<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  ok_to_write_mods  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>> */
/*
	       else if(event.xproperty.window == root &&
		       event.xproperty.atom == IFPA_ok_to_write_mods)
		       {
		       if(XGetWindowProperty(
			       display,
			       root,
			       IFPA_ok_to_write_mods,
			       offset,
			       (long) sizeof(int),
			       FALSE,
			       IFPA_ok_to_write_mods_type,
			       &type,
			       &format,
			       &nitems,
			       &left,
			       &ok_to_write_mods
			       ) == Success && type == IFPA_ok_to_write_mods_type)
			   {
			    if(*ok_to_write_mods) Write_mods(modsDataStruct_ptr);
			   }
		       }
 */
      /*  <<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  ok_to_quit_mods  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>> */
	       else if(event.xproperty.window == root &&
		       event.xproperty.atom == IFPA_mods_quit_ok)
		       {
		       if(XGetWindowProperty(
			       display,
			       root,
			       IFPA_mods_quit_ok,
			       offset,
			       (long) sizeof(int),
			       FALSE,
			       (Atom)IFPA_mods_quit_ok_type,
			       (Atom *)&type,
			       (int *)&format,
			       (unsigned long *)&nitems,
			       (unsigned long *)&left,
			       (unsigned char **)&ok_to_quit_mods
			       ) == Success && type == IFPA_mods_quit_ok_type)
			   {
			    if(*ok_to_quit_mods)
				    XtUnmapWidget(modShell);
			   }
		       }

      /*  <<<<<<<<<<<<<<<>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  default  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<>>>>>>>>>>>>>>> */
	      else
		 {
                  
		  XtDispatchEvent(&event);
                  
		 }
	      break;

	   default:
		   XtDispatchEvent(&event);
		   break;

	   }    /*  End of 'case' statements for the event types... */
   }    /*  End of while loop */

  if(showRatingCurve_widget != NULL)
  {
    XtDestroyWidget(showRatingCurve_widget);
    showRatingCurve_widget = NULL;
  }

  if(showOperationsTable_widget != NULL)
  { XtDestroyWidget(showOperationsTable_widget);
    showOperationsTable_widget=NULL;
  }

  if(tables_data->tultable != NULL) 
  {
    XtDestroyWidget(tables_data->tultable);
    tables_data->tultable = NULL;/*Added by AV */
  }
  
  if(plot_data->main_plot_shell != NULL)
  {
     XtDestroyWidget(plot_data->main_plot_shell);
     plot_data->main_plot_shell = NULL;/*Added by AV */
  }  
 
   
/**  AV Linux port-- causing NEXT /RERUN selection would not run */  
/*if(XtParent(modShell) != NULL)
   XtDestroyWidget(XtParent(modShell)); *** End AV */ 

/*pop down ifp "Other mod" window */

  XtUnmanageChild(modsDataStruct_ptr->widgetData->ifp_modsMessageBox);/*Added by AV */




 
                                                /* We need to do this to keep from having	*/
						/* phantom ShellWidgets hanging around - (TEA)	*/
						/* 10/12/94...					*/
  if(modsDataStruct_ptr->viewerWidgets->viewerShell != NULL)
  {
     XtDestroyWidget(modsDataStruct_ptr->viewerWidgets->viewerShell);
     modsDataStruct_ptr->viewerWidgets->viewerShell = NULL;/*Added by AV */
  }

  free(first_plot);
  free(gif_done);

/* XFlush(XtDisplay(global_toplevel));          */
/*
 * if     (exit_status == Control_RERUN) printf("leaving event_loop because of Rerun\n");
 * else if(exit_status == Control_NEXT) printf("leaving event_loop because of Next\n");
 * else if(exit_status == Control_CONTINUE) printf("leaving event_loop because of Continue\n");
 * else if(exit_status == Control_GO_UPSTREAM) printf("leaving event_loop because of Go Upstream\n");
 * else if(exit_status == Control_QUIT) printf("leaving event_loop because of Quit\n");
 * else printf("leaving event_loop, exit_status = %d\n", exit_status);
 */
 return (exit_status);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/event_loop.c,v $";
 static char rcs_id2[] = "$Id: event_loop.c,v 1.12 2006/03/28 20:43:44 aivo Exp $";}
/*  ===================================================  */

}
/*    handles UHG, UHGCHNG, RRICHNG and ROCHNG display window     */
/*    when user did not click on "done" button          5/3/04 AiV*/
void resetDone(Mods_everythingStruct *data)
{
 
 if ( data->create_flag == 1 &&
        data->mp_doneClick == 0){                           
    XtDestroyWidget(data->modsPlotData->main_plot_shell);
    data->modsPlotData->main_plot_shell = NULL;
/*    XtUnmanageChild(data->modsPlotData->main_plot_shell);  */
    data->create_flag = 0;
    data->mp_doneClick = 1;
    UhgFlag = 0;
 }
 else
 {
    data->create_flag = 0;
    data->mp_doneClick = 1;
    UhgFlag = 0;
 }
}

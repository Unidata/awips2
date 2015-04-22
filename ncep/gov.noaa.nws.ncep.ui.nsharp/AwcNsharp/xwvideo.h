	/* XWVIDEO function Prototypes */
#include	<xwcmn.h>

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <sys/stat.h>
#include <X11/Intrinsic.h>
#include <X11/X.h>
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/DialogS.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Xm.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/FileSB.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/DrawingA.h>
#include <Xm/Separator.h>
#include <Xm/MessageB.h>
#include <Xm/Scale.h>
#include <X11/cursorfont.h>
#include <Xm/CascadeB.h>
#include <Xm/LabelG.h>
#include <Xm/SeparatoG.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushBG.h>
#include <Xm/PanedW.h>
 
	typedef enum _PTRVIS { SHOW = 1, HIDE } PTRVIS;

        void show_precippage4( void );
        void show_heavypcpn( void );
        void show_preciptype( void );
        void show_heavysnow( void );

	void edit_skewt( short pagenum );
	void clear_paramarea( void );
	void show_skewtpage1( void );
	void show_hodopage2( void );
	void show_stormpage3( void );
	void show_shear( void );
	void show_meanwind( void );
	void show_srdata( void );
	void show_page( short page );
	void checkit( float pres );
	short check_vico( void );
	void reset_video( void );
	void set_video( void );
	short set_font( short siz );
	void draw_skewt( void );
	void hpdrawline( short x1, short y1, short x2, short y2, short wid);
	short temp_to_pix( float temp, float pres );
	float pix_to_temp( short x, short y );
	short pres_to_pix( float pres );
	float pix_to_pres( short pix );
	void dry_adiabat( float thta);
	void moist_adiabat(float thetae);
	void mixingratio(float w);
	void isotherm( float temp);
	void isobar( float pres, short flag);
	void trace_temp( short width );
        void trace_temp2(short width);
	void trace_vtmp( short width );
	void trace_wetbulb( short width );
	void trace_dwpt( short width );
	void trace_dwpt2( short width );
	void trace_parcel(float pres, float temp, float dwpt);
	void draw_hodo( void );
	void hodo_to_pix( float dir, float mag, short *x, short *y );
	void pix_to_hodo( short x, short y, float *dir, float *mag );
	void trace_hodo( short width );
	void skewt_params( void );
	void plot_barbs( void );
	void wind_barb( float wdir, float wspd, short x, short y, short siz );
	void make_screen( int argc, char *argv[] );
	void interactive( void );
	void skewt_cursor_data( short x, short y );
	void icing_turb_cursor_data( short x, short y);
	void hodo_cursor_data( short x, short y );
	short switch_modes( short mode );
	void redraw_graph( short mode );
	void show_parcel( void );
	void show_moisture( void );
	void show_instability( void );
	void show_thermoparms( void );
	void show_stormtype( void );
	void disp_param( char *value, short rcol, short rlin);
	void grab_window( short x1, short y1, short x2, short y2, char *buffer);
	void put_window( short x1, short y1, char *buffer);
	void parcel_options( void );
	void general_options( void );
	void reset_options( short mode, short pagenum );
	void print_options( void );

	void inset_options( short mode );
	void draw_hoinset( void );
	void draw_skinset( void );
	void plot_vis( void );
	void vis_xy( float x, float y);
	void write_vis_data( float tim, float ang );
	void plot_storminflow( void );
	void plot_vertsrw(void);
	short vert_coords( float hgt, float maxhgt );
	void plot_thetae(void);
	void show_initiation( void );
	void show_hailpot( void );
	void show_severe( void );
	void Define_Print_Option( short hitnum);
	void setlinestyle ( short style, short width );
	void setcolor ( int color );
	void rectangle ( int type, short x, short y, short width, short height );
	void ellipse ( int type, short x, short y, short width, short height );
	void moveto ( short x, short y );
	void lineto ( short x, short y );
	void setcliprgn (short tlx, short tly, short brx, short bry);
	void print_graphic (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void reset_graphic (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void parcel_popup (Widget w );
	void inset_graphic (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void interp_data (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void option_graphic (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void expose_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data );
        void resize_callback (Widget w, XtPointer *data,
                              XmDrawingAreaCallbackStruct *call_data);
	void position_cursor (Widget w, XtPointer *data, XEvent *event);
        void pointer_update (Widget w, XtPointer *call_data, XEvent *event );
        void update_pointer (Widget w, XtPointer *call_data, XEvent *event );
        void redraw_sounding (Widget w, XtPointer *call_data, XEvent *event );
	void menu_main (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void page_next (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void tog_graph (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void display_SKEWT (Widget w, XmDrawingAreaCallbackStruct *call_data);
	void display_HODO (Widget w, XmDrawingAreaCallbackStruct *call_data);
	void display_ICG (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void display_TURB (Widget w, XmDrawingAreaCallbackStruct *call_data );
        void display_Clouds(Widget w, XmDrawingAreaCallbackStruct *call_data);
	void Toggle_Callback(Widget w, int which, caddr_t call_data );
	void outgtext ( char *st, int x, int y );
	void outtext ( char *st, int x, int y );
	char * itoa ( int value, char *st, int radx );
	GC xs_create_xor_gc(Widget w, char *color);
	void OwnMainLoop (XtAppContext app );
        void load_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void about_cb (Widget w, XtPointer client_data, XtPointer call_data );
        void modellist_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void sharp_load ( Widget w, XtPointer client_data, XtPointer call_data );
	void show_gem_info ( Widget w );
	void show_profiler_vad_info(Widget w);
	void show_model_info ( Widget w );
        void show_pfs_model_info (Widget w);
	void get_gemfile ( Widget w, XtPointer client_data, XtPointer call_data );
        void load_gemfile ( Widget w, XtPointer client_data, XtPointer call_data );
	void get_gemfile_text ( Widget w, XtPointer client_data, XtPointer call_data );
	void get_mdlfile ( Widget w, XtPointer client_data, XtPointer call_data );
	void load_gemfile ( Widget w, XtPointer client_data, XtPointer call_data );
	void set_gempak_file ( Widget w, XtPointer client_data, XtPointer call_data );
	void xsection_hght_cb ( Widget w, XtPointer client_data, XtPointer call_data);
	void psection_hght_cb (Widget w, XtPointer client_data, XtPointer call_data);
	void stationlist_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void Load_stationlist (int list);
	void Load_gem_sounding ();
	void print_soundings ();
	void Load_mdl_sounding ();
        void Load_point_sounding ();
        void show_point_info ( Widget w );
	void sta_select_cb (int which_sta );
	void time_select_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void pv_select_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void gem_info_ok_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void gem_info_cancel_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void gem_info_world_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void gem_info_US_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void gem_info_TROP_SFC_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void gem_info_OFAGX_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void gem_info_PAC_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void gem_info_ATL_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void gem_info_zoom_cb ( Widget w, XtPointer client_data, XtPointer call_data );	
        void gem_info_unzoom_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void gem_info_ICEMAP_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	    void gem_info_STABMAP_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void icemap_info_zoom_cb (Widget w, XtPointer client_data, XtPointer call_data );
        void stabmap_info_print_cb (Widget w, XtPointer client_data, XtPointer call_data );		
        void icemap_info_unzoom_cb (Widget w, XtPointer client_data, XtPointer call_data );
        void DrawIcingData(Display *dsp, Window win, GC gc,int xc,int yc,int bot,int top,
             int ints,int type, int prob,int sgn);
        void icemap_info_exit_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	    void stabmap_info_exit_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void GenIceMap();
		void GenStabMap();
	void parcel_cancel_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void mdl_info_ok_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void mdl_info_cancel_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void pfs_mdl_info_cancel_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void mdl_info_world_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void mdl_info_US_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void mdl_info_TROP_SFC_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void mdl_info_OFAGX_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void mdl_info_ATL_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void mdl_info_PAC_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	
	void mdl_info_zoom_cb ( Widget w, XtPointer client_data, XtPointer call_data );	
        void mdl_info_unzoom_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void pfs_mdl_info_zoom_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void pfs_mdl_info_unzoom_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void pfs_mdl_info_US_cb (Widget w, XtPointer client_data, XtPointer call_data);
	void modellist_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void set_mdlsta ( Widget w, XtPointer client_data, XtPointer call_data );
	void set_pvsounding ( Widget w, XtPointer client_data, XtPointer call_data );
	void set_user_level ( Widget w, XtPointer client_data, XtPointer call_data );
	void mtime_select_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void pfsmtime_select_cb(Widget w, XtPointer client_data,
			       XtPointer call_data);
	void load_mdldata ( Widget w, XtPointer client_data, XtPointer call_data );
	void load_pvsounding ( Widget w, XtPointer client_data, XtPointer call_data );
	void LoadSingleModelSounding(int sub);
	void save_origsndg ();
	void restore_origsndg ();
	void show_about_info ( Widget w );
	void X_Init (int argc, char *argv[]);
        void vvel_profile();
        void clean_uvvs(void);
        void show_profiler_info ( Widget w );
        void show_acars_info ( Widget w );
        void show_torpot(void);
        void update_time_reg (Widget w, XtPointer *call_data, XEvent *event);
        void getModelForTime (Widget w, XtPointer *call_data, XEvent *event);
        static void getTimeOper (Widget w, XtPointer *call_data, XEvent *event);
        static void LoopForward (XtPointer w, XtIntervalId* timer);
        static void LoopBackward (XtPointer w, XtIntervalId* timer);
        void expose_time_reg_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data);
        void resize_time_reg_overlays (Widget w, XtPointer *data, XmDrawingAreaCallbackStruct *call_data);
        void expose_time_oper_reg_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data);
        void resize_time_oper_reg_overlays (Widget w, XtPointer *data, XmDrawingAreaCallbackStruct *call_data);
        void expose_icemap_station_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data);
        void resize_icemap_station_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data);
        void expose_pvmap_station_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data);
        void resize_pvmap_station_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data);
        float F1(float);
        float F2(float);
        float F3(float);
        int getCloudAmount(float temp,float dd); 
        void scaleCB(Widget widget,XtPointer client_data,XtPointer call_data);
        void pv_info_exit_cb ( Widget w);
        void pv_info_update_cb (Widget w);
        void Make_Gif (Widget w, XtPointer *data,XmDrawingAreaCallbackStruct *call_data);
        void GifFileOKCallback(Widget fsb,XtPointer client_data,XtPointer call_data);
        void cvrttitletofilename(char *raobtitle,char *fname, char *ftype);
        static void UpdatePVDisplay (XtPointer w, XtIntervalId* timer);
        void print_planview_indices (Widget w, XmDrawingAreaCallbackStruct *call_data);
        void printctrl_toggleCB(Widget widget, XtPointer client_data,
          XtPointer call_data);
		void stabmap_toggleCB(Widget widget, XtPointer client_data,
		  XtPointer call_data);
		Widget CreateRadioButton(Widget parent, char* name,
		  XtCallbackProc callback,
		  XtPointer client_data);
        void set_g_TvParcelCor(int value);
        int  get_g_TvParcelCor();
        void set_g_Parcel(int value);
        void setg_pfs_model_sw(int value);
        void Load_pfs_mdl_sounding ();

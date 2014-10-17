	/* XWVIDEO function Prototypes */
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
#include <Xm/SelectioB.h>
#include "xwcmn.h"
 
	typedef enum _PTRVIS { SHOW = 1, HIDE } PTRVIS;

       	void show_precippage4( void );
      	void show_heavypcpn( void );
       	void show_preciptype( void );
       	void show_heavysnow( void );
	void show_posnegareas(void);
	void best_guess_ptype(short txtlin, short txtrow);
	void edit_skewt( short pagenum );
	void clear_paramarea( void );
	void show_skewtpage1( void );
	void show_hodopage2( void );
	void show_stormpage3( void );
	void show_partialthicknesses();
	void show_initialphase();
	void show_winterpage5();
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
	void isotherm( float temp, short colr);
	void highlight_temp(float temp, short colr);
	void mark_temp(short colr);
	void isobar( float pres, short flag);
	void trace_temp( short width, short colr );
	void trace_temp2(short width);
	void trace_vtmp( short width );
	void trace_wetbulb( short width );
	void trace_dwpt( short width, short colr );
	void trace_dwpt2( short width);
	void trace_parcel(float pres, float temp, float dwpt);
	void draw_hodo( void );
	void hodo_to_pix( float dir, float mag, short *x, short *y );
	void pix_to_hodo( short x, short y, float *dir, float *mag );
        void trace_esrh(void);	
        void trace_bndry(float dir, float spd);	
	void trace_hodo( short width );
	void skewt_params( void );
	void plot_barbs( void );
	void wind_barb( float wdir, float wspd, short x, short y, short siz );
	void make_screen( int argc, char *argv[] );
	void interactive( void );
	void skewt_cursor_data( short x, short y );
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
	void vis_xy( float x, float y, short colrx, int ulx, int uly, int vwid);
	void write_vis_data( float tim, float ang, int ulx, int uly, int vwid );
	void plot_storminflow( void );
	void plot_vertsrw(void);
	short vert_coords( float hgt, float maxhgt, int tly );
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
	void view_textwin (Widget w, XmDrawingAreaCallbackStruct *call_data );
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
	void page_next (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void tog_graph (Widget w, XmDrawingAreaCallbackStruct *call_data );
	void Toggle_Callback(Widget w, int which, caddr_t call_data );
	void outgtext ( char *st, int x, int y );
	void outtext ( char *st, int x, int y );
	GC xs_create_xor_gc(Widget w, char *color);
	void OwnMainLoop (XtAppContext app );
        void load_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void about_cb (Widget w, XtPointer client_data, XtPointer call_data );
        void modellist_cb ( Widget w, XtPointer client_data, XtPointer call_data );
        void sharp_load ( Widget w, XtPointer client_data, XtPointer call_data );
	void show_gem_info ( Widget w );
	void show_model_info ( Widget w );
	void get_gemfile ( Widget w, XtPointer client_data, XtPointer call_data );
	void get_pfcfile ( Widget w, XtPointer client_data, XtPointer call_data );
	void load_gemfile ( Widget w, XtPointer client_data, XtPointer call_data );
	void load_pfcfile ( Widget w, XtPointer client_data, XtPointer call_data );
	void get_gemfile_text ( Widget w, XtPointer client_data, XtPointer call_data );
	void get_pfcfile_text ( Widget w, XtPointer client_data, XtPointer call_data );
	void get_mdlfile ( Widget w, XtPointer client_data, XtPointer call_data );
	void stationlist_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void stationlist_cb_pfc ( Widget w, XtPointer client_data, XtPointer call_data );
	void Load_stationlist (int list);
	void Load_stationlist_pfc (int list);
	void Load_gem_sounding (void);
	void Load_pfc_sounding (void);
	void Load_mdl_sounding (void);
	void Load_point_sounding (void);
        void show_point_info ( Widget w );
	void sta_select_cb (int which_sta);
	void time_select_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void time_select_cb_pfc ( Widget w, XtPointer client_data, XtPointer call_data );
	void gem_info_ok_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void pfc_info_ok_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void gem_info_cancel_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void pfc_info_cancel_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void parcel_cancel_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void mdl_info_ok_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void mdl_info_cancel_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void modellist_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void set_mdlsta ( Widget w, XtPointer client_data, XtPointer call_data );
	void set_user_level ( Widget w, XtPointer client_data, XtPointer call_data );
	void mtime_select_cb ( Widget w, XtPointer client_data, XtPointer call_data );
	void load_mdldata ( Widget w, XtPointer client_data, XtPointer call_data );
	void save_origsndg (void);
	void gif_output ( Widget w, XtPointer client_data, XtPointer call_data );
	void change_mode ( Widget w, XtPointer client_data, XtPointer call_data );
	void restore_origsndg (void);
	void show_about_info ( Widget w );
	void X_Init (int argc, char *argv[]);
	void vvel_profile(void);
	void clean_uvvs(void);
	void show_profiler_info ( Widget w );
	void show_torpot(void);
	void show_windpot(void);
	void trace_dcape(void);
	int save_gif(char *);
	void set_mml_level ( Widget w, XtPointer client_data, XtPointer call_data );
	void set_mu_level ( Widget w, XtPointer client_data, XtPointer call_data );
	int get_colors(void);
	short get_level_pointer(float pres);
	void show_mainpage(void);
	void main_thermo(void);
	void main_winds(void);
	void plot_barbs2(void);
	void plot_windspeedprofile(void);
	void plot_advectionprofile(void);
	void effective_inflow_layer(float ecape, float ecinh, float *bot, float *top);

	void bunkers_storm_motion(float *u, float *v, float *dir, float *spd);
	void bunkers_left_motion(float *u, float *v, float *dir, float *spd);

	float scp(float stdir, float stspd);
	float sigtorn(float stdir, float stspd);
	float sigtorn_cin(float stdir, float stspd);
	float sigtorn_fixed(float stdir, float stspd);
	float sigtorn_tc(float stdir, float stspd);
	float sigtorn_test(float stdir, float stspd);	
	float CB_sigtor(void);
	void prob_sigt_esrh();
	void show_shear_new( void );

	void show_parcel_new();
	void show_winter_new();
	void show_stp_stats();
	void show_ship_stats(void);
	float show_ebs_stats(void);
        void show_fire(void);

	void prob_sigt_mlcape();
	void prob_sigt_mllcl();
	void prob_sigt_eshear();
	void prob_sigt_stp();
	void prob_sigt_stpc();
	void show_sars();

	float esp(void); 
	float nst(void);
	float damaging_wind(void);
	short ww_type(short *wwtype, short *dcp);
	float bndry_ci(float lift, float distance);

/* mkay. new stuff 8/5/99 */
void processuserdata(Widget widget, XtPointer client_data, XtPointer call_data);
void processuserdatabndry(Widget widget, XtPointer client_data, XtPointer call_data);
void bndry_input(Widget widget, XtPointer client_data, XtPointer call_data);
void resetuserinput(void);
/* mkay. new stuff 8/8/99 */
int init_fonts(void);
short set_font(short font_no);

/* mkay. 2/10/00 why wasn't this here before???!!!! */
int getgtextextent(char *st);

void copytodisplay(void);

int SimpleMsgBox(Widget w, char *text);

void display_dendritic_zone(short width);
void show_hailpage( float *hvars );
void keyboard_press(Widget w, XtPointer *call_data, XEvent *event);
void AdvanceFrame(void);
void BackFrame(void);
void show_hail_new(float *h2);
void autoload(void);


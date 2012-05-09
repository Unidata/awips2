/* 
 * Header file for global variables related any X windows routines
 *
 * mkay 8/19/99
 */

/* Drawing modes */
#define DRAW_SKEWT 0
#define DRAW_HODO  1

/* Display modes */
#define DISPLAY_CONVECTION_LEFT     0
#define DISPLAY_WINTER_LEFT         1
#define DISPLAY_SARS_LEFT           2 
#define DISPLAY_HAIL_LEFT           3
#define DISPLAY_SHIP_LEFT           4
#define DISPLAY_STP_LEFT            5
#define DISPLAY_EBS_LEFT            6
#define DISPLAY_FIRE_LEFT           7

/* Display modes right inset */
#define DISPLAY_CONVECTION_RIGHT    0
#define DISPLAY_WINTER_RIGHT        1
#define DISPLAY_SARS_RIGHT          2
#define DISPLAY_HAIL_RIGHT          3
#define DISPLAY_SHIP_RIGHT          4
#define DISPLAY_STP_RIGHT           5
#define DISPLAY_EBS_RIGHT           6
#define DISPLAY_FIRE_RIGHT          7

/* Hodograph Modes */
#define HODO_NORMAL		0
#define HODO_EFFECTIVE		1
#define HODO_STORMRELATIVE	2
#define HODO_BNDRY		3
#define HODO_MEANWIND		4

/* Inset modes */
#define DISPLAY_LEFT		0
#define DISPLAY_RIGHT		1

#ifndef _WIN32
extern short drawing_mode;
extern short display_mode_left;
extern short display_mode_right;	
extern short inset_mode;	
extern int xwdth, xhght;
extern int xdpth;
extern int raob_mod;
extern short hodo_mode;
extern short auto_mode;

extern Pixel pixels[40];  /* for colors */
extern GC gc;
extern Widget draw_reg;
extern Pixmap canvas;
extern XtAppContext app;
#endif

extern short pagenum;
extern short nobanner;
extern short overlay_previous;
extern short inset_previous;

extern struct _skewt skv; /* Bounds of skew-T area */
extern struct _hodog hov; /* Bounds of hodograph area */
extern struct _stpinset stv; /* Bounds of STP Stats inset area */

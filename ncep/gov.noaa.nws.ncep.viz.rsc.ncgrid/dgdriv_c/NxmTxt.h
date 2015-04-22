/************************************************************************
 * NxmTxt.h								*
 * Contains common declarations for the Text Edit module		*
 *                                                                      *
 **                                                                     *
 * E. Safford/GSC	 8/97	Initial coding				*
 * E. Safford/GSC	10/97	added union to txt_attrib 		*
 * E. Safford/GSC	12/97	modified txt_attrib structure		*
 * C. Lin/EAI		 8/98	take out FONT_TINY,...FONT_HUGE		*
 * T. Piper/SAIC	10/01	Added #include <X11/keysym.h>		*
 * M. Li/SAIC		11/01	Added pgtxt_initIcng and icng_a		*
 ***********************************************************************/

#include <X11/keysym.h>

#define	SOFTWARE	1
#define	HARDWARE	2
#define	ALLFONTS	3

#define MAX_ROTN_SCALE  360
#define MAX_SIZE_SCALE    2

#define MAX_TEXT	255

/* text attribute structure */

typedef struct {
	int	sptxtyp;
	int	fontithw;
	int	gemfont;
	float	frotn;
	float	fsize;
	int	colr;
	int	turb;
	int	fontstyle;
	int	align;
	int	iwidth;
} textattrib_t;		

typedef struct {
	int	is_shown;
	int	is_on;
	int	reset_value;
        union {
	        int	_i;
    	        char	_c[MAX_TEXT]; 
        } value;
} txt_attrib;


void    pgtxt_getAttr (         textattrib_t    *attribs,
                                char            *text );

void    pgtxt_initFonts (       txt_attrib      font_a );

void    pgtxt_initAlign (       txt_attrib      align_a );

void    pgtxt_initRotn (        txt_attrib      rotn_a );

void    pgtxt_initSize (        txt_attrib      size_a );

void    pgtxt_initTurb (        txt_attrib      turb_a );

void    pgtxt_initIcng (        txt_attrib      icng_a );

void    pgtxt_initStyle (       txt_attrib      style_a );

void    pgtxt_initBox (         txt_attrib      box_a );

void    pgtxt_initText (        txt_attrib      text_a );

void    pgtxt_initColor (       txt_attrib      color_a );

void    pgtxt_popupstart (      int             use_ces,
                                int             vg_type,
                                int             obj_id,
                                txt_attrib      font_a,
                                txt_attrib      align_a,
                                txt_attrib      rotn_a,
                                txt_attrib      size_a,
                                txt_attrib      color_a,
                                txt_attrib      turb_a,
				txt_attrib      icng_a,
                                txt_attrib      style_a,
                                txt_attrib      box_a,
                                txt_attrib      text_a,
                                int             show_ctl,
                                Boolean         attr_edit,
                                XtCallbackProc	callback,
                                XtCallbackProc  exit_callback );


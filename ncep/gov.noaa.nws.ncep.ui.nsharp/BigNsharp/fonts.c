#include "fonts.h"

/* extern*/ XFontStruct *font_struct;
extern GC           gc;
extern Widget       draw_reg;

XFontStruct *font_attrib[NFONTS];
Font         font_info[NFONTS];

int init_fonts(void)
{
	int i;

	/* Initialize the fonts */
	for (i=1; i<NFONTS; i++) {
	  font_info[i] = XLoadFont(XtDisplay(draw_reg), font_names[i]);

	  /* Query the font to get our info about it */
	  font_attrib[i] = XQueryFont(XtDisplay(draw_reg), font_info[i]);
	  if (font_attrib[i] == NULL) return 1;
	}

	return 0;
}

short set_font(short font_no)
{

	/* Make sure they've made a sane request */
	if (font_no < 1 || font_no > NFONTS) return 1;

	/* Set our font */
	XSetFont(XtDisplay(draw_reg), gc, font_info[font_no]);

	/* Set our pointer */
	font_struct = font_attrib[font_no];

	return 0;
}

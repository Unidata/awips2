#include <malloc.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

/*Change made because gd1.h is already in /usr/include, but we want the one in the
  ofs system.  So, we changed it at the util level from gd1.h to gd1.h. (Ai made
  the change, but Hank wrote this comment, 9/25/01)*/
#include "gd1_inc/gd1.h"

#include "mtables.h"

static void gdImageBrushApply(gdImagePtr im, int x, int y);
static void gdImageTileApply(gdImagePtr im, int x, int y);

gdImagePtr gdImageCreate(int sx, int sy)
{
	int i;
	gdImagePtr im;
	im = (gdImage *) malloc(sizeof(gdImage));
	im->pixels = (unsigned char **) malloc(sizeof(unsigned char *) * sx);
	im->polyInts = 0;
	im->polyAllocated = 0;
	im->brush = 0;
	im->tile = 0;
	im->style = 0;
	for (i=0; (i<sx); i++) {
		im->pixels[i] = (unsigned char *) calloc(
			sy, sizeof(unsigned char));
	}
	im->sx = sx;
	im->sy = sy;
	im->colorsTotal = 0;
	im->transparent = (-1);
	im->interlace = 0;
	return im;
}

/*NOTE: This will not free the pointer itself, but will only free the
  the pieces that it points to!!!  So, you should follow this with a
  "free(<gdImagePtr var name>)" call!!!*/
void gdImageDestroy(gdImagePtr im)
{
	int i;
        /* Added by Hank (10/16/2001) so that I don't try to free some stuff twice!*/
        if (im->sx > 0)
        {
	    for (i=0; (i<im->sx); i++) {
		free(im->pixels[i]);
	    }
	    free(im->pixels);
            im->sx = 0;	/*This is added as well */
        }
	if (im->polyInts) {
			free(im->polyInts);
	}
	if (im->style) {
		free(im->style);
	}
}

int gdImageColorClosest(gdImagePtr im, int r, int g, int b)
{
	int i;
	long rd, gd, bd;
	int ct = (-1);
	long mindist = 0;
	for (i=0; (i<(im->colorsTotal)); i++) {
		long dist;
		if (im->open[i]) {
			continue;
		}
		rd = (im->red[i] - r);
		gd = (im->green[i] - g);
		bd = (im->blue[i] - b);
		dist = rd * rd + gd * gd + bd * bd;
		if ((i == 0) || (dist < mindist)) {
			mindist = dist;
			ct = i;
		}
	}
	return ct;
}

int gdImageColorExact(gdImagePtr im, int r, int g, int b)
{
	int i;
	for (i=0; (i<(im->colorsTotal)); i++) {
		if (im->open[i]) {
			continue;
		}
		if ((im->red[i] == r) &&
			(im->green[i] == g) &&
			(im->blue[i] == b)) {
			return i;
		}
	}
	return -1;
}

int gdImageColorAllocate(gdImagePtr im, int r, int g, int b)
{
	int i;
	int ct = (-1);
	for (i=0; (i<(im->colorsTotal)); i++) {
		if (im->open[i]) {
			ct = i;
			break;
		}
	}
	if (ct == (-1)) {
		ct = im->colorsTotal;
		if (ct == gdMaxColors) {
			return -1;
		}
		im->colorsTotal++;
	}
	im->red[ct] = r;
	im->green[ct] = g;
	im->blue[ct] = b;
	im->open[ct] = 0;
	return ct;
}

void gdImageColorDeallocate(gdImagePtr im, int color)
{
	/* Mark it open. */
	im->open[color] = 1;
}

void gdImageColorTransparent(gdImagePtr im, int color)
{
	im->transparent = color;
}

void gdImageSetPixel(gdImagePtr im, int x, int y, int color)
{
	int p;
	switch(color) {
		case gdStyled:
		if (!im->style) {
			/* Refuse to draw if no style is set. */
			return;
		} else {
			p = im->style[im->stylePos++];
		}
		if (p != (gdTransparent)) {
			gdImageSetPixel(im, x, y, p);
		}
		im->stylePos = im->stylePos %  im->styleLength;
		break;
		case gdStyledBrushed:
		if (!im->style) {
			/* Refuse to draw if no style is set. */
			return;
		}
		p = im->style[im->stylePos++];
		if ((p != gdTransparent) && (p != 0)) {
			gdImageSetPixel(im, x, y, gdBrushed);
		}
		im->stylePos = im->stylePos %  im->styleLength;
		break;
		case gdBrushed:
		gdImageBrushApply(im, x, y);
		break;
		case gdTiled:
		gdImageTileApply(im, x, y);
		break;
		default:
		if (gdImageBoundsSafe(im, x, y)) {
			 im->pixels[x][y] = color;
		}
		break;
	}
}

static void gdImageBrushApply(gdImagePtr im, int x, int y)
{
	int lx, ly;
	int hy;
	int hx;
	int x1, y1, x2, y2;
	int srcx, srcy;
	if (!im->brush) {
		return;
	}
	hy = gdImageSY(im->brush)/2;
	y1 = y - hy;
	y2 = y1 + gdImageSY(im->brush);
	hx = gdImageSX(im->brush)/2;
	x1 = x - hx;
	x2 = x1 + gdImageSX(im->brush);
	srcy = 0;
	for (ly = y1; (ly < y2); ly++) {
		srcx = 0;
		for (lx = x1; (lx < x2); lx++) {
			int p;
			p = gdImageGetPixel(im->brush, srcx, srcy);
			/* Allow for non-square brushes! */
			if (p != gdImageGetTransparent(im->brush)) {
				gdImageSetPixel(im, lx, ly,
					im->brushColorMap[p]);
			}
			srcx++;
		}
		srcy++;
	}
}

static void gdImageTileApply(gdImagePtr im, int x, int y)
{
	int srcx, srcy;
	int p;
	if (!im->tile) {
		return;
	}
	srcx = x % gdImageSX(im->tile);
	srcy = y % gdImageSY(im->tile);
	p = gdImageGetPixel(im->tile, srcx, srcy);
	/* Allow for transparency */
	if (p != gdImageGetTransparent(im->tile)) {
		gdImageSetPixel(im, x, y,
			im->tileColorMap[p]);
	}
}

int gdImageGetPixel(gdImagePtr im, int x, int y)
{
	if (gdImageBoundsSafe(im, x, y)) {
		return im->pixels[x][y];
	} else {
		return 0;
	}
}

/* Bresenham as presented in Foley & Van Dam */

void gdImageLine(gdImagePtr im, int x1, int y1, int x2, int y2, int color)
{
	int dx, dy, incr1, incr2, d, x, y, xend, yend, xdirflag, ydirflag;
	dx = abs(x2-x1);
	dy = abs(y2-y1);
	if (dy <= dx) {
		d = 2*dy - dx;
		incr1 = 2*dy;
		incr2 = 2 * (dy - dx);
		if (x1 > x2) {
			x = x2;
			y = y2;
			ydirflag = (-1);
			xend = x1;
		} else {
			x = x1;
			y = y1;
			ydirflag = 1;
			xend = x2;
		}
		gdImageSetPixel(im, x, y, color);
		if (((y2 - y1) * ydirflag) > 0) {
			while (x < xend) {
				x++;
				if (d <0) {
					d+=incr1;
				} else {
					y++;
					d+=incr2;
				}
				gdImageSetPixel(im, x, y, color);
			}
		} else {
			while (x < xend) {
				x++;
				if (d <0) {
					d+=incr1;
				} else {
					y--;
					d+=incr2;
				}
				gdImageSetPixel(im, x, y, color);
			}
		}
	} else {
		d = 2*dx - dy;
		incr1 = 2*dx;
		incr2 = 2 * (dx - dy);
		if (y1 > y2) {
			y = y2;
			x = x2;
			yend = y1;
			xdirflag = (-1);
		} else {
			y = y1;
			x = x1;
			yend = y2;
			xdirflag = 1;
		}
		gdImageSetPixel(im, x, y, color);
		if (((x2 - x1) * xdirflag) > 0) {
			while (y < yend) {
				y++;
				if (d <0) {
					d+=incr1;
				} else {
					x++;
					d+=incr2;
				}
				gdImageSetPixel(im, x, y, color);
			}
		} else {
			while (y < yend) {
				y++;
				if (d <0) {
					d+=incr1;
				} else {
					x--;
					d+=incr2;
				}
				gdImageSetPixel(im, x, y, color);
			}
		}
	}
}

/* As above, plus dashing */

#define dashedSet \
	{ \
		dashStep++; \
		if (dashStep == gdDashSize) { \
			dashStep = 0; \
			on = !on; \
		} \
		if (on) { \
			gdImageSetPixel(im, x, y, color); \
		} \
	}

void gdImageDashedLine(gdImagePtr im, int x1, int y1, int x2, int y2, int color)
{
	int dx, dy, incr1, incr2, d, x, y, xend, yend, xdirflag, ydirflag;
	int dashStep = 0;
	int on = 1;
	dx = abs(x2-x1);
	dy = abs(y2-y1);
	if (dy <= dx) {
		d = 2*dy - dx;
		incr1 = 2*dy;
		incr2 = 2 * (dy - dx);
		if (x1 > x2) {
			x = x2;
			y = y2;
			ydirflag = (-1);
			xend = x1;
		} else {
			x = x1;
			y = y1;
			ydirflag = 1;
			xend = x2;
		}
		dashedSet;
		if (((y2 - y1) * ydirflag) > 0) {
			while (x < xend) {
				x++;
				if (d <0) {
					d+=incr1;
				} else {
					y++;
					d+=incr2;
				}
				dashedSet;
			}
		} else {
			while (x < xend) {
				x++;
				if (d <0) {
					d+=incr1;
				} else {
					y--;
					d+=incr2;
				}
				dashedSet;
			}
		}
	} else {
		d = 2*dx - dy;
		incr1 = 2*dx;
		incr2 = 2 * (dx - dy);
		if (y1 > y2) {
			y = y2;
			x = x2;
			yend = y1;
			xdirflag = (-1);
		} else {
			y = y1;
			x = x1;
			yend = y2;
			xdirflag = 1;
		}
		dashedSet;
		if (((x2 - x1) * xdirflag) > 0) {
			while (y < yend) {
				y++;
				if (d <0) {
					d+=incr1;
				} else {
					x++;
					d+=incr2;
				}
				dashedSet;
			}
		} else {
			while (y < yend) {
				y++;
				if (d <0) {
					d+=incr1;
				} else {
					x--;
					d+=incr2;
				}
				dashedSet;
			}
		}
	}
}

int gdImageBoundsSafe(gdImagePtr im, int x, int y)
{
	return (!(((y < 0) || (y >= im->sy)) ||
		((x < 0) || (x >= im->sx))));
}

void gdImageChar(gdImagePtr im, gdFontPtr f, int x, int y, int c, int color)
{
	int cx, cy;
	int px, py;
	int fline;
	cx = 0;
	cy = 0;
	if ((c < f->offset) || (c >= (f->offset + f->nchars))) {
		return;
	}
	fline = (c - f->offset) * f->h * f->w;
	for (py = y; (py < (y + f->h)); py++) {
		for (px = x; (px < (x + f->w)); px++) {
			if (f->data[fline + cy * f->w + cx]) {
				gdImageSetPixel(im, px, py, color);
			}
			cx++;
		}
		cx = 0;
		cy++;
	}
}

void gdImageCharUp(gdImagePtr im, gdFontPtr f, int x, int y, char c, int color)
{
	int cx, cy;
	int px, py;
	int fline;
	cx = 0;
	cy = 0;
	if ((c < f->offset) || (c >= (f->offset + f->nchars))) {
		return;
	}
	fline = (c - f->offset) * f->h * f->w;
	for (py = y; (py > (y - f->w)); py--) {
		for (px = x; (px < (x + f->h)); px++) {
			if (f->data[fline + cy * f->w + cx]) {
				gdImageSetPixel(im, px, py, color);
			}
			cy++;
		}
		cy = 0;
		cx++;
	}
}

void gdImageString(gdImagePtr im, gdFontPtr f, int x, int y, char *s, int color)
{
	int i;
	int l;
	l = strlen(s);
	for (i=0; (i<l); i++) {
		gdImageChar(im, f, x, y, s[i], color);
		x += f->w;
	}
}

void gdImageStringUp(gdImagePtr im, gdFontPtr f, int x, int y, char *s, int color)
{
	int i;
	int l;
	l = strlen(s);
	for (i=0; (i<l); i++) {
		gdImageCharUp(im, f, x, y, s[i], color);
		y -= f->w;
	}
}

/* s and e are integers modulo 360 (degrees), with 0 degrees
  being the rightmost extreme and degrees changing clockwise.
  cx and cy are the center in pixels; w and h are the horizontal
  and vertical diameter in pixels. Nice interface, but slow, since
  I don't yet use Bresenham (I'm using an inefficient but
  simple solution with too much work going on in it; generalizing
  Bresenham to ellipses and partial arcs of ellipses is non-trivial,
  at least for me) and there are other inefficiencies (small circles
  do far too much work). */

void gdImageArc(gdImagePtr im, int cx, int cy, int w, int h, int s, int e, int color)
{
	int i;
	int lx = 0, ly = 0;
	int w2, h2;
	w2 = w/2;
	h2 = h/2;
	while (e < s) {
		e += 360;
	}
	for (i=s; (i <= e); i++) {
		int x, y;
		x = ((long)cost[i % 360] * (long)w2 / costScale) + cx;
		y = ((long)sint[i % 360] * (long)h2 / sintScale) + cy;
		if (i != s) {
			gdImageLine(im, lx, ly, x, y, color);
		}
		lx = x;
		ly = y;
	}
}


#if 0
	/* Bresenham octant code, which I should use eventually */
	int x, y, d;
	x = 0;
	y = w;
	d = 3-2*w;
	while (x < y) {
		gdImageSetPixel(im, cx+x, cy+y, color);
		if (d < 0) {
			d += 4 * x + 6;
		} else {
			d += 4 * (x - y) + 10;
			y--;
		}
		x++;
	}
	if (x == y) {
		gdImageSetPixel(im, cx+x, cy+y, color);
	}
#endif

void gdImageFillToBorder(gdImagePtr im, int x, int y, int border, int color)
{
	int lastBorder;
	/* Seek left */
	int leftLimit, rightLimit;
	int i;
	leftLimit = (-1);
	if (border < 0) {
		/* Refuse to fill to a non-solid border */
		return;
	}
	for (i = x; (i >= 0); i--) {
		if (gdImageGetPixel(im, i, y) == border) {
			break;
		}
		gdImageSetPixel(im, i, y, color);
		leftLimit = i;
	}
	if (leftLimit == (-1)) {
		return;
	}
	/* Seek right */
	rightLimit = x;
	for (i = (x+1); (i < im->sx); i++) {
		if (gdImageGetPixel(im, i, y) == border) {
			break;
		}
		gdImageSetPixel(im, i, y, color);
		rightLimit = i;
	}
	/* Look at lines above and below and start paints */
	/* Above */
	if (y > 0) {
		lastBorder = 1;
		for (i = leftLimit; (i <= rightLimit); i++) {
			int c;
			c = gdImageGetPixel(im, i, y-1);
			if (lastBorder) {
				if ((c != border) && (c != color)) {
					gdImageFillToBorder(im, i, y-1,
						border, color);
					lastBorder = 0;
				}
			} else if ((c == border) || (c == color)) {
				lastBorder = 1;
			}
		}
	}
	/* Below */
	if (y < ((im->sy) - 1)) {
		lastBorder = 1;
		for (i = leftLimit; (i <= rightLimit); i++) {
			int c;
			c = gdImageGetPixel(im, i, y+1);
			if (lastBorder) {
				if ((c != border) && (c != color)) {
					gdImageFillToBorder(im, i, y+1,
						border, color);
					lastBorder = 0;
				}
			} else if ((c == border) || (c == color)) {
				lastBorder = 1;
			}
		}
	}
}

void gdImageFill(gdImagePtr im, int x, int y, int color)
{
	int lastBorder;
	int old;
	int leftLimit, rightLimit;
	int i;
	old = gdImageGetPixel(im, x, y);
	if (color == gdTiled) {
		/* Tile fill -- got to watch out! */
		int p, tileColor;
		int srcx, srcy;
		if (!im->tile) {
			return;
		}
		/* Refuse to flood-fill with a transparent pattern --
			I can't do it without allocating another image */
		if (gdImageGetTransparent(im->tile) != (-1)) {
			return;
		}
		srcx = x % gdImageSX(im->tile);
		srcy = y % gdImageSY(im->tile);
		p = gdImageGetPixel(im->tile, srcx, srcy);
		tileColor = im->tileColorMap[p];
		if (old == tileColor) {
			/* Nothing to be done */
			return;
		}
	} else {
		if (old == color) {
			/* Nothing to be done */
			return;
		}
	}
	/* Seek left */
	leftLimit = (-1);
	for (i = x; (i >= 0); i--) {
		if (gdImageGetPixel(im, i, y) != old) {
			break;
		}
		gdImageSetPixel(im, i, y, color);
		leftLimit = i;
	}
	if (leftLimit == (-1)) {
		return;
	}
	/* Seek right */
	rightLimit = x;
	for (i = (x+1); (i < im->sx); i++) {
		if (gdImageGetPixel(im, i, y) != old) {
			break;
		}
		gdImageSetPixel(im, i, y, color);
		rightLimit = i;
	}
	/* Look at lines above and below and start paints */
	/* Above */
	if (y > 0) {
		lastBorder = 1;
		for (i = leftLimit; (i <= rightLimit); i++) {
			int c;
			c = gdImageGetPixel(im, i, y-1);
			if (lastBorder) {
				if (c == old) {
					gdImageFill(im, i, y-1, color);
					lastBorder = 0;
				}
			} else if (c != old) {
				lastBorder = 1;
			}
		}
	}
	/* Below */
	if (y < ((im->sy) - 1)) {
		lastBorder = 1;
		for (i = leftLimit; (i <= rightLimit); i++) {
			int c;
			c = gdImageGetPixel(im, i, y+1);
			if (lastBorder) {
				if (c == old) {
					gdImageFill(im, i, y+1, color);
					lastBorder = 0;
				}
			} else if (c != old) {
				lastBorder = 1;
			}
		}
	}
}

#ifdef TEST_CODE
void gdImageDump(gdImagePtr im)
{
	int i, j;
	for (i=0; (i < im->sy); i++) {
		for (j=0; (j < im->sx); j++) {
			printf("%d", im->pixels[j][i]);
		}
		printf("\n");
	}
}
#endif

/* Code drawn from ppmtogif.c, from the pbmplus package
**
** Based on GIFENCOD by David Rowley <mgardi@watdscu.waterloo.edu>. A
** Lempel-Zim compression based on "compress".
**
** Modified by Marcel Wijkstra <wijkstra@fwi.uva.nl>
**
** Copyright (C) 1989 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
**
** The Graphics Interchange Format(c) is the Copyright property of
** CompuServe Incorporated.  GIF(sm) is a Service Mark property of
** CompuServe Incorporated.
*/

/*
 * a code_int must be able to hold 2**GIFBITS values of type int, and also -1
 */
typedef int             code_int;

#ifdef SIGNED_COMPARE_SLOW
typedef unsigned long int count_int;
typedef unsigned short int count_short;
#else /*SIGNED_COMPARE_SLOW*/
typedef long int          count_int;
#endif /*SIGNED_COMPARE_SLOW*/

static int colorstobpp(int colors);
static void BumpPixel (void);
static int GIFNextPixel (gdImagePtr im);
static void GIFEncode (FILE *fp, int GWidth, int GHeight, int GInterlace, int Background, int Transparent, int BitsPerPixel, int *Red, int *Green, int *Blue, gdImagePtr im);
static void Putword (int w, FILE *fp);
static void compress (int init_bits, FILE *outfile, gdImagePtr im);
static void output (code_int code);
static void cl_block (void);
static void cl_hash (register count_int hsize);
static void char_init (void);
static void char_out (int c);
static void flush_char (void);
/* Allows for reuse */
static void init_statics(void);

void gdImageGif(gdImagePtr im, FILE *out)
{
	int interlace, transparent, BitsPerPixel;
	interlace = im->interlace;
	transparent = im->transparent;

	BitsPerPixel = colorstobpp(im->colorsTotal);
	/* Clear any old values in statics strewn through the GIF code */
	init_statics();


	/* All set, let's do it. */
	GIFEncode(
		out, im->sx, im->sy, interlace, 0, transparent, BitsPerPixel,
		im->red, im->green, im->blue, im);



}

static int
colorstobpp(int colors)
{
    int bpp = 0;

    if ( colors <= 2 )
        bpp = 1;
    else if ( colors <= 4 )
        bpp = 2;
    else if ( colors <= 8 )
        bpp = 3;
    else if ( colors <= 16 )
        bpp = 4;
    else if ( colors <= 32 )
        bpp = 5;
    else if ( colors <= 64 )
        bpp = 6;
    else if ( colors <= 128 )
        bpp = 7;
    else if ( colors <= 256 )
        bpp = 8;
    return bpp;
    }

/*****************************************************************************
 *
 * GIFENCODE.C    - GIF Image compression interface
 *
 * GIFEncode( FName, GHeight, GWidth, GInterlace, Background, Transparent,
 *            BitsPerPixel, Red, Green, Blue, gdImagePtr )
 *
 *****************************************************************************/

#define TRUE 1
#define FALSE 0

static int Width, Height;
static int curx, cury;
static long CountDown;
static int Pass = 0;
static int Interlace;

/*
 * Bump the 'curx' and 'cury' to point to the next pixel
 */
static void
BumpPixel(void)
{
        /*
         * Bump the current X position
         */
        ++curx;

        /*
         * If we are at the end of a scan line, set curx back to the beginning
         * If we are interlaced, bump the cury to the appropriate spot,
         * otherwise, just increment it.
         */
        if( curx == Width ) {
                curx = 0;

                if( !Interlace )
                        ++cury;
                else {
                     switch( Pass ) {

                       case 0:
                          cury += 8;
                          if( cury >= Height ) {
                                ++Pass;
                                cury = 4;
                          }
                          break;

                       case 1:
                          cury += 8;
                          if( cury >= Height ) {
                                ++Pass;
                                cury = 2;
                          }
                          break;

                       case 2:
                          cury += 4;
                          if( cury >= Height ) {
                             ++Pass;
                             cury = 1;
                          }
                          break;

                       case 3:
                          cury += 2;
                          break;
                        }
                }
        }
}

/*
 * Return the next pixel from the image
 */
static int
GIFNextPixel(gdImagePtr im)
{
        int r;

        if( CountDown == 0 )
                return EOF;

        --CountDown;

        r = gdImageGetPixel(im, curx, cury);

        BumpPixel();

        return r;
}

/* public */

/* This will NOT close the file pointer fp!!!*/
static void
GIFEncode(FILE *fp, int GWidth, int GHeight, int GInterlace, int Background, int Transparent, int BitsPerPixel, int *Red, int *Green, int *Blue, gdImagePtr im)
{
        int B;
        int RWidth, RHeight;
        int LeftOfs, TopOfs;
        int Resolution;
        int ColorMapSize;
        int InitCodeSize;
        int i,len;

        char giftype[6];


        Interlace = GInterlace;

        ColorMapSize = 1 << BitsPerPixel;

        RWidth = Width = GWidth;
        RHeight = Height = GHeight;
        LeftOfs = TopOfs = 0;

        Resolution = BitsPerPixel;

        /*
         * Calculate number of bits we are expecting
         */
        CountDown = (long)Width * (long)Height;

        /*
         * Indicate which pass we are on (if interlace)
         */
        Pass = 0;

        /*
         * The initial code size
         */
        if( BitsPerPixel <= 1 )
                InitCodeSize = 2;
        else
                InitCodeSize = BitsPerPixel;

        /*
         * Set up the current x and y position
         */
        curx = cury = 0;

       /*
         * Write the Magic header
       */

       /* fwrite( Transparent < 0 ? "GIF87a" : "GIF89a", 1, 6, fp ); */


       if(Transparent < 0)
         sprintf(giftype,"GIF87a");
       else
         sprintf(giftype,"GIF89a");


       clearerr(fp);


       fwrite(giftype,sizeof(giftype),1,fp);

        /*
         * Write out the screen width and height
         */


        Putword( RWidth, fp );
        Putword( RHeight, fp );


        /*
         * Indicate that there is a global colour map
         */
        B = 0x80;       /* Yes, there is a color map */

        /*
         * OR in the resolution
         */
        B |= (Resolution - 1) << 5;

        /*
         * OR in the Bits per Pixel
         */
        B |= (BitsPerPixel - 1);

        /*
         * Write it out
         */
        fputc( B, fp );

        /*
         * Write out the Background colour
         */
        fputc( Background, fp );

        /*
         * Byte of 0's (future expansion)
         */
        fputc( 0, fp );

        /*
         * Write out the Global Colour Map
         */
        for( i=0; i<ColorMapSize; ++i ) {
                fputc( Red[i], fp );
                fputc( Green[i], fp );
                fputc( Blue[i], fp );
        }

	/*
	 * Write out extension for transparent colour index, if necessary.
	 */
	if ( Transparent >= 0 ) {
	    fputc( '!', fp );
	    fputc( 0xf9, fp );
	    fputc( 4, fp );
	    fputc( 1, fp );
	    fputc( 0, fp );
	    fputc( 0, fp );
	    fputc( (unsigned char) Transparent, fp );
	    fputc( 0, fp );
	}

        /*
         * Write an Image separator
         */
        fputc( ',', fp );

        /*
         * Write the Image header
         */

        Putword( LeftOfs, fp );
        Putword( TopOfs, fp );
        Putword( Width, fp );
        Putword( Height, fp );

        /*
         * Write out whether or not the image is interlaced
         */
        if( Interlace )
                fputc( 0x40, fp );
        else
                fputc( 0x00, fp );

        /*
         * Write out the initial code size
         */
        fputc( InitCodeSize, fp );

        /*
         * Go and actually compress the data
         */
        compress( InitCodeSize+1, fp, im );

        /*
         * Write out a Zero-length packet (to end the series)
         */
        fputc( 0, fp );

        /* The following output statement was commented out 17 Mar 2000 */
        /* printf("Writing GIF file terminator\n");                     */

        /*
         * Write the GIF file terminator
         */
        fputc( ';', fp );


}

/*
 * Write out a word to the GIF file
 */
static void
Putword(int w, FILE *fp)
{
        fputc( w & 0xff, fp );
        fputc( (w / 256) & 0xff, fp );
}


/***************************************************************************
 *
 *  GIFCOMPR.C       - GIF Image compression routines
 *
 *  Lempel-Ziv compression based on 'compress'.  GIF modifications by
 *  David Rowley (mgardi@watdcsu.waterloo.edu)
 *
 ***************************************************************************/

/*
 * General DEFINEs
 */

#define GIFBITS    12

#define HSIZE  5003            /* 80% occupancy */

#ifdef NO_UCHAR
 typedef char   char_type;
#else /*NO_UCHAR*/
 typedef        unsigned char   char_type;
#endif /*NO_UCHAR*/

/*
 *
 * GIF Image compression - modified 'compress'
 *
 * Based on: compress.c - File compression ala IEEE Computer, June 1984.
 *
 * By Authors:  Spencer W. Thomas       (decvax!harpo!utah-cs!utah-gr!thomas)
 *              Jim McKie               (decvax!mcvax!jim)
 *              Steve Davies            (decvax!vax135!petsd!peora!srd)
 *              Ken Turkowski           (decvax!decwrl!turtlevax!ken)
 *              James A. Woods          (decvax!ihnp4!ames!jaw)
 *              Joe Orost               (decvax!vax135!petsd!joe)
 *
 */
#include <ctype.h>

#define ARGVAL() (*++(*argv) || (--argc && *++argv))

static int n_bits;                        /* number of bits/code */
static int maxbits = GIFBITS;                /* user settable max # bits/code */
static code_int maxcode;                  /* maximum code, given n_bits */
static code_int maxmaxcode = (code_int)1 << GIFBITS; /* should NEVER generate this code */
#ifdef COMPATIBLE               /* But wrong! */
# define MAXCODE(n_bits)        ((code_int) 1 << (n_bits) - 1)
#else /*COMPATIBLE*/
# define MAXCODE(n_bits)        (((code_int) 1 << (n_bits)) - 1)
#endif /*COMPATIBLE*/

static count_int htab [HSIZE];
static unsigned short codetab [HSIZE];
#define HashTabOf(i)       htab[i]
#define CodeTabOf(i)    codetab[i]

static code_int hsize = HSIZE;                 /* for dynamic table sizing */

/*
 * To save much memory, we overlay the table used by compress() with those
 * used by decompress().  The tab_prefix table is the same size and type
 * as the codetab.  The tab_suffix table needs 2**GIFBITS characters.  We
 * get this from the beginning of htab.  The output stack uses the rest
 * of htab, and contains characters.  There is plenty of room for any
 * possible stack (stack used to be 8000 characters).
 */

#define tab_prefixof(i) CodeTabOf(i)
#define tab_suffixof(i)        ((char_type*)(htab))[i]
#define de_stack               ((char_type*)&tab_suffixof((code_int)1<<GIFBITS))

static code_int free_ent = 0;                  /* first unused entry */

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
static int clear_flg = 0;

static int offset;
static long int in_count = 1;            /* length of input */
static long int out_count = 0;           /* # of codes output (for debugging) */

/*
 * compress stdin to stdout
 *
 * Algorithm:  use open addressing double hashing (no chaining) on the
 * prefix code / next character combination.  We do a variant of Knuth's
 * algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
 * secondary probe.  Here, the modular division first probe is gives way
 * to a faster exclusive-or manipulation.  Also do block compression with
 * an adaptive reset, whereby the code table is cleared when the compression
 * ratio decreases, but after the table fills.  The variable-length output
 * codes are re-sized at this point, and a special CLEAR code is generated
 * for the decompressor.  Late addition:  construct the table according to
 * file size for noticeable speed improvement on small files.  Please direct
 * questions about this implementation to ames!jaw.
 */

static int g_init_bits;
static FILE* g_outfile;

static int ClearCode;
static int EOFCode;

static void
compress(int init_bits, FILE *outfile, gdImagePtr im)
{
    register long fcode;
    register code_int i /* = 0 */;
    register int c;
    register code_int ent;
    register code_int disp;
    register code_int hsize_reg;
    register int hshift;

    /* Set up the globals:  g_init_bits - initial number of bits
                            g_outfile   - pointer to output file */

    g_init_bits = init_bits;
    g_outfile = outfile;

    /*
     * Set up the necessary values
     */
    offset = 0;
    out_count = 0;
    clear_flg = 0;
    in_count = 1;
    maxcode = MAXCODE(n_bits = g_init_bits);

    ClearCode = (1 << (init_bits - 1));
    EOFCode = ClearCode + 1;
    free_ent = ClearCode + 2;

    char_init();

    ent = GIFNextPixel( im );

    hshift = 0;
    for ( fcode = (long) hsize;  fcode < 65536L; fcode *= 2L )
        ++hshift;
    hshift = 8 - hshift;                /* set hash code range bound */

    hsize_reg = hsize;
    cl_hash( (count_int) hsize_reg);            /* clear hash table */

    output( (code_int)ClearCode );

#ifdef SIGNED_COMPARE_SLOW
    while ( (c = GIFNextPixel( im )) != (unsigned) EOF ) {
#else /*SIGNED_COMPARE_SLOW*/
    while ( (c = GIFNextPixel( im )) != EOF ) {  /* } */
#endif /*SIGNED_COMPARE_SLOW*/

        ++in_count;

        fcode = (long) (((long) c << maxbits) + ent);
        i = (((code_int)c << hshift) ^ ent);    /* xor hashing */

        if ( HashTabOf (i) == fcode ) {
            ent = CodeTabOf (i);
            continue;
        } else if ( (long)HashTabOf (i) < 0 )      /* empty slot */
            goto nomatch;
        disp = hsize_reg - i;           /* secondary hash (after G. Knott) */
        if ( i == 0 )
            disp = 1;
probe:
        if ( (i -= disp) < 0 )
            i += hsize_reg;

        if ( HashTabOf (i) == fcode ) {
            ent = CodeTabOf (i);
            continue;
        }
        if ( (long)HashTabOf (i) > 0 )
            goto probe;
nomatch:
        output ( (code_int) ent );
        ++out_count;
        ent = c;
#ifdef SIGNED_COMPARE_SLOW
        if ( (unsigned) free_ent < (unsigned) maxmaxcode) {
#else /*SIGNED_COMPARE_SLOW*/
        if ( free_ent < maxmaxcode ) {  /* } */
#endif /*SIGNED_COMPARE_SLOW*/
            CodeTabOf (i) = free_ent++; /* code -> hashtable */
            HashTabOf (i) = fcode;
        } else
                cl_block();
    }
    /*
     * Put out the final code.
     */
    output( (code_int)ent );
    ++out_count;
    output( (code_int) EOFCode );
}

/*****************************************************************
 * TAG( output )
 *
 * Output the given code.
 * Inputs:
 *      code:   A n_bits-bit integer.  If == -1, then EOF.  This assumes
 *              that n_bits =< (long)wordsize - 1.
 * Outputs:
 *      Outputs code to the file.
 * Assumptions:
 *      Chars are 8 bits long.
 * Algorithm:
 *      Maintain a GIFBITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 */

static unsigned long cur_accum = 0;
static int cur_bits = 0;

static unsigned long masks[] = { 0x0000, 0x0001, 0x0003, 0x0007, 0x000F,
                                  0x001F, 0x003F, 0x007F, 0x00FF,
                                  0x01FF, 0x03FF, 0x07FF, 0x0FFF,
                                  0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF };

static void
output(code_int code)
{
    cur_accum &= masks[ cur_bits ];

    if( cur_bits > 0 )
        cur_accum |= ((long)code << cur_bits);
    else
        cur_accum = code;

    cur_bits += n_bits;

    while( cur_bits >= 8 ) {
        char_out( (unsigned int)(cur_accum & 0xff) );
        cur_accum >>= 8;
        cur_bits -= 8;
    }

    /*
     * If the next entry is going to be too big for the code size,
     * then increase it, if possible.
     */
   if ( free_ent > maxcode || clear_flg ) {

            if( clear_flg ) {

                maxcode = MAXCODE (n_bits = g_init_bits);
                clear_flg = 0;

            } else {

                ++n_bits;
                if ( n_bits == maxbits )
                    maxcode = maxmaxcode;
                else
                    maxcode = MAXCODE(n_bits);
            }
        }

    if( code == EOFCode ) {
        /*
         * At EOF, write the rest of the buffer.
         */
        while( cur_bits > 0 ) {
                char_out( (unsigned int)(cur_accum & 0xff) );
                cur_accum >>= 8;
                cur_bits -= 8;
        }

        flush_char();

        fflush( g_outfile );

        if( ferror( g_outfile ) )
		return;
    }
}

/*
 * Clear out the hash table
 */
static void
cl_block (void)             /* table clear for block compress */
{

        cl_hash ( (count_int) hsize );
        free_ent = ClearCode + 2;
        clear_flg = 1;

        output( (code_int)ClearCode );
}

static void
cl_hash(register count_int hsize)          /* reset code table */

{

        register count_int *htab_p = htab+hsize;

        register long i;
        register long m1 = -1;

        i = hsize - 16;
        do {                            /* might use Sys V memset(3) here */
                *(htab_p-16) = m1;
                *(htab_p-15) = m1;
                *(htab_p-14) = m1;
                *(htab_p-13) = m1;
                *(htab_p-12) = m1;
                *(htab_p-11) = m1;
                *(htab_p-10) = m1;
                *(htab_p-9) = m1;
                *(htab_p-8) = m1;
                *(htab_p-7) = m1;
                *(htab_p-6) = m1;
                *(htab_p-5) = m1;
                *(htab_p-4) = m1;
                *(htab_p-3) = m1;
                *(htab_p-2) = m1;
                *(htab_p-1) = m1;
                htab_p -= 16;
        } while ((i -= 16) >= 0);

        for ( i += 16; i > 0; --i )
                *--htab_p = m1;
}

/******************************************************************************
 *
 * GIF Specific routines
 *
 ******************************************************************************/

/*
 * Number of characters so far in this 'packet'
 */
static int a_count;

/*
 * Set up the 'byte output' routine
 */
static void
char_init(void)
{
        a_count = 0;
}

/*
 * Define the storage for the packet accumulator
 */
static char accum[ 256 ];

/*
 * Add a character to the end of the current packet, and if it is 254
 * characters, flush the packet to disk.
 */
static void
char_out(int c)
{
        accum[ a_count++ ] = c;
        if( a_count >= 254 )
                flush_char();
}

/*
 * Flush the packet to disk, and reset the accumulator
 */
static void
flush_char(void)
{
        if( a_count > 0 ) {
                fputc( a_count, g_outfile );
                fwrite( accum, 1, a_count, g_outfile );
                a_count = 0;
        }
}

static void init_statics(void) {
	/* Some of these are properly initialized later. What I'm doing
		here is making sure code that depends on C's initialization
		of statics doesn't break when the code gets called more
		than once. */
	Width = 0;
	Height = 0;
	curx = 0;
	cury = 0;
	CountDown = 0;
	Pass = 0;
	Interlace = 0;
	a_count = 0;
	cur_accum = 0;
	cur_bits = 0;
	g_init_bits = 0;
	g_outfile = 0;
	ClearCode = 0;
	EOFCode = 0;
	free_ent = 0;
	clear_flg = 0;
	offset = 0;
	in_count = 1;
	out_count = 0;
	hsize = HSIZE;
	n_bits = 0;
	maxbits = GIFBITS;
	maxcode = 0;
	maxmaxcode = (code_int)1 << GIFBITS;
}


/* +-------------------------------------------------------------------+ */
/* | Copyright 1990, 1991, 1993, David Koblas.  (koblas@netcom.com)    | */
/* |   Permission to use, copy, modify, and distribute this software   | */
/* |   and its documentation for any purpose and without fee is hereby | */
/* |   granted, provided that the above copyright notice appear in all | */
/* |   copies and that both that copyright notice and this permission  | */
/* |   notice appear in supporting documentation.  This software is    | */
/* |   provided "as is" without express or implied warranty.           | */
/* +-------------------------------------------------------------------+ */


#define        MAXCOLORMAPSIZE         256

#define        TRUE    1
#define        FALSE   0

#define CM_RED         0
#define CM_GREEN       1
#define CM_BLUE                2

#define        MAX_LWZ_BITS            12

#define INTERLACE              0x40
#define LOCALCOLORMAP  0x80
#define BitSet(byte, bit)      (((byte) & (bit)) == (bit))

#define        ReadOK(file,buffer,len) (fread(buffer, len, 1, file) != 0)

#define LM_to_uint(a,b)                        (((b)<<8)|(a))

/* We may eventually want to use this information, but def it out for now */
#if 0
static struct {
       unsigned int    Width;
       unsigned int    Height;
       unsigned char   ColorMap[3][MAXCOLORMAPSIZE];
       unsigned int    BitPixel;
       unsigned int    ColorResolution;
       unsigned int    Background;
       unsigned int    AspectRatio;
} GifScreen;
#endif

static struct {
       int     transparent;
       int     delayTime;
       int     inputFlag;
       int     disposal;
} Gif89 = { -1, -1, -1, 0 };

static int ReadColorMap (FILE *fd, int number, unsigned char (*buffer)[256]);
static int DoExtension (FILE *fd, int label, int *Transparent);
static int GetDataBlock (FILE *fd, unsigned char *buf);
static int GetCode (FILE *fd, int code_size, int flag);
static int LWZReadByte (FILE *fd, int flag, int input_code_size);
static void ReadImage (gdImagePtr im, FILE *fd, int len, int height, unsigned char (*cmap)[256], int interlace, int ignore);

int ZeroDataBlock;

gdImagePtr
gdImageCreateFromGif(FILE *fd)
{
       int imageNumber;
       int BitPixel;
       int ColorResolution;
       int Background;
       int AspectRatio;
       int Transparent = (-1);
       unsigned char   buf[16];
       unsigned char   c;
       unsigned char   ColorMap[3][MAXCOLORMAPSIZE];
       unsigned char   localColorMap[3][MAXCOLORMAPSIZE];
       int             imw, imh;
       int             useGlobalColormap;
       int             bitPixel;
       int             imageCount = 0;
       char            version[4];
       gdImagePtr im = 0;
       ZeroDataBlock = FALSE;

       imageNumber = 1;
       if (! ReadOK(fd,buf,6)) {
		return 0;
	}
       if (strncmp((char *)buf,"GIF",3) != 0) {
		return 0;
	}
       strncpy(version, (char *)buf + 3, 3);
       version[3] = '\0';

       if ((strcmp(version, "87a") != 0) && (strcmp(version, "89a") != 0)) {
		return 0;
	}
       if (! ReadOK(fd,buf,7)) {
		return 0;
	}
       BitPixel        = 2<<(buf[4]&0x07);
       ColorResolution = (int) (((buf[4]&0x70)>>3)+1);
       Background      = buf[5];
       AspectRatio     = buf[6];

       if (BitSet(buf[4], LOCALCOLORMAP)) {    /* Global Colormap */
               if (ReadColorMap(fd, BitPixel, ColorMap)) {
			return 0;
		}
       }
       for (;;) {
               if (! ReadOK(fd,&c,1)) {
                       return 0;
               }
               if (c == ';') {         /* GIF terminator */
                       int i;
                       if (imageCount < imageNumber) {
                               return 0;
                       }
                       /* Terminator before any image was declared! */
                       if (!im) {
                              return 0;
                       }
		       /* Check for open colors at the end, so
                          we can reduce colorsTotal and ultimately
                          BitsPerPixel */
                       for (i=((im->colorsTotal-1)); (i>=0); i--) {
                               if (im->open[i]) {
                                       im->colorsTotal--;
                               } else {
                                       break;
                               }
                       }
                       return im;
               }

               if (c == '!') {         /* Extension */
                       if (! ReadOK(fd,&c,1)) {
                               return 0;
                       }
                       DoExtension(fd, c, &Transparent);
                       continue;
               }

               if (c != ',') {         /* Not a valid start character */
                       continue;
               }

               ++imageCount;

               if (! ReadOK(fd,buf,9)) {
	               return 0;
               }

               useGlobalColormap = ! BitSet(buf[8], LOCALCOLORMAP);

               bitPixel = 1<<((buf[8]&0x07)+1);

               imw = LM_to_uint(buf[4],buf[5]);
               imh = LM_to_uint(buf[6],buf[7]);
	       if (!(im = gdImageCreate(imw, imh))) {
			 return 0;
	       }
               im->interlace = BitSet(buf[8], INTERLACE);
               if (! useGlobalColormap) {
                       if (ReadColorMap(fd, bitPixel, localColorMap)) {
                                 return 0;
                       }
                       ReadImage(im, fd, imw, imh, localColorMap,
                                 BitSet(buf[8], INTERLACE),
                                 imageCount != imageNumber);
               } else {
                       ReadImage(im, fd, imw, imh,
                                 ColorMap,
                                 BitSet(buf[8], INTERLACE),
                                 imageCount != imageNumber);
               }
               if (Transparent != (-1)) {
                       gdImageColorTransparent(im, Transparent);
               }
       }
}

static int
ReadColorMap(FILE *fd, int number, unsigned char (*buffer)[256])
{
       int             i;
       unsigned char   rgb[3];


       for (i = 0; i < number; ++i) {
               if (! ReadOK(fd, rgb, sizeof(rgb))) {
                       return TRUE;
               }
               buffer[CM_RED][i] = rgb[0] ;
               buffer[CM_GREEN][i] = rgb[1] ;
               buffer[CM_BLUE][i] = rgb[2] ;
       }


       return FALSE;
}

static int
DoExtension(FILE *fd, int label, int *Transparent)
{
       static unsigned char     buf[256];

       switch (label) {
       case 0xf9:              /* Graphic Control Extension */
               (void) GetDataBlock(fd, (unsigned char*) buf);
               Gif89.disposal    = (buf[0] >> 2) & 0x7;
               Gif89.inputFlag   = (buf[0] >> 1) & 0x1;
               Gif89.delayTime   = LM_to_uint(buf[1],buf[2]);
               if ((buf[0] & 0x1) != 0)
                       *Transparent = buf[3];

               while (GetDataBlock(fd, (unsigned char*) buf) != 0)
                       ;
               return FALSE;
       default:
               break;
       }
       while (GetDataBlock(fd, (unsigned char*) buf) != 0)
               ;

       return FALSE;
}

static int
GetDataBlock(FILE *fd, unsigned char *buf)
{
       unsigned char   count;

       if (! ReadOK(fd,&count,1)) {
               return -1;
       }

       ZeroDataBlock = count == 0;

       if ((count != 0) && (! ReadOK(fd, buf, count))) {
               return -1;
       }

       return count;
}

static int
GetCode(FILE *fd, int code_size, int flag)
{
       static unsigned char    buf[280];
       static int              curbit, lastbit, done, last_byte;
       int                     i, j, ret;
       unsigned char           count;

       if (flag) {
               curbit = 0;
               lastbit = 0;
               done = FALSE;
               return 0;
       }

       if ( (curbit+code_size) >= lastbit) {
               if (done) {
                       if (curbit >= lastbit) {
                                /* Oh well */
                       }
                       return -1;
               }
               buf[0] = buf[last_byte-2];
               buf[1] = buf[last_byte-1];

               if ((count = GetDataBlock(fd, &buf[2])) == 0)
                       done = TRUE;

               last_byte = 2 + count;
               curbit = (curbit - lastbit) + 16;
               lastbit = (2+count)*8 ;
       }

       ret = 0;
       for (i = curbit, j = 0; j < code_size; ++i, ++j)
               ret |= ((buf[ i / 8 ] & (1 << (i % 8))) != 0) << j;

       curbit += code_size;

       return ret;
}

static int
LWZReadByte(FILE *fd, int flag, int input_code_size)
{
       static int      fresh = FALSE;
       int             code, incode;
       static int      code_size, set_code_size;
       static int      max_code, max_code_size;
       static int      firstcode, oldcode;
       static int      clear_code, end_code;
       static int      table[2][(1<< MAX_LWZ_BITS)];
       static int      stack[(1<<(MAX_LWZ_BITS))*2], *sp;
       register int    i;

       if (flag) {
               set_code_size = input_code_size;
               code_size = set_code_size+1;
               clear_code = 1 << set_code_size ;
               end_code = clear_code + 1;
               max_code_size = 2*clear_code;
               max_code = clear_code+2;

               GetCode(fd, 0, TRUE);

               fresh = TRUE;

               for (i = 0; i < clear_code; ++i) {
                       table[0][i] = 0;
                       table[1][i] = i;
               }
               for (; i < (1<<MAX_LWZ_BITS); ++i)
                       table[0][i] = table[1][0] = 0;

               sp = stack;

               return 0;
       } else if (fresh) {
               fresh = FALSE;
               do {
                       firstcode = oldcode =
                               GetCode(fd, code_size, FALSE);
               } while (firstcode == clear_code);
               return firstcode;
       }

       if (sp > stack)
               return *--sp;

       while ((code = GetCode(fd, code_size, FALSE)) >= 0) {
               if (code == clear_code) {
                       for (i = 0; i < clear_code; ++i) {
                               table[0][i] = 0;
                               table[1][i] = i;
                       }
                       for (; i < (1<<MAX_LWZ_BITS); ++i)
                               table[0][i] = table[1][i] = 0;
                       code_size = set_code_size+1;
                       max_code_size = 2*clear_code;
                       max_code = clear_code+2;
                       sp = stack;
                       firstcode = oldcode =
                                       GetCode(fd, code_size, FALSE);
                       return firstcode;
               } else if (code == end_code) {
                       int             count;
                       unsigned char   buf[260];

                       if (ZeroDataBlock)
                               return -2;

                       while ((count = GetDataBlock(fd, buf)) > 0)
                               ;

                       if (count != 0)
                       return -2;
               }

               incode = code;

               if (code >= max_code) {
                       *sp++ = firstcode;
                       code = oldcode;
               }

               while (code >= clear_code) {
                       *sp++ = table[1][code];
                       if (code == table[0][code]) {
                               /* Oh well */
                       }
                       code = table[0][code];
               }

               *sp++ = firstcode = table[1][code];

               if ((code = max_code) <(1<<MAX_LWZ_BITS)) {
                       table[0][code] = oldcode;
                       table[1][code] = firstcode;
                       ++max_code;
                       if ((max_code >= max_code_size) &&
                               (max_code_size < (1<<MAX_LWZ_BITS))) {
                               max_code_size *= 2;
                               ++code_size;
                       }
               }

               oldcode = incode;

               if (sp > stack)
                       return *--sp;
       }
       return code;
}

static void
ReadImage(gdImagePtr im, FILE *fd, int len, int height, unsigned char (*cmap)[256], int interlace, int ignore)
{
       unsigned char   c;
       int             v;
       int             xpos = 0, ypos = 0, pass = 0;
       int i;
       /* Stash the color map into the image */
       for (i=0; (i<gdMaxColors); i++) {
               im->red[i] = cmap[CM_RED][i];
               im->green[i] = cmap[CM_GREEN][i];
               im->blue[i] = cmap[CM_BLUE][i];
               im->open[i] = 1;
       }
       /* Many (perhaps most) of these colors will remain marked open. */
       im->colorsTotal = gdMaxColors;
       /*
       **  Initialize the Compression routines
       */
       if (! ReadOK(fd,&c,1)) {
               return;
       }
       if (LWZReadByte(fd, TRUE, c) < 0) {
               return;
       }

       /*
       **  If this is an "uninteresting picture" ignore it.
       */
       if (ignore) {
               while (LWZReadByte(fd, FALSE, c) >= 0)
                       ;
               return;
       }

       while ((v = LWZReadByte(fd,FALSE,c)) >= 0 ) {
               /* This how we recognize which colors are actually used. */
               if (im->open[v]) {
                       im->open[v] = 0;
               }
               gdImageSetPixel(im, xpos, ypos, v);
               ++xpos;
               if (xpos == len) {
                       xpos = 0;
                       if (interlace) {
                               switch (pass) {
                               case 0:
                               case 1:
                                       ypos += 8; break;
                               case 2:
                                       ypos += 4; break;
                               case 3:
                                       ypos += 2; break;
                               }

                               if (ypos >= height) {
                                       ++pass;
                                       switch (pass) {
                                       case 1:
                                               ypos = 4; break;
                                       case 2:
                                               ypos = 2; break;
                                       case 3:
                                               ypos = 1; break;
                                       default:
                                               goto fini;
                                       }
                               }
                       } else {
                               ++ypos;
                       }
               }
               if (ypos >= height)
                       break;
       }

fini:
       if (LWZReadByte(fd,FALSE,c)>=0) {
               /* Ignore extra */
       }
}

void gdImageRectangle(gdImagePtr im, int x1, int y1, int x2, int y2, int color)
{
	gdImageLine(im, x1, y1, x2, y1, color);
	gdImageLine(im, x1, y2, x2, y2, color);
	gdImageLine(im, x1, y1, x1, y2, color);
	gdImageLine(im, x2, y1, x2, y2, color);
}

void gdImageFilledRectangle(gdImagePtr im, int x1, int y1, int x2, int y2, int color)
{
	int x, y;
	for (y=y1; (y<=y2); y++) {
		for (x=x1; (x<=x2); x++) {
			gdImageSetPixel(im, x, y, color);
		}
	}
}

void gdImageCopy(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int w, int h)
{
	int c;
	int x, y;
	int tox, toy;
	int i;
	int colorMap[gdMaxColors];
	for (i=0; (i<gdMaxColors); i++) {
		colorMap[i] = (-1);
	}
	toy = dstY;
	for (y=srcY; (y < (srcY + h)); y++) {
		tox = dstX;
		for (x=srcX; (x < (srcX + w)); x++) {
			int nc;
			c = gdImageGetPixel(src, x, y);
			/* Added 7/24/95: support transparent copies */
			if (gdImageGetTransparent(src) == c) {
				tox++;
				continue;
			}
			/* Have we established a mapping for this color? */
			if (colorMap[c] == (-1)) {
				/* If it's the same image, mapping is trivial */
				if (dst == src) {
					nc = c;
				} else {
					/* First look for an exact match */
					nc = gdImageColorExact(dst,
						src->red[c], src->green[c],
						src->blue[c]);
				}
				if (nc == (-1)) {
					/* No, so try to allocate it */
					nc = gdImageColorAllocate(dst,
						src->red[c], src->green[c],
						src->blue[c]);
					/* If we're out of colors, go for the
						closest color */
					if (nc == (-1)) {
						nc = gdImageColorClosest(dst,
							src->red[c], src->green[c],
							src->blue[c]);
					}
				}
				colorMap[c] = nc;
			}
			gdImageSetPixel(dst, tox, toy, colorMap[c]);
			tox++;
		}
		toy++;
	}
}

void gdImageCopyResized(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int dstW, int dstH, int srcW, int srcH)
{
	int c;
	int x, y;
	int tox, toy;
	int ydest;
	int i;
	int colorMap[gdMaxColors];
	/* Stretch vectors */
	int *stx;
	int *sty;
	/* We only need to use floating point to determine the correct
		stretch vector for one line's worth. */
	double accum;
	stx = (int *) malloc(sizeof(int) * srcW);
	sty = (int *) malloc(sizeof(int) * srcH);
	accum = 0;
	for (i=0; (i < srcW); i++) {
		int got;
		accum += (double)dstW/(double)srcW;
		got = floor(accum);
		stx[i] = got;
		accum -= got;
	}
	accum = 0;
	for (i=0; (i < srcH); i++) {
		int got;
		accum += (double)dstH/(double)srcH;
		got = floor(accum);
		sty[i] = got;
		accum -= got;
	}
	for (i=0; (i<gdMaxColors); i++) {
		colorMap[i] = (-1);
	}
	toy = dstY;
	for (y=srcY; (y < (srcY + srcH)); y++) {
		for (ydest=0; (ydest < sty[y-srcY]); ydest++) {
			tox = dstX;
			for (x=srcX; (x < (srcX + srcW)); x++) {
				int nc;
				if (!stx[x - srcX]) {
					continue;
				}
				c = gdImageGetPixel(src, x, y);
				/* Added 7/24/95: support transparent copies */
				if (gdImageGetTransparent(src) == c) {
					tox += stx[x-srcX];
					continue;
				}
				/* Have we established a mapping for this color? */
				if (colorMap[c] == (-1)) {
					/* If it's the same image, mapping is trivial */
					if (dst == src) {
						nc = c;
					} else {
						/* First look for an exact match */
						nc = gdImageColorExact(dst,
							src->red[c], src->green[c],
							src->blue[c]);
					}
					if (nc == (-1)) {
						/* No, so try to allocate it */
						nc = gdImageColorAllocate(dst,
							src->red[c], src->green[c],
							src->blue[c]);
						/* If we're out of colors, go for the
							closest color */
						if (nc == (-1)) {
							nc = gdImageColorClosest(dst,
								src->red[c], src->green[c],
								src->blue[c]);
						}
					}
					colorMap[c] = nc;
				}
				for (i=0; (i < stx[x - srcX]); i++) {
					gdImageSetPixel(dst, tox, toy, colorMap[c]);
					tox++;
				}
			}
			toy++;
		}
	}
	free(stx);
	free(sty);
}

int gdGetWord(int *result, FILE *in)
{
	int r;
	r = getc(in);
	if (r == EOF) {
		return 0;
	}
	*result = r << 8;
	r = getc(in);
	if (r == EOF) {
		return 0;
	}
	*result += r;
	return 1;
}

void gdPutWord(int w, FILE *out)
{
	putc((unsigned char)(w >> 8), out);
	putc((unsigned char)(w & 0xFF), out);
}

int gdGetByte(int *result, FILE *in)
{
	int r;
	r = getc(in);
	if (r == EOF) {
		return 0;
	}
	*result = r;
	return 1;
}

gdImagePtr gdImageCreateFromGd(FILE *in)
{
	int sx, sy;
	int x, y;
	int i;
	gdImagePtr im;
	if (!gdGetWord(&sx, in)) {
		goto fail1;
	}
	if (!gdGetWord(&sy, in)) {
		goto fail1;
	}
	im = gdImageCreate(sx, sy);
	if (!gdGetByte(&im->colorsTotal, in)) {
		goto fail2;
	}
	if (!gdGetWord(&im->transparent, in)) {
		goto fail2;
	}
	if (im->transparent == 257) {
		im->transparent = (-1);
	}
	for (i=0; (i<gdMaxColors); i++) {
		if (!gdGetByte(&im->red[i], in)) {
			goto fail2;
		}
		if (!gdGetByte(&im->green[i], in)) {
			goto fail2;
		}
		if (!gdGetByte(&im->blue[i], in)) {
			goto fail2;
		}
	}
	for (y=0; (y<sy); y++) {
		for (x=0; (x<sx); x++) {
			int ch;
			ch = getc(in);
			if (ch == EOF) {
				gdImageDestroy(im);
				return 0;
			}
			im->pixels[x][y] = ch;
		}
	}
	return im;
fail2:
	gdImageDestroy(im);
fail1:
	return 0;
}

void gdImageGd(gdImagePtr im, FILE *out)
{
	int x, y;
	int i;
	int trans;
	gdPutWord(im->sx, out);
	gdPutWord(im->sy, out);
	putc((unsigned char)im->colorsTotal, out);
	trans = im->transparent;
	if (trans == (-1)) {
		trans = 257;
	}
	gdPutWord(trans, out);
	for (i=0; (i<gdMaxColors); i++) {
		putc((unsigned char)im->red[i], out);
		putc((unsigned char)im->green[i], out);
		putc((unsigned char)im->blue[i], out);
	}
	for (y=0; (y < im->sy); y++) {
		for (x=0; (x < im->sx); x++) {
			putc((unsigned char)im->pixels[x][y], out);
		}
	}
}

gdImagePtr
gdImageCreateFromXbm(FILE *fd)
{
	gdImagePtr im;
	int bit;
	int w, h;
	int bytes;
	int ch;
	int i, x, y;
	char *sp;
	char s[161];
	if (!fgets(s, 160, fd)) {
		return 0;
	}
	sp = &s[0];
	/* Skip #define */
	sp = strchr(sp, ' ');
	if (!sp) {
		return 0;
	}
	/* Skip width label */
	sp++;
	sp = strchr(sp, ' ');
	if (!sp) {
		return 0;
	}
	/* Get width */
	w = atoi(sp + 1);
	if (!w) {
		return 0;
	}
	if (!fgets(s, 160, fd)) {
		return 0;
	}
	sp = s;
	/* Skip #define */
	sp = strchr(sp, ' ');
	if (!sp) {
		return 0;
	}
	/* Skip height label */
	sp++;
	sp = strchr(sp, ' ');
	if (!sp) {
		return 0;
	}
	/* Get height */
	h = atoi(sp + 1);
	if (!h) {
		return 0;
	}
	/* Skip declaration line */
	if (!fgets(s, 160, fd)) {
		return 0;
	}
	bytes = (w * h / 8) + 1;
	im = gdImageCreate(w, h);
	gdImageColorAllocate(im, 255, 255, 255);
	gdImageColorAllocate(im, 0, 0, 0);
	x = 0;
	y = 0;
	for (i=0; (i < bytes); i++) {
		char h[3];
		int b;
		/* Skip spaces, commas, CRs, 0x */
		while(1) {
			ch = getc(fd);
			if (ch == EOF) {
				goto fail;
			}
			if (ch == 'x') {
				break;
			}
		}
		/* Get hex value */
		ch = getc(fd);
		if (ch == EOF) {
			goto fail;
		}
		h[0] = ch;
		ch = getc(fd);
		if (ch == EOF) {
			goto fail;
		}
		h[1] = ch;
		h[2] = '\0';
		sscanf(h, "%x", &b);
		for (bit = 1; (bit <= 128); (bit = bit << 1)) {
			gdImageSetPixel(im, x++, y, (b & bit) ? 1 : 0);
			if (x == im->sx) {
				x = 0;
				y++;
				if (y == im->sy) {
					return im;
				}
				/* Fix 8/8/95 */
				break;
			}
		}
	}
	/* Shouldn't happen */
	fprintf(stderr, "Error: bug in gdImageCreateFromXbm!\n");
	return 0;
fail:
	gdImageDestroy(im);
	return 0;
}

void gdImagePolygon(gdImagePtr im, gdPointPtr p, int n, int c)
{
	int i;
	int lx, ly;
	if (!n) {
		return;
	}
	lx = p->x;
	ly = p->y;
	gdImageLine(im, lx, ly, p[n-1].x, p[n-1].y, c);
	for (i=1; (i < n); i++) {
		p++;
		gdImageLine(im, lx, ly, p->x, p->y, c);
		lx = p->x;
		ly = p->y;
	}
}

int gdCompareInt(const void *a, const void *b);

void gdImageFilledPolygon(gdImagePtr im, gdPointPtr p, int n, int c)
{
	int i;
	int y;
	int y1, y2;
	int ints;
	if (!n) {
		return;
	}
	if (!im->polyAllocated) {
		im->polyInts = (int *) malloc(sizeof(int) * n);
		im->polyAllocated = n;
	}
	if (im->polyAllocated < n) {
		while (im->polyAllocated < n) {
			im->polyAllocated *= 2;
		}
		im->polyInts = (int *) realloc(im->polyInts,
			sizeof(int) * im->polyAllocated);
	}
	y1 = p[0].y;
	y2 = p[0].y;
	for (i=1; (i < n); i++) {
		if (p[i].y < y1) {
			y1 = p[i].y;
		}
		if (p[i].y > y2) {
			y2 = p[i].y;
		}
	}
	for (y=y1; (y <= y2); y++) {
		int interLast = 0;
		int dirLast = 0;
		int interFirst = 1;
		ints = 0;
		for (i=0; (i <= n); i++) {
			int x1, x2;
			int y1, y2;
			int dir;
			int ind1, ind2;
			int lastInd1 = 0;
			if ((i == n) || (!i)) {
				ind1 = n-1;
				ind2 = 0;
			} else {
				ind1 = i-1;
				ind2 = i;
			}
			y1 = p[ind1].y;
			y2 = p[ind2].y;
			if (y1 < y2) {
				y1 = p[ind1].y;
				y2 = p[ind2].y;
				x1 = p[ind1].x;
				x2 = p[ind2].x;
				dir = -1;
			} else if (y1 > y2) {
				y2 = p[ind1].y;
				y1 = p[ind2].y;
				x2 = p[ind1].x;
				x1 = p[ind2].x;
				dir = 1;
			} else {
				/* Horizontal; just draw it */
				gdImageLine(im,
					p[ind1].x, y1,
					p[ind2].x, y1,
					c);
				continue;
			}
			if ((y >= y1) && (y <= y2)) {
				int inter =
					(y-y1) * (x2-x1) / (y2-y1) + x1;
				/* Only count intersections once
					except at maxima and minima. Also,
					if two consecutive intersections are
					endpoints of the same horizontal line
					that is not at a maxima or minima,
					discard the leftmost of the two. */
				if (!interFirst) {
					if ((p[ind1].y == p[lastInd1].y) &&
						(p[ind1].x != p[lastInd1].x)) {
						if (dir == dirLast) {
							if (inter > interLast) {
								/* Replace the old one */
								im->polyInts[ints] = inter;
							} else {
								/* Discard this one */
							}
							continue;
						}
					}
					if (inter == interLast) {
						if (dir == dirLast) {
							continue;
						}
					}
				}
				if (i > 0) {
					im->polyInts[ints++] = inter;
				}
				lastInd1 = i;
				dirLast = dir;
				interLast = inter;
				interFirst = 0;
			}
		}
		qsort(im->polyInts, ints, sizeof(int), gdCompareInt);
		for (i=0; (i < (ints-1)); i+=2) {
			gdImageLine(im, im->polyInts[i], y,
				im->polyInts[i+1], y, c);
		}
	}
}

int gdCompareInt(const void *a, const void *b)
{
	return (*(const int *)a) - (*(const int *)b);
}

void gdImageSetStyle(gdImagePtr im, int *style, int noOfPixels)
{
	if (im->style) {
		free(im->style);
	}
	im->style = (int *)
		malloc(sizeof(int) * noOfPixels);
	memcpy(im->style, style, sizeof(int) * noOfPixels);
	im->styleLength = noOfPixels;
	im->stylePos = 0;
}

void gdImageSetBrush(gdImagePtr im, gdImagePtr brush)
{
	int i;
	im->brush = brush;
	for (i=0; (i < gdImageColorsTotal(brush)); i++) {
		int index;
		index = gdImageColorExact(im,
			gdImageRed(brush, i),
			gdImageGreen(brush, i),
			gdImageBlue(brush, i));
		if (index == (-1)) {
			index = gdImageColorAllocate(im,
				gdImageRed(brush, i),
				gdImageGreen(brush, i),
				gdImageBlue(brush, i));
			if (index == (-1)) {
				index = gdImageColorClosest(im,
					gdImageRed(brush, i),
					gdImageGreen(brush, i),
					gdImageBlue(brush, i));
			}
		}
		im->brushColorMap[i] = index;
	}
}

void gdImageSetTile(gdImagePtr im, gdImagePtr tile)
{
	int i;
	im->tile = tile;
	for (i=0; (i < gdImageColorsTotal(tile)); i++) {
		int index;
		index = gdImageColorExact(im,
			gdImageRed(tile, i),
			gdImageGreen(tile, i),
			gdImageBlue(tile, i));
		if (index == (-1)) {
			index = gdImageColorAllocate(im,
				gdImageRed(tile, i),
				gdImageGreen(tile, i),
				gdImageBlue(tile, i));
			if (index == (-1)) {
				index = gdImageColorClosest(im,
					gdImageRed(tile, i),
					gdImageGreen(tile, i),
					gdImageBlue(tile, i));
			}
		}
		im->tileColorMap[i] = index;
	}
}

void gdImageInterlace(gdImagePtr im, int interlaceArg)
{
	im->interlace = interlaceArg;
}


/*  ==============  Statements containing RCS keywords:  */
 static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/gd/RCS/gd.c,v $";
 static char rcs_id2[] = "$Id: gd.c,v 1.3 2002/02/11 15:10:22 michaelo Exp $";
/*  ===================================================  */


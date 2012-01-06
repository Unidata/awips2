/*
 * The Python Imaging Library.
 * $Id: tkImaging.c,v 1.1 1996/05/09 22:11:39 fredrik Exp $
 *
 * TK interface for Python Imaging objects
 *
 * Copies (parts of) a named display memory to a photo image object.
 * Also contains code to create an display memory.  Under Tk, a
 * display memory is simply an "L" or "RGB" image memory that is
 * allocated in a single block.
 *
 * To use this module, add the following lines to your Tcl_AppInit
 * function (in tkappinit.c).  Put them after the calls to Tcl_Init
 * and Tk_Init:
 *	
 *	{
 *          extern void TkImaging_Init(Tcl_Interp* interp);
 *          TkImaging_Init(interp);
 *      }
 *
 * This registers a Tcl command called "PyImagingPhoto", which
 * is use to communicate between PIL and Tk's PhotoImage handler.
 *
 * Compile and link tkImaging.c with tkappinit.c and _tkinter
 * (see the Setup file for details on how to use tkappinit.c).
 * Note that _tkinter.c must be compiled with WITH_APPINIT.
 *
 * History:
 * 1995-09-12 fl  Created
 * 1996-04-08 fl  Ready for release
 * 1997-05-09 fl  Use command instead of image type
 * 2001-03-18 fl  Initialize alpha layer pointer (struct changed in 8.3)
 * 2003-04-23 fl  Fixed building for Tk 8.4.1 and later (Jack Jansen)
 * 2004-06-24 fl  Fixed building for Tk 8.4.6 and later.
 *
 * Copyright (c) 1997-2004 by Secret Labs AB
 * Copyright (c) 1995-2004 by Fredrik Lundh
 *
 * See the README file for information on usage and redistribution.
 */

/* This is needed for (at least) Tk 8.4.1, otherwise the signature of
** Tk_PhotoPutBlock changes.
*/
#define USE_COMPOSITELESS_PHOTO_PUT_BLOCK

/* This is needed for (at least) Tk 8.4.6 and later, to avoid warnings
   for the Tcl_CreateCommand command. */
#define USE_COMPAT_CONST

#include "tk.h"

#include "Imaging.h"

#include <stdlib.h>


static Imaging
ImagingFind(const char* name)
{
    long id;

    /* FIXME: use CObject instead? */
    id = atol(name);
    if (!id)
	return NULL;

    return (Imaging) id;
}


static int
PyImagingPhotoPut(ClientData clientdata, Tcl_Interp* interp,
               int argc, char **argv)
{
    Imaging im;
    Tk_PhotoHandle photo;
    Tk_PhotoImageBlock block;

    if (argc != 3) {
        Tcl_AppendResult(interp, "usage: ", argv[0],
                         " destPhoto srcImage", (char *) NULL);
        return TCL_ERROR;
    }

    /* get Tcl PhotoImage handle */
    photo = Tk_FindPhoto(interp, argv[1]);
    if (photo == NULL) {
        Tcl_AppendResult(
            interp, "destination photo must exist", (char *) NULL
            );
        return TCL_ERROR;
    }

    /* get PIL Image handle */
    im = ImagingFind(argv[2]);
    if (!im) {
	Tcl_AppendResult(interp, "bad name", (char*) NULL);
	return TCL_ERROR;
    }
    if (!im->block) {
	Tcl_AppendResult(interp, "bad display memory", (char*) NULL);
	return TCL_ERROR;
    }

    /* Active region */
#if 0
    if (src_xoffset + xsize > im->xsize)
	xsize = im->xsize - src_xoffset;
    if (src_yoffset + ysize > im->ysize)
	ysize = im->ysize - src_yoffset;
    if (xsize < 0 || ysize < 0
	|| src_xoffset >= im->xsize
	|| src_yoffset >= im->ysize)
	return TCL_OK;
#endif

    /* Mode */

    if (strcmp(im->mode, "1") == 0 || strcmp(im->mode, "L") == 0) {
	block.pixelSize = 1;
	block.offset[0] = block.offset[1] = block.offset[2] = 0;
    } else if (strncmp(im->mode, "RGB", 3) == 0) {
	block.pixelSize = 4;
	block.offset[0] = 0;
	block.offset[1] = 1;
	block.offset[2] = 2;
	block.offset[3] = 0; /* no alpha (or reserved, under 8.2) */
    } else {
        Tcl_AppendResult(interp, "Bad mode", (char*) NULL);
	return TCL_ERROR;
    }

    block.width = im->xsize;
    block.height = im->ysize;
    block.pitch = im->linesize;
    block.pixelPtr = (unsigned char*) im->block;
#if 0
    block.pixelPtr = (unsigned char*) im->block +
	             src_yoffset * im->linesize +
	             src_xoffset * im->pixelsize;
#endif

    if (strcmp(im->mode, "RGBA") == 0) {
        /* Copy non-transparent pixels to photo image */
        int x, y;
        Tk_PhotoImageBlock run;

        /* Clear current contents */
        Tk_PhotoBlank(photo);

        /* Setup run descriptor */
        run.height = 1;
        run.pitch = block.pitch;
        run.pixelSize = block.pixelSize;
        run.offset[0] = 0;
        run.offset[1] = 1;
        run.offset[2] = 2;
	run.offset[3] = 0; /* no alpha (or reserved, under 8.2) */

        /* Copy opaque runs to photo image */
        for (y = 0; y < block.height; y++) {
            unsigned char* p = block.pixelPtr + y*block.pitch;
            unsigned char* s = p;
            int   w = 0;
            for (x = 0; x < block.width; x++) {
                if (p[3]) {
                    /* opaque: add pixel to current run */
                    if (w == 0)
                        s = p;
                    w = w + 1;
                } else if (s) {
                    /* copy run to photo image */
                    if (w > 0) {
                        run.width = w;
                        run.pixelPtr = s;
                        Tk_PhotoPutBlock(photo, &run, x-w, y, run.width, 1);
                    }
                    w = 0;
                }
                p += block.pixelSize;
            }
            if (w > 0) {
                /* copy final run, if any */
                run.width = w;
                run.pixelPtr = s;
                Tk_PhotoPutBlock(photo, &run, x-w, y, run.width, 1);
          }
        }

    } else

        /* Copy opaque block to photo image, and leave the rest to TK */
        Tk_PhotoPutBlock(photo, &block, 0, 0, block.width, block.height);

    return TCL_OK;
}


static int
PyImagingPhotoGet(ClientData clientdata, Tcl_Interp* interp,
               int argc, char **argv)
{
    Tk_PhotoHandle photo;
    Tk_PhotoImageBlock block;

    if (argc != 2) {
        Tcl_AppendResult(interp, "usage: ", argv[0],
                         " srcPhoto", (char *) NULL);
        return TCL_ERROR;
    }

    /* get Tcl PhotoImage handle */
    photo = Tk_FindPhoto(interp, argv[1]);
    if (photo == NULL) {
        Tcl_AppendResult(
            interp, "source photo must exist", (char *) NULL
            );
        return TCL_ERROR;
    }

    Tk_PhotoGetImage(photo, &block);

    printf("pixelPtr = %p\n", block.pixelPtr);
    printf("width = %d\n", block.width);
    printf("height = %d\n", block.height);
    printf("pitch = %d\n", block.pitch);
    printf("pixelSize = %d\n", block.pixelSize);
    printf("offset = %d %d %d %d\n", block.offset[0], block.offset[1],
           block.offset[2], block.offset[3]);

    Tcl_AppendResult(
        interp, "this function is not yet support", (char *) NULL
        );

    return TCL_ERROR;
}


void
TkImaging_Init(Tcl_Interp* interp)
{
    Tcl_CreateCommand(interp, "PyImagingPhoto", PyImagingPhotoPut,
                      (ClientData) 0, (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "PyImagingPhotoGet", PyImagingPhotoGet,
                      (ClientData) 0, (Tcl_CmdDeleteProc*) NULL);
}

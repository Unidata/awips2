/*
 * The Python Imaging Library.
 * $Id: ZipDecode.c 2134 2004-10-06 08:55:20Z fredrik $
 *
 * decoder for ZIP (deflated) image data.
 *
 * history:
 * 1996-12-14 fl   Created (for PNG)
 * 1997-01-15 fl   Prepared to read TIFF/ZIP
 * 2001-11-19 fl   PNG incomplete read patch (from Bernhard Herzog)
 *
 * Copyright (c) Fredrik Lundh 1996.
 * Copyright (c) Secret Labs AB 1997-2001.
 *
 * See the README file for information on usage and redistribution.
 */


#include "Imaging.h"

#ifdef	HAVE_LIBZ

#include "Zip.h"


/* -------------------------------------------------------------------- */
/* Decoder								*/
/* -------------------------------------------------------------------- */

int
ImagingZipDecode(Imaging im, ImagingCodecState state, UINT8* buf, int bytes)
{
    ZIPSTATE* context = (ZIPSTATE*) state->context;
    int err;
    int n;
    UINT8* ptr;
    int i, bpp;

    if (!state->state) {

	/* Initialization */
	if (context->mode == ZIP_PNG || context->mode == ZIP_PNG_PALETTE)
	    context->prefix = 1; /* PNG */

	/* Expand standard buffer to make room for the (optional) filter
	   prefix, and allocate a buffer to hold the previous line */
	free(state->buffer);
	state->buffer = (UINT8*) malloc(state->bytes+1);
	context->previous = (UINT8*) malloc(state->bytes+1);
	if (!state->buffer || !context->previous) {
	    state->errcode = IMAGING_CODEC_MEMORY;
	    return -1;
	}

        context->last_output = 0;

	/* Initialize to black */
	memset(context->previous, 0, state->bytes+1);

	/* Setup decompression context */
	context->z_stream.zalloc = (alloc_func)0;
	context->z_stream.zfree = (free_func)0;
	context->z_stream.opaque = (voidpf)0;

	err = inflateInit(&context->z_stream);
	if (err < 0) {
	    state->errcode = IMAGING_CODEC_CONFIG;
	    return -1;
	}

	/* Ready to decode */
	state->state = 1;

    }

    /* Setup the source buffer */
    context->z_stream.next_in = buf;
    context->z_stream.avail_in = bytes;

    /* Decompress what we've got this far */
    while (context->z_stream.avail_in > 0) {

	context->z_stream.next_out = state->buffer + context->last_output;
	context->z_stream.avail_out =
            state->bytes + context->prefix - context->last_output;

	err = inflate(&context->z_stream, Z_NO_FLUSH);

	if (err < 0) {
	    /* Something went wrong inside the compression library */
	    if (err == Z_DATA_ERROR)
		state->errcode = IMAGING_CODEC_BROKEN;
	    else if (err == Z_MEM_ERROR)
		state->errcode = IMAGING_CODEC_MEMORY;
	    else
		state->errcode = IMAGING_CODEC_CONFIG;
	    free(context->previous);
	    inflateEnd(&context->z_stream);
	    return -1;
	}

	n = state->bytes + context->prefix - context->z_stream.avail_out;

	if (n < state->bytes + context->prefix) {
            context->last_output = n;
	    break; /* need more input data */
	}

	/* Apply predictor */
	switch (context->mode) {
	case ZIP_PNG:
	    switch (state->buffer[0]) {
	    case 0:
		break;
	    case 1:
		/* prior */
		bpp = (state->bits + 7) / 8;
		for (i = bpp+1; i <= state->bytes; i++)
		    state->buffer[i] += state->buffer[i-bpp];
		break;
	    case 2:
		/* up */
		for (i = 1; i <= state->bytes; i++)
		    state->buffer[i] += context->previous[i];
		break;
	    case 3:
		/* average */
		bpp = (state->bits + 7) / 8;
		for (i = 1; i <= bpp; i++)
		    state->buffer[i] += context->previous[i]/2;
		for (; i <= state->bytes; i++)
		    state->buffer[i] +=
			(state->buffer[i-bpp] + context->previous[i])/2;
		break;
	    case 4:
		/* paeth filtering */
		bpp = (state->bits + 7) / 8;
		for (i = 1; i <= bpp; i++)
		    state->buffer[i] += context->previous[i];
		for (; i <= state->bytes; i++) {
		    int a, b, c;
		    int pa, pb, pc;

		    /* fetch pixels */
		    a = state->buffer[i-bpp];
		    b = context->previous[i];
		    c = context->previous[i-bpp];

		    /* distances to surrounding pixels */
		    pa = abs(b - c);
		    pb = abs(a - c);
		    pc = abs(a + b - 2*c);

		    /* pick predictor with the shortest distance */
		    state->buffer[i] +=
			(pa <= pb && pa <= pc) ? a : (pb <= pc) ? b : c;

		}
		break;
	    default:
		state->errcode = IMAGING_CODEC_UNKNOWN;
		free(context->previous);
		inflateEnd(&context->z_stream);
		return -1;
	    }
	    break;
	case ZIP_TIFF_PREDICTOR:
	    bpp = (state->bits + 7) / 8;
	    for (i = bpp+1; i <= state->bytes; i++)
		state->buffer[i] += state->buffer[i-bpp];
	    break;
	}

	/* Stuff data into the image */
	state->shuffle((UINT8*) im->image[state->y + state->yoff] + 
		       state->xoff * im->pixelsize,
		       state->buffer + context->prefix,
		       state->xsize);

	state->y++;

        /* all inflate output has been consumed */
        context->last_output = 0;

	if (state->y >= state->ysize || err == Z_STREAM_END) {

	    /* The image and the data should end simultaneously */
	    /* if (state->y < state->ysize || err != Z_STREAM_END)
		state->errcode = IMAGING_CODEC_BROKEN; */

	    free(context->previous);
	    inflateEnd(&context->z_stream);
	    return -1; /* end of file (errcode=0) */

	}

	/* Swap buffer pointers */
	ptr = state->buffer;
	state->buffer = context->previous;
	context->previous = ptr;

    }

    return bytes; /* consumed all of it */

}

#endif

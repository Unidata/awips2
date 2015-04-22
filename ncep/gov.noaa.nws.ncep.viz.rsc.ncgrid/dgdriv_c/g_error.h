/************************************************************************
 * G_ERROR.H								*
 *									*
 **									*
 * Log:									*
 * T. Piper/SAIC	01/05	Added G_MOTIF				*
 ***********************************************************************/

/* Return codes */

#define G_NORMAL	(   0 )
				/* Normal */
#define G_NDWTBL	(   2 )
				/* No dwell table, use defaults */
#define G_NWSIZE	( -39 )
				/* Window size has changed */
#define G_NOMETA	( -42 )
				/* Error opening metafile */
#define G_NMAXFR	( -43 )
				/* Too many frames */
#define G_NMDATA	( -44 )
				/* Too much data for metafile */
#define G_NWINDW	( -45 )
				/* Maximum number of windows opened */
#define G_NCLRAL	( -46 )
				/* Color cell allocation failure */
#define G_NIWNAM	( -47 )
				/* Invalid window name */
#define G_NIMGFL	( -48 )
				/* Cannot open image file */
#define G_NIMGTBL	( -49 )
				/* Cannot find image table file
				   "imgtyp.tbl" */
#define G_NIMGENT	( -50 )
				/* Cannot find the image type entry */
#define G_NWUSED	( -51 )
				/* Window is already in use */
#define G_NMEMRY	( -52 )
				/* Memory allocation failure */
#define G_BADATOM	( -53 )
				/* Error in interning an atom of xw */
#define G_NSRDAT	( -54 )
				/* Cannot get shared data */
#define G_NGRAFCOL	( -55 )
				/* Not enough graphic colors */
#define G_NIMGTYP	( -56 )
				/* Unknown image type */
#define G_NICBANK	( -57 )
				/* Invalid color bank ID */
#define G_NFILENM	( -58 )
				/* File name is too long */
#define G_NIMGCOL	( -59 )
				/* Not enough image colors */
#define G_NIMCORD	( -60 )
				/* Invalid image display coordinates */
#define G_NIMGFMT	( -61 )
				/* Invalid image format */
#define G_ZEROCB	( -62 )
				/* 0 color in a color bank */
#define G_NIDSIZ	( -63 )
				/* Invalid device size */
#define G_NCBALOC	( -64 )
				/* Color bank not allocated */
#define G_NEWWIN	( -65 )
				/* New window was created */
#define G_NOPSFL	( -66 )
				/* Error opening PostScript file */
#define G_NOCLOS	( -67 )
				/* Cannot close last window */
#define G_NOFNTFL	( -68 )
				/* No font file found */
#define G_NOBITMP	( -69 )
				/* No open bitmap found */
#define G_NORDOPN	( -70 )
				/* Could not open raster for read */
#define G_NOWROPN	( -71 )
				/* Could not open 6 bit for write */
#define G_NOPROD	( -72 )
				/* No matching product in table */
#define G_NOTBL		( -73 )
				/* Product table not found */
#define G_NOEDGE	( -74 )
				/* Missing Edge table */
#define G_NOISCHD	( -75 )
				/* No matching record in ISCHED array */
#define G_BADSUB	( -76 )
				/* Bad or invalid subset requested */
#define G_BADPXV	( -77 )
				/* Bad min/max pixel values in
				   image table */
#define G_NOUTFL	( -78 )
				/* Cannot open UTF output file */
#define G_NAFSMX	( -79 )
				/* Maximum size for AFOS file exceeded */
#define G_NROAM		( -80 )
				/* Roam window out of pixmap bounds */
#define G_NDISP		( -83 )
				/* DISPLAY not set or invalid */
#define G_MOTIF		( -84 )
				/* MOTIF version no longer supported */

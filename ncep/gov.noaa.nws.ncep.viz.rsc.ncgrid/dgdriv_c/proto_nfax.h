/************************************************************************
 * proto_nfax.h								*
 *									*
 * This file contains header files and global variables for use in the	*
 * NFAX routines.							*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC          1/01   					*
 ***********************************************************************/

#ifndef PROTO_NFAX
#define	PROTO_NFAX


/*
 *  NFAX prototypes
 */

void 	cpg_dmpct ( 	char 		*plane,	
			char 		*descr,
			int 		start_pack,		
			int 		plane_sz,	
			ConvertRec  	crec[],
			int 		*numcuts,
			int 		*iret );

void 	cpg_shoct ( 	char 		*pfname,
			char 		*pdesc,  
			int 		*iret );

void 	cpg_sixrd ( 	char 		*pfname,
			char 		*psubset,
			char 		*pdesc,  
			int 		*ixlen,
			int 		*iylen,
			int 		*bpp,
			int 		*iret );

void 	fontsave_cb ( 	Widget, long, XmSelectionBoxCallbackStruct* );

void 	fontsel_dlg_CB ( Widget, long, XmSelectionBoxCallbackStruct* );

void 	fontsz_CB ( 	Widget, long, XmSelectionBoxCallbackStruct* );

unsigned int get_depth ( Widget 	w );

void 	pg_find_start (	char 		*plane,
			int 		peof,
			int 		*start_at );

void 	pg_find_stop ( 	char 		*plane,
			int 		peof,
			int 		start_at,
			int 		*stop_at );

int	pg_getfile (	char *fname );

int 	pg_getfname (	char 		*subset, 
			char 		*fname, 
			char 		*descr );

void 	pg_getsix ( 	char 		*outl,
			int 		sixnum,
			char 		*ch,
			int 		*iret );

void 	pg_mtch ( 	FILE 		*fp,
			char 		*srch_str,  
			char 		*srch_str2, 
			char 		*prod_str );	

void 	pg_print ( 	char 		*infname,
			int 		xsz,
			int 		ysz,
			int 		sample,
			char 		*prt_name,
			int 		prt_size,
			char 		*outfname,
			int 		*iret );

void 	pg_prse ( 	char   		*prod_str,	
			PrdRec 		*pr,	
			int 		*iret );


void 	pg_rdhdr ( 	char 		*plane,
			int 		start_at,
			char 		*text );

void 	pg_read_cuts ( 	char 		*subset,	
			char 		*plane,
			int 		start_pack,	
			int 		plane_sz,
			ConvertRec 	crec[],
			int 		*numcuts,	
			int 		*match_cut,	
			int 		*iret );
			
void 	pg_rindex ( 	char 		*pname,
			PrdRec 		*prec,
			int 		*iret );

void 	pg_rlst ( 	char 		*wheel,
			char 		*subset,
			PrdRec 		*prec,
			int 		*iret );

void 	pg_setbits (
			char 		*map,
			int 		start,
			int 		run,
			int 		value );

int 	pg_xplane ( 	int 		ixsz,
			int 		*iysz,
			char 		*outl,
			int 		start_at,
			int 		stop_at,
			char 		*bits );

void 	set_scroll_sz ( Cardinal 	x,
			Cardinal 	y );

void 	vexitCB ( 	Widget, long, XtPointer );

int 	vfiledisp ( 	char 		*fname,
			Cardinal	xsz,
			Cardinal 	ysz,
			int		samp_fact,
			int 		mirror,
			int 		flip,
			Pixmap 		*pxmap );

int 	vfindload ( 	int 		node,
			PrdRec 		*pr );

void 	vhoriz_scrollCB ( Widget, XtPointer, XtPointer );

void 	vmenuCB ( 	Widget, String, XtPointer );

void 	vnodeCB	( 	Widget, long, XtPointer );

void 	vraster_openCB (Widget, long, XmSelectionBoxCallbackStruct* );

int 	vrdbmap ( 	char 		*filename,
			Cardinal 	xsize,
			Cardinal	ysize,
			int		samp_factor,
			int 		mirror,
			int 		flip,
			Pixmap 		*pmap,
			int 		*iret );

void 	vsizeCB	( 	Widget, long, XtPointer );

void 	vshowmap ( 	Cardinal	xse,
			Cardinal 	yse,
			int 		xorig,
			int 		yorig,
			GC		gc,
			Pixmap 		*pmap,
			Widget 		w );

void 	vtitle ( 	Widget 		w,
			char 		*label );

void 	vvert_scrollCB ( Widget, XtPointer, XtPointer );

void 	vwheelCB ( 	Widget, long, XtPointer );

#endif	/* PROTO_NFAX */

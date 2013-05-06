/************************************************************************
 * GEMPRM.H								*
 *									*
 * This header file defines all of the parameters used in the GEMPAK	*
 * software. It also defines all the C routines with an underline at	*
 * the end of the routine name, if necessary. It includes as well all	*
 * C function prototypes in the common nawips libraries except those	*
 * requiring the VG_DBStruct structure.					*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 4/94						*
 * S. Jacobs/NMC	 6/94	General clean up			*
 * P. Bruehl/Unidata	 8/94	Added mv_swp4 to UNDERSCORE called by	*
 *				new xsatim.c				*
 * P. Bruehl/Unidata	 8/94	Added G_NWSIZE, G_NWINDW & G_NCLRAL	*
 *				returncodes for Dave Himes (COMET)	*
 * P. Bruehl/Unidata	 8/94	Added xenanm & xclosp to UNDERSCORE	*
 *				define list (D.Himes/COMET)		*
 * S. Jacobs/NMC	10/94	Added mtextc to UNDERSCORE		*
 * D. Himes/COMET	12/94	Added xslutf to UNDERSCORE		*
 * C. Lin/EAI		 3/95	Added xcamgr,xcsdat,xgsdat,xgbank to	*
 *				UNDERSCORE; Added G_NIWNAM, G_NIMGFL,	*
 *				G_NIMGTBL, G_NIMGENT, G_NWUSED		*
 * J. Cowie/COMET	 5/95	Added image file types, xsicmn		*
 *				to UNDERSCORE list			*
 * J. Cowie/COMET	 5/95	Added error codes G_NIMCORD and	G_IMGFMT*
 * S. Jacobs/NMC	 7/95	Added DC, ER and SS libraries		*
 * L. Williams/EAI       8/95   increase REPMAX 10000->15000		*
 *                              increase MAXTYP    30->60		*
 * C. Lin/EAI            9/95   Added G_ZEROCB				*
 * J. Cowie/COMET	 9/95	Added G_NIDSIZ, xresize, xclosw		*
 *				to UNDERSCORE list			*
 * J. Cowie/COMET	10/95	Removed IFNONE, renumbered		*
 * J. Cowie/COMET	11/95	Removed G_NLUTFL;add G_NCBALOC,G_NICBANK*
 * S. Jacobs/NCEP	12/95	Added gb_gbdh to UNDERSCORE		*
 * D. Plummer/NCEP	 2/96	Added gb_diag to UNDERSCORE		*
 * G. Krueger/EAI	 3/96	Added G_FSRCH, G_BSRCH, LLBSIZ, LLSCRN,	*
 *				LLPATH					*
 * D. Plummer/NCEP	 3/96	Added MMHDRS				*
 * M. Linda/GSC		 3/96	Added G_NEWWIN				*
 * S. Jacobs/NCEP	 4/96	Added G_NOPSFL				*
 * G. Krueger/EAI	 4/96	Added TMCK for Kelvin to Celsius conv	*
 * S. Jacobs/NCEP	 4/96	Added all PS C funcs to UNDERSCORE	*
 * S. Jacobs/NCEP	 5/96	Added xqdatt,mqdatt,pqdatt to UNDERSCORE*
 * S. Schotz/NCEP	 5/96	Increased MAXTYP from 60 to 200		*
 * S. Schotz/NCEP	 5/96	Increased REPMAX from 15000 to 20000	*
 * J. Cowie/COMET	 3/96	Added mv_btoi_				*
 * S. Jacobs/NCEP	 6/96	Removed the DA library from UNDERSCORE	*
 * S. Jacobs/NCEP	 7/96	Added the GEMPAK file and source types	*
 * E. Wehner/EAi	 7/96	Added error codes for FAX driver	*
 *				and function protos.			*
 * S. Jacobs/NCEP	 7/96	Added IFINVD; Added MV_ITOB to		*
 *				UNDERSCORE list				*
 * S. Jacobs/NCEP	11/96	Added pscnam to	UNDERSCORE		*
 * K. Tyle/GSC		12/96	Added ER_INIT, ER_LMSG, ER_SMSG,	*
 *				ER_STAT, ER_WBUF, IN_BDTA, DC_WBUF	*
 *				to UNDERSCORE list			*
 * S. Jacobs/NCEP	 1/97	Added error G_BADPXV; Added psicmn,	*
 *				pqclrs, psatim to UNDERSORE list	*
 * E. Safford/GSC	 2/97	Added errors G_NOUTFL and G_NAFSMX and	*
 *				utf driver UNDERSCORE list.		*
 * S. Jacobs/NCEP	 2/97	Reorganized; Moved UNDERSORE list to	*
 *				uscore.h; Moved error codes to error.h;	*
 *				Made consistent with FORTRAN parm file	*
 * D.W.Plummer/NCEP	 5/97	Fix to G_NINT C macro			*
 * S. Maxwell/GSC	 7/97	Added LLMXLN				*
 * T. Lee/GSC		10/97	Added MFCOUN				*
 * D.W.Plummer/NCEP	 1/99	Added CLO location types		*
 * S. Law/GSC		01/99	Added statute miles/meters conversions	*
 * D.W.Plummer/NCEP	 2/99	Added CNTY_BNDS location type		*
 * S. Law/GSC		03/99	Added G_DIST				*
 * D.W.Plummer/NCEP	 4/99	Add M2NM,NM2M (meters, naut miles conv)	*
 * D.W.Plummer/NCEP	 4/99	Add MARINE and COASTAL CLO types	*
 * S. Law/GSC		05/99	Moved dttm and fdttm from nmap_dttm.h	*
 * D. Kidwell/NCEP	 5/99	Increased LLSTFL from 3800 to 4800 and	*
 *				MMHDRS from 4000 to 5000		*
 * S. Law/GSC		05/99	Add m/s <--> mph (statute)		*
 * G. Grosshans/SPC	06/99   Add m/s <--> nautical miles/hour	*
 * S. Jacobs/NCEP	 8/99	Added CAT_* and SCAT_* parameters	*
 * S. Jacobs/NCEP	 9/99	Added new data type for date/time strngs*
 * S. Law/GSC		11/99	CAT_MISC -> CAT_MSC			*
 * E. Safford/GSC	12/99	move cursor definitions in from nmapprm *
 * H. Zeng/EAI		05/00	changed cursor definitions		*
 * M. Li/GSC		05/00	Added MXFLSZ and MXNMFL			*
 * S. Law/GSC		06/00	added MXTIME and MAXTMPLT		*
 * R. Curtis/EAI	09/00	Added IFNCDF for NetCDF files		*
 * R. Curtis/EAI	10/00	Removed MXTIME definition		*
 * A. Hardy/GSC		11/00	Added coordinate system abbreviations	*
 * S. Chiswell/Unidata	11/00	Added IFNEXZ for compressed NIDS files	*
 * M. Li/GSC		12/00	Added nmpdef.h				*
 * S. Jacobs/NCEP	 2/01	Made MTLNUX a separate machine type	*
 * S. Jacobs/NCEP	 2/01	Increased MAXTYP 200 -> 300		*
 * S. Jacobs/NCEP	 2/01	Increased REPMAX 20000 -> 80000 (SPC)	*
 * S. Jacobs/NCEP	 3/01	Added new type nmlst_t for string arrays*
 * H. Zeng/EAI		03/01	Added pgen group type related defs.     *
 * J. Wu/GSC		04/01	Added logo-related definitions.		*
 * D.W.Plummer/NCEP	 5/01	Added macros DDTODM and DMTODD		*
 * J. Wu/GSC		 5/01	add NWS_LOGO & increase MAX_LOGO to 2	*
 * D.W.Plummer/NCEP	 6/01	Added MXTAGN (max tag name length)	*
 * S. Jacobs/NCEP	 1/02	Increased REPMAX 80000 -> 100000 (SPC)	*
 * E. Safford/SAIC	04/02	added cvgdef.h				*
 * S. Jacobs/NCEP	 5/02	Fixed parentheses on DDTODM		*
 * R. Tian/SAIC		05/02	Added IFNFAX fax image type		*
 * S. Jacobs/NCEP	 7/02	Restored DDTODM to previous version	*
 * R. Tian/SAIC		9/02	Increase LLMXGD from 97000 to 275000	*
 * m.gamazaychikov/SAIC 01/03	Added a new parameter NFILLPAT=7	*
 * H. Zeng/XTRIA	07/03	Added Feet to Meters conversion		*
 * E. Safford/SAIC	11/03	Moved MAX_LAYERS in from pgprm.h        *
 * D.W.Plummer/NCEP	11/03	Added G_MOD macro			*
 * J. Wu/SAIC		01/04	move MXERST from errcmn.h (max # of	*
 *				error messages)				*
 * D.W.Plummer/NCEP	02/04	Added G_MALLOC macro			*
 * E. Safford/SAIC	02/04	added _8_BIT				*
 * D.W.Plummer/NCEP	04/04	Added G_REALLOC and G_FREE macros	*
 * T. Piper/SAIC	06/04	Increased REPMAX 100000 > 150000 (SPC)	*
 * F. J. Yen/NCEP 	06/04	Corrected constant HR2S from 2.74e-4F	*
 * J. Wu/SAIC		07/04	add definitions for display filter	*
 * B. Yin/SAIC		 9/04	renamed a parameter of G_MALLOC		*
 * B. Yin/SAIC		 3/05	added enum for arrow directions		*
 * D. Plummer/NCEP	03/05	added LLMXTG				*
 * D. Kidwell/NCEP	 4/05	Increased MMPARM from 40 to 44		*
 * T. Piper/SAIC	05/05	Increased LLMXPT from 10K to 50K	*
 * T. Piper/SAIC	 6/05	Increased LLMDGG from 2304000 to 4000000* 
 * T. Piper/SAIC	07/05	Increased LLMXPT from 50K to 80K	*
 * T. Piper/SAIC	10/05	Added MAXPTS from vgstruct.h		*
 * T. Piper/SAIC	01/06	Added G_CALLOC, G_DIFF, & G_DIFFD	*
 * m.gamazaychikov/SAIC 01/06   Added a new parameter MXTMPL = 49	*
 * T. Piper/SAIC	06/06	Removed 'Location types'; in clo.tbl	*
 * D. PlummerNCEP	07/06   Increase LLMXGD to 750000 grid points   *
 * S. Gilbert/NCEP      08/06   Added sys_U                             *
 * M. Li/SAIC		08/06	Increase GRP_MXELE from 20 to 25	*
 * D.W.Plummer/NCEP	10/06	Increase LLMXGD & LLMXTG to 1M		*
 * T. Piper/SAIC	02/07	Renamed error.h to g_error.h		*
 * F. J. Yen/NCEP	03/07	Moved MAX_CNTY in from vgstruct.h	* 
 * T. Piper/SAIC	05/07	Increased LLMXLN to 128 + 1		*
 * T. Piper/SAIC        08/07	Increased LLSTFL from 4800 to 30000	*
 * T. Piper/SAIC	08/07	Redefined MMHDRS to LLSTFL + LLMXTM	*
 * E. Safford/SAIC	12/07	add G_Boolean and G_Cardinal		*
 * T. Piper/SAIC	02/08	Moved REPMAX to nwx_cmn.h		*
 * M. Li/SAIC		02/08	Added CAT_ENS				*
 ***********************************************************************/

#ifndef _GEMPRM_H
#define _GEMPRM_H

/*---------------------------------------------------------------------*/

/*
 *  Types that mirror those provided by XtIntrinsic.h via XmAll.h.  New
 *  programs should use these, rather than the X/Motif types.  
 */
typedef char		G_Boolean;
typedef unsigned int	G_Cardinal;


/*---------------------------------------------------------------------*/

#include <sys/param.h>	/* needed for MAXPATHLEN */
#include "uscore.h"
#include "g_error.h"
#include "nmpdef.h"  
#include "cvgdef.h"

/*---------------------------------------------------------------------*/

/*  Missing data definitions */

#define	RMISSD	( -9999.0F )	/* Missing data value		 */
#define	RDIFFD	( 0.1F )	/* Missing value fuzziness	 */
#define	IMISSD	( -9999 )	/* Missing integer value	 */

/*---------------------------------------------------------------------*/

/*  Physical and mathematical constants */

#define	PI	( 3.14159265 )
#define	HALFPI	( PI / 2.0 )
#define	TWOPI	( 2.0 * PI )
#define	PI4TH	( PI / 4.0 )
#define	PI3RD	( PI / 3.0 )	/* PI,...			*/
#define	DTR	( PI / 180.0 )
#define	RTD	( 180.0 / PI )	/* Degrees <--> Radians		*/
#define	S2HR	( 3600.0F )
#define	HR2S	( 2.778e-4F )	/* Seconds <--> Hours		*/
#define	SM2M	( 1609.34F )
#define	M2SM	( 6.21e-4F )	/* Statute miles <--> Meters	*/
#define	MS2SMH	( S2HR * M2SM )
#define	SMH2MS	( HR2S * SM2M )	/* Meters/Second <--> Miles/Hour*/
#define	NM2M	( 1852.0F )
#define	M2NM	( 5.4e-4F )	/* Nautical miles <--> Meters	*/
#define	F2M	( 0.3048 )
#define	M2F	( 3.2808 )	/* Feet  <--> Meters		*/
#define	MS2NMH	( S2HR * M2NM )
#define	NMH2MS	( HR2S * NM2M )	/* Nautical miles/hour <-> Meters/Sec */
#define	RADIUS	( 6371200.0F )	/* Earth radius			*/
#define	OMEGA	( 7.2921e-5 )	/* Earth angular veclocity	*/
#define	GRAVTY	( 9.80616F )	/* Acceleration of gravity	*/
#define	RDGAS	( 287.04F )
#define	RKAP	( RDGAS / GRAVTY )	/* Gas constant of dry air	*/
#define	RKAPPA	( 2.0F / 7.0F )
#define	AKAPPA	( 7.0F / 2.0F )	/* Poisson constant;inverse	*/
#define	GAMUSD	( 6.5F )	/* US std atmos lapse rate	*/
#define	TMCK	( 273.15F )	/* Celsius -> Kelvin		*/

/*---------------------------------------------------------------------*/

/*  File information parameters */

#define	MMKEY	(   12 )	/* Maximum # of keys */
#define	MMHDRS	( LLSTFL + LLMXTM ) /* Maximum # of headers */
#define	MMPRT	(   20 )	/* Maximum # of parts */
#define	MMLIST	(   20 )	/* Maximum search list */
#define	MMFREE	(   62 )	/* Number of free pairs */
#define	MMFILE	(    3 )	/* Maximum # of open files */
#define	MBLKSZ	(  128 )	/* Block size */
#define	MCACHE	(    8 )	/* # of cached records */
#define	MMPARM	(   44 )	/* Maximum # of parameters */
#define	MMFHDR	(   10 )	/* Maximum # of file hdrs */
#define	MMSRCH	(   30 )	/* Max # of cond searches */
#define LLPATH  ( MAXPATHLEN )	/* Largest path name. */
/*  Consciously using 100 rather than MAXNAMLEN  */
#define	MXFLSZ	( 100 + 1 )	/* Maximum file name size (not including path) */
/*  PLEASE, do NOT use FILE_NAMESZ; it is being deprecated  */
#define FILE_NAMESZ ( MXFLSZ ) /* Largest file name */
#define FILE_FULLSZ ( LLPATH + MXFLSZ ) /* Largest full (path + file) name */
#define	MXNMFL	(  1000 )	/* Maximum number of files per directory       */
#define	MXTMPL	( 48 + 1 )	/* Maximum template size (not including path) */
#define	MMFLDP	( MMFILE * MMPRT )

/*---------------------------------------------------------------------*/

/*  Set the machine type */

#define MTVAX	(  2 )
#define MTSUN	(  3 )
#define MTIRIS	(  4 )
#define MTAPOL	(  5 )
#define MTIBM	(  6 )
#define MTIGPH	(  7 )
#define MTULTX	(  8 )
#define MTHP	(  9 )
#define MTALPH	( 10 )
#define MTLNUX	( 11 )

#ifdef SunOS
#define MTMACH	MTSUN
#endif

#ifdef IRIX
#define MTMACH	MTIRIS
#endif

#ifdef DomainOS
#define MTMACH	MTAPOL
#endif

#ifdef AIX
#define MTMACH	MTIBM
#endif

#ifdef ULTRIX
#define MTMACH	MTULTX
#endif

#ifdef HPUX
#define MTMACH	MTHP
#endif

#ifdef OSF1
#define MTMACH	MTALPH
#endif

#ifdef Linux
#define MTMACH	MTLNUX
#endif

/*---------------------------------------------------------------------*/

#if defined(IRIX) || defined(OSF1) || defined(ULTRIX)
#define MMRECL	( 1 )		/* Multiplier for RECL in file  */
#else
#define MMRECL	( 4 )		/* create/open (usually 4 on UNIX sys) */
#endif

#define MDREAL		(   1 )
#define MDINTG		(   2 )
#define MDCHAR		(   3 )
#define MDRPCK		(   4 )
#define MDGRID		(   5 )	/* Data types in DM lib */
#define MDGNON		(   0 )
#define MDGGRB		(   1 )
#define MDGNMC		(   2 )
#define MDGDIF		(   3 )
#define MDGDEC		(   4 )	/* Grid packing types */
#define MFSF		(   1 )
#define MFSN		(   2 )
#define MFGD		(   3 )	/* Data file types */
#define MFUNKN		(  99 )
#define MFAIRW		(   1 )
#define MFMETR		(   2 )
#define MFSHIP		(   3 )	/* Unknown, airways, metar, ship data source */
#define MFBUOY		(   4 )
#define MFSYNP		(   5 )	/* Buoy and synoptic data source */
#define MFRAOB		(   4 )
#define MFVAS		(   5 )	/* Rawinsonde and VAS data source */
#define MFGRID		(   6 )	/* Grid source */
#define MFCOUN  	(   7 )	/* Watch-by-county data source */
#define MFTEXT		( 100 )	/* Text */

/*---------------------------------------------------------------------*/

/*  Declarations for array sizes in programs */

#define	LLAXIS		(      64 )	/* Max # of axis labels */
#define	LLCLEV		(      50 )	/* Max # of contour lvls */
#define	LLGDHD		(     128 )	/* Max grid hdr length */
#define	LLMDGG		( 4000000 )	/* Max mem for intern grids */
#define	LLMXDT		( MMPARM * LLMXLV ) /* Max # data points */
#define	LLMXGD		( 1000000 )	/* Max # grid points */
#define	LLMXGT		(    1000 )	/* Max # grid times */
#define	LLMXLN		( 128 + 1 )	/* Max length of input */
#define	LLMXLV		(     500 )	/* Max # levels/station */
#define	LLMXPT		(   80000 )	/* Max # lines, polygons, ... */
#define	LLMXST		(      20 )	/* Max # stations in list */
#define	LLMXTG		( 1000000 )	/* Ultimate max # grid points */
#define	LLMXTM		(     200 )	/* Max # times/dataset */
#define	LLNANL		(     128 )	/* Grid anl block length */
#define	LLNNAV		(     256 )	/* Grid nav block length */
#define	LLOAGD		(     400 )	/* Max # grids from 1 OA */
#define	LLSTFL		(   30000 )	/* Max # stations in file */
#define	LLSTHL		(      20 )	/* Max header size */
#define	LLTMCX		(     100 )	/* Max # of time xsect pts */
#define MAX_CNTY        (     400 )     /* Max # of counties in watch */
#define	MAXTMPLT	(      50 )	/* Maximum data templates */
#define	MXLOOP		(      30 )	/* Maximum frames in loop */

/*---------------------------------------------------------------------*/

/*  Image file types.  Defined here and in gemprm.OS */

#define	IFINVD		( -1 )	/* Invalid image file format */
#define	IFAREA		(  1 )	/* MCIDAS AREA file	*/
#define	IFGINI		(  2 )	/* AWIPS GINI format	*/
#define	IFNIDS		(  3 )	/* Nexrad NIDS format	*/
#define	IFNOWR		(  4 )	/* WSI NOWRad format	*/
#define	IFNCDF		(  5 )	/* NetCDF file		*/
#define	IFNEXZ		(  6 )	/* NEXRAD zlib compressed */
#define	IFNFAX		(  7 )	/* 6-bit FAX file */
#define	IA2DB		(  8 )	/* A2DB sat image */

/*---------------------------------------------------------------------*/

/*  Data category and subcategory types */

#define	CAT_NIL		( 0 )	/* None - Not useful */
#define	CAT_IMG		( 1 )	/* Images */
#define	CAT_SFC		( 2 )	/* Surface observations */
#define	CAT_SFF		( 3 )	/* Surface forecast (e.g., MOS, TAF) */
#define	CAT_SND		( 4 )	/* Upper air observations */
#define	CAT_SNF		( 5 )	/* Upper air forecast */
#define	CAT_GRD		( 6 )	/* Grid */
#define	CAT_VGF		( 7 )	/* Vector graphics file */
#define	CAT_MSC		( 8 )	/* Misc (e.g., Watches, Warnings) */
#define	CAT_ENS		( 9 )	/* Ensembles */ 

#define	SCAT_NIL	( 0 )	/* None - Not useful */
#define	SCAT_SFC	( 1 )	/* Surface data in daily files */
#define	SCAT_SHP	( 2 )	/* Ship format data in hourly files */
#define	SCAT_SFF	( 3 )	/* Surface forecast (e.g., MOS, TAF) */
#define	SCAT_FFG	( 4 )	/* Flash flood guidance */
#define	SCAT_SND	( 5 )	/* Upper air data */
#define	SCAT_SNF	( 6 )	/* Upper air forecast */
#define	SCAT_FCT	( 7 )	/* Grid - forecast */
#define	SCAT_ANL	( 8 )	/* Grid - analysis */

/*---------------------------------------------------------------------*/

#define	NO_IMG		( 0 )
#define	SAT_IMG		( 1 )
#define	RAD_IMG		( 2 )

/*---------------------------------------------------------------------*/

/*  ASCII character constants */

#define	CHNULL	( (char)   0 )	/* Null */
#define	CHCTLA	( (char)   1 )	/* Control A - Start of header */
#define	CHCTLC	( (char)   3 )	/* Control C - End of text */
#define	CHTAB	( (char)   9 )	/* Tab */
#define	CHLF	( (char)  10 )	/* Line feed */
#define	CHFF	( (char)  12 )	/* Form feed */
#define	CHCR	( (char)  13 )	/* Carriage return */
#define	CHCAN	( (char)  24 )	/* Cancel */
#define	CHESC	( (char)  27 )	/* Escape */
#define	CHFS	( (char)  28 )	/* FS */
#define	CHGS	( (char)  29 )	/* GS */
#define	CHRS	( (char)  30 )	/* Record separator */
#define	CHUS	( (char)  31 )	/* US */
#define	CHSPAC	( (char)  32 )	/* Space */
#define	CHBKSL	( (char)  92 )	/* Backslash */
#define	CHTLDA	( (char) 126 )	/* Tilda */

/*---------------------------------------------------------------------*/

/*  Other useful constants, specifically for C */

#define LLBSIZ		(  1024 ) /* I/O Buffer size */
#define LLSCRN		(    80 ) /* Size of screen input/output strings. */
#define MAXTYP		(   300 ) /* Maximum number of data types */
#define MXERST		(   100 ) /* Maximum # of error messages in buffer */  

#define DTTMSZ		(    21 ) /* GEMPAK date/time string size */
#define DTTMS_SIZE	(    12 ) /* GEMPAK time string size (YYMMDD/HHMM) */
#define FDTTMS_SIZE	(DTTMS_SIZE + 6) /* forcast version (YYMMDD/HHMMfHHHMM) */

#define NDAYS		(    15 )
#define NFILES		( NDAYS * 24 ) /* Maximum number of days saved by LDM,
				   Number of files for NDAYS */

#define G_TRUE		(   1 ) /* TRUE */
#define G_FALSE		(   0 ) /* FALSE */
#define G_RESET		(  -1 ) /* RESET attributes value */
#define G_FSRCH		(   0 ) /* Search forward */
#define G_BSRCH		(   1 ) /* Search backward */

/*---------------------------------------------------------------------*/

/*  Macro definitions */

#define	G_MAX(a,b)	( ( (a) > (b) ) ? (a) :  (b) )
#define	G_MIN(a,b)	( ( (a) < (b) ) ? (a) :  (b) )
#define	G_ABS(a)	( ( (a) >= 0.0F  ) ? (a) : -(a) )
#define	G_NINT(x)	(((x)<0.0F)?((((x)-(float)((int)(x)))<=-.5F)?(int)((x)-.5F):(int)(x)):((((x)-(float)((int)(x)))>=.5F)?(int)((x)+.5F):(int)(x)))

/*
 *  This macro returns a double, though the parameters can be any 
 *  consitent type 
 */
#define	G_DIST(x1, y1, x2, y2) (sqrt ((double)		\
			      (((x2 - x1) * (x2 - x1)) +  \
			       ((y2 - y1) * (y2 - y1)))))

/*
 *  This macro is for comparing floating point equality
 */
#define	GDIFFD	0.000001
#define	G_DIFF(x,y)	( G_ABS(x-y) < GDIFFD )
#define G_DIFFT(x,y,t)	( G_ABS(x-y) < t )

/*
 *  This macro performs a modulus for floating point types
 */
#define	G_MOD(x,y) 	( (float)( (x)/(y) ) - (int)( (x)/(y) ) )

#define	ERMISS(xxxx)	( G_ABS(xxxx-RMISSD) < RDIFFD )

/*
 *  Convert Degrees-minutes to Degrees-decimal
 */
#define	DMTODD(xxxx)	( (int)((xxxx)/100.0F) + (float)(fmod((double)(xxxx),100.0)) / 60.0F )

/*
 *  Convert Degrees-decimal to Degrees-minutes
 */
#define	DDTODM(xxxx)    ( ((int)(xxxx)*100.0F) + G_NINT( (int)(((xxxx)-(int)(xxxx))*100.0) * .60 ) )

/*
 *  Allocate memory
 */
#define	G_CALLOC(p,type,np,str)  { if ((np) > 0) { \
		(p)=(type*)calloc((size_t)(np), sizeof(type)); \
		if (p==((type*)NULL)) { \
		    fprintf(stderr, "GEMPAK calloc failure: %s\n", (str) ); \
		} \
		} \
		else \
		    (p)=(type*)NULL; \
		}

#define	G_MALLOC(p,type,np,str)  { if ((np) > 0) { \
		(p)=(type*)malloc((np)*sizeof(type)); \
		if (p==((type*)NULL)) { \
		    fprintf(stderr, "GEMPAK malloc failure: %s\n", (str) ); \
		} \
		} \
		else \
		    (p)=(type*)NULL; \
		}

#define	G_REALLOC(p,type,np,str)  { if ((np) > 0) { \
		(p)=(type*)realloc((p),(np)*sizeof(type)); \
		if (p==((type*)NULL)) { \
		    fprintf(stderr, "GEMPAK realloc failure: %s\n", (str) ); \
		} \
		} \
		else \
		    (p)=(type*)NULL; \
		}

#define	G_FREE(p,type)      {if (p!=((type*)NULL))  \
		{free(p); (p)= (type*)NULL;}}


/*---------------------------------------------------------------------*/

/*  Type definitions */

typedef char	dattm_t[DTTMSZ];	/* GEMPAK date/time type */
typedef char	dttms_t[DTTMS_SIZE];	/* GEMPAK date/time group */
typedef char	fdttms_t[FDTTMS_SIZE];	/* forecast version */
typedef char	nmlst_t[MXFLSZ];	/* Type for list of names */
typedef enum	{ UP_LIST, DOWN_LIST } Arrow_Dir_t;	/* arrow directions */		

/*---------------------------------------------------------------------*/

/*
 *  Display filter definitions
 */
#define	MAX_FILTER_NUM		( 50 )
#define	DSPLY_FILTER_SZ		( 10 )
typedef char	filter_t[DSPLY_FILTER_SZ];

/*---------------------------------------------------------------------*/

/*
 *  Cursor definitions
 */
#define	CURS_DEFAULT		( 0 )
#define	CURS_POINT_SELECT	( 1 )
#define	CURS_BUSY		( 2 )


/*---------------------------------------------------------------------*/

/*
 *  Special predefined map button index in mapw
 */
#define EXTRA_PDFBTN		( 2 )    /* extra predefined geog buttons */
#define SAT_BTN			( 2 )    /* SAT button (next to last button) */
#define CUSTOM_BTN		( 1 )    /* customize button (=last button) */

/*---------------------------------------------------------------------*/

/* 
 *  Coordinate system abbreviations 
 */
#ifdef COORDSYS_GLOBAL

char sys_S[] = "S";
char sys_D[] = "D";
char sys_N[] = "N";
char sys_V[] = "V";
char sys_P[] = "P";
char sys_L[] = "L";
char sys_U[] = "U";
char sys_W[] = "W";
char sys_M[] = "M";
char sys_Q[] = "Q";
char sys_I[] = "I";
char sys_G[] = "G";

#else

extern char sys_S[];
extern char sys_D[];
extern char sys_N[];
extern char sys_V[];
extern char sys_P[];
extern char sys_L[];
extern char sys_U[];
extern char sys_W[];
extern char sys_M[];
extern char sys_Q[];
extern char sys_I[];
extern char sys_G[];

#endif
/*---------------------------------------------------------------------*/

/*
 *  PGEN group type related definitions.
 */
#define	GRPTYPE_TBL		"grptyp.tbl"
#define	GRP_MXELE		( 25 )	/* Max label items per group type     */
#define	GRP_MXINFO		( 128 )	/* Max chars for attribute info       */
#define	GRP_MXCHR		( 10 )	/* Max chars per item(including NULL) */
#define	NON_GRPID		( 127 )	/* An invalid group type id           */

/*---------------------------------------------------------------------*/

/*
 *  LOGO related definitions.
 */
#define	NOAA_LOGO		( 1 )
#define	NWS_LOGO		( 2 )
#define	MAX_LOGO		( 2 )	/* Max logo items available */

/*---------------------------------------------------------------------*/

/*
 *  Maximum tag name length (based on 80 column table max).
 */
#define	MXTAGN			( 77 )

/*---------------------------------------------------------------------*/

#define	MAX_LAYERS		( 20 )	/* Layer controls */
#define	MAXPTS			( 500 ) /* Maximum number of points in a line */
#define	NFILLPAT		( 7 )	/* Number of Fill Patterns in PGEN */

/*---------------------------------------------------------------------*/

/*
 *  Display Depth definitions
 */
#define	_8_BIT			( 8 )

/*---------------------------------------------------------------------*/

#include "proto.h"

#endif	/* _GEMPRM_H */

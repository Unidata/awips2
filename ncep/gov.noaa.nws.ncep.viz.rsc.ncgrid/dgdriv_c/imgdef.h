/************************************************************************
 * IMGDEF								*
 *									*
 * Common area for image header information				*
 *									*
 * NOTE: If you change anything in this file, make sure you change 	*
 *	 IMGDEF.CMN too.  Also, update xsicmn.c in the XW device 	*
 *	 driver.  You only need to have entries here for the first 	*
 *	 NIMCMN variables found in imgdef.cmn.				*
 **									*
 * Log:									*
 * J. Cowie/COMET	 3/95						*
 * C. Lin/EAI	         6/95	add icbank				*
 * C. Lin/EAI	         4/96	add EXTERN				*
 * S. Jacobs/NCEP	 1/97	Changed EXTERN to IMGDEF; Added image	*
 *				array declarations			*
 * C. Lin/EAI	         2/97	add _IMGDEF_H_				*
 * J. Cowie/COMET        1/97   Renamed variables                       *
 * S. Jacobs/NCEP	 4/97	Removed dpysize from lastimg_t struct	*
 * T. Piper/GSC		 5/99	Increased MXRWCL for roam of US 1km VIS *
 * D.W.Plummer/NCEP	 3/03	Add more image info from IMGDEF.CMN	*
 * T. Piper/SAIC	10/05	Removed MXRWCL; dynamically allocating	*
 ***********************************************************************/

#ifndef	_IMGDEF_H_
#define	_IMGDEF_H_

#define MNSCAL	0.0001

typedef struct {
	char	filename[256];
	size_t	imgsize;
} lastimg_t;

#ifdef IMGDEF

	int		imftyp ;
				/* Image file type */
	int		imbank ;
				/* Color bank ID */
	int		imdoff ;
				/* data start offset (bytes) */
	int		imldat ;
				/* length of data in file */
				
	int		imnpix, imnlin, imdpth ;
				/* full x, y dimensions, 
				   pixel depth (bytes) */

	float		rmxres, rmyres ;
				/* x, y pixel resolutions (km) */
				   
	int		imleft, imtop, imrght, imbot ;
				/* Bounds of subimage */
			
	float		rmxysc ;
				/* X-Y image scaling factor */

	int		imbswp ;
				/* byte swapped data flag */
				
	int		imnchl, imprsz, imdcsz, imclsz, imlvsz, imvald ;
				/* Things for reading AREA files */
	int		imrdfl ;
				/* radial product flag (NIDS) */
				
	int		immnpx, immxpx ;
				/* Min/max pixel values to use */
				
	int		imsorc, imtype ;
				/* Image source, type ID */
	int		imradf ;
				/* Radar image flag */
	float		rmbelv ;
				/* Radar beam elevation */
	int		immode ;
				/* Operational mode of radar
				   0 = maintenance
				   1 = clear air
				   2 = precip/storm 		*/
	int		imdate, imtime ;
				/* Image date (yyyymmdd), time (hhmmss) */
	char		cmsorc[21], cmtype[9] ;
				/* Image source (eg., "GOES8"),
				   Product type (eg., "IR") */
	char		cmstyp[5], cmcalb[5] ;
				/* Image source type (eg., "VISR", "GVAR"),
				   Calibration units (eg., "BRIT", "RAW") */


	unsigned char	*imgData;
	unsigned char	*rawData;
	unsigned int	last_rawsize;
				/* Image data arrays */

	lastimg_t	lastimg;
				/* Current image name */

#else

	extern int	imftyp ;
	extern int	imbank ;
	extern int	imdoff ;
	extern int	imldat ;
	extern int	imnpix, imnlin, imdpth ;
	extern float	rmxres, rmyres ;
	extern int	imleft, imtop, imrght, imbot ;
	extern float	rmxysc ;
	extern int	imbswp ;
	extern int	imnchl, imprsz, imdcsz, imclsz, imlvsz, imvald ;
	extern int	imrdfl ;
	extern int	immnpx, immxpx ;
	extern int	imsorc, imtype ;
	extern int	imradf ;
	extern float	rmbelv ;
	extern int	immode ;
	extern int	imdate, imtime ;
	extern char	cmsorc[21], cmtype[9] ;
	extern char	cmstyp[5], cmcalb[5] ;

	extern unsigned char	*imgData;
	extern unsigned char	*rawData;
	extern unsigned int	last_rawsize;

	extern lastimg_t	lastimg;

#endif

#endif /* _IMGDEF_H_ */

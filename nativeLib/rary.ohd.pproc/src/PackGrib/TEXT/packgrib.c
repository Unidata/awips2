/*
 * packgrib.c
 *
 *  Created on: Aug 26, 2011
 *      Author: snaples
 */
/*
** File:  packgrib_.c
**
** Author:  Bob Dattore
**          NCAR/DSS
**          dattore@ucar.edu
**          (303) 497-1825
**
** Latest Revision:  07 Jun 06
**
** Purpose:  to provide a single Fortran-callable routine for packing GRIB grids
**
** Notes:   1) There are several routines defined in this file, but the only one
**             that is callable from Fortran is PACKGRIB.
**
**          2) The user is expected to have some understanding of the GRIB
**             format, as some of the input to PACKGRIB consists of codes, etc.
**             that are defined by the GRIB format.  You can find a description
**             of the GRIB format at
**             http://dss.ucar.edu/docs/formats/grib/gribdoc.
**
**          3) PACKGRIB has been tested in Sun/Solaris, SGI/IRIX, and Linux
**             environments - please report any problems to dattore@ucar.edu.
**
**          4) Notes for Linux users:
**               if your C-compiler does not already predefine either the
**               "LINUX" or the "linux" macro, compile with -Dlinux.
**
**          5) Users running in a 64-bit environment will need to compile as
**               as 32-bit by adding the appropriate compiler option for both
**               the C and Fortran compilers
**
** Compiling:
**    prompt> cc -c packgrib_.c
**    prompt> f77 your_fortran_program.f packgrib_.o
**
**    If you get a warning about conflicting types for built-in function
**    'round', comment out or remove the lines below that define the routine
**    "int round(double val)".
**
** Example Fortran program:
**    You can download a example Fortran program from
**    http://dss.ucar.edu/libraries/grib/c_routines/packgrib_example.f.  This
**    program will write a GRIB file containing one GRIB record.
**
**
** Fortran calling syntax:
**    INTEGER   GRIB_LBL(43)
**    CHARACTER PDS_EXT*256
**    REAL      GRIDPOINTS(NUM_I,NUM_J)
**    INTEGER   OUTPUT_BUFFER(ODIM)
**
**    CALL PACKGRIB(GRIB_LBL,PDS_EXT,IPLEN,GRIDPOINTS,IGIDIM,MISS_VAL,
**   +OUTPUT_BUFFER,ODIM,LENGTH)
**
** When calling PACKGRIB:
**   GRIB_LBL    is the user-filled array of codes and values to be packed into
**               the description sections of the grid
**   PDS_EXT     is the optional user-filled array of supplemental information
**               to be appended to the GRIB PDS
**   IPLEN       is the user-defined number of characters in PDS_EXT that should
**               be transferred to octets 40 - 40+IPLEN of the PDS (use
**               IPLEN = 0 if you don't need this feature)
**   GRIDPOINTS  is the user-filled array of real values that will be packed
**               into the grid.  GRIDPOINTS should be dimensioned at least NUM_I
**               by NUM_J, where NUM_I is the number of gridpoints in the
**               i-direction and NUM_J is the number of gridpoints in the
**               j-direction.
**   IGIDIM      the first dimension of GRIDPOINTS
**   MISS_VAL    is the value used to indicate missing data in the grid
**   ODIM        is the user-defined dimension of OUTPUT_BUFFER and should be at
**               least as large as the expected length of the GRIB grid
**
**   **NOTE**: Since Fortran is pass-by-reference, be sure to use declared
**             variables or parameters, and not constants, when you pass IPDIM,
**             IGIDIM, MISS_VAL, and ODIM to PACKGRIB
**   Example of an *INCORRECT* call to PACKGRIB:
**    CALL PACKGRIB(GRIB_LBL,PDS_EXT,0,GRIDPOINTS,145,-999.,OUTPUT_BUFFER,
**   +50000,LENGTH)
**
** On return from PACKGRIB:
**   OUTPUT_BUFFER  is the buffer containing the GRIB representation of the grid
**   LENGTH         is the total length in octets (8-bit bytes) of the GRIB grid
**
** Overview of GRIB_LBL:
**   The first 29 locations in GRIB_LBL contain information about the GRIB grid
**   that must always be present.  The remainder of GRIB_LBL contains the grid
**   description, which must also be present, but is dependent on the type of
**   grid being packed.
**
** Description for GRIB_LBL(1) to GRIB_LBL(29):
**   GRIB_LBL(1):   GRIB Edition number
**   GRIB_LBL(2):   Grid number from NCEP ON 388 or 255 (user supplied grid)
**   GRIB_LBL(3):   Parameter Table Version number
**   GRIB_LBL(4):   Center ID
**   GRIB_LBL(5):   Generating Process ID number
**   GRIB_LBL(6):   Reserved - currently ignored
**   GRIB_LBL(7):   0=Grid Description Section not included; 1=GDS included
**   GRIB_LBL(8):   Parameter Code
**   GRIB_LBL(9):   Level Type Code
**   GRIB_LBL(10):  Value for First Level
**   GRIB_LBL(11):  Value for Second Level (or 0)
**   GRIB_LBL(12):  Year (4-digits - YYYY)
**   GRIB_LBL(13):  Month
**   GRIB_LBL(14):  Day
**   GRIB_LBL(15):  Time (HHMM - HH=hour, MM=minutes)
**   GRIB_LBL(16):  Forecast Time Unit
**   GRIB_LBL(17):  P1 (or 0)
**   GRIB_LBL(18):  P2 (or 0)
**   GRIB_LBL(19):  Time Range Indicator
**   GRIB_LBL(20):  Number included in Average (or 0)
**   GRIB_LBL(21):  Number of Missing Grids in Average
**   GRIB_LBL(22):  Sub-center ID
**   GRIB_LBL(23):  Decimal scale factor
**   GRIB_LBL(24):  Binary Data Section flag
**   GRIB_LBL(25):  Width in bits of a packed data point
**   GRIB_LBL(26):  Length in octets of the Grid Description Section
**   GRIB_LBL(27):  Reserved - currently ignored
**   GRIB_LBL(28):  Reserved - currently ignored
**   GRIB_LBL(29):  Data representation type
**
**   For Latitude/Longitude and Gaussian Lat/Lon Grids:
**     GRIB_LBL(30):  Number of points along a latitude circle
**     GRIB_LBL(31):  Number of points along a longitude meridian
**     GRIB_LBL(32):  Latitude of the first gridpoint (*1000)
**     GRIB_LBL(33):  Longitude of the first gridpoint (*1000)
**     GRIB_LBL(34):  Resolution and component flags
**     GRIB_LBL(35):  Latitude of the last gridpoint (*1000)
**     GRIB_LBL(36):  Longitude of the last gridpoint (*1000)
**     GRIB_LBL(37):  Latitude increment (*1000) for Lat/Lon grid
**                    -OR-
**                    Number of latitude circles between equator and pole for
**                    Gaussian Lat/Lon grid
**     GRIB_LBL(38):  Longitude increment (*1000)
**     GRIB_LBL(39):  Scanning mode flags
**
**   For Polar Stereographic Grids:
**     GRIB_LBL(30):  Number of points in the X-direction
**     GRIB_LBL(31):  Number of points in the Y-direction
**     GRIB_LBL(32):  Latitude of the first gridpoint (*1000)
**     GRIB_LBL(33):  Longitude of the first gridpoint (*1000)
**     GRIB_LBL(34):  Resolution and component flags
**     GRIB_LBL(35):  Longitude of grid orientation (*1000)
**     GRIB_LBL(36):  X-direction grid length in meters
**     GRIB_LBL(37):  Y-direction grid length in meters
**     GRIB_LBL(38):  Projection center flag
**     GRIB_LBL(39):  Scanning mode flags
**
**   For Lambert conformal Grids:
**     GRIB_LBL(30):  \
**         .           |
**         .            >  same as for Polar Stereographic grids
**         .           |
**     GRIB_LBL(39):  /
**     GRIB_LBL(40):  First latitude at which cone cuts the sphere (*1000)
**     GRIB_LBL(41):  Second latitude at which cone cuts the sphere (*1000)
**     GRIB_LBL(42):  Latitude of southern pole (*1000)
**     GRIB_LBL(43):  Longitude of southern pole (*1000)
**
**
** Description for PDS_EXT:
**   This array is free-form and can contain any values that the user wants to
**   specify, provided they are values that will fit into an 8-bit field.
**
** (added by DTM OHD/HSEB) BUT....
**   This should be set to null string of zero length as NCEP uses this area for
**   ensemble forecast information.  Degribbers are checking the length of the PDS and
**   if longer than 28 bytes, then the extra bytes are searched for ensemble info.
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void setBits(size_t *buf,size_t loc,size_t off,size_t bits)
{
  size_t mask=0;
  size_t size=sizeof(size_t)*8,wskip,bskip,lclear,rclear,left,right,more;
  size_t n;

/* no work to do */
  if (bits == 0)
    return;

/* create a mask to use when right-shifting (necessary because different
   compilers do different things when right-shifting a signed bit-field) */
  mask=1;
  for (n=1; n < size; n++) {
    mask<<=1;
    mask++;
  }

  if (bits > size) {
    fprintf(stderr,"Error: packing %d bits into a %d-bit field\n",bits,size);
    printf("Error 4: packing %d bits into a %d-bit field\n",bits,size);
    exit(1);
  }
  else {
/* get number of words and bits to skip before packing begins */
    wskip=off/size;
    bskip=off % size;
    lclear=bskip+bits;
    rclear=size-bskip;
    left= (rclear != size) ? (buf[wskip]&(mask<<rclear)) : 0;
    if (lclear <= size) {
/* all bits to be packed are in the current word */
/* clear the field to be packed */
	right=(lclear != size) ? (buf[wskip]&~(mask<<(size-lclear))) : 0;
/* fill the field to be packed */
	buf[wskip]=left|right|(loc<<(rclear-bits));
    }
    else {
/* bits to be packed cross a word boundary(ies) */
/* clear the bit field to be packed */
	more=bits-rclear;
	buf[wskip]=left|((loc>>more)&~(mask<<rclear));
/* clear the next (or part of the next) word and pack those bits */
	while (more > size) {
	  more-=size;
	  buf[++wskip]=((loc>>more)&~(mask<<size));
	}
	wskip++;
	more=size-more;
	right= (more != size) ? (buf[wskip]&~(mask<<more)) : 0;
	buf[wskip]=right|(loc<<more);
    }
  }
}

#if defined(LINUX) || defined(linux)
void p_swap4(size_t *buf,size_t length)
{
  unsigned char *c_buf=(unsigned char *)buf,temp;
  size_t n;

  for (n=0; n < length*4; n+=4) {
    temp=c_buf[n+0];
    c_buf[n+0]=c_buf[n+3];
    c_buf[n+3]=temp;
    temp=c_buf[n+1];
    c_buf[n+1]=c_buf[n+2];
    c_buf[n+2]=temp;


  }
}
#endif

int iround(double val)
{
  return ( (val >= 0.) ? (int)(val+0.5000001) : (int)(val-0.5000001) );
}


size_t real2ibm(double native_real)
{
  size_t ibm_real=0;
  size_t sign=0,fr=0;
  int exp=64;
  const double full=0xffffff;
  size_t size=sizeof(size_t)*8,off=0;

  if (native_real != 0.) {
    if (native_real < 0.) {
      sign=1;
      native_real=-native_real;
    }
    native_real/=pow(2.,-24.);
    while (exp > 0 && native_real < full) {
      native_real*=16.;
      exp--;
    }
    while (native_real > full) {
      native_real/=16.;
      exp++;
    }
    fr=iround(native_real);

    if (size > 32) {
	off=size-32;
	setBits(&ibm_real,0,0,off);
    }
    setBits(&ibm_real,sign,off,1);
    setBits(&ibm_real,exp,off+1,7);
    setBits(&ibm_real,fr,off+8,24);
  }

  return ibm_real;
}

void packIS(int *grib_lbl,size_t *out_buf,size_t *out_buf_len,size_t *grib_length)
{
  size_t length;

  switch (grib_lbl[0]) {
    case 0:
	*grib_length=4;
	break;
    case 1:
	*grib_length=8;
/* GRIB Edition - set to 1 */
	setBits(out_buf,1,56,8);
	break;
  }
  if (*grib_length > *out_buf_len) {
    fprintf(stderr,"Error: dimension of output buffer not large enough\n");
    printf("Error 5: dimension of output buffer not large enough\n");
    exit(1);
  }
/* "GRIB" message */
  setBits(out_buf,0x47524942,0,32);
}

void packPDS(int *grib_lbl,char *pds_ext,size_t *pds_ext_length,size_t *out_buf,
             size_t *out_buf_len,size_t *grib_length,size_t *off,unsigned char *pds_flag)
{
  short sign;
  size_t pds_len;
  int dval,yr,cent,hr,min;
  size_t n;
  unsigned char *c_buf=(unsigned char *)out_buf;

  switch (grib_lbl[0]) {
    case 0:
	*off=32;
	*grib_length+=24;
/* length of PDS */
	setBits(out_buf,24,*off,24);
/* Edition number */
	setBits(out_buf,0,*off+24,8);
/* force the decimal scale factor to zero */
	grib_lbl[22]=0;
	break;
    case 1:
	*off=64;
	pds_len= (*pds_ext_length == 0) ? 28 : 40+*pds_ext_length;

	*grib_length+=pds_len;
/* length of PDS */
	setBits(out_buf,pds_len,*off,24);
/* table version */
	setBits(out_buf,grib_lbl[2],*off+24,8);
	break;
    default:
	fprintf(stderr,"Error: invalid GRIB edition number %d\n",grib_lbl[0]);
	printf("ERROR: invalid GRIB edition number %d\n",grib_lbl[0]);
	exit(1);
  }

  if (*grib_length > *out_buf_len) {
    fprintf(stderr,"Error: dimension of output buffer not large enough\n");
    printf("Error 6: dimension of output buffer not large enough\n");
    exit(1);
  }
/* center ID */
  setBits(out_buf,grib_lbl[3],*off+32,8);
/* generating process */
  setBits(out_buf,grib_lbl[4],*off+40,8);
/* grid type */
  setBits(out_buf,grib_lbl[1],*off+48,8);
/* flag to include GDS */
  if (grib_lbl[6] == 1) (*pds_flag)|=0x80;
/* parameter code */
  setBits(out_buf,grib_lbl[7],*off+64,8);
/* level type */
  setBits(out_buf,grib_lbl[8],*off+72,8);
  switch (grib_lbl[8]) {
    case 100:
    case 103:
    case 105:
    case 107:
    case 109:
    case 111:
    case 113:
    case 115:
    case 125:
    case 160:
    case 200:
    case 201:
/* first level */
	setBits(out_buf,grib_lbl[9],*off+80,16);
	break;
    default:
/* first level */
	setBits(out_buf,grib_lbl[9],*off+80,8);
/* second level */
	setBits(out_buf,grib_lbl[10],*off+88,8);
  }
/* year */
  yr=grib_lbl[11] % 100;
  if (grib_lbl[0] > 0)
    if (yr == 0) yr=100;
  setBits(out_buf,yr,*off+96,8);
/* month */
  setBits(out_buf,grib_lbl[12],*off+104,8);
/* day */
  setBits(out_buf,grib_lbl[13],*off+112,8);
/* hour */
  hr=grib_lbl[14]*0.01;
  setBits(out_buf,hr,*off+120,8);
/* minutes */
  min=grib_lbl[14] % 100;
  setBits(out_buf,min,*off+128,8);
/* forecast units */
  setBits(out_buf,grib_lbl[15],*off+136,8);
/* P1 */
  setBits(out_buf,grib_lbl[16],*off+144,8);
/* P2 */
  setBits(out_buf,grib_lbl[17],*off+152,8);
/* time range */
  setBits(out_buf,grib_lbl[18],*off+160,8);
  switch (grib_lbl[18]) {
    case 3:
    case 4:
    case 51:
    case 113:
    case 114:
    case 115:
    case 116:
    case 117:
    case 123:
    case 124:
/* number in average */
      setBits(out_buf,grib_lbl[19],*off+168,16);
	break;
    default:
/* number in average */
      setBits(out_buf,0,*off+168,16);
  }
/* missing grids in average */
  setBits(out_buf,grib_lbl[20],*off+184,8);

/* if GRIB0, no more packing to do */
  if (grib_lbl[0] == 0) {
    *off+=192;
    return;
  }

/* century */
  cent=grib_lbl[11]*0.01;
  if (yr != 100) cent++;
  setBits(out_buf,cent,*off+192,8);
/* sub-center ID */
  setBits(out_buf,grib_lbl[21],*off+200,8);
/* decimal scale factor */
  dval=grib_lbl[22];
  sign=0;
  if (dval < 0) {
    sign=1;
    dval=-dval;
  }
  setBits(out_buf,sign,*off+208,1);
  setBits(out_buf,dval,*off+209,15);

  if (*pds_ext_length > 0) {
#if defined(LINUX) || defined(linux)
    p_swap4((size_t *)pds_ext,(*pds_ext_length+3)/4);
    for (n=0; n < ((*pds_ext_length+3)/4)*4; n++)
#else
    for (n=0; n < *pds_ext_length; n++)
#endif
	c_buf[48+n]=pds_ext[n];
  }
  *off+=pds_len*8;
}

void packGDS(int *grib_lbl,size_t *out_buf,size_t *out_buf_len,size_t *grib_length,size_t *off)
{
  short sign;
  int dum;

  *grib_length+=grib_lbl[25];
  if (*grib_length > *out_buf_len) {
    fprintf(stderr,"Error: dimension of output buffer not large enough\n");
    printf("Error 7: dimension of output buffer not large enough\n");
    exit(1);
  }
/* length of the GDS */
  setBits(out_buf,grib_lbl[25],*off,24);
/* octet holding PV/PL */
  setBits(out_buf,255,*off+32,8);
/* data representation type */
  setBits(out_buf,grib_lbl[28],*off+40,8);
  switch (grib_lbl[28]) {
/* Latitude/Longitude grid */
    case 0:
/* Gaussian Lat/Lon grid */
    case 4:
/* number of latitudes */
	setBits(out_buf,grib_lbl[29],*off+48,16);
/* number of longitudes */
	setBits(out_buf,grib_lbl[30],*off+64,16);
/* latitude of first gridpoint */
	if (grib_lbl[31] < 0) {
	  sign=1;
	  grib_lbl[31]=-grib_lbl[31];
	}
	else
	  sign=0;
	setBits(out_buf,sign,*off+80,1);
	setBits(out_buf,grib_lbl[31],*off+81,23);
/* longitude of first gridpoint */
	if (grib_lbl[32] < 0) {
	  sign=1;
	  grib_lbl[32]=-grib_lbl[32];
	}
	else
	  sign=0;
	setBits(out_buf,sign,*off+104,1);
	setBits(out_buf,grib_lbl[32],*off+105,23);
/* resolution and component flags */
	setBits(out_buf,grib_lbl[33],*off+128,8);
/* latitude of last gridpoint */
	if (grib_lbl[34] < 0) {
	  sign=1;
	  grib_lbl[34]=-grib_lbl[34];
	}
	else
	  sign=0;
	setBits(out_buf,sign,*off+136,1);
	setBits(out_buf,grib_lbl[34],*off+137,23);
/* longitude of last gridpoint */
	if (grib_lbl[35] < 0) {
	  sign=1;
	  grib_lbl[35]=-grib_lbl[35];
	}
	else
	  sign=0;
	setBits(out_buf,sign,*off+160,1);
	setBits(out_buf,grib_lbl[35],*off+161,23);
/* longitude increment */
	setBits(out_buf,grib_lbl[37],*off+184,16);
/* latitude increment */
	setBits(out_buf,grib_lbl[36],*off+200,16);
/* scanning mode flag */
	setBits(out_buf,grib_lbl[38],*off+216,8);
	*off+=256;
	break;
/* Lambert conformal grid */
    case 3:
/* Polar Stereographic grid */
    case 5:
/* number of points in the x-direction */
	setBits(out_buf,grib_lbl[29],*off+48,16);
/* number of points in the y-direction */
	setBits(out_buf,grib_lbl[30],*off+64,16);
/* latitude of first gridpoint */
	if (grib_lbl[31] < 0) {
	  sign=1;
	  grib_lbl[31]=-grib_lbl[31];
	}
	else
	  sign=0;
	setBits(out_buf,sign,*off+80,1);
	setBits(out_buf,grib_lbl[31],*off+81,23);
/* longitude of first gridpoint */
	if (grib_lbl[32] < 0) {
	  sign=1;
	  grib_lbl[32]=-grib_lbl[32];
	}
	else
	  sign=0;
	setBits(out_buf,sign,*off+104,1);
	setBits(out_buf,grib_lbl[32],*off+105,23);
/* resolution and component flags */
	setBits(out_buf,grib_lbl[33],*off+128,8);
/* longitude of grid orientation */
	if (grib_lbl[34] < 0) {
	  sign=1;
	  grib_lbl[34]=-grib_lbl[34];
	}
	else
	  sign=0;
	setBits(out_buf,sign,*off+136,1);
	setBits(out_buf,grib_lbl[34],*off+137,23);
/* x-direction grid length */
	setBits(out_buf,grib_lbl[35],*off+160,24);
/* y-direction grid length */
	setBits(out_buf,grib_lbl[36],*off+184,24);
/* projection center flag */
	setBits(out_buf,grib_lbl[37],*off+208,8);
/* scanning mode flag */
	setBits(out_buf,grib_lbl[38],*off+216,8);
	if (grib_lbl[28] == 3) {
	  if (grib_lbl[39] < 0) {
	    sign=1;
	    grib_lbl[39]=-grib_lbl[39];
	  }
	  else
	    sign=0;
	  setBits(out_buf,sign,*off+224,1);
	  setBits(out_buf,grib_lbl[39],*off+225,23);
	  if (grib_lbl[40] < 0) {
	    sign=1;
	    grib_lbl[40]=-grib_lbl[40];
	  }
	  else
	    sign=0;
	  setBits(out_buf,sign,*off+248,1);
	  setBits(out_buf,grib_lbl[40],*off+249,23);
	  if (grib_lbl[41] < 0) {
	    sign=1;
	    grib_lbl[41]=-grib_lbl[41];
	  }
	  else
	    sign=0;
	  setBits(out_buf,sign,*off+272,1);
	  setBits(out_buf,grib_lbl[41],*off+273,23);
	  if (grib_lbl[42] < 0) {
	    sign=1;
	    grib_lbl[42]=-grib_lbl[42];
	  }
	  else
	    sign=0;
	  setBits(out_buf,sign,*off+296,1);
	  setBits(out_buf,grib_lbl[42],*off+297,23);
	  *off+=336;
	}
	else
	  *off+=256;
	break;
    default:
	fprintf(stderr,"Error: unable to pack grids with representation %d\n",grib_lbl[28]);
	printf("Error 8: unable to pack grids with representation %d\n",grib_lbl[28]);
	exit(1);
  }
}

void packBDS(int *grib_lbl,float *gridpoints,size_t *gi_len,float miss_val,size_t *out_buf,
             size_t *out_buf_len,size_t *grib_length,size_t off,unsigned char *pds_flag)
{
  size_t n,m,*packed,ref_val;
  size_t bds_length,max_pack,num_missing=0;
  size_t num_points=grib_lbl[29]*grib_lbl[30],num_octets;
  int sign,eval,E=0,cnt=0,gcnt=0,bcnt=0;
  float max,min,range,e,d=pow(10.,grib_lbl[22]);
  size_t *bitmap,bms_length,missing_octets,ub;

  if ((grib_lbl[23] & 0x40) == 0)
/* simple packing */
    num_octets=num_points*grib_lbl[24];
  else {
/* second-order packing */
    fprintf(stderr,"Error: complex packing not currently supported\n");
    printf("Error 9: complex packing not currently supported\n");
    exit(1);
  }

  bitmap=(size_t *)malloc(sizeof(size_t)*num_points);
  cnt=0;
  min=1.e30;
  max=-min;
  for (n=0; n < grib_lbl[30]; n++) {
    for (m=0; m < *gi_len; m++) {
	if (m < grib_lbl[29]) {
	  if (gridpoints[gcnt] != miss_val) {
	    bitmap[cnt]=1;
	    if (gridpoints[gcnt] > max) max=gridpoints[gcnt];
	    if (gridpoints[gcnt] < min) min=gridpoints[gcnt];
	  }
	  else {
	    bitmap[cnt]=0;
	    num_missing++;
	  }
	  cnt++;
	}
	gcnt++;
    }
  }

/*  printf("min = %f max = %f num missing = %d\n",min,max, num_missing);*/

  if (num_missing > 0) {
    bms_length=6+(num_points+7)/8;
    if ( (bms_length % 2) != 0) bms_length++;
    missing_octets=(grib_lbl[24]*num_missing+7)/8;
    num_octets-=(grib_lbl[24]*num_missing);

/* pack a bit-map section */
    *grib_length+=bms_length;
    if (*grib_length > *out_buf_len) {
	fprintf(stderr,"Error: dimension of output buffer not large enough\n");
	printf("Error 10: dimension of output buffer not large enough\n");
	exit(1);
    }
/* set flag to show BMS included */
    (*pds_flag)^=0x40;
/* length of the BMS */
    setBits(out_buf,bms_length,off,24);
/* unused bits at end of section */
    ub=((bms_length-6)*8) % num_points;
    setBits(out_buf,ub,off+24,8);
/* table reference set to show that a bitmap follows */
    setBits(out_buf,0,off+32,16);
    off+=48;
    for (n=0; n < num_points; n++) {
	setBits(out_buf,bitmap[n],off,1);
	off++;
    }
    off+=ub;
  }

  if (min > 1.e29)
    min=0.;
  else {
    range=(max-min)*d;

/*    printf("d = %f range=%f max=%f min=%f\n",d,range,max,min);*/
    if (range == 0) {
	grib_lbl[24]=0;
	num_octets=0;
    }
    else {
	max_pack=pow(2.,grib_lbl[24])-1;
	/* printf("max_pack=%d\n",max_pack);*/
	if (range != 0.) {
	  while (iround(range) <= max_pack) {
	    range*=2.;
	    E--;
	  }
	  while (iround(range) > max_pack) {
	    range*=0.5;
	    E++;
	  }
	}
    }
  }

/*  printf("E=%d\n",E);*/

  num_octets=(num_octets+7)/8;
  bds_length=11+num_octets;
  if ((bds_length % 2) != 0) bds_length++;
  *grib_length+=bds_length;
  if (*grib_length > *out_buf_len) {
    fprintf(stderr,"Error: dimension of output buffer not large enough\n");
    printf("Error 1: dimension of output buffer not large enough\n");
    exit(1);
  }
/* length of the BDS */
  setBits(out_buf,bds_length,off,24);
/* flag */
  setBits(out_buf,grib_lbl[23],off+24,4);
/* set the unused bits at the end of the BDS */
  if (grib_lbl[24] > 0) {
    ub=(bds_length-11)*8-(grib_lbl[24]*(num_points-num_missing));
    setBits(out_buf,ub,off+28,4);
  }
  else
    setBits(out_buf,8,off+28,4);

/* binary scale factor */
  /*E=-1; */      /* DTM this is in accordance with the precip grib grids in gribit */

  setBits(out_buf,abs(E),off+32,16);
  if (E < 0)
    setBits(out_buf,1,off+32,1);
  ref_val=real2ibm(min*d);
/* IBM representation of the reference value */
  setBits(out_buf,ref_val,off+48,32);
/* bit width of the packed data points */
  setBits(out_buf,grib_lbl[24],off+80,8);
  if (grib_lbl[24] == 0) {
/*    printf(" width of packed data point=%d\n",grib_lbl[24]);*/
    free(bitmap);
    return;
  }

  e=pow(2.,E);

  if ((grib_lbl[23] & 0x40) == 0) {
/* simple packing */
    off+=88;
    packed=(size_t *)malloc(sizeof(size_t)*num_points);
    cnt=0;
    switch (grib_lbl[28]) {
	case 0:
	case 3:
	case 4:
	case 5:
	  gcnt=0;
	  for (n=0; n < grib_lbl[30]; n++) {
	    for (m=0; m < *gi_len; m++) {
		if (m < grib_lbl[29]) {
		  if (bitmap[bcnt] == 1) {
		    packed[cnt]= iround((gridpoints[gcnt]-min)*d/e);
		    cnt++;
		  }
		  bcnt++;
		}
		gcnt++;
	    }
	  }
	  for (n=0; n < cnt; n++) {
	    setBits(out_buf,packed[n],off,grib_lbl[24]);
	    off+=grib_lbl[24];
	  }
	  break;
	default:
	  fprintf(stderr,"Error: unable to pack grids with representation %d\n",grib_lbl[28]);
	  printf("Error 2: unable to pack grids with representation %d\n",grib_lbl[28]);
	  exit(1);
    }
  }

  free(bitmap);
  free(packed);
}

void packEND(size_t *out_buf,size_t *out_buf_len,size_t *grib_length)
{
  *grib_length+=4;
  if (*grib_length > *out_buf_len) {
    fprintf(stderr,"Error: dimension of output buffer not large enough\n");
    printf("Error 3: dimension of output buffer not large enough\n");
    exit(1);
  }
/* "7777" */
  setBits(out_buf,0x37373737,(*grib_length-4)*8,32);
}

int packgrib(int *grib_lbl,char *pds_ext,size_t *pds_ext_length,
            float *gridpoints,size_t *gi_len,float *miss_val,
	    size_t *out_buf,size_t *out_buf_len,size_t *grib_length)
{
  size_t off;
  unsigned char pds_flag=0x0;

  packIS(grib_lbl,out_buf,out_buf_len,grib_length);
  packPDS(grib_lbl,pds_ext,pds_ext_length,out_buf,out_buf_len,grib_length,&off,&pds_flag);
  if (grib_lbl[6] == 1) packGDS(grib_lbl,out_buf,out_buf_len,grib_length,&off);
  packBDS(grib_lbl,gridpoints,gi_len,*miss_val,out_buf,out_buf_len,grib_length,off,&pds_flag);
  packEND(out_buf,out_buf_len,grib_length);
/*  printf("grib label 0=%d\n",grib_lbl[0]);*/
  if (grib_lbl[0] == 1) {
    setBits(out_buf,*grib_length,32,24);
    setBits(out_buf,pds_flag,120,8);
  }
  else
    setBits(out_buf,pds_flag,88,8);

#if defined(LINUX) || defined(linux)
  p_swap4(out_buf,(*grib_length+3)/4);
#endif
  return 0;
}


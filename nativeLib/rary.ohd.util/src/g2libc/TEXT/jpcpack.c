#include <stdlib.h>
#include <math.h>
#include "g2libc_inc/grib2.h"

void jpcpack(g2float *fld,g2int width,g2int height,g2int *idrstmpl,
             unsigned char *cpack,g2int *lcpack)
//$$$  SUBPROGRAM DOCUMENTATION BLOCK
//                .      .    .                                       .
// SUBPROGRAM:    jpcpack
//   PRGMMR: Gilbert          ORG: W/NP11    DATE: 2003-08-17
//
// ABSTRACT: This subroutine packs up a data field into a JPEG2000 code stream.
//   After the data field is scaled, and the reference value is subtracted out,
//   it is treated as a grayscale image and passed to a JPEG2000 encoder.
//   It also fills in GRIB2 Data Representation Template 5.40000 with the
//   appropriate values.
//
// PROGRAM HISTORY LOG:
// 2003-08-17  Gilbert
//
// USAGE:    jpcpack(g2float *fld,g2int width,g2int height,g2int *idrstmpl,
//                   unsigned char *cpack,g2int *lcpack);
//   INPUT ARGUMENT LIST:
//     fld[]    - Contains the data values to pack
//     width    - number of points in the x direction
//     height   - number of points in the y direction
//     idrstmpl - Contains the array of values for Data Representation
//                Template 5.40000
//                [0] = Reference value - ignored on input
//                [1] = Binary Scale Factor
//                [2] = Decimal Scale Factor
//                [3] = number of bits for each data value - ignored on input
//                [4] = Original field type - currently ignored on input
//                      Data values assumed to be reals.
//                [5] = 0 - use lossless compression
//                    = 1 - use lossy compression
//                [6] = Desired compression ratio, if idrstmpl[5]=1.
//                      Set to 255, if idrstmpl[5]=0.
//
//   OUTPUT ARGUMENT LIST: 
//     idrstmpl - Contains the array of values for Data Representation
//                Template 5.0
//                [0] = Reference value - set by jpcpack routine.
//                [1] = Binary Scale Factor - unchanged from input
//                [2] = Decimal Scale Factor - unchanged from input
//                [3] = Number of bits containing each grayscale pixel value
//                [4] = Original field type - currently set = 0 on output.
//                      Data values assumed to be reals.
//                [5] = 0 - use lossless compression
//                    = 1 - use lossy compression
//                [6] = Desired compression ratio, if idrstmpl[5]=1
//     cpack    - The packed data field 
//     lcpack   - length of packed field cpack.
//
// REMARKS: None
//
// ATTRIBUTES:
//   LANGUAGE: C
//   MACHINE:  IBM SP
//
//$$$
{
      g2int  *ifld;
      static g2int  zero=0;
      static g2float alog2=0.69314718;       //  ln(2.0)
      g2int  j,nbits,imin,imax,maxdif,nbittot,left;
      g2int  ndpts,nbytes,nsize;
      g2float  bscale,dscale,rmax,rmin,temp;
      unsigned char *ctemp;
      
      ifld=0;
      ndpts=width*height;
      bscale=int_power(2.0,-idrstmpl[1]);
      dscale=int_power(10.0,idrstmpl[2]);
//
//  Find max and min values in the data
//
      rmax=fld[0];
      rmin=fld[0];
      for (j=1;j<ndpts;j++) {
        if (fld[j] > rmax) rmax=fld[j];
        if (fld[j] < rmin) rmin=fld[j];
      }
//
//  If max and min values are not equal, pack up field.
//  If they are equal, we have a constant field, and the reference
//  value (rmin) is the value for each point in the field and
//  set nbits to 0.
//
      if (rmin != rmax) {
        ifld=(g2int *)malloc(ndpts*sizeof(g2int));
        //
        //  Determine which algorithm to use based on user-supplied 
        //  binary scale factor and number of bits.
        //
        if (idrstmpl[1] == 0) {
           //
           //  No binary scaling and calculate minumum number of 
           //  bits in which the data will fit.
           //
           imin=(g2int)rint(rmin*dscale);
           imax=(g2int)rint(rmax*dscale);
           maxdif=imax-imin;
           temp=log((double)(maxdif+1))/alog2;
           nbits=(g2int)ceil(temp);
           rmin=(g2float)imin;
           //   scale data
           for(j=0;j<ndpts;j++)
             ifld[j]=(g2int)rint(fld[j]*dscale)-imin;
        }
        else {
           //
           //  Use binary scaling factor and calculate minumum number of 
           //  bits in which the data will fit.
           //
           rmin=rmin*dscale;
           rmax=rmax*dscale;
           maxdif=(g2int)rint((rmax-rmin)*bscale);
           temp=log((double)(maxdif+1))/alog2;
           nbits=(g2int)ceil(temp);
           //   scale data
           for (j=0;j<ndpts;j++)
             ifld[j]=(g2int)rint(((fld[j]*dscale)-rmin)*bscale);
        }
        //
        //  Pack data into full octets, then do JPEG 2000 encode.
        //  and calculate the length of the packed data in bytes
        //
        nbytes=(nbits+7)/8;
        nsize=nbytes*ndpts;
        ctemp=calloc(ndpts,nbytes);
        sbits(ctemp,ifld,0,nbytes*8,0,ndpts);
        *lcpack=enc_jpeg2000(ctemp,width,height,nbits,idrstmpl[5],idrstmpl[6],cpack,nsize);
        if (*lcpack <= 0) {
           printf("jpcpack: ERROR Packing JPC = %d\n",*lcpack);
        }
        free(ctemp);

      }
      else {
        nbits=0;
        *lcpack=0;
      }

//
//  Fill in ref value and number of bits in Template 5.0
//
      mkieee(&rmin,idrstmpl+0,1);   // ensure reference value is IEEE format
      idrstmpl[3]=nbits;
      idrstmpl[4]=0;         // original data were reals
      if (idrstmpl[5] == 0) idrstmpl[6]=255;       // lossy not used
      if (ifld != 0) free(ifld);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/g2libc/RCS/jpcpack.c,v $";
 static char rcs_id2[] = "$Id: jpcpack.c,v 1.1 2004/09/16 17:42:10 dsa Exp $";}
/*  ===================================================  */

}

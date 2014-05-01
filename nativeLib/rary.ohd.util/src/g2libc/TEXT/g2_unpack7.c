#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include "g2libc_inc/grib2.h"

g2int g2_unpack7(unsigned char *cgrib,g2int *iofst,g2int igdsnum,g2int *igdstmpl,
               g2int idrsnum,g2int *idrstmpl,g2int ndpts,g2float **fld)
//$$$  SUBPROGRAM DOCUMENTATION BLOCK
//                .      .    .                                       .
// SUBPROGRAM:    g2_unpack7 
//   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2002-10-31
//
// ABSTRACT: This subroutine unpacks Section 7 (Data Section)
//           as defined in GRIB Edition 2.
//
// PROGRAM HISTORY LOG:
// 2002-10-31  Gilbert
// 2002-12-20  Gilbert - Added GDT info to arguments
//                       and added 5.51 processing.
// 2003-08-29  Gilbert  - Added support for new templates using
//                        PNG and JPEG2000 algorithms/templates.
//
// USAGE:    int g2_unpack7(unsigned char *cgrib,g2int *iofst,g2int igdsnum,
//                          g2int *igdstmpl, g2int idrsnum,
//                          g2int *idrstmpl, g2int ndpts,g2float **fld)
//   INPUT ARGUMENTS:
//     cgrib    - char array containing Section 7 of the GRIB2 message
//     iofst    - Bit offset of the beginning of Section 7 in cgrib.
//     igdsnum  - Grid Definition Template Number ( see Code Table 3.0)
//                ( Only used for DRS Template 5.51 )
//     igdstmpl - Pointer to an integer array containing the data values for
//                the specified Grid Definition
//                Template ( N=igdsnum ).  Each element of this integer
//                array contains an entry (in the order specified) of Grid
//                Definition Template 3.N
//                ( Only used for DRS Template 5.51 )
//     idrsnum  - Data Representation Template Number ( see Code Table 5.0)
//     idrstmpl - Pointer to an integer array containing the data values for
//                the specified Data Representation
//                Template ( N=idrsnum ).  Each element of this integer
//                array contains an entry (in the order specified) of Data
//                Representation Template 5.N
//     ndpts    - Number of data points unpacked and returned.
//
//   OUTPUT ARGUMENTS:      
//     iofst    - Bit offset at the end of Section 7, returned.
//     fld      - Pointer to a float array containing the unpacked data field.
//
//   RETURN VALUES:
//     ierr     - Error return code.
//                0 = no error
//                2 = Not section 7
//                4 = Unrecognized Data Representation Template
//                5 = need one of GDT 3.50 through 3.53 to decode DRT 5.51
//                6 = memory allocation error
//
// REMARKS: None
//
// ATTRIBUTES:
//   LANGUAGE: C
//   MACHINE:
//
//$$$//
{
      g2int ierr,isecnum;
      g2int ipos,lensec;
      g2float *lfld;

      ierr=0;
      *fld=0;     //NULL

      gbit(cgrib,&lensec,*iofst,32);        // Get Length of Section
      *iofst=*iofst+32;    
      gbit(cgrib,&isecnum,*iofst,8);         // Get Section Number
      *iofst=*iofst+8;

      if ( isecnum != 7 ) {
         ierr=2;
         //fprintf(stderr,"g2_unpack7: Not Section 7 data.\n");
         return(ierr);
      }

      ipos=(*iofst/8);
      lfld=(g2float *)calloc(ndpts,sizeof(g2float));
      if (lfld == 0) {
         ierr=6;
         return(ierr);
      }
      else {
         *fld=lfld;
      }

      if (idrsnum == 0) 
        simunpack(cgrib+ipos,idrstmpl,ndpts,lfld);
      else if (idrsnum == 2 || idrsnum == 3) 
        comunpack(cgrib+ipos,idrsnum,idrstmpl,ndpts,lfld);
      else if (idrsnum == 50) {            // Spectral Simple
        simunpack(cgrib+ipos,idrstmpl,ndpts-1,lfld+1);
        rdieee(idrstmpl+4,lfld+0,1);
      }
      else if (idrsnum == 51)              //  Spectral complex
        if ( igdsnum>=50 && igdsnum <=53 ) 
          specunpack(cgrib+ipos,idrstmpl,ndpts,igdstmpl[0],igdstmpl[2],igdstmpl[2],lfld);
        else {
          fprintf(stderr,"g2_unpack7: Cannot use GDT 3.%d to unpack Data Section 5.51.\n",igdsnum);
          ierr=5;
          if ( lfld != 0 ) free(lfld);
          *fld=0;     //NULL
          return(ierr);
        }
#ifdef USE_JPEG2000
      else if (idrsnum == 40000) {
        jpcunpack(cgrib+ipos,lensec-5,idrstmpl,ndpts,lfld);
        }
#endif  /* USE_JPEG2000 */
#ifdef USE_PNG
      else if (idrsnum == 40010) {
        pngunpack(cgrib+ipos,lensec-5,idrstmpl,ndpts,lfld);
        }
#endif  /* USE_PNG */
      else {
        fprintf(stderr,"g2_unpack7: Data Representation Template 5.%d not yet implemented.\n",idrsnum);
        ierr=4;
        if ( lfld != 0 ) free(lfld);
        *fld=0;     //NULL
        return(ierr);
      }

      *iofst=*iofst+(8*lensec);
      
      return(ierr);    // End of Section 7 processing


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/g2libc/RCS/g2_unpack7.c,v $";
 static char rcs_id2[] = "$Id: g2_unpack7.c,v 1.1 2004/09/16 17:42:10 dsa Exp $";}
/*  ===================================================  */

}

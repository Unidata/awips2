#include "SPE.h"

/*----------------------------------------------------------------------------*/
/*   function to convert unsigned char to short int (mm)                      */
/*                                                                            */
/*   upper_thres = upper limit of defined values                              */
/*   mfactor = multiplicative factor used to define lower precision values    */
/*   lower_thres, upper_thres and mfactor are read from SPE.h file            */
/*   (these values should be read from .../ffg_image_trans.txt file)          */
/*                                                                            */
/*   miss_value = missing value indicator                                     */
/*   fill_value = fill    value indicator                                     */
/*   miss_value, fill_value are read from SPE.h file                          */
/*                                                                            */
/*   calling routine: ReadSPE                                                 */
/*----------------------------------------------------------------------------*/

short int MPEUtil_ffconv(unsigned char uchar)
{

   short int ival=0, xmrg_factor=100;

   if(uchar == 0) 
        ival = 0;
   else if (uchar < upper_thres)
      ival = (uchar) * mmfactor * xmrg_factor;
   else if(uchar >= upper_thres)
      ival = miss_value;

   return ival;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEUtil/RCS/ffconv.c,v $";
 static char rcs_id2[] = "$Id: ffconv.c,v 1.2 2007/11/07 19:46:00 lawrence Exp $";}
/*  ===================================================  */

}

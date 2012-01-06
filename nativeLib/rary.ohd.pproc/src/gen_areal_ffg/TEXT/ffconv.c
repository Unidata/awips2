#include "gen_areal_ffg.h"

/*----------------------------------------------------------------------------*/
/*   function to convert unsigned char to float (mm)                          */
/*                                                                            */
/*   lower_thres = threshold value for change over to lower precision values  */
/*   upper_thres = upper limit of defined values                              */
/*   mfactor = multiplicative factor used to define lower precision values    */
/*   lower_thres, upper_thres and mfactor are read from read_gridded_ffg.h file */
/*   (these values should be read from .../ffg_image_trans.txt file)          */
/*                                                                            */
/*   miss_value_float = missing value indicator (type  = float)               */
/*   fill_value_float = fill    value indicator (type  = float)               */
/*   miss_value_float, fill_value_float are read from fmg.h file              */
/*                                                                            */
/*   calling routine: read_gridded_ffg                                        */
/*----------------------------------------------------------------------------*/

float ffconv(unsigned char uchar)

{

float fval=0.0;

if(uchar == 0) 
   fval = miss_value_float;
else if(uchar <= lower_thres) 
   fval = uchar - 1; 
else if (uchar <= upper_thres)
   fval = (lower_thres - 1) + ((uchar - 1) - (lower_thres - 1))*mfactor;
else if(uchar >= upper_thres)
   fval = miss_value_float;

return fval;

}

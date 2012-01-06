//
// CalibratorGvarG9.java
//

/*
This source file is part of the edu.wisc.ssec.mcidas package and is
Copyright (C) 1998 - 2009 by Tom Whittaker, Tommy Jasmin, Tom Rink,
Don Murray, James Kelly, Bill Hibbard, Dave Glowacki, Curtis Rueden
and others.
 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA
*/

package edu.wisc.ssec.mcidas;

import java.io.DataInputStream;
import java.io.IOException;
import java.lang.String;

/**
 * CalibratorGvarG9 creates a Calibrator object designed specifically
 * to deal with GOES 9 data.  Not fully implemented at present - some
 * calibrations remain to be done.  It provides all the constants
 * specific to the GOES 9 imager and sounder sensors.
 *
 * @version 1.3 6 Aug 1999
 * @author Tommy Jasmin, SSEC
 */

public class CalibratorGvarG9 extends CalibratorGvar {

  protected static float [] imager9FK1  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder9FK1 = new float[NUM_BANDS_SOUNDER];
  protected static float [] imager9FK2  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder9FK2 = new float[NUM_BANDS_SOUNDER];
  protected static float [] imager9TC1  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder9TC1 = new float[NUM_BANDS_SOUNDER];
  protected static float [] imager9TC2  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder9TC2 = new float[NUM_BANDS_SOUNDER];

  // the following static init block sets the class temp/rad constants
  static {

    imager9FK1[0] = 0.0000000E0f;
    imager9FK1[1] = 0.1988078E6f;
    imager9FK1[2] = 0.3873241E5f;
    imager9FK1[3] = 0.9717210E4f;
    imager9FK1[4] = 0.6899470E4f;

    sounder9FK1[0]  = 0.3765120E4f;
    sounder9FK1[1]  = 0.3981160E4f;
    sounder9FK1[2]  = 0.4281880E4f;
    sounder9FK1[3]  = 0.4678910E4f;
    sounder9FK1[4]  = 0.4962590E4f;
    sounder9FK1[5]  = 0.5860420E4f;
    sounder9FK1[6]  = 0.6770320E4f;
    sounder9FK1[7]  = 0.8958910E4f;
    sounder9FK1[8]  = 0.1296593E5f;
    sounder9FK1[9]  = 0.2839828E5f;
    sounder9FK1[10] = 0.3420134E5f;
    sounder9FK1[11] = 0.4252514E5f;
    sounder9FK1[12] = 0.1240574E6f;
    sounder9FK1[13] = 0.1280114E6f;
    sounder9FK1[14] = 0.1348497E6f;
    sounder9FK1[15] = 0.1678142E6f;
    sounder9FK1[16] = 0.1888012E6f;
    sounder9FK1[17] = 0.2258565E6f;

    imager9FK2[0] = 0.0000000E0f;
    imager9FK2[1] = 0.3677020E4f;
    imager9FK2[2] = 0.2131620E4f;
    imager9FK2[3] = 0.1344410E4f;
    imager9FK2[4] = 0.1199380E4f;

    sounder9FK2[0]  = 0.9801200E3f;
    sounder9FK2[1]  = 0.9985200E3f;
    sounder9FK2[2]  = 0.1023050E4f;
    sounder9FK2[3]  = 0.1053740E4f;
    sounder9FK2[4]  = 0.1074620E4f;
    sounder9FK2[5]  = 0.1135870E4f;
    sounder9FK2[6]  = 0.1191850E4f;
    sounder9FK2[7]  = 0.1308490E4f;
    sounder9FK2[8]  = 0.1480080E4f;
    sounder9FK2[9]  = 0.1922130E4f;
    sounder9FK2[10] = 0.2045030E4f;
    sounder9FK2[11] = 0.2199040E4f;
    sounder9FK2[12] = 0.3142140E4f;
    sounder9FK2[13] = 0.3175180E4f;
    sounder9FK2[14] = 0.3230740E4f;
    sounder9FK2[15] = 0.3475050E4f;
    sounder9FK2[16] = 0.3614260E4f;
    sounder9FK2[17] = 0.3836740E4f;

    imager9TC1[0] = 0.0000000E0f;
    imager9TC1[1] = 0.5864000E0f;
    imager9TC1[2] = 0.4841000E0f;
    imager9TC1[3] = 0.3622000E0f;
    imager9TC1[4] = 0.2014000E0f;
 
    sounder9TC1[0]  = 0.9900000E-2f;
    sounder9TC1[1]  = 0.1190000E-1f;
    sounder9TC1[2]  = 0.1220000E-1f;
    sounder9TC1[3]  = 0.1190000E-1f;
    sounder9TC1[4]  = 0.1350000E-1f;
    sounder9TC1[5]  = 0.4400000E-1f;
    sounder9TC1[6]  = 0.1345000E0f;
    sounder9TC1[7]  = 0.1193000E0f;
    sounder9TC1[8]  = 0.4070000E-1f;
    sounder9TC1[9]  = 0.1438000E0f;
    sounder9TC1[10] = 0.2762000E0f;
    sounder9TC1[11] = 0.1370000E0f;
    sounder9TC1[12] = 0.1890000E-1f;
    sounder9TC1[13] = 0.1980000E-1f;
    sounder9TC1[14] = 0.1910000E-1f;
    sounder9TC1[15] = 0.5310000E-1f;
    sounder9TC1[16] = 0.6120000E-1f;
    sounder9TC1[17] = 0.3042000E0f;
 
    imager9TC2[0] = 0.0000000E0f;
    imager9TC2[1] = 0.9992000E0f;
    imager9TC2[2] = 0.9989000E0f;
    imager9TC2[3] = 0.9988000E0f;
    imager9TC2[4] = 0.9992000E0f;
 
    sounder9TC2[0]  = 0.1000000E1f;
    sounder9TC2[1]  = 0.9999000E0f;
    sounder9TC2[2]  = 0.9999000E0f;
    sounder9TC2[3]  = 0.9999000E0f;
    sounder9TC2[4]  = 0.9999000E0f;
    sounder9TC2[5]  = 0.9998000E0f;
    sounder9TC2[6]  = 0.9995000E0f;
    sounder9TC2[7]  = 0.9996000E0f;
    sounder9TC2[8]  = 0.9999000E0f;
    sounder9TC2[9]  = 0.9996000E0f;
    sounder9TC2[10] = 0.9993000E0f;
    sounder9TC2[11] = 0.9997000E0f;
    sounder9TC2[12] = 0.1000000E1f;
    sounder9TC2[13] = 0.1000000E1f;
    sounder9TC2[14] = 0.1000000E1f;
    sounder9TC2[15] = 0.9999000E0f;
    sounder9TC2[16] = 0.9999000E0f;
    sounder9TC2[17] = 0.9996000E0f;

  }

  /**
   *
   * constructor
   *
   * @param dis         data input stream
   * @param ad          AncillaryData object
   * @param calBlock    calibration parameters array
   *
   */

  public CalibratorGvarG9(DataInputStream dis, AncillaryData ad, int [] cb)
    throws IOException
  {
    super(dis, ad, cb);
  }
  
  public CalibratorGvarG9(final int id, final int[] cal) {
	super(id, cal);
  }

  /**
   *
   * calibrate from radiance to temperature
   *
   * @param inVal       input data value
   * @param band        channel/band number
   * @param sId         sensor id number
   *
   */

  public float radToTemp(float inVal, int band, int sId) {
 
    double expn;
    double temp;
    float outVal;
 
    if ((sId % 2) == 0) {
      expn = (imager9FK1[band - 1] / inVal) + 1.0;
      temp = imager9FK2[band - 1] / Math.log(expn);
      outVal = (float) ((temp - imager9TC1[band - 1]) / imager9TC2[band - 1]);
    } else {
      expn = (sounder9FK1[band - 1] / inVal) + 1.0;
      temp = sounder9FK2[band - 1] / Math.log(expn);
      outVal = (float) ((temp - sounder9TC1[band - 1]) / sounder9TC2[band - 1]);
    }
 
    return (outVal);
  }

}

//
// CalibratorGvarG8.java
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

/**
 * CalibratorGvarG8 creates a Calibrator object designed specifically
 * to deal with GOES 8 data.  Not fully implemented at present - some
 * calibrations remain to be done.  It provides all the constants
 * specific to the GOES 8 imager and sounder sensors.
 *
 * @version 1.4 6 Aug 1999
 * @author Tommy Jasmin, SSEC
 */

public class CalibratorGvarG8 extends CalibratorGvar {

  protected static float [] imager8FK1  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder8FK1 = new float[NUM_BANDS_SOUNDER];
  protected static float [] imager8FK2  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder8FK2 = new float[NUM_BANDS_SOUNDER];
  protected static float [] imager8TC1  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder8TC1 = new float[NUM_BANDS_SOUNDER];
  protected static float [] imager8TC2  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder8TC2 = new float[NUM_BANDS_SOUNDER];
 
  // the following static init block sets the class temp/rad constants
  static {

    imager8FK1[0] = 0.0000000E0f;
    imager8FK1[1] = 0.1999862E6f;
    imager8FK1[2] = 0.3879239E5f;
    imager8FK1[3] = 0.9737930E4f;
    imager8FK1[4] = 0.6944640E4f;

    sounder8FK1[0]  = 0.3756810E4f;
    sounder8FK1[1]  = 0.4011100E4f;
    sounder8FK1[2]  = 0.4296870E4f;
    sounder8FK1[3]  = 0.4681130E4f;
    sounder8FK1[4]  = 0.4975250E4f;
    sounder8FK1[5]  = 0.5881410E4f;
    sounder8FK1[6]  = 0.6787440E4f;
    sounder8FK1[7]  = 0.8873710E4f;
    sounder8FK1[8]  = 0.1299794E5f;
    sounder8FK1[9]  = 0.2862932E5f;
    sounder8FK1[10] = 0.3424830E5f;
    sounder8FK1[11] = 0.4311430E5f;
    sounder8FK1[12] = 0.1242353E6f;
    sounder8FK1[13] = 0.1281235E6f;
    sounder8FK1[14] = 0.1351482E6f;
    sounder8FK1[15] = 0.1691671E6f;
    sounder8FK1[16] = 0.1882350E6f;
    sounder8FK1[17] = 0.2257944E6f;

    imager8FK2[0] = 0.0000000E0f;
    imager8FK2[1] = 0.3684270E4f;
    imager8FK2[2] = 0.2132720E4f;
    imager8FK2[3] = 0.1345370E4f;
    imager8FK2[4] = 0.1201990E4f;

    sounder8FK2[0]  = 0.3765120E4f;
    sounder8FK2[1]  = 0.3981160E4f;
    sounder8FK2[2]  = 0.4281880E4f;
    sounder8FK2[3]  = 0.4678910E4f;
    sounder8FK2[4]  = 0.4962590E4f;
    sounder8FK2[5]  = 0.5860420E4f;
    sounder8FK2[6]  = 0.6770320E4f;
    sounder8FK2[7]  = 0.8958910E4f;
    sounder8FK2[8]  = 0.1296593E5f;
    sounder8FK2[9]  = 0.2839828E5f;
    sounder8FK2[10] = 0.3420134E5f;
    sounder8FK2[11] = 0.4252514E5f;
    sounder8FK2[12] = 0.1240574E6f;
    sounder8FK2[13] = 0.1280114E6f;
    sounder8FK2[14] = 0.1348497E6f;
    sounder8FK2[15] = 0.1678142E6f;
    sounder8FK2[16] = 0.1888012E6f;
    sounder8FK2[17] = 0.2258565E6f;

    imager8TC1[0] = 0.0000000E0f;
    imager8TC1[1] = 0.6357000E0f;
    imager8TC1[2] = 0.6060000E0f;
    imager8TC1[3] = 0.3735000E0f;
    imager8TC1[4] = 0.2217000E0f;

    sounder8TC1[0]  = 0.1230000E-1f;
    sounder8TC1[1]  = 0.1330000E-1f;
    sounder8TC1[2]  = 0.1860000E-1f;
    sounder8TC1[3]  = 0.1500000E-1f;
    sounder8TC1[4]  = 0.1650000E-1f;
    sounder8TC1[5]  = 0.4740000E-1f;
    sounder8TC1[6]  = 0.1318000E0f;
    sounder8TC1[7]  = 0.1200000E0f;
    sounder8TC1[8]  = 0.4260000E-1f;
    sounder8TC1[9]  = 0.1505000E0f;
    sounder8TC1[10] = 0.2743000E0f;
    sounder8TC1[11] = 0.1447000E0f;
    sounder8TC1[12] = 0.2240000E-1f;
    sounder8TC1[13] = 0.2200000E-1f;
    sounder8TC1[14] = 0.2170000E-1f;
    sounder8TC1[15] = 0.5790000E-1f;
    sounder8TC1[16] = 0.6230000E-1f;
    sounder8TC1[17] = 0.3675000E0f;

    imager8TC2[0] = 0.0000000E0f;
    imager8TC2[1] = 0.9991000E0f;
    imager8TC2[2] = 0.9986000E0f;
    imager8TC2[3] = 0.9987000E0f;
    imager8TC2[4] = 0.9992000E0f;

    sounder8TC2[0]  = 0.9999000E0f;
    sounder8TC2[1]  = 0.9999000E0f;
    sounder8TC2[2]  = 0.9999000E0f;
    sounder8TC2[3]  = 0.9999000E0f;
    sounder8TC2[4]  = 0.9999000E0f;
    sounder8TC2[5]  = 0.9998000E0f;
    sounder8TC2[6]  = 0.9995000E0f;
    sounder8TC2[7]  = 0.9996000E0f;
    sounder8TC2[8]  = 0.9999000E0f;
    sounder8TC2[9]  = 0.9996000E0f;
    sounder8TC2[10] = 0.9993000E0f;
    sounder8TC2[11] = 0.9997000E0f;
    sounder8TC2[12] = 0.1000000E1f;
    sounder8TC2[13] = 0.1000000E1f;
    sounder8TC2[14] = 0.1000000E1f;
    sounder8TC2[15] = 0.9999000E0f;
    sounder8TC2[16] = 0.9999000E0f;
    sounder8TC2[17] = 0.9995000E0f;

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

  public CalibratorGvarG8(DataInputStream dis, AncillaryData ad, int [] cb) 
    throws IOException 
  {
    super(dis, ad, cb);
  }

  
  public CalibratorGvarG8(final int id, final int[] cal) {
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
      expn = (imager8FK1[band - 1] / inVal) + 1.0;
      temp = imager8FK2[band - 1] / Math.log(expn);
      outVal = (float) ((temp - imager8TC1[band - 1]) / imager8TC2[band - 1]);
    } else {
      expn = (sounder8FK1[band - 1] / inVal) + 1.0;
      temp = sounder8FK2[band - 1] / Math.log(expn);
      outVal = (float) ((temp - sounder8TC1[band - 1]) / sounder8TC2[band - 1]);
    }

    return (outVal);
  }

}

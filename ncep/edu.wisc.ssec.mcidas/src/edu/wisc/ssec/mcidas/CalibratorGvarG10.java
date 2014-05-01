//
// CalibratorGvarG10.java
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
 * CalibratorGvarG10 creates a Calibrator object designed specifically
 * to deal with GOES 10 data.  Not fully implemented at present - some
 * calibrations remain to be done.  It provides all the constants 
 * specific to the GOES 10 imager and sounder sensors.
 *
 * @version 1.3 6 Aug 1999
 * @author Tommy Jasmin, SSEC
 */

public class CalibratorGvarG10 extends CalibratorGvar {

  protected static float [] imager10FK1  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder10FK1 = new float[NUM_BANDS_SOUNDER];
  protected static float [] imager10FK2  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder10FK2 = new float[NUM_BANDS_SOUNDER];
  protected static float [] imager10TC1  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder10TC1 = new float[NUM_BANDS_SOUNDER];
  protected static float [] imager10TC2  = new float[NUM_BANDS_IMAGER];
  protected static float [] sounder10TC2 = new float[NUM_BANDS_SOUNDER];

  // the following static init block sets the class temp/rad constants
  static {

    imager10FK1[0] = 0.00000E0f;
    imager10FK1[1] = 0.19841E6f;
    imager10FK1[2] = 0.39086E5f;
    imager10FK1[3] = 0.97744E4f;
    imager10FK1[4] = 0.68286E4f;

    sounder10FK1[0]  = 0.37305E4f;
    sounder10FK1[1]  = 0.40039E4f;
    sounder10FK1[2]  = 0.43124E4f;
    sounder10FK1[3]  = 0.46616E4f;
    sounder10FK1[4]  = 0.49734E4f;
    sounder10FK1[5]  = 0.58698E4f;
    sounder10FK1[6]  = 0.68161E4f;
    sounder10FK1[7]  = 0.89404E4f;
    sounder10FK1[8]  = 0.12973E5f;
    sounder10FK1[9]  = 0.28708E5f;
    sounder10FK1[10] = 0.34401E5f;
    sounder10FK1[11] = 0.43086E5f;
    sounder10FK1[12] = 0.12468E6f;
    sounder10FK1[13] = 0.12882E6f;
    sounder10FK1[14] = 0.13532E6f;
    sounder10FK1[15] = 0.16853E6f;
    sounder10FK1[16] = 0.18862E6f;
    sounder10FK1[17] = 0.22487E6f;

    imager10FK2[0] = 0.00000E0f;
    imager10FK2[1] = 0.36745E4f;
    imager10FK2[2] = 0.21381E4f;
    imager10FK2[3] = 0.13470E4f;
    imager10FK2[4] = 0.11953E4f;

    sounder10FK2[0]  = 0.97710E3f;
    sounder10FK2[1]  = 0.10004E4f;
    sounder10FK2[2]  = 0.10255E4f;
    sounder10FK2[3]  = 0.10524E4f;
    sounder10FK2[4]  = 0.10754E4f;
    sounder10FK2[5]  = 0.11365E4f;
    sounder10FK2[6]  = 0.11945E4f;
    sounder10FK2[7]  = 0.13076E4f;
    sounder10FK2[8]  = 0.14804E4f;
    sounder10FK2[9]  = 0.19291E4f;
    sounder10FK2[10] = 0.20490E4f;
    sounder10FK2[11] = 0.22087E4f;
    sounder10FK2[12] = 0.31474E4f;
    sounder10FK2[13] = 0.31818E4f;
    sounder10FK2[14] = 0.32345E4f;
    sounder10FK2[15] = 0.34800E4f;
    sounder10FK2[16] = 0.36131E4f;
    sounder10FK2[17] = 0.38311E4f;

    imager10TC1[0] = 0.00000f;
    imager10TC1[1] = 0.62226f;
    imager10TC1[2] = 0.61438f;
    imager10TC1[3] = 0.27791f;
    imager10TC1[4] = 0.21145f;

    sounder10TC1[0]  = 0.00988f;
    sounder10TC1[1]  = 0.01196f;
    sounder10TC1[2]  = 0.01245f;
    sounder10TC1[3]  = 0.01245f;
    sounder10TC1[4]  = 0.01366f;
    sounder10TC1[5]  = 0.04311f;
    sounder10TC1[6]  = 0.13973f;
    sounder10TC1[7]  = 0.11707f;
    sounder10TC1[8]  = 0.03979f;
    sounder10TC1[9]  = 0.14968f;
    sounder10TC1[10] = 0.27603f;
    sounder10TC1[11] = 0.13049f;
    sounder10TC1[12] = 0.02008f;
    sounder10TC1[13] = 0.01834f;
    sounder10TC1[14] = 0.02017f;
    sounder10TC1[15] = 0.05292f;
    sounder10TC1[16] = 0.05330f;
    sounder10TC1[17] = 0.28683f;

    imager10TC2[0] = 0.00000f;
    imager10TC2[1] = 0.99912f;
    imager10TC2[2] = 0.99857f;
    imager10TC2[3] = 0.99905f;
    imager10TC2[4] = 0.99919f;

    sounder10TC2[0]  = 0.99995f;
    sounder10TC2[1]  = 0.99994f;
    sounder10TC2[2]  = 0.99994f;
    sounder10TC2[3]  = 0.99995f;
    sounder10TC2[4]  = 0.99994f;
    sounder10TC2[5]  = 0.99983f;
    sounder10TC2[6]  = 0.99947f;
    sounder10TC2[7]  = 0.99959f;
    sounder10TC2[8]  = 0.99988f;
    sounder10TC2[9]  = 0.99962f;
    sounder10TC2[10] = 0.99933f;
    sounder10TC2[11] = 0.99970f;
    sounder10TC2[12] = 0.99997f;
    sounder10TC2[13] = 0.99997f;
    sounder10TC2[14] = 0.99997f;
    sounder10TC2[15] = 0.99992f;
    sounder10TC2[16] = 0.99992f;
    sounder10TC2[17] = 0.99961f;
 
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

  public CalibratorGvarG10(DataInputStream dis, AncillaryData ad, int [] cb) 
    throws IOException
  {
    super(dis, ad, cb);
  }
  
  public CalibratorGvarG10(int sensorId, int[] cb) {
	super(sensorId, cb);
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
      expn = (imager10FK1[band - 1] / inVal) + 1.0;
      temp = imager10FK2[band - 1] / Math.log(expn);
      outVal = (float) ((temp - imager10TC1[band - 1]) / imager10TC2[band - 1]);
    } else {
      expn = (sounder10FK1[band - 1] / inVal) + 1.0;
      temp = sounder10FK2[band - 1] / Math.log(expn);
      outVal = (float) 
        ((temp - sounder10TC1[band - 1]) / sounder10TC2[band - 1]);
    }
 
    return (outVal);
  }
 
}

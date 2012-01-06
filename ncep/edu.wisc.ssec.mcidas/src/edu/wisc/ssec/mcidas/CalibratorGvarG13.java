//
// CalibratorGvarG13.java
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
import edu.wisc.ssec.mcidas.AncillaryData;

/**
 * CalibratorGvarG13 creates a Calibrator object designed specifically
 * to deal with GOES 13 data.  Not fully implemented at present - some
 * calibrations remain to be done.  It provides all the constants 
 * specific to the GOES 13 imager and sounder sensors.
 *
 * @version 1.3 6 Aug 1999
 * @author Tommy Jasmin, SSEC
 */




public class CalibratorGvarG13 extends CalibratorGvar {

  // the following static init block sets the class temp/rad constants
  protected static float [] imager13FK1  = {0.f, 0.20075E6f, 0.42508E5f, 0.98050E4f, 0.50173E4f};

  protected static float [] sounder13FK1 = 
    {0.37330E4f, 0.40351E4f, 0.42661E4f,
     0.46920E4f, 0.49953E4f, 0.58021E4f,
     0.68338E4f, 0.89420E4f, 0.13042E5f,
     0.28772E5f, 0.34368E5f, 0.42901E5f,
     0.12513E6f, 0.12812E6f, 0.13482E6f,
     0.16892E6f, 0.18938E6f, 0.22608E6f};

  protected static float [] imager13FK2  = {0.f, 0.36890E4f, 0.21987E4f, 0.13484E4f, 0.10786E4f};

  protected static float [] sounder13FK2 = 
    {0.97732E3f,   0.10030E4f,   0.10218E4f,
     0.10547E4f,   0.10770E4f,   0.11321E4f,
     0.11956E4f,   0.13077E4f,   0.14830E4f,
     0.19305E4f,   0.20483E4f,   0.22055E4f,
     0.31512E4f,   0.31761E4f,   0.32305E4f,
     0.34826E4f,   0.36179E4f,   0.38380E4f};

  protected static float [] imager13TC1  = {0.f, 1.47950f, 3.96964f, .36350f, .09502f};

  protected static float [] sounder13TC1 = 
    {.00944f, .01022f, .01011f,
     .01291f, .01353f, .04272f,
     .12493f, .12033f, .03838f,
     .15609f, .28000f, .18057f,
     .01799f, .01809f, .02012f,
     .05092f, .05740f, .29874f};

  protected static float [] imager13TC2  = {0.f, .99794f, .99112f, .99876f, .99960f};

  protected static float [] sounder13TC2 = 
     {.99996f, .99995f, .99995f,
      .99994f, .99994f, .99983f,
      .99952f, .99958f, .99988f,
      .99961f, .99932f, .99959f,
      .99997f, .99997f, .99997f,
      .99992f, .99992f, .99959f};

  /**
   *
   * constructor
   *
   * @param dis         data input stream
   * @param ad          AncillaryData object
   * @param calBlock    calibration parameters array
   *
   */

  public CalibratorGvarG13(DataInputStream dis, AncillaryData ad, int [] cb) 
    throws IOException
  {
    super(dis, ad, cb);
  }


  public CalibratorGvarG13(int sensorId, int[] cb) {
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
      expn = (imager13FK1[band - 1] / inVal) + 1.0;
      temp = imager13FK2[band - 1] / Math.log(expn);
      outVal = (float) ((temp - imager13TC1[band - 1]) / imager13TC2[band - 1]);
    } else {
      expn = (sounder13FK1[band - 1] / inVal) + 1.0;
      temp = sounder13FK2[band - 1] / Math.log(expn);
      outVal = (float) 
        ((temp - sounder13TC1[band - 1]) / sounder13TC2[band - 1]);
    }
 
    return (outVal);
  }
 
}

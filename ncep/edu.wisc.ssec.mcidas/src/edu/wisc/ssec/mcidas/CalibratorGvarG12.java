//
// CalibratorGvarG12.java
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
 * CalibratorGvarG12 creates a Calibrator object designed specifically
 * to deal with GOES 10 data.  Not fully implemented at present - some
 * calibrations remain to be done.  It provides all the constants 
 * specific to the GOES 10 imager and sounder sensors.
 *
 * @version 1.3 6 Aug 1999
 * @author Tommy Jasmin, SSEC
 */

public class CalibratorGvarG12 extends CalibratorGvar {

  // the following static init block sets the class temp/rad constants
  protected static float [] imager12FK1  = {0.f, 0.20096E6f, 0.43702E5f, 0.96859E4f, 0.50471E4f};

  protected static float [] sounder12FK1 = 
  {0.37778E4f, 0.40086E4f, 0.43085E4f, 
   0.47041E4f, 0.50134E4f, 0.58645E4f,
   0.69071E4f, 0.90388E4f, 0.12972E5f,
   0.28931E5f, 0.34531E5f, 0.43340E5f,
   0.12492E6f, 0.12822E6f, 0.13535E6f,
   0.16981E6f, 0.18954E6f, 0.22538E6f};

  protected static float [] imager12FK2  = {0.f, 0.36902E4f,0.22191E4f,0.13430E4f,0.10807E4f};

  protected static float [] sounder12FK2 = 
    {0.98121E3f,   0.10008E4f,   0.10252E4f,
     0.10556E4f,   0.10783E4f,   0.11361E4f,
     0.11998E4f,   0.13124E4f,   0.14803E4f,
     0.19340E4f,   0.20516E4f,   0.22130E4f,
     0.31494E4f,   0.31769E4f,   0.32347E4f,
     0.34888E4f,   0.36189E4f,   0.38340E4f};

  protected static float [] imager12TC1  = {0.f, .69703f, 5.08315f, .37554f, .09537f};

  protected static float [] sounder12TC1 = 
    {.01010f, .01252f, .01229f, 
     .01189f, .01264f, .04189f, 
     .13474f, .12341f, .03844f, 
     .15764f, .27420f, .13683f, 
     .02124f, .01780f, .02037f, 
     .04933f, .05386f, .28872f};

  protected static float [] imager12TC2  = {0.f, .99902f, .98872f, .99872f, .99960f};

  protected static float [] sounder12TC2 = 
     {.99995f, .99994f, .99994f,
      .99995f, .99995f, .99983f, 
      .99949f, .99957f, .99988f, 
      .99960f, .99934f, .99969f, 
      .99996f, .99997f, .99997f, 
      .99993f, .99992f, .99961f};


  /**
   *
   * constructor
   *
   * @param dis         data input stream
   * @param ad          AncillaryData object
   * @param calBlock    calibration parameters array
   *
   */

  public CalibratorGvarG12(DataInputStream dis, AncillaryData ad, int [] cb) 
    throws IOException
  {
    super(dis, ad, cb);
  }


  public CalibratorGvarG12(int sensorId, int[] cb) {
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
      expn = (imager12FK1[band - 1] / inVal) + 1.0;
      temp = imager12FK2[band - 1] / Math.log(expn);
      outVal = (float) ((temp - imager12TC1[band - 1]) / imager12TC2[band - 1]);
    } else {
      expn = (sounder12FK1[band - 1] / inVal) + 1.0;
      temp = sounder12FK2[band - 1] / Math.log(expn);
      outVal = (float) 
        ((temp - sounder12TC1[band - 1]) / sounder12TC2[band - 1]);
    }
 
    return (outVal);
  }
 
}

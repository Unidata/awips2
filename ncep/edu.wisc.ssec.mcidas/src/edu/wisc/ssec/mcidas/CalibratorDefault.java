//
// CalibratorDefault.java
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
 * CalibratorDefault creates a Calibrator object designed to
 * act in the absence of a Calibrator for a particular sensor.
 * In other words, if a sensor is not supported yet, just pass
 * the input value on as the calibrated value.
 *
 * @version 1.2 6 Aug 1999
 * @author Tommy Jasmin, SSEC
 */

public class CalibratorDefault implements Calibrator {

  // public static final int CAL_NONE = -1;
  // public static final int CAL_MIN  = 1;
  // public static final int CAL_RAW  = 1;
  // public static final int CAL_RAD  = 2;
  // public static final int CAL_ALB  = 3;
  // public static final int CAL_TEMP = 4;
  // public static final int CAL_BRIT = 5;
  // public static final int CAL_MAX  = 5;

  // var to store current cal type
  protected static int curCalType = 0;

  /**
   *
   * constructor - does nothing for default calibrator
   *
   * @param dis         data input stream
   * @param ad          AncillaryData object
   *
   */

  public CalibratorDefault(DataInputStream dis, AncillaryData ad) 
    throws IOException

  {
    return;
  }

  /**
   *
   * set calibration type of current (input) data
   *
   * @param calType     one of the types defined in Calibrator interface
   *
   */

  public int setCalType(int calType) {
    if ((calType < Calibrator.CAL_MIN) || (calType > Calibrator.CAL_MAX)) {
      return -1;
    }
    curCalType = calType;
    return 0;
  }

  /**
   *
   * calibrate data buffer to specified units.
   *
   * @param inputData   input data buffer
   * @param band        channel/band number
   * @param calTypeOut  units to convert input buffer to
   *
   */

  public float[] calibrate (
    float[] inputData,
    int band,
    int calTypeOut
  )

  {

    // create the output data buffer
    float[] outputData = new float[inputData.length];

    // just call the other calibrate routine for each data point
    for (int i = 0; i < inputData.length; i++) {
      outputData[i] = calibrate(inputData[i], band, calTypeOut);
    }

    // return the calibrated buffer
    return outputData;

  }

  /**
   *
   * calibrate single value to specified units.
   *
   * @param inputPixel  input data value
   * @param band        channel/band number
   * @param calTypeOut  units to convert input buffer to
   *
   */

  public float calibrate (
    float inputPixel,
    int band,
    int calTypeOut
  )

  {

    float outputData = 0.0f;

    // validate, then calibrate for each combination starting with cur type
    switch (curCalType) {

      case CAL_RAW:
        outputData = inputPixel;
        break;

      case CAL_RAD:
        outputData = inputPixel;
        break;

      case CAL_ALB:
        outputData = inputPixel;
        break;

      case CAL_TEMP:
        outputData = inputPixel;
        break;

      case CAL_BRIT:
        outputData = inputPixel;
        break;

    }

    return outputData;

  }

}

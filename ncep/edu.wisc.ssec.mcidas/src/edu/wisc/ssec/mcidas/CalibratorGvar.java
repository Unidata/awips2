//
// CalibratorGvar.java
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
 * CalibratorGvar creates a Calibrator object designed specifically
 * to deal with GVAR data.  Not fully implemented at present - some
 * calibrations remain to be done.
 *
 * @version 1.5 16 Nov 1998
 * @author Tommy Jasmin, SSEC
 */

public abstract class CalibratorGvar implements Calibrator {

  protected static final int NUM_BANDS_IMAGER = 5;
  protected static final int NUM_BANDS_SOUNDER = 18;

  protected static final int NUM_VIS_DETECTORS = 8;
  protected static final int NUM_IR_DETECTORS  = 2;
  protected static final int NUM_IR_BANDS      = 4;

  protected static final int LOOKUP_TABLE_SZ_IMGR = 1024;
  protected static final int LOOKUP_TABLE_SZ_SNDR = 32768;

  // var to store current cal type
  protected static int curCalType = 0;
  protected static int index = 0;

  protected float [] visBiasCoef  = new float [NUM_VIS_DETECTORS];
  protected float [] visGain1Coef = new float [NUM_VIS_DETECTORS];
  protected float [] visGain2Coef = new float [NUM_VIS_DETECTORS];
  protected float visRadToAlb = 0.0f;
  protected float [][] irBiasCoef = new float [NUM_IR_DETECTORS][NUM_IR_BANDS];
  protected float [][] irGainCoef = new float [NUM_IR_DETECTORS][NUM_IR_BANDS];
  protected float [] sBiasCoef = new float [NUM_BANDS_SOUNDER];
  protected float [] sGainCoef = new float [NUM_BANDS_SOUNDER];
  protected float [][] lookupTable;

  // used in calibrator method
  private static float gain = 0.0f;
  private static float bias = 0.0f;
  private static int scale = 1;
  private static int bandNum = 0;
  private static int sid = 0;

  /**
   *
   * constructor
   *
   * @param dis         data input stream 
   * @param ad          AncillaryData object
   * @param calBlock    calibration parameters array
   *
   */

  public CalibratorGvar (
    DataInputStream dis, 
    AncillaryData ad, 
    int [] calBlock
  ) 
    throws IOException

  {
	  this(ad.getSensorId(), calBlock);
  }
	  
	  
  public CalibratorGvar(final int sensorId, int[] calBlock) {

    int calIndex = 0;
    sid = sensorId;

    // now, correct for satellites starting with G12 (sid = 78)
    int irOffset = 2;
    if (sid > 77) irOffset = 0;

    //System.out.println("xxx sid = "+sid);
    if ((sid % 2) == 0) {

      // initialize lookup table
      lookupTable = new float [NUM_BANDS_IMAGER] [LOOKUP_TABLE_SZ_IMGR];

      for (int i = 0; i < NUM_BANDS_IMAGER; i++) {
        for (int j = 0; j < LOOKUP_TABLE_SZ_IMGR; j++) {
          lookupTable [i][j] = Float.NaN;
        }
      }

      // read in an imager format cal block
      for (int i = 0; i < NUM_VIS_DETECTORS; i++) {
        visBiasCoef[i] = (float) 
          ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

      for (int i = 0; i < NUM_VIS_DETECTORS; i++) {
        visGain1Coef[i] = (float) 
          ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

      for (int i = 0; i < NUM_VIS_DETECTORS; i++) {
        visGain2Coef[i] = (float) 
          ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

      visRadToAlb = (float) ConversionUtility.GouldToNative(calBlock[calIndex]);
      calIndex++;

      for (int i = 0; i < NUM_IR_BANDS; i++) {
        irBiasCoef[0][(i + irOffset) % NUM_IR_BANDS] = 
          (float) ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

      for (int i = 0; i < NUM_IR_BANDS; i++) {
        irBiasCoef[1][(i + irOffset) % NUM_IR_BANDS] = 
          (float) ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

      for (int i = 0; i < NUM_IR_BANDS; i++) {
        irGainCoef[0][(i + irOffset) % NUM_IR_BANDS] = 
          (float) ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

      for (int i = 0; i < NUM_IR_BANDS; i++) {
        irGainCoef[1][(i + irOffset) % NUM_IR_BANDS] = 
          (float) ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

    } else {

      // initialize lookup table
      lookupTable = new float [NUM_BANDS_SOUNDER + 1] [LOOKUP_TABLE_SZ_SNDR];
      for (int i = 0; i < NUM_BANDS_SOUNDER + 1; i++) {
        for (int j = 0; j < LOOKUP_TABLE_SZ_SNDR; j++) {
          lookupTable [i][j] = Float.NaN;
        }
      }

      // read in a sounder format cal block
      for (int i = 0; i < NUM_VIS_DETECTORS / 2; i++) {
        visBiasCoef[i] = (float) 
          ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

      for (int i = 0; i < NUM_VIS_DETECTORS / 2; i++) {
        visGain1Coef[i] = (float) 
          ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

      for (int i = 0; i < NUM_VIS_DETECTORS / 2; i++) {
        visGain2Coef[i] = (float) 
          ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

      visRadToAlb = (float) ConversionUtility.GouldToNative(calBlock[calIndex]);
      calIndex++;

      for (int i = 0; i < NUM_BANDS_SOUNDER; i++) {
        sBiasCoef[i] = (float) 
          ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

      for (int i = 0; i < NUM_BANDS_SOUNDER; i++) {
        sGainCoef[i] = (float) 
          ConversionUtility.GouldToNative(calBlock[calIndex]);
        calIndex++;
      }

    }

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
   * calibrate from radiance to temperature
   *
   * @param inVal     	input data value
   * @param band        channel/band number
   * @param sId        	sensor id number
   *
   */

  public abstract float radToTemp(float inVal, int band, int sId);

  /**
   *
   * calibrate data buffer to specified units.
   *
   * @param inputData	input data buffer
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
    //System.out.println("####  input pixel="+inputPixel);

    //System.out.println("####  cal band = "+band);
    //System.out.println("####  len lookup = "+lookupTable.length+" "+lookupTable[3].length);

    // load gain and bias constants based on band requested
    if (band != bandNum) {
      bandNum = band;
      if ((sid % 2) == 0) {
        if (band == 1) {
          gain = visGain1Coef[0];
          bias = visBiasCoef[0];
        } else {
          gain = irGainCoef[0][band - 2];
          bias = irBiasCoef[0][band - 2];
          //System.out.println("####  band="+band+"  gain="+gain+"  bias"+bias);
        }
        scale = 32;

      } else {
        if (band == 19) {
          gain = visGain1Coef[0];
          bias = visBiasCoef[0];
        } else {
          gain = sGainCoef[band - 1];
          bias = sBiasCoef[band - 1];
        }
        scale = 2;
      }
    }

    // check lookup table first, if there is an entry, use it
    if (curCalType == CAL_BRIT) {
      // one byte values are signed, so take absolute value for index
      index = (int) inputPixel + 128; 
    } else {
      // otherwise scale down the 1K possible 15 bit values to an index
      index = (int) inputPixel / scale;
    }
    //System.out.println("xxx band = "+band+" index = "+index+" scale="+scale+ " inputPixel"+inputPixel);

    if (!(Float.isNaN(lookupTable[band - 1][index]))) {
      return (lookupTable[band - 1][index]);
    }

    // validate, then calibrate for each combination starting with cur type
    switch (curCalType) {

      case CAL_RAW:

        outputData = inputPixel;

        // if they want raw, just break right away
        if (calTypeOut == CAL_RAW) {
          break;
        }

        // convert to radiance 
        if ((sid % 2) == 0) {
          outputData = inputPixel / scale;
        }
        outputData = (outputData - bias) / gain;

        // if they want radiance we are done
        if (calTypeOut == CAL_RAD) {
          break;
        }

        // otherwise, convert to temperature
        outputData = radToTemp(outputData, band, sid);

        // if they want temperature, break here 
        if (calTypeOut == CAL_TEMP) {
          break;
        }

        // compute brightness from temperature
        if (outputData >= 242.0f) {
          outputData = Math.max(660 - (int) (outputData * 2), 0);
        } else {
          outputData = Math.min(418 - (int) outputData, 255);
        }

        // if they want brightness, break here 
        if (calTypeOut == CAL_BRIT) {
          break;
        }

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

    lookupTable[band - 1][index] = outputData;
    return outputData;

  }

}

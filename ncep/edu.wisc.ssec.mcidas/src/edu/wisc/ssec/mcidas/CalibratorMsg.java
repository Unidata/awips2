//
// CalibratorMsg.java
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

/**
 * Calibration routines for the Meteosat Second Generation (MSG) instrument.
 * <p>
 * At the time this module was written the 
 * <a href="http://www.ssec.wisc.edu/mcidas/doc/prog_man/2006/">
 * McIDAS documentation
 * </a> for the <a href="http://www.esa.int/SPECIALS/MSG/">MSG Instrument</a>
 * did not reflect that calibration data is now stored by band in the 
 * calibration block.
 * </p>
 * <p>
 * Current understanding is that an MSG ASCII calibration block should be 
 * prefixed by the string MSGT followed by the following 6 values for the 
 * Inverse Planck Funciton corresponding to each band:
 * <br/><br/>
 * <table border="1" cellpadding="2">
 *   <tr><th>Name</th><th>Calculation</th><th>Units</th></tr>
 *   <tr><td><b>C1W3</b></td><td colspan="2">C1 * W^3</td></tr>
 *   <tr><td></td><td>C1 = 2.0e5 * P * C^2</td><td>radiance/(cm^-1)^3)</td></tr>
 *   <tr><td></td><td>W = 1.0e2 * (band wave numbers)</td><td>(cm^-1)</td></tr>
 *   <tr><td><b>C2W</b></td><td colspan="2">C2 * W</td></tr>
 *   <tr><td></td><td>C2 = P * C / B</td><td>K/(cm^-1)</td></tr>
 *   <tr><td></td><td>W = 1.0e2 * (band wave numbers)</td><td>(cm^-1)</td></tr>
 *   <tr><td><b>ALPHA</b></td><td colspan="2">gain adjustment</td></tr>
 *   <tr><td><b>BETA</b></td><td colspan="2">bias adjustment</td></tr>
 *   <tr><td><b>GAIN</b></td><td colspan="2">gain</td></tr>
 *   <tr><td><b>OFFSET</b></td><td colspan="2">offset</td></tr>
 * </table>
 * <br/>
 * Where C = speed of light, P = planck constant, B = Boltzmann constant.
 * </p>
 * <p>
 * Each string value is 17 characters with 10 decimal places (fortran E17.10), 
 * and should be parseable using <code>Double.parseDouble</code>. After 
 * converting cal block from ints to a string calibration values for the first
 * band, including the header should look like:
 * </p>
 * <p>
 * <i>MSGT 0.0000000000E+00 0.0000000000E+00 0.0000000000E+00 0.0000000000E+00 
 * 0.2312809974E-01-0.1179533087E+01</i>
 * </p>
 * @author Bruce Flynn, SSEC
 * @version $Id: CalibratorMsg.java,v 1.7 2009-03-02 23:34:50 curtis Exp $
 */
public class CalibratorMsg implements Calibrator {

    private static final int C1W3 = 0;
    private static final int C2W = 1;
    private static final int ALPHA = 2;
    private static final int BETA = 3;
    private static final int GAIN = 4;
    private static final int OFFSET = 5;

    /** Size of cal block values (fortran E17.10 format code). */
    private static final int FMT_SIZE = 17;
    /** Size of a band in the cal block in bytes. */
    private static final int BAND_SIZE = 103;
    /** Size of the cal block header in bytes. */
    private static final int HDR_SIZE = 4;
    /** Cal block header string. */
    private static final String HEADER = "MSGT";

    /** Coefficients for individual bands. */
    private final float[] bandCoefs = new float[] {
        21.21f,
        23.24f,
        19.77f,
        0f,
        0f,
        0f,
        0f,
        0f,
        0f,
        0f,
        0f,
        22.39f
    };

    /** Coefficients used in the inverse planck function. */
    private final double[][] planckCoefs;

    /** Cal block converted from an int array. */
    private byte[] calBytes;
    /** 
     * Current cal type as set by <code>setCalType</code>
     */
    private int curCalType = CAL_RAW;
    
    /**
     * Construct this object according to the calibration data provided.
     * On initalization the calibration array is cloned and all bands available
     * are read from the block.
     *
     * @param cal calibration block from an MSG AREA file.
     * @throws CalibratorException on invalid calibration block.
     */
    public CalibratorMsg(final int[] cal)  throws CalibratorException { 
        int[] calBlock = (int[]) cal.clone();

        // convert int[] to bytes
        calBytes = calIntsToBytes(calBlock);
       
        // if the header is incorrect, flip and try again
        String msgt = new String(calBytes, 0, 4);
        if (!msgt.equals(HEADER)) {
            McIDASUtil.flip(calBlock, 0, calBlock.length - 1);
            calBytes = calIntsToBytes(calBlock);
            msgt = new String(calBytes, 0, 4);
            if (!msgt.equals(HEADER)) {
                throw new IllegalArgumentException(
                    "Invalid calibration block header: " + msgt
                );
            }
        }

        planckCoefs = getCalCoefs();
    }
    
    /**
     * 
     * @param coefs
     * @throws CalibratorException
     */
    public CalibratorMsg(final double[][] coefs) throws CalibratorException {
    	planckCoefs = coefs;
    }

    /**
     * Set the input data calibration type.
     *
     * @param calType calibration type from <code>Calibrator</code>
     * @return 0 if calibration type is valid, -1 otherwise.
     */
    public int setCalType(int calType) {
        if ((calType < Calibrator.CAL_MIN) || (calType > Calibrator.CAL_MAX)) {
            return -1;
        }
        curCalType = calType;
        return 0;
    }

    /**
     * Calibrate an array of pixels from the current calibration type according
     * to the parameters provided.
     *
     * @param input pixel array to calibrate.
     * @param band channel for which to perform calibration.
     * @param calTypeOut Calibration type constant.
     * @return calibrated pixel value or <code>Float.NaN</code> if the
     * calibration type cannot be performed for the specified band.
     */
    public float[] calibrate(final float[] input, final int band, 
        final int calTypeOut) {
        
        if (calTypeOut == curCalType || calTypeOut == CAL_NONE) { // no-op
          return (float[])input.clone();
        }
      
        float[] output = new float[input.length];

        for (int i = 0; i < input.length; i++) {
            output[i] = calibrate(input[i], band, calTypeOut);
        }

        return output;
    }

    /**
     * Calibrate a pixel from the current calibration type according to the 
     * parameters provided.
     * 
     * @param inputPixel pixel value to calibrate.
     * @param band channel for which to perform calibration.
     * @param calTypeOut Calibration type constant.
     * @return calibrated pixel value or <code>Float.NaN</code> if the
     * calibration type cannot be performed for the specified band.
     */
    public float calibrate(
        final float inputPixel, 
        final int band, 
        final int calTypeOut) {
        float pxl;

        if (calTypeOut == curCalType || calTypeOut == CAL_NONE) { // no-op
          return inputPixel;
        }
        
        switch (curCalType) {
            case CAL_ALB:
                throw new UnsupportedOperationException(
                    "Calibration from reflectance not implemented"
                );
            case CAL_TEMP:
                throw new UnsupportedOperationException(
                    "Calibration from temperature not implemented"
                );
            case CAL_BRIT:
                throw new UnsupportedOperationException(
                    "Calibration from brightness not implemented"
                );
            case CAL_RAD:
                throw new UnsupportedOperationException(
                    "Calibration from radiance not implemented"
                );
            case CAL_RAW:
                pxl = calibrateFromRaw(inputPixel, band, calTypeOut);
                break;

            default:
                throw new IllegalArgumentException(
                    "Unknown calibration type"
                );
        }

        return pxl;
    }

    /**
     * Calibrate a pixel from RAW data according to the parameters provided.
     *
     * @param inputPixel pixel value to calibrate.
     * @param band channel for which to perform calibration.
     * @param calTypeOut Calibration type constant.
     * @return calibrated pixel value, <code>Float.NaN</code> if the
     * calibration type cannot be performed for the specified band.
     */
    public float calibrateFromRaw(
        final float inputPixel,
        final int band,
        final int calTypeOut) {
    	
        if (calTypeOut == CAL_RAW || calTypeOut == CAL_NONE) { // no-op
            return inputPixel;
        }
      
        double[] coefs = planckCoefs[band - 1];

        double pxl = inputPixel * coefs[GAIN] + coefs[OFFSET];

        if (pxl < 0) {
            pxl = 0.0;
        }

        // Visible and near-visible (VIS006, VIS008, IR016, HRV)
        if (band < 4 || band == 12) {
            switch (calTypeOut) {
                case CAL_TEMP: // can't do temp 
                    pxl = Double.NaN;
                    break;
                
                case CAL_RAD: // radiance
                    break;
            
                case CAL_ALB: // reflectance
                    pxl = (pxl / bandCoefs[band-1]) * 100.0;
                    if (pxl < 0) {
                        pxl = 0.0;
                    } else if (pxl > 100) {
                        pxl = 100.0;
                    }
                    break;
                
                case CAL_BRIT: // brightness
                    pxl = (pxl / bandCoefs[band-1]) * 100.0;
                    if (pxl < 0) {
                        pxl = 0.0;
                    } else if (pxl > 100) {
                        pxl = 100.0;
                    }
                    pxl = Math.sqrt(pxl) * 25.5;
                    break;

                default:
                    throw new IllegalArgumentException(
                        "Unknown calibration type: " + calTypeOut
                    );               
                }

        // IR Channel
        } else {
        
            switch (calTypeOut) {
                case CAL_TEMP: // temperature
                    if (pxl > 0) {
                        pxl = (coefs[C2W] / Math.log(1.0 + coefs[C1W3] / pxl) 
                            - coefs[BETA]) / coefs[ALPHA];
                    }
                    break;

                case CAL_RAD: // radiance
                    break;
            
                case CAL_ALB: // can't do reflectance
                    pxl = Double.NaN;
                    break;
                
                case CAL_BRIT: // brightness
                    if (pxl > 0) {
                        pxl = (coefs[C2W] / Math.log(1.0 + coefs[C1W3] / pxl) 
                            - coefs[BETA]) / coefs[ALPHA];
                        pxl = greyScale(pxl);
                    } else {
                        pxl = 255.0;
                    }
                    break;

                default:
                    throw new IllegalArgumentException(
                        "Unsupported calibration type: " + calTypeOut
                    );

            } 
        }

        return (float) pxl;
    }
    
    /**
     * Convert a brightness temperature to grey scale.
     * 
     * @param val temperature value in kelvin.
     * @return brightness value.
     */
    private double greyScale(final double val) {
        final int tempLim = 242;
        final int c1 = 418;
        final int c2 = 660;
        double ret;
        
        if (val < tempLim) {
          ret = Math.min(c1 - val, 255);
        } else {
          ret = Math.max(c2 - 2 * val, 0);
        }
        
        return ret;
    }

    /**
     * Get cal block coefs, converting from bytes to strings.
     *
     * @return array of cal coefs by band.
     * @throws CalibratorExcpetion when unable to parse calibration block.
     */
    private double[][] getCalCoefs() throws CalibratorException {

        double[][] coefs = new double[12][6];
        
        for (int i = 0; i < coefs.length; i++) {
            
            // add 1 to band size for extra whitespace between bands
            final int bandOffset = (i * (BAND_SIZE + 1)) + HDR_SIZE;

            String[] strVals = getBandVals(
                new String(calBytes, bandOffset, BAND_SIZE)
            );

            try {
                coefs[i][C1W3] = Double.parseDouble(strVals[0]);
                coefs[i][C2W] = Double.parseDouble(strVals[1]);
                coefs[i][ALPHA] = Double.parseDouble(strVals[2]);
                coefs[i][BETA] = Double.parseDouble(strVals[3]);
                coefs[i][GAIN] = Double.parseDouble(strVals[4]);
                coefs[i][OFFSET] = Double.parseDouble(strVals[5]);
            } catch (NumberFormatException e) {
                throw new CalibratorException(
                  "Unable to parse values from calibration block for band "
                    + (i + 1)
                );
            }
        }
        return coefs;
    }

    /**
     * Split a line corresponding to a single band in the calibration block
     * into <code>String</code>s. There are some additional operations 
     * performed to identify and correct an issue where some signed values
     * were not separated by whitespace.
     *
     * @param line the line as mentioned above.
     * @return array where each value is the string representation of a value
     * from the cal block corresponding to a band.
     */
    private String[] getBandVals(final String line) {
  
        String[] strVals = new String[6];
        for (int i = 0, j = 0; i < strVals.length; i++, j += FMT_SIZE) {
            strVals[i] = line.substring(j, j + FMT_SIZE);
        }

        return strVals;
    }

    /**
     * Convert an array of ints to an array of bytes.
     *
     * @param orig array of ints to convert
     * @return array of bytes
     */
    private byte[] calIntsToBytes(final int[] orig) {
        byte[] bites = new byte[orig.length * 4];
        for (int i = 0, j = 0; i < orig.length; i++) {
            bites[j++] = (byte) (orig[i] & 0x000000ff);
            bites[j++] = (byte) ((orig[i] >> 8) & 0x000000ff); 
            bites[j++] = (byte) ((orig[i] >> 16) & 0x000000ff);
            bites[j++] = (byte) ((orig[i] >> 24) & 0x000000ff);
        }
        return bites;
    }

}

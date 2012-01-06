//
// ConversionUtility.java
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
 * A collection of methods for doing various conversions.
 * <p>
 *
 * @version 1.3, 16 Nov 98
 * @author Tommy Jasmin, SSEC 
 */

public class ConversionUtility 

{

  /**
   * Find the distance in km between two points given the lats/lons.
   *
   * @param lat1 - latitude of first point
   * @param lat2 - latitude of second point
   * @param lon1 - longitude of first point
   * @param lon2 - longitude of second point
   * @return - the distance in km between the two input points 
   */

  public static float LatLonToDistance (
    float lat1,
    float lon1,
    float lat2,
    float lon2
  ) 

  {

    double arcl;
    double cplat;
    double cplon;
    double crlat;
    double crlon;
    double dist;
    double plat;
    double plon;
    double rlat;
    double rlon;
    double srlat;
    double srlon;
    double splat;
    double splon;
    double xx;
    double yy;
    double zz;
   
    float r = 6371.0f;
    float z = 0.017453292f;
   
    rlat = (double) (lat1) * z;
    rlon = (double) (lon1) * z;
    plat = (double) (lat2) * z;
    plon = (double) (lon2) * z;
   
    crlat = Math.cos(rlat);
    crlon = Math.cos(rlon);
    srlat = Math.sin(rlat);
    srlon = Math.sin(rlon);
    cplat = Math.cos(plat);
    cplon = Math.cos(plon);
    splat = Math.sin(plat);
    splon = Math.sin(plon);
   
    xx = (cplat * cplon) - (crlat * crlon);
    yy = (cplat * splon) - (crlat * srlon);
    zz = splat - srlat;
   
    dist = Math.sqrt((xx * xx) + (yy * yy) + (zz * zz));
    arcl = 2.0 * Math.asin(dist / 2.0) * r;
   
    return (float) (arcl);

  }

  /**
   * Convert a latitude or longitude in dddmmss format to floating point.
   *
   * @param dddmmss - latitude or longitude
   * @return - floating point representation of the input parameter.
   */

  public static float FloatLatLon (
    int dddmmss
  )
 
  {
    int inVal;
    float negVal;
    float retVal;
 
    if (dddmmss < 0) {
      inVal = -dddmmss;
      negVal = -1.0f;
    } else {
      inVal = dddmmss;
      negVal = 1.0f;
    }
 
    retVal = (float) (inVal / 10000) +
      (float) ((inVal / 100) % 100) / 60.0f +
      (float) (inVal % 100) / 3600.0f;
 
    retVal = negVal * retVal;
    return (retVal);
 
  }
 
  /**
   * Convert a Gould format floating point number to native double format
   *
   * @param inVal - input Gould value
   * @return - the input value converted to double floating point
   */

  public static double GouldToNative(int inVal) {

    int sign;
    int exponent;
    float mant;
    int byte0;
    int byte1;
    int byte2;
    double dblMant;
    double tempVal;
    double nativeVal;

   /*
    * an example conversion:
    *
    * input value (hex): BE DA 4D 07
    *  a) convert to binary:
    *     1011 1110 1101 1010 0100 1101 0000 0111
    *  b) sign bit is set, so take twos complement:
    *     0100 0001 0010 0101 1011 0010 1111 1001
    *  c) convert this back to hex: 41 25 B2 F9
    *  d) mantissa = 2470649
    *  e) exponent = 65
    *  f) tempVal = mantissa / 16 exp (70 - 65)
    *  g) outputVal = tempVal * sign = -2.3561944
    *
    */

    // set up munging bytes
    byte0 = (inVal & 0x000000FF);
    byte1 = ((inVal >> 8) & 0x000000FF);
    byte2 = ((inVal >> 16) & 0x000000FF);

    sign = 1;
    if ((inVal & 0x80000000) != 0) {
      sign = -1;
      inVal = -inVal;
    }

    exponent = ((inVal & 0x7F000000) >> 24);
    if (exponent == 0) {
      exponent = 64;
    }

    // determine the value of the mantissa, load into a double
    mant = (float) (byte0 + (byte1 * 256) + (byte2 * 65536));
    mant = Math.abs(mant);
    dblMant = (double) mant;

    // now adjust the mantissa according to the exponent
    tempVal = Math.pow((double) 16, (double) (70 - exponent));
    nativeVal = dblMant / tempVal;
    nativeVal = nativeVal * sign;

    return nativeVal;

  }

  /**
   * swap the bytes of an integer array
   *
   * @param array[] array of integers to be flipped
   * @param first starting element of the array
   * @param last last element of array to flip
   *
   */

  public static void swap (int array[], int first, int last) 

  {
    int i, k;
    for (i = first; i <= last; i++) {
      k = array[i];
      array[i] = ( (k >>> 24) & 0xff) | ( (k >>> 8) & 0xff00) |
                 ( (k & 0xff) << 24 )  | ( (k & 0xff00) << 8);
    }
  }
 
}

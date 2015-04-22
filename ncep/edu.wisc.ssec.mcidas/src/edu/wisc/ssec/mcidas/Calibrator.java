//
// Calibrator.java
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
 * interface for creating Calibrator classes.
 *
 * @version 1.2 16 Nov 1998
 * @author Tommy Jasmin, SSEC
 */

public interface Calibrator {

  public static final int CAL_NONE = -1;
  public static final int CAL_MIN  = 1;
  public static final int CAL_RAW  = 1;
  public static final int CAL_RAD  = 2;
  public static final int CAL_ALB  = 3;
  public static final int CAL_TEMP = 4;
  public static final int CAL_BRIT = 5;
  public static final int CAL_MAX  = 5;

  /** Meteosat Second Generation imager. */
  public static final int SENSOR_MSG_IMGR = 51;
  /** GOES 8 imager. */
  public static final int SENSOR_GOES8_IMGR = 70;
  /** GOES 8 sounder. */
  public static final int SENSOR_GOES8_SNDR = 71;
  /** GOES 9 imager. */
  public static final int SENSOR_GOES9_IMGR = 72;
  /** GOES 9 sounder. */
  public static final int SENSOR_GOES9_SNDR = 73;
  /** GOES 10 imager. */
  public static final int SENSOR_GOES10_IMGR = 74;
  /** GOES 10 sounder. */
  public static final int SENSOR_GOES10_SNDR = 75;
  /** GOES 12 imager. */
  public static final int SENSOR_GOES12_IMGR = 78;
  /** GOES 12 sounder. */
  public static final int SENSOR_GOES12_SNDR = 79;
  /** GOES 13 imager. */
  public static final int SENSOR_GOES13_IMGR = 180;
  /** GOES 13 sounder. */
  public static final int SENSOR_GOES13_SNDR = 181;
  
  public int setCalType (
    int calType
  );

  public float[] calibrate (
    float[] inputData,
    int band,
    int calTypeOut
  );

  public float calibrate (
    float inputPixel,
    int band,
    int calTypeOut
  );

}

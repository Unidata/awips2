//
// CalibratorFactory.java
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
 * Utility class for creating <code>Calibrator</code> instances.
 * 
 * @author Bruce Flynn, SSEC
 * @version $Id: CalibratorFactory.java,v 1.7 2009-05-06 18:45:53 rickk Exp $
 */
public final class CalibratorFactory {

	/** Disallow instatiantion. */
	private CalibratorFactory() {}
	
    /**
     * Get an appropriate <code>Calibrator</code> for the sensor id provided.
     * This assumes a RAW source data format. See the McIDAS Users Guide 
     * <a href="http://www.ssec.wisc.edu/mcidas/doc/users_guide/current/app_c-1.html">Appendix C</a>
     * for a table of sensor ids.
     * @param id Sensor id from the directory block
     * @param cal Calibration block used to initialize the <code>Calibrator
     * </code>
     * @return initialized <code>Calibrator</code> with a source calibration
     * type of RAW.
     * @throws CalibratorException on an error initializing the object.
     */
	public final static Calibrator getCalibrator(final int id, final int[] cal) 
		throws CalibratorException {
		
		return getCalibrator(id, Calibrator.CAL_RAW, cal);
	}
	
    /**
     * Get an appropriate <code>Calibrator</code> for the sensor id provided.
     * See the McIDAS Users Guide 
     * <a href="http://www.ssec.wisc.edu/mcidas/doc/users_guide/current/app_c-1.html">Appendix C</a>
     * for a table of sensor ids.
     * @param id Sensor id from the directory block
     * @param srcType the source data type from the directory block
     * @param cal Calibration block used to initialize the 
     *        <code>Calibrator</code>
     * @return initialized <code>Calibrator</code>.
     * @throws CalibratorException on an error initializing the object or if the
     *         sensor is unknown.
     */
	public final static Calibrator getCalibrator(
			final int id, final int srcType, final int[] cal) 
		throws CalibratorException {
		
		Calibrator calibrator = null;
	    switch (id) {
	      
	      case Calibrator.SENSOR_MSG_IMGR:
	    	  calibrator = new CalibratorMsg(cal);
	    	  calibrator.setCalType(srcType);
	    	  break;
	        
	      case Calibrator.SENSOR_GOES8_IMGR:
	      case Calibrator.SENSOR_GOES8_SNDR:
	    	  calibrator = new CalibratorGvarG8(id, cal);
	    	  calibrator.setCalType(srcType);
	    	  break;
	    	  
	      case Calibrator.SENSOR_GOES9_IMGR:
	      case Calibrator.SENSOR_GOES9_SNDR:
	    	  calibrator = new CalibratorGvarG9(id, cal);
	    	  calibrator.setCalType(srcType);
	    	  break;
	    	  
	      case Calibrator.SENSOR_GOES10_IMGR:
	      case Calibrator.SENSOR_GOES10_SNDR:
	    	  calibrator = new CalibratorGvarG10(id, cal);
	    	  calibrator.setCalType(srcType);
	    	  break;
	    	  
	      case Calibrator.SENSOR_GOES12_IMGR:
	      case Calibrator.SENSOR_GOES12_SNDR:
	    	  calibrator = new CalibratorGvarG12(id, cal);
	    	  calibrator.setCalType(srcType);
	    	  break;
	    	  
	      case Calibrator.SENSOR_GOES13_IMGR:
	      case Calibrator.SENSOR_GOES13_SNDR:
	    	  calibrator = new CalibratorGvarG13(id, cal);
	    	  calibrator.setCalType(srcType);
	    	  break;

	      default:
	        throw new CalibratorException(
	            "Unknown or unimplemented sensor id: " + id
	        );
	    }
	    return calibrator;
	}
  
  /**
   * Check if there is a <code>Calibrator</code> implemented for a sensor.
   * 
   * @param id Id of the sensor from the McIDAS Users Guide
   * <a href="http://www.ssec.wisc.edu/mcidas/doc/users_guide/current/app_c-1.html">Appendix C</a>
   * @return True if there is an implemented <code>Calibrator</code>, false
   *         otherwise.
   * @see The McIDAS Users Guide 
   */
  public final static boolean hasCalibrator(int id) {
    switch (id) {
      case Calibrator.SENSOR_GOES13_IMGR:
      case Calibrator.SENSOR_GOES13_SNDR:
      case Calibrator.SENSOR_GOES12_IMGR:
      case Calibrator.SENSOR_GOES12_SNDR:
      case Calibrator.SENSOR_GOES10_IMGR:
      case Calibrator.SENSOR_GOES10_SNDR:
      case Calibrator.SENSOR_GOES8_IMGR:
      case Calibrator.SENSOR_GOES8_SNDR:
      case Calibrator.SENSOR_GOES9_IMGR:
      case Calibrator.SENSOR_GOES9_SNDR:
      case Calibrator.SENSOR_MSG_IMGR:
        return true;
      default:
        return false;
    }
  }
}

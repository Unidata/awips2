/**
 * WindGroup - decodes wind data
 * 
 * This java class intends to serve as an utility only for UAIR decoder.
 * 
 * HISTORY
 *
 * * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2010      210				L. Lin     	Initial coding
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.uair.util;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

public class WindGroup {
	
	private static float windSpeed;
	
	private static float windDirection;
	
	public static float getWindSpeed() {
		return windSpeed;
	}

	public void setWindSpeed(float windSpeed) {
		this.windSpeed = windSpeed;
	}

	public static float getWindDirection() {
		return windDirection;
	}

	public void setWindDirection(float windDirection) {
		this.windDirection = windDirection;
	}

	/**
	 * This subroutine decodes a wind field in the form dddff where ddd
	 * is the direction in degrees and ff is the wind speed.
	 * If last digit of ddd is not equal to 0, then it becomes ddfff.
	 * In other words, wind speed should be "dff" not "ff".
	 * 
	 * @param report The input report
	 * @param windKnot The input windKnot is flag to indicate wind in knot or not.
	 * @return 
	 */
	public static void WindField(String windGroup, Boolean windKnot) {
		
		windSpeed = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		windDirection = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		
		if ( windGroup != null ) {
			if ( windGroup.length() == 5 ) {
				String ddd = windGroup.substring(0,3);
				String ff = windGroup.substring(3,5);
				
				String wDir = windGroup.substring(0,2);
				String wSpeed = windGroup.substring(2,5);
				
				if ( ! ddd.substring(0,2).equals("//") && ! ff.equals("//")) {
					/*
					 * The tens and hundreds of the wind direction is encoded in the
					 * first two digits. 
					 */
					int idir = Integer.parseInt(wDir);
					idir = idir * 10;

					//The wind speed is encoded in the last three digits.
					int ispeed = Integer.parseInt(wSpeed);
					if ( ispeed >= 500 ) {
						/*
						 * The units value of the direction rounded to 0 or 5 is
						 * multiplied by 100 and added to the wind speed.
						 */
						ispeed = ispeed - 500;
						idir = idir + 5;
					}

					/*
					 * Set the output values.  Return missing data if direction is
					 * greater than 360 or field is '31313'.
					 */
					if ( idir == 360 ) {
						idir = 0;
					}
					windSpeed = ispeed;
					if ( windKnot ) {
						windSpeed = (float) (windSpeed / 1.9425);
					}
					windDirection = idir;
				} else 	if ( ! ddd.equals("///") && ff.equals("//")) {
					// wind speed missing
					int idir = Integer.parseInt(ddd);
					windDirection = idir;
				}
			} 	
		}
	}
}

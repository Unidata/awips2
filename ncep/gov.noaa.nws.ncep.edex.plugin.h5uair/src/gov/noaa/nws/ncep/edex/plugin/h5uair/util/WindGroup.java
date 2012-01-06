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

package gov.noaa.nws.ncep.edex.plugin.h5uair.util;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

public class WindGroup {
	
	private static float sped;
	
	private static float drct;
	
	public static float getSped() {
		return sped;
	}

	public void setSped (float sped) {
		this.sped = sped;
	}

	public static float getDrct() {
		return drct;
	}

	public void setDrct(float drct) {
		this.drct = drct;
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
		
		sped = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		drct = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		
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
					sped = ispeed;
					if ( windKnot ) {
						sped = (float) (sped / 1.9425);
					}
					drct = idir;
				} else 	if ( ! ddd.equals("///") && ff.equals("//")) {
					// wind speed missing
					int idir = Integer.parseInt(ddd);
					drct = idir;
				}
			} 	
		}
	}
}

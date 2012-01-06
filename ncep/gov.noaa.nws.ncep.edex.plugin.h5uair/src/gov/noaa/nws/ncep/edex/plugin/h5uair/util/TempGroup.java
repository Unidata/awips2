/**
 * TempGroup - decodes temperature data
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

public class TempGroup {
	
	private static float temperature;
	
	private static float dewpointTemp;
	
	public static float getTemperature() {
		return temperature;
	}

	public static void setTemperature(float temperature) {
		TempGroup.temperature = temperature;
	}

	public static float getDewpointTemp() {
		return dewpointTemp;
	}

	public static void setDewpointTemp(float dewpointTemp) {
		TempGroup.dewpointTemp = dewpointTemp;
	}

	/**
	 * This subroutine decodes a temperature/dewpoint field.  The field
	 * is encoded as TTtDD where TT is temperature in degrees Celsius,
	 * t is approximate tenths of degree and sign indicator, and DD is
	 * the dewpoint depression in degrees Celsius.  
	 * 
	 * @param tempGroup The input temperature data
	 */
	public static void TempField(String tempGroup) {
		
		temperature = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		dewpointTemp = IDecoderConstantsN.UAIR_FLOAT_MISSING;;
		
		if ( tempGroup.length() == 5 ) {
			String ttt = tempGroup.substring(0,3);
			String dd = tempGroup.substring(3,5);
			
			if ( ! ttt.substring(2,3).equals("/") ) {
				/*
				 * If the integer is even, the temperature is positive.
				 * Otherwise,  the temperature is negative.
				 */
				int ittt = Integer.parseInt(ttt);
				int isign = ittt%2;
				if ( isign == 1 ) {
					temperature = - ittt;
				} else {
					temperature = ittt;
				}
				temperature = (float) (temperature / 10.);
				/*
				 * If the dewpoint given is less than 50, the dewpoint is
				 * given in tenths of a degree.  For numbers greater than
				 * 55, the dewpoint is given in whole degrees plus 50.
				 * The values 51 - 55 are not used.
				 */
				if ( ! dd.equals("//") ) {
					int idd = Integer.parseInt(dd);
					if ( idd <= 50 ) {
						dewpointTemp = (float) (idd / 10.);
					} else {
						dewpointTemp = idd - 50;
					}
					dewpointTemp = temperature - dewpointTemp;
				}
				
			}	
		} 
	} 
}

/**
 * PressureHeightGroup
 * 
 * decodes a pressure/height field in the form PPhhh.
 * For data below 100 mb, PP is the pressure in tens of millibars and
 * hhh is the last three digits of the height in meters below 500 mb
 * and the last three digits of height in decameters at/above 500 mb.
 * For data above 100 mb, PP is the pressure in millibars and hhh is
 * the last three digits of the height in decameters.
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

import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5UairRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

public class PressureHeightGroup {
	
	private static float pressure;
	
	private static float height;
	
	public static float getPressure() {
		return pressure;
	}

	public static void setPressure(float pressure) {
		PressureHeightGroup.pressure = pressure;
	}

	public static float getHeight() {
		return height;
	}

	public static void setHeight(float height) {
		PressureHeightGroup.height = height;
	}

	/**
	 * Decodes pressure and height data
	 * 
	 * @param presGroup The input pressure code group
	 * @param above The input above is flag
	 * @param level the input pressure level
	 * @param stationNumber The input station number
	 * @param dataType The input data type
	 * @return 
	 */
	public static void PressureHeightField(String presGroup, Boolean above, 
			int level, String stationNumber, String dataType, H5UairRecord record) {
		
		pressure = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		height = IDecoderConstantsN.UAIR_FLOAT_MISSING;
		int iabove = 0;
		int ipres = IDecoderConstantsN.UAIR_INTEGER_MISSING;
		int ihhh = IDecoderConstantsN.UAIR_INTEGER_MISSING;
		Boolean drop = false;
		Boolean ship = false;

		String pressValue[][] = { { "99", "00", "92", "85", "70", "50",  
				              "40", "30", "25", "20", "15", "10" },
				            { "70", "50", "30", "20", "10", "07",
				              "05", "03", "02", "01", "xx" , "//"}
		};
			
		if ( dataType.substring(0,2).equals("XX") ) {
			// dropsonde data
			drop = true;
		} else if ( dataType.substring(0,2).equals("UU") ) {
			// ship data
			ship = true;
		}
		
		if (above) {
			iabove =1;
		}

		if ( presGroup.length() == 5 && ( level < 12 )) {
			String pp = presGroup.substring(0,2);
			String hhh = presGroup.substring(2,5);
			
			if (pp.equals(pressValue[iabove][level])) {
				if ( ! pp.equals("//") ) {
					ipres = Integer.parseInt(pp);
				}
				if ( ! hhh.equals("///") ) {
					ihhh = Integer.parseInt(hhh);
				}
				
				if (! above) {
					/*
					 * Encoded pressure was pressure / 10.  00 is 1000.
					 * Above 100mb, encoded pressure is whole number.
					 */
					if ( ipres != IDecoderConstantsN.UAIR_INTEGER_MISSING ) {
						ipres = ipres * 10;
						if ( ipres == 0 ) {
							ipres = 1000;
						}
						//Take care of the 925 mb level, which comes in as 920.
						if ( ipres == 920 ) {
							ipres = 925;
						}
						pressure = ipres;
					}

					/*
					 * For data below 100 mb, use the pressure to decode the
					 * height.  This algorithm is from the U. of Wisconsin and
					 * differs slightly from the PROFS algorithm.
					 */
					if ( ihhh != IDecoderConstantsN.UAIR_INTEGER_MISSING ) {
						height = ihhh;
						
						if ( (ipres == 1000) && (height > 500)) {
							height = 500 - height;
						} else if ( (ipres == 925) && (height < 200) && (! drop) ) {
							height = height + 1000;
						} else if ( (ipres == 850) && (height < 900) ) {
							height = height + 1000;
						} else if ( (ipres == 700) && (height < 500) ) {
							height = height + 3000;
						} else if ( ipres == 700 ) {
							height = height + 2000;
						} else if ( ipres <= 500 ) {
							ihhh = ihhh * 10;
							height = height * 10;
							if ( (ipres == 300) && (ihhh < 3000) ) {
								height = height + 10000;
							} else if ( (ipres == 250) && (ihhh < 5000) ) {
								height = height + 10000;
							} else if ( (ipres == 200) && (ihhh < 7000) ) {
								height = height + 10000;
							} else if (ipres <= 150) {
								height = height + 10000;
							}
						}
					}
				} else {
					pressure = ipres;					
					/*
					 * Compute the height above 100 mb.  The ten thousands digit
					 * is added here.  The value may need to be changed in the
					 * future if it proves incorrect.
					 */
					if ( ihhh != IDecoderConstantsN.UAIR_INTEGER_MISSING ) {
						ihhh = ihhh * 10;
						height = ihhh;
						if ( ipres == 70 ) {
							height = height + 10000;
						} else if ( (ipres == 50) && (ihhh >= 8000)) {
							height = height + 10000;
						} else if ( (ipres == 50) && (height < 8000) ) {
							height = height + 20000;
						} else if ( ipres >= 20 ) {
							height = height + 20000;
						} else if ( (ipres == 10) && (height > 8000) ) {
							height = height + 20000;
						} else if ( (ipres == 10) && (height < 8000) ) {
							height = height + 30000;	
						} else if ( ipres >= 3 ) {
							height = height + 30000;
						} else if ( (ipres == 2) && (height > 8000) ) {
							height = height + 30000;
						} else if ( (ipres == 2) && (height < 8000) ) {
							height = height + 40000;	
						} else {
							height = height + 40000;	
						}
					}
				}
				/*
				 * This subroutine decodes the surface data from a TTAA 
				 */
				if ( pp.equals("99") && ! above ) {
					int ihhhSurface = Integer.parseInt(hhh);
					pressure = ihhhSurface;
					if ( ihhhSurface < 100 ) {
						pressure = pressure + 1000;	
					}
					if ( ! drop && stationNumber != null ) {			
						height = IDecoderConstantsN.UAIR_INTEGER_MISSING;
						height = (float) record.getSelv();
					} else if ( drop ){
						// dropsonde data
						height = IDecoderConstantsN.UAIR_INTEGER_MISSING;
					} else {
						// ship data
						height = ShipMobile.getSurfaceHeight();
					}
				}
			}
		} 
	} 
}

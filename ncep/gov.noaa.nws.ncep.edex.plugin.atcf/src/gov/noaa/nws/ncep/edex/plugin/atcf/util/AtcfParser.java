/**
 *
 * AtcfParser
 * 
 * This class provides parser processing utilities for the Automated Tropical Cyclone Forecast ATCF Decoder Plug-In.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/23/10		283			F. J. Yen	Initial Creation
 * 09/02/10		283			F. J. Yen	Added call to set dataTime
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */

package gov.noaa.nws.ncep.edex.plugin.atcf.util;

import java.util.Calendar;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.time.DataTime;

import gov.noaa.nws.ncep.common.dataplugin.atcf.AtcfRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

public class AtcfParser {
	/**
	 * Constructor
	 */
	private static final float RMISSD = IDecoderConstantsN.FLOAT_MISSING;
	public AtcfParser () {
	}
	
	/*
	 * processFields: Parse the fields in theBulletin and
	 * sets them in AtcfRecord record and returns record.
	 * 
	 * @param theBulletin         the raw ATCF record (treated as bulletin) string
	 */
	public static AtcfRecord processFields(String theBulletin) {
		
		AtcfRecord record = null;
		final Integer maxFields = 50;
		record = new AtcfRecord();
		
		theBulletin = theBulletin.replaceAll(" ", "").replace("\r"," ").replace("\n", " ");
		Scanner sc = new Scanner(theBulletin).useDelimiter("[, ]");
		String[] atcfField = new String[maxFields];
		int fieldIdx = 0;
		while (sc.hasNext() && (fieldIdx < maxFields)) {
			atcfField[fieldIdx] = sc.next();
			fieldIdx++;
		}   

		if (fieldIdx > 0 && !atcfField[0].equals("")) {
			record.setBasin(atcfField[0]);
		}
		if (fieldIdx > 1 && !atcfField[1].equals("")) {
			record.setCycloneNum(Integer.parseInt(atcfField[1]));
		}
		if (fieldIdx > 2 && !atcfField[2].equals("")) {
			Calendar warnTm = processWarnTime(atcfField[2]);
			record.setWarnTime(warnTm);
			/*
			 * Put the warning time in the second field of the DataURI
			 * using setDataTime.  This creates the reftime timestamp
			 * and also sets forecasttime to a nonnull value.  Although
			 * forecasttime is not used in bufrssha, it can not be null
			 * when retrieving record from db.
			 */
			DataTime dataTime=new DataTime(warnTm);
			record.setDataTime(dataTime);
		}
		if (fieldIdx > 3 && !atcfField[3].equals("")) {
			record.setTechniqueNum(Integer.parseInt(atcfField[3]));
		}
		if (fieldIdx > 4 && !atcfField[4].equals("")) {
			record.setTechnique(atcfField[4]);
		}
		if (fieldIdx > 5 && !atcfField[5].equals("")) {
			record.setFcstHour(Integer.parseInt(atcfField[5]));
		}
		if (fieldIdx > 6 && !atcfField[6].equals("")) {
			record.setClat(processLatLon(atcfField[6]));
		}
		if (fieldIdx > 7 && !atcfField[7].equals("")) {
				record.setClon(processLatLon(atcfField[7]));
		}
		if (fieldIdx > 8 && !atcfField[8].equals("")) {
			record.setWindMax((float)(Integer.parseInt(atcfField[8])));
		}
		if (fieldIdx > 9 && !atcfField[9].equals("")) {
			record.setMslp((float)(Integer.parseInt(atcfField[9])));
		}
		if (fieldIdx > 10 && !atcfField[10].equals("")) {
			record.setIntensity(atcfField[10]);
		}
		if (fieldIdx > 11 && !atcfField[11].equals("")) {
			record.setRadWind((float)(Integer.parseInt(atcfField[11])));
		}
		if (fieldIdx > 12 && !atcfField[12].equals("")) {
			record.setRadWindQuad(atcfField[12]);
		}
		if (fieldIdx > 13 && !atcfField[13].equals("")) {
			record.setQuad1WindRad((float)(Integer.parseInt(atcfField[13])));
		}
		if (fieldIdx > 14 && !atcfField[14].equals("")) {
			record.setQuad2WindRad((float)(Integer.parseInt(atcfField[14])));
		}
		if (fieldIdx > 15 && !atcfField[15].equals("")) {
			record.setQuad3WindRad((float)(Integer.parseInt(atcfField[15])));
		}
		if (fieldIdx > 16 && !atcfField[16].equals("")) {
			record.setQuad4WindRad((float)(Integer.parseInt(atcfField[16])));
		}
		if (fieldIdx > 17 && !atcfField[17].equals("")) {
			record.setClosedP((float)(Integer.parseInt(atcfField[17])));
		}
		if (fieldIdx > 18 && !atcfField[18].equals("")) {
			record.setRadClosedP((float)(Integer.parseInt(atcfField[18])));
		}
		if (fieldIdx > 19 && !atcfField[19].equals("")) {
			record.setMaxWindRad((float)(Integer.parseInt(atcfField[19])));
		}
		if (fieldIdx > 20 && !atcfField[20].equals("")) {
			record.setGust((float)(Integer.parseInt(atcfField[20])));
		}
		if (fieldIdx > 21 && !atcfField[21].equals("")) {
			record.setEyeSize((float)(Integer.parseInt(atcfField[21])));
		}
		if (fieldIdx > 22 && !atcfField[22].equals("")) {
			record.setSubRegion(atcfField[22]);
		}
		if (fieldIdx > 23 && !atcfField[23].equals("")) {
			record.setMaxSeas((float)(Integer.parseInt(atcfField[23])));
		}
		if (fieldIdx > 24 && !atcfField[24].equals("")) {
			record.setForecaster(atcfField[24]);
		}
		if (fieldIdx > 25 && !atcfField[25].equals("")) {
			record.setStormDrct((float)(Integer.parseInt(atcfField[25])));
		}
		if (fieldIdx > 26 && !atcfField[26].equals("")) {
			record.setStormSped((float)(Integer.parseInt(atcfField[26])));
		}
		if (fieldIdx > 27 && !atcfField[27].equals("")) {
			record.setStormName(atcfField[27]);
		}
		if (fieldIdx > 28 && !atcfField[28].equals("")) {
			record.setStormDepth(atcfField[28]);
		}
		if (fieldIdx > 29 && !atcfField[29].equals("")) {
			record.setRadWave((float)(Integer.parseInt(atcfField[29])));
		}
		if (fieldIdx > 30 && !atcfField[30].equals("")) {	
			record.setRadWaveQuad(atcfField[30]);
		}
		if (fieldIdx > 31 && !atcfField[31].equals("")) {
			record.setQuad1WaveRad((float)(Integer.parseInt(atcfField[31])));
		}
		if (fieldIdx > 32 && !atcfField[32].equals("")) {
			record.setQuad2WaveRad((float)(Integer.parseInt(atcfField[32])));
		}
		if (fieldIdx > 33 && !atcfField[33].equals("")) {
			record.setQuad3WaveRad((float)(Integer.parseInt(atcfField[33])));
		}
		if (fieldIdx > 34 && !atcfField[34].equals("")) {
			record.setQuad4WaveRad((float)(Integer.parseInt(atcfField[34])));
		}
		if (fieldIdx > 35 && !atcfField[35].equals("")) {
			record.setUserDefined(atcfField[35]);
		}
		if (fieldIdx > 36 && !atcfField[36].equals("")) {
			record.setUserData(atcfField[36]);
		}
			
		return record;
	}

	/*
	 * processLatLon: Convert the string latitude/longitude in tenths of a degree with appended N|S|E|W 
	 * to return the floating point (+|-)latitude/longitude in degrees without N|S|E|W 
	 * 
	 * @param latLonStr         latitude/longitude string such as 144N or 1056W
	 * 							in tenths
	 */
	public static Float processLatLon(String latLonStr) {
		/*
		 * Regular expression for the latitude or longitude
		 */
		Float latLon=RMISSD;
		final String LATLON = "(\\d{1,4})(N|S|E|W)";
		final Pattern latLonPattern = Pattern.compile(LATLON);
		Matcher latLonMatcher = latLonPattern.matcher (latLonStr);
		if (latLonMatcher.find()) {
			latLon = (float)(Integer.parseInt(latLonMatcher.group(1)));
			if (latLonMatcher.group(2).equals("S") || 
					latLonMatcher.group(2).equals("W")) {
				latLon = -.1f * latLon;
			} else {
				latLon = .1f * latLon;
			}
		}		
		return latLon;	
	}

	/*
	 * processWarnTime: Process the warning date/time string and return the Calendar warnTime
	 * 
	 * @param warnTimeStr		warning date/time string
	 */
	public static Calendar processWarnTime(String warnTimeStr) {
		/*
		 * Regular expression for the warning date/time string
		 */
		final String WARN_DATETIME = "(\\d{4})(\\d{2})(\\d{2})(\\d{2})";
		final Pattern warnDateTimePattern = Pattern.compile(WARN_DATETIME);
		Calendar warnTime = Calendar.getInstance();
		Matcher warnDateTimeMatcher = warnDateTimePattern.matcher(warnTimeStr);
		if (warnDateTimeMatcher.find()) {
			warnTime.set(Calendar.YEAR, Integer.parseInt(warnDateTimeMatcher
					.group(1)));
			warnTime.set(Calendar.MONTH, Integer
					.parseInt(warnDateTimeMatcher.group(2)) - 1);
			warnTime.set(Calendar.DAY_OF_MONTH, Integer
					.parseInt(warnDateTimeMatcher.group(3)));
			warnTime.set(Calendar.HOUR_OF_DAY, Integer
					.parseInt(warnDateTimeMatcher.group(4)));
			warnTime.set(Calendar.MINUTE, 0);
			warnTime.set(Calendar.SECOND, 0);
			warnTime.set(Calendar.MILLISECOND, 0);
		}
		return warnTime;
	}

}

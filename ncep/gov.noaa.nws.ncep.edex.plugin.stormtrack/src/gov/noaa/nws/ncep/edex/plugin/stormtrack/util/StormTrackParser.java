/**
 *
 * StormTrackParser
 * 
 * This class provides parser processing utilities for the StormTrack Decoder Plug-In.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/23/10		283			F. J. Yen	Initial Creation
 * 09/02/10		283			F. J. Yen	Added call to set dataTime
 * 8/2011					T. Lee		Added ENSCYC
 * 10/2012      858         G. Hull     rm forecastHr column and set forecasttime in dataTime
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */

package gov.noaa.nws.ncep.edex.plugin.stormtrack.util;

import java.util.Calendar;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.time.DataTime;

import gov.noaa.nws.ncep.common.dataplugin.stormtrack.StormTrackRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

public class StormTrackParser {
	/**
	 * Constructor
	 */
	private static final float RMISSD = IDecoderConstantsN.FLOAT_MISSING;
	public StormTrackParser () {
	}
	
	/*
	 * processFields: Parse the fields in theBulletin and
	 * sets them in StormTrackRecord record and returns record.
	 * 
	 * @param theBulletin         the raw StormTrack record (treated as bulletin) string
	 */
	public static StormTrackRecord processFields(String theBulletin) {
		
		StormTrackRecord record = null;
		final Integer maxFields = 50;
		record = new StormTrackRecord();
		
		theBulletin = theBulletin.replaceAll(" ", "").replace("\r"," ").replace("\n", " ");
		Scanner sc = new Scanner(theBulletin).useDelimiter("[, ]");
		String[] stormTrackField = new String[maxFields];
		int fieldIdx = 0;
		while (sc.hasNext() && (fieldIdx < maxFields)) {
			stormTrackField[fieldIdx] = sc.next();
			fieldIdx++;
		}   

		if (fieldIdx > 0 && !stormTrackField[0].equals("")) {
			record.setBasin(stormTrackField[0]);
		}
		if (fieldIdx > 1 && !stormTrackField[1].equals("")) {
			record.setCycloneNum(stormTrackField[1]);
		}
		if (fieldIdx > 2 && !stormTrackField[2].equals("")) {
			Calendar warnTm = processWarnTime(stormTrackField[2]);
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
		if (fieldIdx > 3 && !stormTrackField[3].equals("")) {
			record.setTechniqueNum(Integer.parseInt(stormTrackField[3]));
		}
		if (fieldIdx > 4 && !stormTrackField[4].equals("")) {
			record.setModel(stormTrackField[4]);
		}
		if (fieldIdx > 5 && !stormTrackField[5].equals("")) {
//			record.setFcstHour(Integer.parseInt(stormTrackField[5]));
			int fcstSecs = Integer.parseInt(stormTrackField[5])*60*60;			
			record.setDataTime( 
					new DataTime( record.getDataTime().getRefTime(), fcstSecs ) );			
		}
		if (fieldIdx > 6 && !stormTrackField[6].equals("")) {
			record.setClat(processLatLon(stormTrackField[6]));
		}
		if (fieldIdx > 7 && !stormTrackField[7].equals("")) {
				record.setClon(processLatLon(stormTrackField[7]));
		}
		if (fieldIdx > 8 && !stormTrackField[8].equals("")) {
			record.setWindMax((float)(Integer.parseInt(stormTrackField[8])));
		}
		if (fieldIdx > 9 && !stormTrackField[9].equals("")) {
			record.setMslp((float)(Integer.parseInt(stormTrackField[9])));
		}
		if (fieldIdx > 10 && !stormTrackField[10].equals("")) {
			record.setStormType(stormTrackField[10]);
		}
		if (fieldIdx > 11 && !stormTrackField[11].equals("")) {
			record.setWindCategory((float)(Integer.parseInt(stormTrackField[11])));
		}
		if (fieldIdx > 12 && !stormTrackField[12].equals("")) {
			record.setWindCode(stormTrackField[12]);
		}
		if (fieldIdx > 13 && !stormTrackField[13].equals("")) {
			record.setQuad1WindRad((float)(Integer.parseInt(stormTrackField[13])));
		}
		if (fieldIdx > 14 && !stormTrackField[14].equals("")) {
			record.setQuad2WindRad((float)(Integer.parseInt(stormTrackField[14])));
		}
		if (fieldIdx > 15 && !stormTrackField[15].equals("")) {
			record.setQuad3WindRad((float)(Integer.parseInt(stormTrackField[15])));
		}
		if (fieldIdx > 16 && !stormTrackField[16].equals("")) {
			record.setQuad4WindRad((float)(Integer.parseInt(stormTrackField[16])));
		}
		if (fieldIdx > 17 && !stormTrackField[17].equals("")) {
			record.setClosedP((float)(Integer.parseInt(stormTrackField[17])));
		}
		if (fieldIdx > 18 && !stormTrackField[18].equals("")) {
			record.setRadClosedP((float)(Integer.parseInt(stormTrackField[18])));
		}
		if (fieldIdx > 19 && !stormTrackField[19].equals("")) {
			record.setMaxWindRad((float)(Integer.parseInt(stormTrackField[19])));
		}
		if (fieldIdx > 20 && !stormTrackField[20].equals("")) {
			record.setGust((float)(Integer.parseInt(stormTrackField[20])));
		}
		if (fieldIdx > 21 && !stormTrackField[21].equals("")) {
			record.setEyeSize((float)(Integer.parseInt(stormTrackField[21])));
		}
		if (fieldIdx > 22 && !stormTrackField[22].equals("")) {
			record.setSubRegion(stormTrackField[22]);
		}
		if (fieldIdx > 23 && !stormTrackField[23].equals("")) {
			record.setMaxSeas((float)(Integer.parseInt(stormTrackField[23])));
		}
		if (fieldIdx > 24 && !stormTrackField[24].equals("")) {
			record.setForecaster(stormTrackField[24]);
		}
		if (fieldIdx > 25 && !stormTrackField[25].equals("")) {
			record.setStormDrct((float)(Integer.parseInt(stormTrackField[25])));
		}
		if (fieldIdx > 26 && !stormTrackField[26].equals("")) {
			record.setStormSped((float)(Integer.parseInt(stormTrackField[26])));
		}
		if (fieldIdx > 27 && !stormTrackField[27].equals("")) {
			record.setStormName(stormTrackField[27]);
		}
		if (fieldIdx > 28 && !stormTrackField[28].equals("")) {
			record.setStormDepth(stormTrackField[28]);
		}
		if (fieldIdx > 29 && !stormTrackField[29].equals("")) {
			record.setWaveHght((float)(Integer.parseInt(stormTrackField[29])));
		}
		if (fieldIdx > 30 && !stormTrackField[30].equals("")) {	
			record.setWaveCode(stormTrackField[30]);
		}
		if (fieldIdx > 31 && !stormTrackField[31].equals("")) {
			record.setQuad1WaveRad((float)(Integer.parseInt(stormTrackField[31])));
		}
		if (fieldIdx > 32 && !stormTrackField[32].equals("")) {
			record.setQuad2WaveRad((float)(Integer.parseInt(stormTrackField[32])));
		}
		if (fieldIdx > 33 && !stormTrackField[33].equals("")) {
			record.setQuad3WaveRad((float)(Integer.parseInt(stormTrackField[33])));
		}
		if (fieldIdx > 34 && !stormTrackField[34].equals("")) {
			record.setQuad4WaveRad((float)(Integer.parseInt(stormTrackField[34])));
		}
		if (fieldIdx > 35 && !stormTrackField[35].equals("")) {
			record.setUserDefined(stormTrackField[35]);
		}
		if (fieldIdx > 36 && !stormTrackField[36].equals("")) {
			record.setUserData(stormTrackField[36]);
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
		final String LATLON = "(\\d{1,4})(N|S|E|W| )";
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

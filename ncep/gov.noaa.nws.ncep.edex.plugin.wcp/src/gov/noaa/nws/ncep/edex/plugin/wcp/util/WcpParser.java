/**
 *
 * WcpParser
 * 
 * This class provides parser processing utilities for the Watch Corner Point WCP Decoder Plug-In.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 17Apr2009		37		F. J. Yen	Refactored from WcpDecoder for TO10 and to
 * 									    allow more unit testing
 * 24Aug2009		37		F. J. Yen	Modified for migration to TO11
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */

package gov.noaa.nws.ncep.edex.plugin.wcp.util;

import com.raytheon.uf.common.time.DataTime;
import java.util.Calendar;
import java.util.Date;
import java.text.SimpleDateFormat;
import java.text.ParseException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpRecord;
import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpLatlons;
import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpSevrln;

public class WcpParser {
	/**
	 * Constructor
	 */
	public WcpParser () {
	}
	
	public Integer curIndex;
		/*
		 * SEVRRPRT_EXP is the regular expression for the remainder of the
		 * string for the "SEVR" type of report.
		 */
		final String SEVRRPRT_EXP = "(\\d{6}) (\\d{4}) W(T|S)(\\d{4}) (\\d{4})\\x0d\\x0d\\x0a";
		final Pattern sevrrptPattern = Pattern.compile(SEVRRPRT_EXP);
		/*
		 * NOWATCH_EXP is the regular expression for the remainder of the string
		 * for the "NO WATCHES CURRENTLY ACTIVE" report.
		 */
		final String NOWATCH_EXP = "CURRENTLY ACTIVE";
		final Pattern nowatchPattern = Pattern.compile(NOWATCH_EXP);
		String theBulletin = null;
		byte[] messageData = null;

	/*
	 * ProcessWMO: Parse the WMO line and store issue time and any designatorBBB
	 * 
	 * @param wmoline The bulletin message
	 * @param record  WcpRecord
	 */
	public static void processWMO(String wmoline, Calendar createdTime,
			WcpRecord record) {
		/*
		 * Regular expression for WMO header, station ID, issue date, and
		 * possible designator BBB)
		 */
		final String WMO_EXP = "(WWUS60) (KWNS) ([0-9]{6})( ([A-Z]{3}))?";
		final Pattern wmoPattern = Pattern.compile(WMO_EXP);
		Matcher theMatcher = wmoPattern.matcher(wmoline);
		if (theMatcher.find()) {
			/*
			 * Set the BBB designator, if there
			 */
			if (theMatcher.group(5) == null) {
				record.setDesignatorBBB(" ");
			} else {
				record.setDesignatorBBB(theMatcher.group(5));
			}
			/*
			 * Set the issue time
			 */
			Calendar issueTime = transformDate(theMatcher.group(3),
													createdTime);
			record.setIssueTime(issueTime);
			/*
			 * Put the issue time in the second field of the DataURI
			 * using setDataTime.
			 */
			DataTime dataTime=new DataTime(issueTime);
			record.setDataTime(dataTime);
		}
	}

	/*
	 * processSevr: Process the SEVR segment (1) header (start time, end time,
	 * watch number) (2) points
	 */
	public static void processSevr(String segment, WcpRecord record) {
		/*
		 * Regular expression for WCP header line
		 */
		// final String SEGHDR_EXP = "([0-9]{6}) ([0-9]{4}) (WT|WS)([0-9]{4}) "
		// + "([0-9]{4})\\x0d\\x0d\\x0a";
		final String SEGHDR_EXP = "([0-9]{6}) ([0-9]{4}) (WT|WS)([0-9]{4}) "
				+ "([0-9]{4})\\x0d\\x0d\\x0a((\\d{5})\\.(\\d{5})( |\\x0d\\x0d\\x0a|;))+";
		final Pattern segmentPattern = Pattern.compile(SEGHDR_EXP);
		/*
		 * Regular expression used for extracting Lat/Lon
		 */
		final String LATLON_EXP = "((\\d{5})\\.(\\d{5})( |\\x0d\\x0d\\x0a|;))+";
		final Pattern latlonPattern = Pattern.compile(LATLON_EXP);
		Calendar stTime = null;
	    Calendar enTime = null;
		if (record == null)
			return;
		Matcher segMatch = segmentPattern.matcher(segment);
		if (segMatch.find()) {
			WcpSevrln curSevrln = new WcpSevrln();
			/*
			 * Replace white spaces with blank.
			 */
			curSevrln.setSevrLines(segMatch.group().replace('\r', ' ').replace(
					'\003', ' ').replace('\000', ' ').replace('\001', ' '));
			curSevrln.setEventType(segMatch.group(3));
			curSevrln.setWatchNumber(segMatch.group(4));
			stTime = Calendar.getInstance();
			enTime = Calendar.getInstance();
			/*
			 * Obtain the start time and end time
			 */
			processSevrDateTime(segMatch.group(1), segMatch.group(2), segMatch
					.group(5), stTime, enTime);
			/*
			 * Set startTime, endTime, and fileCrTime
			 */
			curSevrln.setStartTime(stTime);
			curSevrln.setEndTime(enTime);

			Matcher latlonMatcher = latlonPattern.matcher(segment);
			/*
			 * Find the lat/lon coordinates
			 */
			if (latlonMatcher.find()) {
				/*
				 * Process the lat/lon values
				 */
				processLatlons(latlonMatcher.group(), segMatch.group(4),
						stTime, curSevrln);
				/*
				 * Add current SEVR WcpSevrLn to the set
				 */
				record.addWcpSevrLn(curSevrln);
			}
		}
	}

	/*
	 * processFileCreatedDate: Process the "FILE CREATED DATE" line returning
	 * the Calendar currentTime
	 */
	public static Calendar processFileCreatedDate(String fileCreatedString,
			WcpRecord record) {
		/*
		 * Regular expression for the File Created date/time
		 */
		final String FCD_EXP = "(FILE CREATED (\\d{2})-([A-V]{3})-"
				+ "(\\d{2}) AT (\\d{2}):(\\d{2}):(\\d{2}))";
		final Pattern filecrPattern = Pattern.compile(FCD_EXP);
		Calendar currentTime = Calendar.getInstance();
		Matcher filecrMatcher = filecrPattern.matcher(fileCreatedString);
		if (filecrMatcher.find()) {
			try {
				SimpleDateFormat month = new SimpleDateFormat("MMM");
				Date date = (Date) month.parse(filecrMatcher.group(3));
				currentTime.setTime(date);
			} catch (ParseException e) {
				System.out.println("Exception :" + e);
			}
			currentTime.set(Calendar.YEAR, Integer.parseInt(filecrMatcher
					.group(4)) + 2000);
			currentTime.set(Calendar.MONTH, currentTime.get(Calendar.MONTH));
			currentTime.set(Calendar.DAY_OF_MONTH, Integer
					.parseInt(filecrMatcher.group(2)));
			currentTime.set(Calendar.HOUR_OF_DAY, Integer
					.parseInt(filecrMatcher.group(5)));
			currentTime.set(Calendar.MINUTE, Integer.parseInt(filecrMatcher
					.group(6)));
			currentTime.set(Calendar.SECOND, 0);
			currentTime.set(Calendar.MILLISECOND, 0);
		}
		return currentTime;
	}

	/*
	 * processSevrDateTime: Process the SEVR date/time line groups and set start
	 * time stTime and end time enTime
	 */
	private static void processSevrDateTime(String yymmdd, String startHhmm,
			String endHhmm, Calendar stTime, Calendar enTime) {
		int year = Integer.parseInt(yymmdd.substring(0, 2)) + 2000;
		int month = Integer.parseInt(yymmdd.substring(2, 4));
		int day = Integer.parseInt(yymmdd.substring(4, 6));
		int stHour = Integer.parseInt(startHhmm.substring(0, 2));
		int stMin = Integer.parseInt(startHhmm.substring(2, 4));
		int enHour = Integer.parseInt(endHhmm.substring(0, 2));
		int enMin = Integer.parseInt(endHhmm.substring(2, 4));
		/*
		 * Adjust for calendar month by subtracting 1
		 */
		month = month - 1;
		/*
		 * Set start time and end time
		 */
		stTime.setTimeInMillis(0);
		stTime.set(year, month, day, stHour, stMin, 0);
		enTime.setTimeInMillis(0);
		enTime.set(year, month, day, enHour, enMin, 0);
		/*
		 * Set to next day for ending time if necessary
		 */
		int istHhmm = stHour * 100 + stMin;
		int ienHhmm = enHour * 100 + enMin;
		if (ienHhmm <= istHhmm) {
			enTime.add(Calendar.DAY_OF_MONTH, 1);
		}
	}

	/*
	 * processLatlons: Process the lat/lons and put in the database set.
	 */
	private static void processLatlons(String latlons, String watchNumber,
			Calendar startTime, WcpSevrln curSevrln ) {
		String latdeg = null;
		String latmin = null;
		String londeg = null;
		String lonmin = null;	
		String[] latnlon = null;
		int latlonIndex = 0;
		int len;
		Float flat, flon;
		/*
		 * Split into lat/lon pairs and store in llpair.
		 */
		String[] llpair = Pattern.compile(" |\\x0d\\x0d\\x0a|;").split(latlons);
		for (int i = 0; i < llpair.length; i++) {
			latlonIndex++;
			/*
			 * Construct new WcpLatlons record to hold lat/lon values
			 */
			WcpLatlons curLatlons = new WcpLatlons();
			curLatlons.setIndex(latlonIndex);
			/*
			 * Split the lat long pair llpair into latitude and longitude. Then
			 * separate the degrees from the minutes and convert to floating
			 * point degrees and add lat, lon, and index to set.
			 */
			latnlon = llpair[i].split("\\.");
			len = latnlon[1].length();
			latdeg = latnlon[0].substring(0, len - 2);
			latmin = latnlon[0].substring(len - 2);
			flat = (float) (Integer.parseInt(latdeg))
					+ (float) (Integer.parseInt(latmin)) / 60.f;
			len = latnlon[1].length();
			londeg = latnlon[1].substring(0, len - 2);
			lonmin = latnlon[1].substring(len - 2);
			flon = -1.0f
					* ((float) (Integer.parseInt(londeg)) + (float) (Integer
							.parseInt(lonmin)) / 60.f);
			curLatlons.setLat(flat);
			curLatlons.setLon(flon);
			/*
			 * Set the number of lat/lon points
			 */
			curSevrln.setNumPnts(latlonIndex);
			/*
			 * add current lat, lon, and index to set.
			 */
			curSevrln.addWcpLatLon(curLatlons);
		}
	}

	/*
	 * transformDate: Transform a date in format "DDHHMM" into a Calendar
	 * object. The year and month are from createdTime. Return Calendar
	 * currentDate.
	 */
	private static Calendar transformDate(String date, Calendar createdTime) {
		Calendar currentDate = Calendar.getInstance();
		int month = createdTime.get(Calendar.MONTH);
		int year = createdTime.get(Calendar.YEAR);
		int day = Integer.parseInt(date.substring(0, 2).trim());
		int hour = Integer.parseInt(date.substring(2, 4).trim());
		int minute = Integer.parseInt(date.substring(4, 6).trim());
		currentDate.set(Calendar.YEAR, year);
		currentDate.set(Calendar.MONTH, month);
		currentDate.set(Calendar.DAY_OF_MONTH, day);
		currentDate.set(Calendar.HOUR_OF_DAY, hour);
		currentDate.set(Calendar.MINUTE, minute);
		currentDate.set(Calendar.SECOND, 0);
		currentDate.set(Calendar.MILLISECOND, 0);
		return currentDate;
	}
}

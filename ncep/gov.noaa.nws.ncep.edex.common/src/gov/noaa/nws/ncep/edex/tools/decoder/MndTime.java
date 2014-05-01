/*
 * 
 * MndTime
 * 
 * This java class processes MND (Mass News Disseminator) block.
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 10/2008		14			T. Lee		Creation
 * 04/2009		14			T. Lee		Used log4j logger
 * 06/2009		128			T. Lee		Added UTC/Zulu; Returned UTC 
 * 										or null for MND time
 * 07/2009		128			T. Lee		Migration to TO11
 * 01/26/2011   N/A         M. Gao      Refactor the logic of parsing MndTime string
 *                                      Now the regular expression is more flexible. 
 *                                      It can tolerate extra spaces at the beginning, ending 
 *                                      or in between of words. It can either take THU abbreviation
 *                                      or Thursday. The similar flexibility applies to MON and MONDAY too.
 *                                      It can also understand both format listed as below: 
 *                                      1018 PM CDT THU APR 1 2010 or
 *                                      1018 PM CDT THURSDAY 1 APR  2010 
 * </pre>
 * 
 * @author T.Lee
 * @version 1.0       
 */

package gov.noaa.nws.ncep.edex.tools.decoder;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public final class MndTime {
    private final Log logger = LogFactory.getLog(getClass());
    
	/** MND time calendar*/
	private Calendar mndTm = null;

	/** MND time string */
	private String mndTmStr = null;

	/**
	 * Constructor the MND time
	 * 
	 * @param messageData
	 */
	public MndTime (byte[] messageData) {
		mndTm = processMndTime (messageData);
	}

	/**
	 * Get MND time
	 * 
	 * @return mndTm
	 */
	public Calendar getMndTime() {
		return mndTm;   	
	}

	/**
	 * Get MND time string
	 * 
	 * @return mndTmStr
	 */
	public String getMndTimeString() {
		return mndTmStr;
	}

	/**
	 * Return MND time as Calendar object.
	 * 
	 * @param tm the Matcher object
	 * @return cal  
	 */

	public Calendar processMndTime (byte[] msg) {
		String s = new String(msg);
		SimpleDateFormat sdf;
		
		/*
		 * MND time (local format)
		 */
//		final String MNDTIME_EXP_LOCAL = "(\\d{3,4}) ([A-Za-z]{2}) ([A-Za-z]{3}) " +
//			"([A-Za-z]{3}) ([A-Za-z]{3}) (\\d{1,2}) (\\d{4})\r\r\n";
//		Pattern pt = Pattern.compile(MNDTIME_EXP_LOCAL);
//		Matcher tm = pt.matcher(s);
//		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
//
//		/*
//		 * Decode local time pattern
//		 */
//		if (tm.find()) {
//			String group1;
//			String group6;
//			mndTmStr = tm.group(0).trim();
//			sdf = new SimpleDateFormat ("HHmm a zzz EEE MMM dd yy");
//			group1 = tm.group(1);
//			if ( tm.group(1).length() == 3 ) {
//				group1 = "0"+tm.group(1);   				
//			}
//			/*
//			 * changes made are: 
//			 * 	1. Handle both cases of "1 APR" and "APR 1" for the string fields
//			 * 	   group(5) and group(6)
//			 */
////			group6 = tm.group(6);
////			if ( tm.group(6).length() == 1 ) {
////				group6 = "0"+tm.group(6);
////			}
////			String mnd = group1+" "+tm.group(2)+" "+tm.group(3)+" "+ tm.group(4)+" "+
////				tm.group(5)+" "+group6+" "+tm.group(7);
//			String [] monthAndDayStringArray = verifyAndRetrieveMonthAndDay(tm.group(5), tm.group(6)); 
//			String mnd = group1+" "+tm.group(2)+" "+tm.group(3)+" "+ tm.group(4)+" "+
//			monthAndDayStringArray[0]+" "+ monthAndDayStringArray[1] +" "+tm.group(7);
//			try {
//				java.util.Date parsedDate = sdf.parse(mnd); 
//				cal.setTime(parsedDate);
//			} catch (ParseException pe) {
//				if ( logger.isInfoEnabled()) {
//				    logger.info ( "Errors in processing MND local time");
//				}
//				if(isTimeZoneInvalid(tm.group(3))) {
//					setDayOfMonthAndMonthAndYearToCalendar(cal, monthAndDayStringArray[1], 
//							monthAndDayStringArray[0], tm.group(7)); 
//				}
//			}
		/*
		 * expression pattern string match something like "800 AM PDT THU APR 1 2010"
		 */
		final String MNDTIME_EXP_LOCAL_1 = "( *\\d{3,4})( *[A-Za-z]{2})( *[A-Za-z]{3})" +
			"( *[A-Za-z]{3,9})( *[A-Za-z]{3,9})( *\\d{1,2})( *\\d{4} *)\r\r\n";
		Pattern pattern1 = Pattern.compile(MNDTIME_EXP_LOCAL_1);
		Matcher matcher1 = pattern1.matcher(s);
		/*
		 * expression pattern string match something like "800 AM PDT THU 1 APR 2010"
		 */
		final String MNDTIME_EXP_LOCAL_2 = "( *\\d{3,4})( *[A-Za-z]{2})( *[A-Za-z]{3})" +
			"( *[A-Za-z]{3,9})( *\\d{1,2})( *[A-Za-z]{3,9})( *\\d{4} *)\r\r\n";
		Pattern pattern2 = Pattern.compile(MNDTIME_EXP_LOCAL_2);
		Matcher matcher2 = pattern2.matcher(s);
		
		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		Matcher matcher = getMatcher(matcher1, matcher2);

		Pattern pt = null;
		Matcher tm = null;
		/*
		 * Decode local time pattern
		 */
		if (matcher != null) {
			mndTmStr = matcher.group(0).trim();
			sdf = new SimpleDateFormat ("HHmm a zzz EEE MMM dd yy");
			String group1;
			group1 = matcher.group(1).trim();
			if ( group1.length() == 3 ) {
				group1 = "0"+matcher.group(1);   				
			}
			/*
			 * changes made are: 
			 * 	1. Handle both cases of "1 APR" and "APR 1" for the string fields
			 * 	   group(5) and group(6)
			 */
			String group2 = matcher.group(2).trim(); 
			String group3 = matcher.group(3).trim(); 
			String group4 = matcher.group(4).trim(); 
			String group5 = matcher.group(5).trim(); 
			String group6 = matcher.group(6).trim(); 
			String group7 = matcher.group(7).trim(); 
			String [] monthAndDayStringArray = verifyAndRetrieveMonthAndDay(group5, group6); 
			String mnd = group1+" "+ group2 + " "+ group3 +" "+ group4 + " " +
			      monthAndDayStringArray[0] + " "+ monthAndDayStringArray[1] +" "+ group7;
			try {
				java.util.Date parsedDate = sdf.parse(mnd); 
				cal.setTime(parsedDate);
			} catch (ParseException pe) {
				if ( logger.isInfoEnabled()) {
					logger.info ( "Errors in processing MND local time");
				}
				if(isTimeZoneInvalid(group3)) {
					setDayOfMonthAndMonthAndYearToCalendar(cal, monthAndDayStringArray[1], 
							monthAndDayStringArray[0], group7); 
				}
			}

		} else {

			/*
			 * UTC format, e.g., 1500 UTC THU MAY 28 2009
			 */
			final String MNDTIME_EXP_UTC = "(\\d{3,4}) UTC ([A-Za-z]{3}) ([A-Za-z]{3}) " +
					"(\\d{1,2}) (\\d{4})\r\r\n";

			pt = Pattern.compile(MNDTIME_EXP_UTC);
			tm = pt.matcher(s);

			/*
			 * decode UTC pattern
			 */
			if (tm.find()) {
				String group1, group4;
				sdf = new SimpleDateFormat ("HHmm zzz EEE MMM dd yyyy");
				mndTmStr = tm.group(0).trim();
				try {
					group1 = tm.group(1);
					if ( tm.group(1).length() == 3 ) {
						group1 = "0"+tm.group(1);   				
					}
					group4 = tm.group(4);
					if ( tm.group(4).length() == 1 ) {
						group4 = "0"+tm.group(4);
					}
					String mnd = group1+" UTC "+tm.group(2)+" "+tm.group(3)+" "+ group4+
						" "+tm.group(5);
					cal.setTime(sdf.parse(mnd));
					cal.set(Calendar.SECOND, 0);
					cal.set(Calendar.MILLISECOND, 0);
				} catch (Exception e) {
					if ( logger.isInfoEnabled()) {
					    logger.info ( "Errors in processing MND UTC time");
					}
				}

			} else {

				/*
				 * Check Zulu pattern
				 */
				String MNDTIME_EXP_ZULU = "(\\d{5,6})(z|Z) ([A-Za-z]{3}) (\\d{2})//(\r\r\n|\r\n)";
				pt = Pattern.compile(MNDTIME_EXP_ZULU);
				tm = pt.matcher(s);

				/*
				 * Check UTC pattern
				 */
				if (tm.find()) {
					String group1;
					sdf = new SimpleDateFormat ("ddHHmm zzz MMM yy");
					mndTmStr = tm.group(0).trim();
					try {
						group1 = tm.group(1);
						if ( tm.group(1).length() == 5 ) {
							group1 = "0"+tm.group(1);   				
						}
						String time = group1+" UTC "+tm.group(3)+" "+tm.group(4);
						cal.setTime(sdf.parse(time));
						cal.set(Calendar.SECOND, 0);
						cal.set(Calendar.MILLISECOND, 0);
					} catch (Exception e) {
						if ( logger.isInfoEnabled()) {
						    logger.info ( "Errors in processing MND zulu time");
						}
					}
				} else {

					/*
					 * Check Zulu pattern
					 */
					MNDTIME_EXP_ZULU = "(\\d{5,6})(z|Z)([A-Za-z]{3})(\\d{4})//(\n|\r\n)";
					pt = Pattern.compile(MNDTIME_EXP_ZULU);
					tm = pt.matcher(s);

					/*
					 * Check UTC pattern
					 */
					if (tm.find()) {
						String group1;
						sdf = new SimpleDateFormat ("ddHHmm zzz MMM yyyy");
						mndTmStr = tm.group(0).trim();
						try {
							group1 = tm.group(1);
							if ( tm.group(1).length() == 5 ) {
								group1 = "0"+tm.group(1);   				
							}
							String time = group1+" UTC "+tm.group(3)+" "+tm.group(4);
							cal.setTime(sdf.parse(time));
							cal.set(Calendar.SECOND, 0);
							cal.set(Calendar.MILLISECOND, 0);
						} catch (Exception e) {
							if ( logger.isInfoEnabled()) {
							    logger.info ( "Errors in processing MND zulu time");
							}
						}
					} else {

						/*
						 * return null if no MND time
						 */
						cal = null;
					}
				}
			}
		}
		return cal;
	}
	
	private String stripExtraSpace(String str) {
		if(isStringEmpty(str))
			return ""; 
		StringBuilder builder = new StringBuilder(str.length());
		String [] strArray = str.split(" "); 
		displayStringArray(strArray); 
		/*
		 * find the first string that can be parsed as an integer 
		 * and discard any strings in front of it
		 */
		int firstIntegerStringIndex = findFirstIntegerStringIndex(strArray); 
		for(int i=firstIntegerStringIndex; i< strArray.length; i++) {
			builder.append(strArray[i].trim())
				   .append(" "); 
		}
		return builder.toString().trim(); 
	}

	private void displayStringArray(String[] strArray) {
		if(strArray == null)
			System.out.println("=====, the input strArray is NULL and thus can not be displayed!!!!"); 
		else {
			System.out.println("=======, the total number of String array is = " + strArray.length);
			int arrayIndex = 1; 
			for(String eachString : strArray) {
				System.out.println(" Array Intem No." + arrayIndex + ":= "+eachString + " with LENGTH ="+eachString.length()); 
				arrayIndex++; 
			}
		}
	}
	
	/**
	 * 
	 * @param timezoneString
	 * @return
	 */
	private String replaceInvalidTimezoneValue(String timezoneString) {
		String timezoneValueReturned = timezoneString; 
		if(timezoneValueReturned != null && timezoneValueReturned.trim().length() > 0) {
			if(timezoneValueReturned.equalsIgnoreCase("PLT"))
				timezoneValueReturned = "PDT"; 
		}
		return timezoneValueReturned; 
	}
	
	/**
	 * 
	 * @param strArray
	 * @return
	 */
	private int findFirstIntegerStringIndex(String[] strArray) {
		int firstIntegerStringIndex = 0; 
		if(strArray != null) {
			for(String eachString : strArray) {
				if(isIntegerString(eachString))
					break; 
				firstIntegerStringIndex++; 
			}
		}
		return firstIntegerStringIndex; 
	}
	
	private boolean isIntegerString(String str) {
		boolean isIntegerString = false; 
		try {
			Integer.parseInt(str);
			isIntegerString = true; 
		} catch(NumberFormatException nfe) {
			//do nothing
		}
		return isIntegerString; 
	}
	
	/**
	 * a helper method to check is a string is empty
	 * @param str
	 * @return
	 */
	private boolean isStringEmpty(String str) {
		boolean isEmpty = false; 
		if(str == null || str.trim().length() == 0)
			isEmpty = true; 
		return isEmpty; 
	}
	/**
	 * 
	 * @param matcher1
	 * @param matcher2
	 * @return
	 */
	private Matcher getMatcher(Matcher matcher1, Matcher matcher2) {
		Matcher matcher = null; 
		if(matcher1 != null && matcher1.find())
			matcher = matcher1; 
		else if(matcher2 != null && matcher2.find())
			matcher = matcher2; 
		return matcher; 
	}
	
	/**
	 * 
	 * @param timeZoneString
	 * @return
	 */
	private boolean isTimeZoneInvalid(String timeZoneString) {
		boolean isInvalid = false; 
		if(isStringEqual("PLT", timeZoneString))
			isInvalid = true; 
		return isInvalid; 
	}
	
	/**
	 * A helper method to parse and monthString and yearString directly
	 * and then set to the calendar object
	 * @param calendar
	 * @param monthString
	 * @param yearString
	 */
	private void setDayOfMonthAndMonthAndYearToCalendar(Calendar calendar, String dayOfMonthString, 
			String monthString, String yearString) {
		if(calendar == null)
			return; 
		/*
		 * set day of month 
		 */
		int dayOfMonthInt = getDayOfMonthInt(dayOfMonthString); 
		if(isDayOfMonthIntValid(dayOfMonthInt))
			calendar.set(Calendar.DAY_OF_MONTH, dayOfMonthInt); 
		
		/*
		 * set month 
		 */
		int monthId = getMonthId(monthString); 
		if(isMonthIdValid(monthId))
			calendar.set(Calendar.MONTH, monthId); 
		
		/*
		 * Now set year value
		 */
		int yearInt = getYearInt(yearString); 
		if(isYearIntValid(yearInt))
			calendar.set(Calendar.YEAR, yearInt); 
	}
	
	private int getMonthId(String monthString) {
		int monthId = -1; 
		if(isStringEqual("JAN", monthString) || isStringEqual("JANUARY", monthString))
			monthId = Calendar.JANUARY; 
		else if(isStringEqual("FEB", monthString) || isStringEqual("FEBRUARY", monthString))
			monthId = Calendar.FEBRUARY; 
		else if(isStringEqual("MAR", monthString) || isStringEqual("MARCH", monthString))
			monthId = Calendar.MARCH; 
		else if(isStringEqual("APR", monthString) || isStringEqual("APRIL", monthString))
			monthId = Calendar.APRIL; 
		else if(isStringEqual("MAY", monthString))
			monthId = Calendar.MAY; 
		else if(isStringEqual("JUN", monthString) || isStringEqual("JUNE", monthString))
			monthId = Calendar.JUNE; 
		else if(isStringEqual("JUL", monthString) || isStringEqual("JULY", monthString))
			monthId = Calendar.JULY; 
		else if(isStringEqual("AUG", monthString) || isStringEqual("AUGUST", monthString))
			monthId = Calendar.AUGUST; 
		else if(isStringEqual("SEP", monthString) || isStringEqual("SEPTEMBER", monthString))
			monthId = Calendar.SEPTEMBER; 
		else if(isStringEqual("OCT", monthString) || isStringEqual("OCTOBER", monthString))
			monthId = Calendar.OCTOBER; 
		else if(isStringEqual("NOV", monthString) || isStringEqual("NOVEMBER", monthString))
			monthId = Calendar.NOVEMBER; 
		else if(isStringEqual("DEC", monthString) || isStringEqual("DECEMBER", monthString))
			monthId = Calendar.DECEMBER; 
		return monthId; 
	}
	
	private boolean isStringEqual(String str1, String str2) {
		boolean isEqual = false; 
		if(str1.equalsIgnoreCase(str2))
			isEqual = true; 
		return isEqual; 
	}
	
	/**
	 * 
	 * @param yearString
	 * @return
	 */
	private int getYearInt(String yearString) {
		int yearInt = -1; 
		if(yearString != null) {
			try {
				yearInt = Integer.parseInt(yearString); 
			} catch(NumberFormatException nfe) {
				//do nothing
			}
		}
		return yearInt; 
	}
	
	private int getDayOfMonthInt(String dayOfMonthString) {
		int dayOfMonthInt = -1; 
		if(dayOfMonthString != null) {
			try {
				dayOfMonthInt = Integer.parseInt(dayOfMonthString); 
			} catch(NumberFormatException nfe) {
				//do nothing
			}
		}
		return dayOfMonthInt; 
	}

	
	/**
	 * 
	 * @param monthId
	 * @return
	 */
	private boolean isMonthIdValid(int monthId) {
		boolean isValid = true; 
		if(isIntegerNegative(monthId))
			isValid = false; 
		return isValid; 
	}

	/**
	 * 
	 * @param yearInt
	 * @return
	 */
	private boolean isYearIntValid(int yearInt) {
		boolean isValid = true; 
		if(isIntegerNegative(yearInt))
			isValid = false; 
		return isValid; 
	}

	private boolean isDayOfMonthIntValid(int dayOfMonthInt) {
		boolean isValid = true; 
		if(isIntegerNegative(dayOfMonthInt))
			isValid = false; 
		return isValid; 
	}

	/**
	 * a method to return true if the input is a negative number
	 * @param intValue
	 * @return
	 */
	private boolean isIntegerNegative(int intValue) {
		return intValue < 0 ?  true : false; 
	}
	
	/*
	 * A helper method to retrieve month and day values
	 * Case No.1:  12 APR
	 * Case No.2: APR 12
	 */
	private String[] verifyAndRetrieveMonthAndDay(String monthAndDayValue1, String monthAndDayValue2) {
		String [] monthAndDayStringArray = new String[2]; 
		if(canStringBeParsedAsInteger(monthAndDayValue2)) {
			monthAndDayStringArray[0] = monthAndDayValue1; 
			monthAndDayStringArray[1] = monthAndDayValue2; 
		} else if(canStringBeParsedAsInteger(monthAndDayValue1)) {
			monthAndDayStringArray[0] = monthAndDayValue2; 
			monthAndDayStringArray[1] = monthAndDayValue1; 
		}
		if(monthAndDayStringArray[1] != null && monthAndDayStringArray[1].length()==1)
			monthAndDayStringArray[1] = "0" + monthAndDayStringArray[1]; 
			
		checkStringArray(monthAndDayStringArray); 
		return monthAndDayStringArray; 
	}
	
	/*
	 * A helper method to check each element of the array
	 * to make sure there is no any null value exists in the array
	 * Assign an empty string value to any null element of the array
	 */
	private void checkStringArray(String[] stringArray) {
		for(int i=0; i<stringArray.length; i++) {
			if(stringArray[i] == null)
				stringArray[i] = ""; 
		}
	}
	
	/*
	 * A helper method to check if the string parameter 
	 * can be parsed into an integer
	 */
	private boolean canStringBeParsedAsInteger(String str) {
		boolean checkResult = false; 
		try {
			Integer.parseInt(str); 
			checkResult = true; 
		} catch(NumberFormatException nfe) {
			//do nothing
		}
		return checkResult; 
	}
}
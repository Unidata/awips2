/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.plugin.shef.util;

import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFErrorCodes;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * SHEF Utility class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/19/08     387         M. Duff     Initial creation.
 * 12/12/08     1786        J. Sanchez  Handled date parsing exceptions.  
 * 01/12/08     1846        J. Sanchez  Update parseCreationDate to use default year.
 * May 14, 2014 2536        bclement    moved WMO Header to common, removed unused log
 * 
 * </pre>
 */
public final class ShefUtil {

	/**
	 * Parses the date/data string and sets the time using the record date and
	 * time zone as a base time value.
	 * 
	 * @param time - time string
	 * @param timeZoneCode - the time zone code
	 * @param recordDate - SHEF Record date
	 * 
	 * @return Date object corresponding to the date/data string
	 * @throws ParseException 
	 */
	public static synchronized Date parseObsTime(String time, String timeZoneCode, Date recordDate) throws ParseException {
	    
	    String[] parts = null;
		String ds = "00";  // seconds
		String dn = "00";  // minutes
		String dh = null;  // hour
		String dd = null;  // day
		String dm = null;  // month
		String dy = null;  // year
		String dj = null;  // Julian day
		String dt = null;  // century
		String timeString = null;
		Calendar recordTime = Calendar.getInstance();
		recordTime.setTime(recordDate);

		
		if (time.indexOf("/") > -1) {
			/* Split the string apart and set the individual values */
			parts = time.split("/");
			for (int i = 0; i < parts.length; i++) {
				if (parts[i].startsWith(ShefConstants.DD)) {
					dd = parts[i].substring(2);
				} else if (parts[i].startsWith(ShefConstants.DH)) {
					dh = parts[i].substring(2);
				} else if (parts[i].startsWith(ShefConstants.DJ)) {
					dj = parts[i].substring(2);
				} else if (parts[i].startsWith(ShefConstants.DM)) {
					dm = parts[i].substring(2);
				} else if (parts[i].startsWith(ShefConstants.DN)) {
					dn = parts[i].substring(2);
				} else if (parts[i].startsWith(ShefConstants.DS)) {
					ds = parts[i].substring(2);
				} else if (parts[i].startsWith(ShefConstants.DY)) {
					dy = parts[i].substring(2);
				} else if (parts[i].startsWith(ShefConstants.DT)) {
					dt = parts[i].substring(2);
				}
			}
			
			/*
			 * Put the pieces together to get a full time stamp
			 * Start with the century and work to the right
			 */
			StringBuffer sb = new StringBuffer();
			
			if (dt != null && dy != null) {
			    sb.append(dt + dy);
            }
			else if (dy != null) {
			    dy = String.valueOf(recordTime.get(Calendar.YEAR)).substring(0, 2);
				sb.append(dt + dy);
			}
			else if (dy == null) {
				int year = recordTime.get(Calendar.YEAR);
				sb.append(year);
			}
			
			if (dj != null) {
				Calendar c = Calendar.getInstance();
				c.setTime(recordDate);
				String day = String.valueOf(c.get(Calendar.DAY_OF_MONTH));
				String month = String.valueOf(c.get(Calendar.MONTH) + 1);
				month = pad(month);
				day = pad(day);
				
				c.set(Calendar.DAY_OF_YEAR, Integer.parseInt(dj));
				day = String.valueOf(c.get(Calendar.DAY_OF_MONTH));
				day = pad(day);
				month = String.valueOf(c.get(Calendar.MONTH) + 1);
				month = pad(month);
				sb = sb.delete(sb.length() - 2, sb.length());
				sb.append(month + day);
				dd = String.valueOf(day);
			}
			else{
			    if (dm != null) {
	                sb.append(dm);
	            } else {
	                dm = String.valueOf(recordTime.get(Calendar.MONTH) + 1);
	                dm = pad(dm);
	                sb.append(dm);
	            }
	            if (dd != null) {
	                sb.append(dd);
	            }
	            else{
	                dm = String.valueOf(recordTime.get(Calendar.DAY_OF_MONTH));
                    dm = pad(dm);
                    sb.append(dm);
	            }
			}
			
			if (dh == null) {
                
                /*
                 * the default value is 24 for local time 
                 * and 12 for GMT/Z time
                 */
                if (timeZoneCode.equalsIgnoreCase(ShefConstants.GMT) ||
                        timeZoneCode.equalsIgnoreCase(ShefConstants.Z)) {
                    dh = "12";
                } else {
                    dh = "24";
                }

				sb.append(dh);
			}
			
			if (dh.length() == 4) {
				sb.append(ds);	
			} else if (dh.length() == 2) {
				sb.append(dn + ds);
			}
			
			timeString = sb.toString();
		} else {
			StringBuffer sb = new StringBuffer();
			String year = String.valueOf(recordTime.get(Calendar.YEAR));
			String month = String.valueOf(recordTime.get(Calendar.MONTH) + 1);
			String day = String.valueOf(recordTime.get(Calendar.DAY_OF_MONTH));
			day = pad(day);
			month = pad(month);
			String hour = "24";
            if (timeZoneCode.equalsIgnoreCase(ShefConstants.GMT) || 
                    timeZoneCode.equalsIgnoreCase(ShefConstants.Z)) {
                    hour = "12";
                }
			
			if(time.startsWith(ShefConstants.DC)){		
			    time = time.substring(2);
			    sb.append(year + time + dn + ds);
			}
			else if (time.startsWith(ShefConstants.DD)) {
				time = time.substring(2);
				
				int length = time.length();
				switch (length) {
				case 2:
					sb.append(year + month + time + hour + dn + ds);
					break;
				case 4:
					sb.append(year + month + time + dn + ds);
					break;
				case 6:
					sb.append(year + month + time + ds);
					break;
				case 8:
					sb.append(year + month + time);
					break;
				}
			} else if (time.startsWith(ShefConstants.DH)) {
				time = time.substring(2);
				int length = time.length();
				switch (length) {
					case 2:
						sb.append(year + month + day + time + dn + ds);
						break;
					case 4:
						sb.append(year + month + day + time + ds);
						break;
					case 6:
						sb.append(year + month + day + time);
						break;
				}
			} else if (time.startsWith(ShefConstants.DM)) {
				time = time.substring(2);
				
				int length = time.length();
				switch (length) {
					case 2:
						sb.append(year + time + day + hour + dn + ds);
						break;
					case 4:
						sb.append(year + time + hour + dn + ds);
						break;
					case 6:
						sb.append(year + time + dn + ds);
						break;
					case 8:
						sb.append(year + time + ds);
						break;
					case 10:
						sb.append(year + time);
						break;
				}
			} else if (time.startsWith(ShefConstants.DN)) {
				time = time.substring(2);
				
				if (time.length() == 2) {
					sb.append(year + month + day + hour + time + ds);
				} else {
					sb.append(year + month + day + hour + time);
				}
			} else if (time.startsWith(ShefConstants.DS)) {
				time = time.substring(2);
				
				sb.append(year + month + day + hour + dn + time);
			} else if (time.startsWith(ShefConstants.DY)) {
				/* need to get the equivalent DT */
				String yr = String.valueOf(recordTime.get(Calendar.YEAR));
				time = time.substring(2);
				time = yr.substring(0, 2) + time;

				int length = time.length();
				switch (length) {
					case 4:
						sb.append(time + month + day + hour + dn + ds);
						break;
					case 6:
						sb.append(time + day + hour + dn + ds);
						break;
					case 8:
						sb.append(time + hour + dn + ds);
						break;
					case 10:
						sb.append(time + dn + ds);
						break;
					case 12:
						sb.append(time + ds);
						break;
				}
			} else if (time.startsWith(ShefConstants.DT)) {
				time = time.substring(2);

				
				int length = time.length();
				switch (length) {
					case 2:
						sb.append(time + year + month + day + hour + dn + ds);
						break;
					case 4:
						sb.append(time + month + day + hour + dn + ds);
						break;
					case 6:
						sb.append(time + day + hour + dn + ds);
						break;
					case 8:
						sb.append(time + hour + dn + ds);
						break;
					case 10:
						sb.append(time + dn + ds);
						break;
				}
			} else {
				/*
				 * No date/data override present, need to use the 
				 * Date/Time on the record itself
				 */

				
				int length = time.length();
				switch (length) {
					case 4:
						sb.append(year + time + hour + dn + ds);
						break;
					case 6:
						sb.append(year + time + dn + ds);
						break;
					case 8:
						sb.append(year + time + ds);
						break;
					case 14:
						sb.append(time);
						break;
				}
			}
			timeString = sb.toString();
		}
		
		Date returnDate = null;
		
		synchronized(ShefConstants.YYYYMMDDHHMMSS_FORMAT){
		    returnDate = ShefConstants.YYYYMMDDHHMMSS_FORMAT.parse(timeString);
		}
		return returnDate;
	}
	
	/**
	 * Parses the Creation Date value
	 * 
	 * @param creationDate - the string to parse
	 * 
	 * @return - Date object corresponding to the creation date string
	 * 
	 * @throws ParseException
	 */
	public static synchronized Date parseCreationDate(String creationDate) throws ParseException {
		SimpleDateFormat format = null;
		int length = creationDate.length();
		String hour = "00";
		String min = "00";
		String year = null;


		//Adds a default year
		switch (length) {
            case 4:  // MMDD
                synchronized(ShefConstants.MMDD_FORMAT){
                    format = ShefConstants.MMDD_FORMAT;
                }
                year = getYear(format.parse(creationDate));
                creationDate = year + creationDate + hour + min;
                length = creationDate.length();
                break;
            case 6: //MMDDHH
                synchronized(ShefConstants.MMDDHH_FORMAT){
                    format = ShefConstants.MMDDHH_FORMAT;
                }
                year = getYear(format.parse(creationDate));
                creationDate = year + creationDate + min;
                length = creationDate.length();
                break;
            case 8: //MMDDHHMM
                synchronized(ShefConstants.MMDDHHMM_FORMAT){
                    format = ShefConstants.MMDDHHMM_FORMAT;
                }
                year = getYear(format.parse(creationDate));
                creationDate = year + creationDate;
                length = creationDate.length();
                break;
        }

		switch (length) {
			case 10:
			    synchronized(ShefConstants.YYMMDDHHMM_FORMAT){
			        format = ShefConstants.YYMMDDHHMM_FORMAT;
			    }
				break;
			case 12:  
			    synchronized(ShefConstants.YYYYMMDDHHMM_FORMAT){
			        format = ShefConstants.YYYYMMDDHHMM_FORMAT;
			    }
				break;
			default:
				return null;
		}
		return format.parse(creationDate);
	}
	
	/**
	 * Determines the year when “yy” is not explicitly coded, 
	 * a 12-month window is used to assign the year that causes 
	 * the date code to be nearest the current system date at 
	 * the time of decoding.
	 * 
	 * @param creationDate Date creation date 
	 * @return int the year the creationDate is nearest to
	 */
	private static String getYear(Date creationDate){
	    Calendar cal = Calendar.getInstance();
	    Date currentDate = null;
	    int year = 0;
	    
	    cal.setTimeInMillis(System.currentTimeMillis());
	    year = cal.get(Calendar.YEAR); //retrieves current year
	    currentDate = cal.getTime();
	    
	    cal.setTime(creationDate);
	    cal.set(Calendar.YEAR, year);	    

	    if(cal.after(currentDate)){
	        --year; //uses the previous year
	    }

	    return String.valueOf(year);
	}
	
	/**
	 * Returns a Date object of the time offset.
	 *
	 * @param obsTimeDate - the time of the observation
	 * @param relativeValue - the relative time string
	 * @param record 
	 * @return - Date object of correct time
	 */
	public static synchronized Date getRelativeTimeValue(Date obsTimeDate, String relativeValue, ShefRecord record) {
		/* Set the relative time */
		Calendar obsTimeCal = Calendar.getInstance();
		obsTimeCal.setTime(obsTimeDate);
		
		char[] timeArray = relativeValue.toCharArray();
		boolean positive = true;
		
		if (timeArray[3] == '-') {
			positive = false;
		}
		String relativeAmount = null;
		if (relativeValue.equals("DRE")) {
			relativeAmount = relativeValue;
		} else {
			relativeAmount = relativeValue.substring(4);
		}
		char indicator = timeArray[2];
		switch (indicator) {
			case 'S':
				if (positive) {
					obsTimeCal.add(Calendar.SECOND, new Integer(relativeAmount));
				} else {
					obsTimeCal.add(Calendar.SECOND, (new Integer(relativeAmount)) * -1);
				}
				break;
			case 'N':
				if (positive) {
					obsTimeCal.add(Calendar.MINUTE, new Integer(relativeAmount));
				} else {
					obsTimeCal.add(Calendar.MINUTE, (new Integer(relativeAmount)) * -1);
				}
				break;
			case 'H':
				if (positive) {
					obsTimeCal.add(Calendar.HOUR, new Integer(relativeAmount));
				} else {
					obsTimeCal.add(Calendar.HOUR, (new Integer(relativeAmount)) * -1);
				}
				break;
			case 'D':
				if (positive) {
					obsTimeCal.add(Calendar.DAY_OF_MONTH, new Integer(relativeAmount));
				} else {
					obsTimeCal.add(Calendar.DAY_OF_MONTH, (new Integer(relativeAmount)) * -1);
				}
				break;
			case 'M':
				if (positive) {
					obsTimeCal.add(Calendar.MONTH, new Integer(relativeAmount));
				} else {
					obsTimeCal.add(Calendar.MONTH, (new Integer(relativeAmount)) * -1);
				}
				break;
			case 'E':
				int lastDayMonth = obsTimeCal.getActualMaximum(Calendar.DAY_OF_MONTH);
				obsTimeCal.set(Calendar.DAY_OF_MONTH, lastDayMonth);
				break;
			case 'Y':
				if (positive) {
					obsTimeCal.add(Calendar.YEAR, new Integer(relativeAmount));
				} else {
					obsTimeCal.add(Calendar.YEAR, (new Integer(relativeAmount)) * -1);
				}
				break;
		}
		if (record.getTimeZoneObject() != null) {
			obsTimeCal.setTimeZone(record.getTimeZoneObject());
		} else {
			obsTimeCal.setTimeZone(ShefConstants.GMT_ZONE);
		}
		return obsTimeCal.getTime();
	}

	/**
	 * Returns the full 4 digit year.  Underlying logic reused from
	 * original SHEF decode software sh4dt0.f
	 * 
	 * @param dateString 
	 * 			date as a string value "mmdd" or "yymmdd" or “ccyymmdd”
	 * @return int - 4 digit year
	 * @throws ParseException
	 */
	public static synchronized int getFullYear(String dateString) throws ParseException {
		Date date = null;
		Calendar calendar = new GregorianCalendar();
		int returnVal = -999;
		
		/* Current date/time */
		Calendar now = new GregorianCalendar();	
		int currentYear = now.get(Calendar.YEAR);
		int currentMonth = now.get(Calendar.MONTH);
		int currentDay = now.get(Calendar.DAY_OF_MONTH);

		if (dateString.length() == 4) {
			/* Use a 6 month window to find the correct year */
			
			/* Passed in date */
		    synchronized(ShefConstants.MMDD_FORMAT){
		        date = ShefConstants.MMDD_FORMAT.parse(dateString);
		    }
			calendar.setTime(date);
			int pMonth = calendar.get(Calendar.MONTH);
			int pDay = calendar.get(Calendar.DAY_OF_MONTH);
			
			int fullYear = currentYear;
			
			int diff = pMonth - currentMonth;
			if (diff > 6) {
				fullYear--;
			} else if (diff < -6) {
				fullYear++;
			} else if (diff == -6 && pDay < currentDay) {
				fullYear++;
			} else if (diff == 6 && pDay > currentDay ) {
				fullYear--;
			}
			returnVal = fullYear;
		} else if (dateString.length() == 6) {
			/* 
			 * 10 years in the future and 90 years in the past 
			 * to find the correct century.  Jave date classes 
			 * uses a 20/80 year rule so using the SHEF logic 
			 * from sh4dt0.f. 
			 */
			int pYear = Integer.parseInt(dateString.substring(0, 2));
			int pMonth = Integer.parseInt(dateString.substring(2,4));
			int pDay = Integer.parseInt(dateString.substring(4));

			int century = currentYear/100;

			if (currentYear >= 2000) {
				currentYear = currentYear - 2000;
			} else {
				currentYear = currentYear - 1900;
			}
			
			int diff = pYear - currentYear;
			if (diff > 10) {
				century--;
			} else if (diff < -90) {
				century++;
			} else if (diff == -90 && pMonth < currentMonth) {
				century++;
			} else if (diff == 10 && pMonth > currentMonth) {
				century--;
			} else if (diff == -90 && pMonth == currentMonth && pDay < currentDay) {
				century++;
			} else if (diff == -90 && pMonth == currentMonth && pDay > currentDay) {
				century--;
			}
			returnVal = century * 100 + pYear;
		} else {  // recordDate length must be 8
			date = ShefConstants.YYYYMMDD_FORMAT.parse(dateString);	
			calendar.setTime(date);
			returnVal = calendar.get(Calendar.YEAR);
		}
		return returnVal;
	}
	
	/**
	 * Creates an identifier
	 * @param wmoHeader - The WMO Header object
	 * @return - the identifier
	 */
	public static synchronized String createIdentifier(WMOHeader wmoHeader) {
		String retVal = null;
		StringBuffer sb = new StringBuffer();
		sb.append(wmoHeader.getWmoHeader() + "::::");
		sb.append(wmoHeader.getA1() + "::::");
		sb.append(wmoHeader.getA2() + "::::");
		sb.append(wmoHeader.getIi() + "::::");
		sb.append(wmoHeader.getT1() + "::::");
		sb.append(wmoHeader.getT2() + "::::");
		sb.append(wmoHeader.getBBBIndicator() + "::::");
		sb.append(wmoHeader.getCccc() + "::::");
		sb.append(wmoHeader.getYYGGgg());
		retVal = sb.toString();
		return retVal;
	}
	
	/**
	 * Pad a 0 to the front of a 1 digit String.
	 *   Turns month 2 into 02
	 *   
	 * @param value - the value to pad
	 * @return - the padded value
	 */
	private static synchronized String pad(String value) {
		if (value.length() < 2) {
			value = "0" + value;
		}
		return value;
	}
	
	/**
	 * Checks that a value is with a given range.
	 * @param lower Lower bounds value.
	 * @param value The value to check.
	 * @param upper Upper bounds value.
	 * @return Is the value between the lower and upper bounds.
	 */
	public static boolean between(int lower, int value, int upper) {
	    return ((lower < value) && (value < upper));
	}
	
    public static final int validatePEDTSEP(String pedtsep) {
        // Set to non-error code for switch statement flowdown.
        int err = -1;

        // 01 2 34 5 6
        // pe d ts e p
        // case 7-2 fall though to the next level. A value of -1 in err
        // indicates that we are starting at that case.
        if (pedtsep != null) {
            if(pedtsep.length() == 4) {
                pedtsep += "Z";
            }
            
            switch (pedtsep.length()) {
            case 7: {
                if (ShefParm.getProbabilityCodeValue(pedtsep.substring(6, 7)) == null) {
                    // "Non-existent value for given probability parameter code",
                    err = SHEFErrorCodes.LOG_063;
                } else {
                    err = 0;
                }
            }
            case 6: {
                if (err <= 0) {
                    if (ShefParm.getExtremumCode(pedtsep.substring(5, 6)) == null) {
                        // "Non-existent value for given extremum parameter code"
                        err = SHEFErrorCodes.LOG_061;
                    } else {
                        err = 0;
                    }
                }
            }
            case 5: // Type Source
                if (err <= 0) {
                    if (ShefParm.getTypeSourceCode(pedtsep.substring(3, 5)) == null) {
                        // "Non-existent value for given type and source parameter code"
                        err = SHEFErrorCodes.LOG_034;
                    } else {
                        err = 0;
                    }
                }
            case 3: {
                // Duration
                if (err <= 0) {
                    if (ShefParm.getDurationCodeValue(pedtsep.substring(2, 3)) == null) {
                        // "Non-existent value for given duration parameter code"
                        err = SHEFErrorCodes.LOG_060;
                    } else {
                        err = 0;
                    }
                }
            }
            case 2: {
                if (err <= 0) {
                    if (ShefParm.getPhysicalElementConversionFactor(pedtsep.substring(0, 2)) == null) {
                        // "An expected parameter code is missing"
                        err = SHEFErrorCodes.LOG_003;
                    } else {
                        err = 0;
                    }
                }
                break;
            }
            case 1: {
                // This is an error
                // "Parameter code too short or field misinterpreted as parameter code",
                err = 64;
                break;
            }
            default: {
                // "Parameter code too short or field misinterpreted as parameter code",
                err = SHEFErrorCodes.LOG_064;
            }
            }
        } else {
            err = SHEFErrorCodes.LOG_003;
        }
        return err;
    }    

    /**
     * 
     * @param data
     * @param defValue
     * @return
     */
    public static double getDouble(Object data, double defValue) {
        double retValue = defValue;
        if(data != null) {
            try {
                retValue = ((Number) data).doubleValue();
            } catch(ClassCastException cce) {
                retValue = defValue;
            }
        }
        return retValue;
    }

    /**
     * 
     * @param data
     * @param defValue
     * @return
     */
    public static float getFloat(Object data, float defValue) {
        float retValue = defValue;
        if(data != null) {
            try {
                retValue = ((Number) data).floatValue();
            } catch(ClassCastException cce) {
                retValue = defValue;
            }
        }
        return retValue;
    }
    
    /**
     * 
     * @param data
     * @param defValue
     * @return
     */
    public static short getShort(Object data, short defValue) {
        short retValue = defValue;
        if(data != null) {
            try {
                retValue = ((Number) data).shortValue();
            } catch(ClassCastException cce) {
                retValue = defValue;
            }
        }
        return retValue;
    }
    
    /**
     * 
     * @param data
     * @param defValue
     * @return
     */
    public static int getInt(Object data, int defValue) {
        int retValue = defValue;
        if(data != null) {
            try {
                retValue = ((Number) data).intValue();
            } catch(ClassCastException cce) {
                retValue = defValue;
            }
        }
        return retValue;
    }
    
    /**
     * 
     * @param data
     * @param defValue
     * @return
     */
    public static String getString(Object data, String defValue) {
        String retValue = defValue;
        if(data != null) {
            try {
                retValue = (String) data;
            } catch(ClassCastException cce) {
                retValue = defValue;
            }
        }
        return retValue;
    }
   
    /**
     * 
     * @param data
     * @param defValue
     * @return
     */
    public static Date getDate(Object data, Date defValue) {
        Date retValue = defValue;
        if(data != null) {
            try {
                retValue = new Date(((Timestamp) data).getTime());
            } catch(ClassCastException cce) {
                retValue = defValue;
            }
        }
        return retValue;
    }
    
}

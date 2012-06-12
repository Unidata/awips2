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

package com.raytheon.edex.plugin.taf.common;

import java.io.Serializable;
import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * 
 * Class representing a period of time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *      
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02Aug2003    000169      C. Backhaus     Initial Release
 * 18Sep2003    000010      C. Backhaus     Code review clean up.
 * 09Dec2003    377         J. Wilwerding   Adjust date for month rollover
 * 23Mar2005    1055        D. Weeks        Modified for use to decode TAFs for JET 
 * 30Aug2006                B. Phillippe    Modified for use to decode TAFs for AWIPS
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Embeddable
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class TafPeriod implements Serializable, ISerializableObject {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
     * The forecast valid starting date
     */
    @DynamicSerializeElement
    @XmlElement
    @Column
    private Calendar startDate;

    // This time is only used for BECMG groups. It marks the end time of the
    // BECMG transition period.
    @DynamicSerializeElement
    @XmlElement
    @Column
    private Calendar transitionEndDate;

    /**
     * The forecast valid ending date
     */
    @DynamicSerializeElement
    @XmlElement
    @Column
    private Calendar endDate;

    /**
     * Empty constructor for period. Internal values set to null.
     */
    public TafPeriod() {
    }

    /**
     * Constructor
     * 
     * @param sDate
     * @param eDate
     */
    public TafPeriod(Calendar sDate, Calendar eDate) {
        startDate = sDate;
        if (startDate != null) {
            startDate.set(Calendar.SECOND, 0);
            startDate.set(Calendar.MILLISECOND, 0);
        }
        endDate = eDate;
        if (endDate != null) {
            endDate.set(Calendar.SECOND, 0);
            endDate.set(Calendar.MILLISECOND, 0);
        }
    }

    /**
     * 
     * @param period
     * @return
     */
    public static TafPeriod copy(TafPeriod period) {
        TafPeriod periodCopy = new TafPeriod();
        periodCopy.startDate = TimeTools.copy(period.startDate);
        periodCopy.transitionEndDate = TimeTools.copy(period.transitionEndDate);
        periodCopy.endDate = TimeTools.copy(period.endDate);

        return periodCopy;
    }

    /**
     * Constructor that parses string. This constructor parses the string passed
     * into the method into beginning and ending dates. The dates specified may
     * only cover a 24 hour period but may extend into the next calendar day.
     * 
     * @param aValidPeriod
     *            String containing the valid forecast times to be parsed
     * @throws TAFFormatException
     *             If dates are not in proper format
     */
    public static TafPeriod parseValidPeriod(String aValidPeriod,
            WMOHeader header) {

        aValidPeriod = new String(aValidPeriod.trim());

        // decode day
        int day = Integer.parseInt(aValidPeriod.substring(0, 2).trim());

        // decode start and end hours
        int hour1 = Integer.parseInt(aValidPeriod.substring(2, 4).trim());
        int hour2 = Integer.parseInt(aValidPeriod.substring(4, 6).trim());

        // Get the current time : In GMT!
        Calendar startTime = TimeTools.getSystemCalendar(header.getYear(),
                header.getMonth(), header.getDay());

        return TafPeriod.determineValidPeriod(startTime, day, hour1, hour2);
    }

    /**
     * This method determines starting and ending dates based on the input
     * values.
     * 
     * @param baseTime
     * @param day
     * @param hour1
     * @param hour2
     * @return TAFPeriod that represents starting and ending dates.
     */
    public static TafPeriod determineValidPeriod(Calendar baseTime, int day,
            int hour1, int hour2) {

        Calendar sTime = TimeTools.copy(baseTime);

        // get start time day
        int startTimeDay = sTime.get(Calendar.DAY_OF_MONTH);

        int nearEndOfMonth = sTime.getLeastMaximum(Calendar.DAY_OF_MONTH) - 1;

        // TAFs are suppose to cover a short time frame. Adjust for month
        // and year rollover if day difference is 'large'. Subtract one
        // from the greatestMinimum in case month is Jan and year is a
        // leap year.
        if (startTimeDay == 1 && day >= nearEndOfMonth) {
            // we are at the beginning of a month and the TAF is for the
            // end of the month. Adjust month to previous.
            sTime.add(Calendar.MONTH, -1);
        } else if (startTimeDay >= nearEndOfMonth && day < 3) {
            // we are at the end of the month and the TAF is for the
            // beginning of the next month. Adjust month to next.
            sTime.add(Calendar.MONTH, 1);
        }

        sTime.set(Calendar.DAY_OF_MONTH, day);
        sTime.set(Calendar.HOUR_OF_DAY, hour1);
        sTime.set(Calendar.MINUTE, 0);
        sTime.set(Calendar.SECOND, 0);

        // Set the ending time for the period
        Calendar eTime = TimeTools.copy(sTime);

        // Add a day to ending time if the hour is less than or equal
        // to the the start hour
        if (hour2 <= hour1) {
            eTime.add(Calendar.DAY_OF_MONTH, 1);
        }

        if (hour2 == 24) {
            eTime.add(Calendar.DAY_OF_MONTH, 1);
            hour2 = 0;
        }
        eTime.set(Calendar.HOUR_OF_DAY, hour2);
        eTime.set(Calendar.MINUTE, 0);
        eTime.set(Calendar.SECOND, 0);

        return new TafPeriod(sTime, eTime);
    }

    /**
     * This method determines starting and ending dates based on the input
     * values from a 30 Hour TAF.
     * 
     * @param baseTime
     * @param day1
     * @param hour1
     * @param day2
     *            The
     * @param hour2
     * @return TAFPeriod that represents starting and ending dates.
     */
    public static TafPeriod determineValidPeriod(Calendar baseTime, int day1,
            int hour1, int day2, int hour2) {

        Calendar sTime = TimeTools.copy(baseTime);

        // get start time day
        int startTimeDay = sTime.get(Calendar.DAY_OF_MONTH);

        int nearEndOfMonth = sTime.getLeastMaximum(Calendar.DAY_OF_MONTH) - 1;

        // TAFs are suppose to cover a short time frame. Adjust for month
        // and year rollover if day difference is 'large'. Subtract one
        // from the greatestMinimum in case month is Jan and year is a
        // leap year.
        if (startTimeDay == 1 && day1 >= nearEndOfMonth) {
            // we are at the beginning of a month and the TAF is for the
            // end of the month. Adjust month to previous.
            sTime.add(Calendar.MONTH, -1);
        } else if (startTimeDay >= nearEndOfMonth && day1 < 3) {
            // we are at the end of the month and the TAF is for the
            // beginning of the next month. Adjust month to next.
            sTime.add(Calendar.MONTH, 1);
        }

        sTime.set(Calendar.DAY_OF_MONTH, day1);
        sTime.set(Calendar.HOUR_OF_DAY, hour1);
        sTime.set(Calendar.MINUTE, 0);
        sTime.set(Calendar.SECOND, 0);

        // Set the ending time for the period
        Calendar eTime = TimeTools.copy(sTime);

        // Add a day to ending time if the hour is less than or equal
        // to the the start hour
        if (hour2 <= hour1) {
            eTime.add(Calendar.DAY_OF_MONTH, 1);
        }

        if (hour2 == 24) {
            eTime.add(Calendar.DAY_OF_MONTH, 1);
            hour2 = 0;
        }
        eTime.set(Calendar.HOUR_OF_DAY, hour2);
        eTime.set(Calendar.MINUTE, 0);
        eTime.set(Calendar.SECOND, 0);

        return new TafPeriod(sTime, eTime);
    }

    /**
     * This method determines the valid period for a change group of type
     * BECOMING or TEMPORARY which have the format SSEE where SS is the start
     * hour and EE is the end hour of the valid period. The valid period for the
     * entire TAF must be supplied in order to determine the correct dates
     * associated with the hours.
     * 
     * @param aStartTimeHour
     * @param aEndTimeHour
     * @param aTAFValidPeriod
     * @param isChangeCodeBecoming
     *            true if change code is Becoming
     * @return
     */
    public static TafPeriod determineChangeGroupPeriodSSEE(int hour1,
            int hour2, TafPeriod aTAFValidPeriod, boolean isChangeCodeBecoming) {

        Calendar tafStartTime = aTAFValidPeriod.getStartDate();

        Calendar sDate = TimeTools.copy(tafStartTime);
        if (hour1 < tafStartTime.get(Calendar.HOUR_OF_DAY)) {
            sDate.add(Calendar.DAY_OF_MONTH, 1);
        }
        sDate.set(Calendar.HOUR_OF_DAY, hour1);
        sDate.set(Calendar.MINUTE, 0);
        sDate.set(Calendar.SECOND, 0);
        sDate.set(Calendar.MILLISECOND, 0);

        Calendar eDate = TimeTools.copy(tafStartTime);
        if (hour2 <= tafStartTime.get(Calendar.HOUR_OF_DAY)) {
            eDate.add(Calendar.DAY_OF_MONTH, 1);
        }
        eDate.set(Calendar.HOUR_OF_DAY, hour2);
        eDate.set(Calendar.MINUTE, 0);
        eDate.set(Calendar.SECOND, 0);
        eDate.set(Calendar.MILLISECOND, 0);

        TafPeriod period = new TafPeriod();

        // All groups use the start date as is.
        period.setStartDate(sDate);

        if (isChangeCodeBecoming) {
            period.setTransitionEndDate(eDate);
        } else {
            period.setEndDate(eDate);
        }

        return period;
    }

    /**
     * This method determines the valid period for a change group of type
     * BECOMING, PROB, PROB TEMP, or TEMPO which have the format DDhh/DDhh where
     * DDhh/DDhh are the start and end day and hour for a 30 hour change group.
     * The TAFs' valid period is supplied in order to determine the month and
     * year.
     * 
     * @param aStartTimeHour
     * @param aEndTimeHour
     * @param aTAFValidPeriod
     * @param isChangeCodeBecoming
     *            true if change code is Becoming
     * @return
     */
    public static TafPeriod determineChangeGroupPeriodDDhhDDhh(int day1,
            int hour1, int day2, int hour2, TafPeriod aTAFValidPeriod,
            boolean isChangeCodeBecoming) {

        Calendar sDate = setDayHourMin(aTAFValidPeriod.getStartDate(), day1,
                hour1, 0);

        Calendar eDate = setDayHourMin(aTAFValidPeriod.getStartDate(), day2,
                hour2, 0);

        TafPeriod period = new TafPeriod();

        // All groups use the start date as is.
        period.setStartDate(sDate);
        if (isChangeCodeBecoming) {
            period.setTransitionEndDate(eDate);
        } else {
            period.setEndDate(eDate);
        }

        return period;
    }

    /**
     * This method determines the valid period for a change group of type FroM
     * which have the format SSss where SS is the start hour and ss is the start
     * minute of the valid period. The valid period for the entire TAF must be
     * supplied in order to determine the correct dates associated with the
     * time.
     * 
     * @param aStartTimeHour
     * @param aStartTimeMin
     * @param aTAFValidPeriod
     * @return
     */
    public static TafPeriod determineChangeGroupPeriodSSss(int hour, int min,
            TafPeriod aTAFValidPeriod) {

        Calendar startDate = TimeTools.copy(aTAFValidPeriod.getStartDate());
        if (hour <= startDate.get(Calendar.HOUR_OF_DAY)) {
            startDate.add(Calendar.DAY_OF_MONTH, 1);
        }
        startDate.set(Calendar.HOUR_OF_DAY, hour);
        startDate.set(Calendar.MINUTE, min);
        startDate.set(Calendar.SECOND, 0);
        startDate.set(Calendar.MILLISECOND, 0);

        // Have to wait until the next group to figure the actual stop datetime.
        return new TafPeriod(startDate, null);
    }

    /**
     * This method determines the valid period for a change group of type FroM
     * which have the format SSss where SS is the start hour and ss is the start
     * minute of the valid period. The valid period for the entire TAF must be
     * supplied in order to determine the correct dates associated with the
     * time.
     * 
     * @param aStartTimeHour
     * @param aStartTimeMin
     * @param aTAFValidPeriod
     * @return
     */
    public static TafPeriod determineChangeGroupPeriodDDhhmm(int day, int hour,
            int min, TafPeriod aTAFValidPeriod) {

        Calendar sDate = setDayHourMin(aTAFValidPeriod.getStartDate(), day,
                hour, min);

        TafPeriod period = new TafPeriod();

        // All groups use the start date as is.
        period.setStartDate(sDate);

        return period;
    }

    /**
     * Converts object to string
     * 
     * @return String representing the valid forecast period
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("start:");
        if (startDate != null) {
            sb.append(formatDate(startDate));
        }
        sb.append(":trans:");
        if (transitionEndDate != null) {
            sb.append(formatDate(transitionEndDate));
        }
        sb.append(":end:");
        if (endDate != null) {
            sb.append(formatDate(endDate));
        }
        return sb.toString();
    }

    /**
     * @return the theStartDate
     */
    public Calendar getStartDate() {
        return startDate;
    }

    /**
     * @param theStartDate
     *            the theStartDate to set
     */
    public void setStartDate(Calendar start) {
        startDate = start;
    }

    /**
     * @return the transitionEndDate
     */
    public Calendar getTransitionEndDate() {
        return transitionEndDate;
    }

    /**
     * @param transitionEndDate
     *            the transitionEndDate to set
     */
    public void setTransitionEndDate(Calendar transitionEndDate) {
        this.transitionEndDate = transitionEndDate;
    }

    /**
     * @return the theEndDate
     */
    public Calendar getEndDate() {
        return endDate;
    }

    /**
     * @param theEndDate
     *            the theEndDate to set
     */
    public void setEndDate(Calendar end) {
        endDate = end;
    }

    public static String formatDate(Calendar dateTime) {
        return String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", dateTime);
    }

    /**
     * 
     * @param day
     * @param hour
     * @param min
     * @return
     */
    public static Calendar setDayHourMin(Calendar base, int day, int hour,
            int min) {

        Calendar cal = null;
        Calendar target = TimeTools.copy(base);
        target.set(Calendar.SECOND, 0);
        target.set(Calendar.MILLISECOND, 0);

        // Back up at most 1 day.
        target.add(Calendar.DAY_OF_MONTH, -1);
        for (int i = 0; i < 4; i++) {
            int sDay = target.get(Calendar.DAY_OF_MONTH);
            if (sDay == day) {
                cal = TimeTools.copy(target);
                cal.set(Calendar.HOUR_OF_DAY, hour);
                cal.set(Calendar.MINUTE, min);
                break;
            } else {
                target.add(Calendar.DAY_OF_MONTH, 1);
            }
        }
        return cal;
    }    
    
    public static final void main(String [] args) {
        
        
        Calendar cA = TimeTools.getBaseCalendar(2012, 4, 1);
        
        Calendar cB = setDayHourMin(cA, 31, 23, 0);
        
        System.out.println(formatDate(cA));
        System.out.println(formatDate(cB));
        
        
        
        
    }
    
    
    
    
    
    
    
}

package com.raytheon.uf.common.datadelivery.bandwidth.data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Class to allow for relative specification of time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2012 726        jspinks     Initial release.
 * 
 * </pre>
 * 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class RelativeTime {

    @XmlAttribute(required = true)
    private String minuteOfDay;

    @XmlAttribute(required = false)
    private String dayOfWeek;

    @XmlAttribute(required = false)
    private String dayOfMonth;

    @XmlAttribute(required = false)
    private String weekOfMonth;

    @XmlAttribute(required = false)
    private String weekOfYear;

    @XmlAttribute(required = false)
    private String month;

    /**
     * @param minuteOfDay
     *            the minuteOfDay to set
     */
    public void setMinuteOfDay(String minuteOfDay) {
        this.minuteOfDay = minuteOfDay;
    }

    /**
     * @return the minuteOfDay
     */
    public String getMinuteOfDay() {
        return minuteOfDay;
    }

    /**
     * @param dayOfWeek
     *            the dayOfWeek to set
     */
    public void setDayOfWeek(String dayOfWeek) {
        this.dayOfWeek = dayOfWeek;
    }

    /**
     * @return the dayOfWeek
     */
    public String getDayOfWeek() {
        return dayOfWeek;
    }

    /**
     * @param dayOfMonth
     *            the dayOfMonth to set
     */
    public void setDayOfMonth(String dayOfMonth) {
        this.dayOfMonth = dayOfMonth;
    }

    /**
     * @return the dayOfMonth
     */
    public String getDayOfMonth() {
        return dayOfMonth;
    }

    /**
     * @param month
     *            the month to set
     */
    public void setMonth(String month) {
        this.month = month;
    }

    /**
     * @return the month
     */
    public String getMonth() {
        return month;
    }

    /**
     * @param weekOfMonth
     *            the weekOfMonth to set
     */
    public void setWeekOfMonth(String weekOfMonth) {
        this.weekOfMonth = weekOfMonth;
    }

    /**
     * @return the weekOfMonth
     */
    public String getWeekOfMonth() {
        return weekOfMonth;
    }

    /**
     * @param weekOfYear
     *            the weekOfYear to set
     */
    public void setWeekOfYear(String weekOfYear) {
        this.weekOfYear = weekOfYear;
    }

    /**
     * @return the weekOfYear
     */
    public String getWeekOfYear() {
        return weekOfYear;
    }

}

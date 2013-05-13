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

package com.raytheon.uf.common.time;

import java.io.Serializable;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.EnumSet;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.Embedded;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.CalendarConverter;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Represents the time associated with a data item
 * 
 * Partial Port of AWIPS I D2D DataTime class
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         Jim Ramer   Original Code
 * Jun 18, 2007            chammack    Partial port to Java
 * Apr 12, 2013 1857       bgonzale    Added Index annotations to getter methods.
 * Mar 02, 2013 1970       bgonzale    Removed Index annotations.
 * 
 * </pre>
 * 
 * A DataTime has methods that allow the user to obtain the valid time,
 * reference time, forecast time, or the valid period. Constructors are
 * available that allow one to create a DataTime from all of these, or with as
 * little as the reference time.
 * 
 * The units of forecast time are seconds, and the internal representations of
 * the reference time and valid period are such that they have in effect the
 * units of seconds...the punchline being that no time requiring a precision of
 * less than a second can be correctly represented.
 * 
 * Some definitions:
 * 
 * Observation Time: The time an observation is taken.
 * 
 * Valid Time: Most essential time characteristic of any data. That point in
 * time when a given piece of data correctly represents or is expected to
 * represent some atmospheric state. This term has usually been applied to model
 * data. Applied liberally, this definition means that for observational data,
 * the valid time is the same as the observation time.
 * 
 * Initial Time: This term applies to data with a forecast component... most
 * often model data. This is the time most representative of the data that went
 * into initializing the model. It is also called the analysis time or model run
 * time.
 * 
 * Reference Time: This term serves as a unifying concept between data that has
 * an forecast component and data that does not. For data that has a forecast
 * component, the reference time is the same as the initial time. For data that
 * has no forecast component, the reference time is the same as the observation
 * time.
 * 
 * Forecast Time: This is the length of time between the reference time and the
 * valid time.
 * 
 * Valid Period: Often, a piece of data describes some atmospheric state not at
 * a single point in time, but over some period. Rainfall, for example, is not
 * usually reported as an instantaneous rate, but rather an accumulation over
 * some amount of time. The valid period is the range of valid times over which
 * such an accumulation is done.
 * 
 * 
 * @author chammack
 * @version 1.0
 */
@Embeddable
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataTime implements Comparable<DataTime>, Serializable,
        ISerializableObject, Cloneable {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** Defines possible time sort keys */
    public static enum SortKey {
        INITIAL_TIME, FORECAST_TIME, VALID_TIME
    };

    /** The major sort key */
    @Transient
    protected SortKey majorKey = SortKey.VALID_TIME;

    /** The minor sort key */
    @Transient
    protected SortKey minorKey = SortKey.FORECAST_TIME;
    
    /** The reference time */
    @Column(name = "refTime")
    @DynamicSerializeElement
    @XmlAttribute
    protected Date refTime;

    /** The forecast time (in seconds from reference time) */
    @Column(name = "forecastTime")
    @DynamicSerializeElement
    @XmlAttribute
    protected int fcstTime = 0;

    /** The period over which the data is valid */
    @Embedded
    @DynamicSerializeElement
    @XmlElement
    protected TimeRange validPeriod;

    /** Is data to be sorted using match mode */
    @Transient
    protected boolean matchMode;

    @Transient
    protected boolean visible = true;

    /** Data flags */
    public enum FLAG {
        NO_VALID_PERIOD, FCST_USED, PERIOD_USED
    };

    /** Data format flag */
    private static SimpleDateFormat DATE_FORMAT = new SimpleDateFormat(
            "EEE HH:mm'Z' dd-MMM-yy");

    /** The set of flags set on the time */
    @Column
    @Type(type = "com.raytheon.edex.db.mapping.DataTimeFlagType")
    @XmlElement
    @DynamicSerializeElement
    protected EnumSet<FLAG> utilityFlags = EnumSet.noneOf(FLAG.class);

    /** The legend for the string */
    @Transient
    private String legend;

    @DynamicSerializeElement
    @XmlAttribute
    @Transient
    protected Double levelValue = -1.0;

    private static Pattern datePattern = Pattern.compile(TimeUtil.DATE_STRING);

    private static Pattern periodPattern = Pattern.compile("\\[("
            + TimeUtil.DATE_STRING + ")--(" + TimeUtil.DATE_STRING + ")\\]");

    private static Pattern fcstPattern = Pattern
            .compile("\\((\\d{1,20}(:\\d\\d)?)\\)");

    public DataTime(Date date) {
        this.refTime = date;
        validPeriod = new TimeRange(refTime, refTime);
        utilityFlags = EnumSet.noneOf(FLAG.class);
    }

    public DataTime(Date refTime, int fcstTime) {
        this.refTime = refTime;
        this.fcstTime = fcstTime;
        long validTimeMillis = refTime.getTime() + ((long) fcstTime) * 1000;
        validPeriod = new TimeRange(validTimeMillis, validTimeMillis);
        utilityFlags = EnumSet.of(FLAG.FCST_USED);
    }

    public DataTime(String value) {

        Matcher m = datePattern.matcher(value);
        CalendarConverter conv = new CalendarConverter();
        if (m.find()) {

            refTime = ((Calendar) conv.convert(Calendar.class, m.group()))
                    .getTime();
            validPeriod = new TimeRange(refTime, refTime);
            utilityFlags = EnumSet.noneOf(FLAG.class);
        }

        m = fcstPattern.matcher(value);
        if (m.find()) {
            if (m.group(1).contains(":")) {
                String[] fcstTimeTokens = m.group(1).split(":");
                fcstTime = Integer.parseInt(fcstTimeTokens[0]) * 3600
                        + Integer.parseInt(fcstTimeTokens[1]) * 60;
            } else {
                fcstTime = Integer.parseInt(m.group(1)) * 3600;
            }
            if (refTime != null) {
                long validTimeMillis = refTime.getTime() + ((long) fcstTime)
                        * 1000;
                validPeriod = new TimeRange(validTimeMillis, validTimeMillis);
            }
            utilityFlags = EnumSet.of(FLAG.FCST_USED);
        }

        m = periodPattern.matcher(value);
        if (m.find()) {

            Calendar cal1 = (Calendar) conv.convert(Calendar.class, m.group(1));
            Calendar cal2 = (Calendar) conv.convert(Calendar.class, m.group(9));
            validPeriod = new TimeRange(cal1, cal2);

            if (!cal1.equals(cal2)) {
                utilityFlags.add(FLAG.PERIOD_USED);
            }
        }
    }

    /**
     * Constructor for case of data without a forecast component. Associated
     * with a single point in time
     * 
     * @param refTime
     *            the reference time
     */
    public DataTime(Calendar refTime) {
        this.refTime = refTime.getTime();
        // fcstTime = 0;
        validPeriod = new TimeRange(refTime, refTime);
        utilityFlags = EnumSet.noneOf(FLAG.class);
    }

    /**
     * Constructor for case of data without a forecast component and the valid
     * period has no necessary connection to the reference time
     * 
     * @param refTime
     *            the reference time
     * @param validPeriod
     *            the valid period
     */
    public DataTime(Calendar refTime, TimeRange validPeriod) {
        this.refTime = refTime.getTime();
        // fcstTime = 0;
        this.validPeriod = validPeriod;
        if (!validPeriod.getStart().equals(validPeriod.getEnd())) {
            utilityFlags = EnumSet.of(FLAG.PERIOD_USED);
        }
    }

    /**
     * Constructor for case of data without a forecast component and the valid
     * period has no necessary connection to the reference time
     * 
     * @param refTime
     *            the reference time
     * @param validPeriod
     *            the valid period
     */
    public DataTime(long refTime, TimeRange validPeriod) {
        this.refTime = new Date(refTime);
        // fcstTime = 0;
        this.validPeriod = validPeriod;
        if (!validPeriod.getStart().equals(validPeriod.getEnd())) {
            utilityFlags = EnumSet.of(FLAG.PERIOD_USED);
        }
    }

    /**
     * Constructor for the case of data with a forecast component associated
     * with a single point in time.
     * 
     * @param refTime
     *            the reference time
     * @param fcstTime
     *            the forecast time
     */
    public DataTime(Calendar refTime, int fcstTime) {
        this.refTime = refTime.getTime();
        this.fcstTime = fcstTime;
        long validTimeMillis = refTime.getTimeInMillis() + ((long) fcstTime)
                * 1000;
        validPeriod = new TimeRange(validTimeMillis, validTimeMillis);
        utilityFlags = EnumSet.of(FLAG.FCST_USED);
    }

    /**
     * Constructor for the case of data with a forecast component and valid
     * period that has no necessary connection to valid time
     * 
     * @param refTime
     *            the reference time
     * @param fcstTime
     *            the forecast time
     * @param validPeriod
     *            the valid period
     */
    public DataTime(Calendar refTime, int fcstTime, TimeRange validPeriod) {
        this.refTime = refTime.getTime();
        this.fcstTime = fcstTime;
        this.validPeriod = validPeriod;
        utilityFlags = EnumSet.of(FLAG.FCST_USED);
        if (!validPeriod.getStart().equals(validPeriod.getEnd())) {
            utilityFlags.add(FLAG.PERIOD_USED);
        }
    }

    /**
     * Default Constructor
     */
    public DataTime() {

    }

    /**
     * @return the refTime
     */
    @Index(name = "refTimeIndex")
    public Date getRefTime() {
        return this.refTime;
    }

    /**
     * @return the refTime
     */
    public Calendar getRefTimeAsCalendar() {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(this.refTime);
        return cal;
    }

    /**
     * @return the fcstTime
     */
    @Index(name = "fcstTimeIndex")
    public int getFcstTime() {
        return fcstTime;
    }

    /**
     * @return the validPeriod
     */
    public TimeRange getValidPeriod() {
        return validPeriod;
    }

    /**
     * @return the valid time
     */
    public Calendar getValidTime() {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis(refTime.getTime() + (1000 * ((long) fcstTime)));

        return cal;
    }

    /**
     * @return a time matching forecast time in seconds
     */
    public long getMatchFcst() {
        return 60 * (fcstTime / 60);
    }

    /**
     * @return a time matching ref time
     */
    public long getMatchRef() {
        return refTime.getTime();
    }

    /**
     * 
     * @return get the matching valid time
     */
    public long getMatchValid() {
        return refTime.getTime() + 60 * ((((long) fcstTime) * 1000) / 60);
    }

    public Double getLevelValue() {
        return levelValue;
    }

    public void setLevelValue(Double levelValue) {
        this.levelValue = levelValue == null ? -1.0 : levelValue;
    }

    public boolean isSpatial() {
        return this.levelValue >= 0.0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        return equals(obj, false);
    }

    public boolean equals(Object obj, boolean ignoreSpatial) {

        if (obj == null || !(obj instanceof DataTime))
            return false;

        DataTime rhs = (DataTime) obj;

        if (((DataTime) obj).getRefTime() == null) {
            return fcstTime == rhs.fcstTime;
        }

        Date rt1 = new Date(refTime.getTime());
        Date rt2 = new Date(rhs.refTime.getTime());

        if (ignoreSpatial) {
            return (rt1.equals(rt2) && fcstTime == rhs.fcstTime && validPeriod
                    .equals(rhs.validPeriod));
        } else {
            return (rt1.equals(rt2) && fcstTime == rhs.fcstTime
                    && validPeriod.equals(rhs.validPeriod) && levelValue
                    .equals(rhs.levelValue));
        }
    }

    /**
     * This routine determines which characteristics of a DataTime object,
     * reference time, valid time, or forecast time, affect how relational
     * operators >, <, >=, and <= behave. Default is to sort primarily on the
     * valid time and secondarily on the reference time.
     * 
     * @param majorKey
     *            the major sort key
     * @param minorKey
     *            the minor sort key
     * @param matchMode
     *            the match mode flag
     */
    public void setSortKeys(SortKey majorKey, SortKey minorKey,
            boolean matchMode) {
        this.majorKey = majorKey;
        this.minorKey = minorKey;
        this.matchMode = matchMode;
    }

    /**
     * Returns true if the left hand side is greater than the right hand side
     * 
     * @param rhs
     *            the right hand side
     * @return true if left hand side is greater than
     */
    public boolean greaterThan(DataTime rhs) {

        if (rhs.getRefTime() == null) {
            return (fcstTime > rhs.getFcstTime());

        } else {
            if (matchMode) {
                switch (majorKey) {
                case INITIAL_TIME:
                    if (getMatchRef() > rhs.getMatchRef())
                        return true;
                    if (getMatchRef() < rhs.getMatchRef())
                        return false;
                    break;
                case FORECAST_TIME:
                    if (getRefTime().getTime() == 0)
                        return false;
                    if (getMatchFcst() > rhs.getMatchFcst())
                        return true;
                    if (getMatchFcst() < rhs.getMatchFcst())
                        return false;
                    break;
                case VALID_TIME:
                    if (getMatchValid() > rhs.getMatchValid())
                        return true;
                    if (getMatchValid() < rhs.getMatchValid())
                        return false;
                }
                switch (minorKey) {
                case INITIAL_TIME:
                    if (getMatchRef() > rhs.getMatchRef())
                        return true;
                    if (getMatchRef() < rhs.getMatchRef())
                        return false;
                    break;
                case FORECAST_TIME:
                    if (getMatchFcst() > rhs.getMatchFcst())
                        return true;
                    if (getMatchFcst() < rhs.getMatchFcst())
                        return false;
                    break;
                case VALID_TIME:
                    if (getMatchFcst() > rhs.getMatchFcst())
                        return true;
                    if (getMatchFcst() < rhs.getMatchFcst())
                        return false;
                }
                if (getLevelValue() > rhs.getLevelValue()) {
                    return true;
                }
            } else {
                switch (majorKey) {
                case INITIAL_TIME:
                    if (refTime.getTime() > rhs.refTime.getTime())
                        return true;
                    if (refTime.getTime() < rhs.refTime.getTime())
                        return false;
                    break;
                case FORECAST_TIME:
                    if (fcstTime > rhs.fcstTime)
                        return true;
                    if (fcstTime < rhs.fcstTime)
                        return false;
                    break;
                case VALID_TIME:
                    if (refTime.getTime() + (((long) fcstTime) * 1000) > rhs.refTime
                            .getTime() + (((long) rhs.fcstTime) * 1000))
                        return true;
                    if (refTime.getTime() + (((long) fcstTime) * 1000) < rhs.refTime
                            .getTime() + (((long) rhs.fcstTime) * 1000))
                        return false;
                    if (refTime.getTime() > rhs.getRefTime().getTime())
                        return true;
                    if (refTime.getTime() < rhs.getRefTime().getTime())
                        return false;
                }
                switch (minorKey) {
                case INITIAL_TIME:
                    if (refTime.getTime() > rhs.refTime.getTime())
                        return true;
                    break;
                case FORECAST_TIME:
                    if (fcstTime > rhs.fcstTime)
                        return true;
                    break;
                case VALID_TIME:
                    if (refTime.getTime() + (((long) fcstTime) * 1000) > rhs.refTime
                            .getTime() + (((long) rhs.fcstTime) * 1000))
                        return true;
                }
            }
        }
        return false;

    }

    /**
     * Returns the datatime string in D2D legend format
     * 
     * @return the legend time string
     */
    public String getLegendString() {
        if (legend != null)
            return legend;

        if (refTime.getTime() <= 1)
            return ("");

        StringBuffer legendBuffer = new StringBuffer();

        // Get the valid time we will use
        Date validTime;
        if ((utilityFlags.contains(FLAG.NO_VALID_PERIOD)))
            validTime = validPeriod.getEnd();
        else {
            validTime = new Date(refTime.getTime() + (((long) fcstTime) * 1000));
        }

        // Encode the perturbation index in the time.
        long p = fcstTime % 60;

        if (p > 0) {
            legendBuffer.append("Perturbation " + p + " ");
        }

        NumberFormat nf = NumberFormat.getInstance();
        nf.setMinimumIntegerDigits(2);
        nf.setMinimumFractionDigits(0);
        nf.setMaximumFractionDigits(2);
        nf.setMaximumIntegerDigits(2);

        NumberFormat nf2 = (NumberFormat) NumberFormat.getInstance().clone();
        nf2.setMaximumIntegerDigits(3);

        // code initial time like a green time and the forecast time in hours if
        // forecast time is non-zero.
        if (fcstTime > 0 || utilityFlags.contains(FLAG.FCST_USED)) {

            long fcstTimeInSec = fcstTime;
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.setTime(this.refTime);
            String day = nf.format(cal.get(Calendar.DAY_OF_MONTH));
            String hour = nf.format(cal.get(Calendar.HOUR_OF_DAY));
            String minute = nf.format(cal.get(Calendar.MINUTE));
            String forcastTime = nf2.format(fcstTimeInSec / 3600);
            String forcastTimeUnit = "HR";

            if (cal.get(Calendar.MINUTE) == 0) {
                minute = "";
                if (fcstTimeInSec > 864000 && fcstTimeInSec % 86400 == 0) {
                    forcastTime = nf2.format(fcstTimeInSec / 86400);
                    forcastTimeUnit = "DAYS";
                }
            }

            legendBuffer.append(String.format("%s.%s%s %3s%s ", day, hour,
                    minute, forcastTime, forcastTimeUnit));

        }

        // Add a notation of the length of the valid period if non-zero.
        else if (validPeriod.getDuration() > 0
                && (!utilityFlags.contains(FLAG.NO_VALID_PERIOD))) {
            // validPeriod duration is in millis, convert to minutes
            int m = (int) validPeriod.getDuration() / 1000 / 60;
            // Convert minutes to hours
            int h = m / 60;
            // Get minutes remaining after hours
            m = m % 60;
            if (m == 0)
                legendBuffer.append(h + "hrs ");
            else
                legendBuffer.append(h + ":" + m + " ");

            if (validPeriod.getStart().equals(validTime))
                legendBuffer.append("Begn ");
            else if (validPeriod.getEnd().equals(validTime))
                legendBuffer.append("Endg ");
            else
                legendBuffer.append("Incl ");
        }

        DATE_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
        legendBuffer.append(DATE_FORMAT.format(validTime));

        this.legend = legendBuffer.toString();
        return this.legend;

    }

    /**
     * Return true if the date is null
     * 
     * @return true if the date is null
     */
    public boolean isNull() {
        if (this.refTime.getTime() == 0)
            return true;

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    public int compareTo(DataTime o) {
        if (this.equals(o))
            return 0;

        if (this.greaterThan(o))
            return 1;

        return -1;
    }

    public void setRefTime(Date refTime) {
        this.refTime = refTime;
    }

    public void setFcstTime(int fcstTime) {
        this.fcstTime = fcstTime;
    }

    public void setValidPeriod(TimeRange validPeriod) {
        this.validPeriod = validPeriod;
    }

    public EnumSet<FLAG> getUtilityFlags() {
        return utilityFlags;
    }

    public void setUtilityFlags(EnumSet<FLAG> utilityFlags) {
        if (utilityFlags != null) {
            this.utilityFlags = utilityFlags;
        } else {
            this.utilityFlags = EnumSet.noneOf(FLAG.class);
        }
    }

    /**
     * States if the DataTime is visible
     * 
     * @return
     */
    public boolean isVisible() {
        return visible;
    }

    /**
     * Set whether the dataTime is visible or not, if not visible, data at this
     * datatime should not be displayed
     * 
     * @param visible
     */
    public void setVisible(boolean visible) {
        this.visible = visible;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();

        if (refTime != null) {
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.setTime(this.refTime);
            buffer.append(TimeUtil.formatCalendar(cal).replaceAll("_", " "));
        }
        if (utilityFlags.contains(FLAG.FCST_USED)) {
            int hrs = fcstTime / 3600;
            int mins = (fcstTime - hrs * 3600) / 60;
            if (fcstTime % 3600 == 0) {
                buffer.append(" (").append(hrs).append(")");
            } else {
                buffer.append(" (").append(hrs).append(":").append(mins)
                        .append(")");
            }
        }
        if (utilityFlags.contains(FLAG.PERIOD_USED)) {
            buffer.append("[");
            buffer.append(TimeUtil.formatDate(validPeriod.getStart())
                    .replaceAll("_", " "));
            buffer.append("--");
            buffer.append(TimeUtil.formatDate(validPeriod.getEnd()).replaceAll(
                    "_", " "));
            buffer.append("]");

        }
        return buffer.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        HashCodeBuilder hashBuilder = new HashCodeBuilder();
        hashBuilder.append(refTime).append(fcstTime);
        if (validPeriod != null && validPeriod.isValid()) {
            hashBuilder.append(validPeriod.getStart());
            hashBuilder.append(validPeriod.getEnd());
        }
        hashBuilder.append(levelValue);
        return hashBuilder.toHashCode();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public DataTime clone() {
        boolean hasForecast = utilityFlags.contains(FLAG.FCST_USED)
                || fcstTime > 0;
        boolean hasTimePeriod = utilityFlags.contains(FLAG.PERIOD_USED);
        DataTime rval = null;
        if (hasForecast && hasTimePeriod) {
            rval = new DataTime(this.getRefTimeAsCalendar(),
                    this.getFcstTime(), this.getValidPeriod().clone());
        } else {
            if (hasForecast) {
                rval = new DataTime(this.getRefTimeAsCalendar(),
                        this.getFcstTime());
            } else if (hasTimePeriod) {
                rval = new DataTime(this.getRefTimeAsCalendar(), this
                        .getValidPeriod().clone());
            } else {
                rval = new DataTime(this.getRefTimeAsCalendar());
            }
        }
        rval.levelValue = levelValue;
        return rval;
    }
}
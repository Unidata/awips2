package com.raytheon.uf.common.datadelivery.registry;

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

import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request Time XML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2011             dhladky     Initial creation
 * Jul 24, 2012 0955        djohnson    Use List instead of ArrayList.
 * Jul 24, 2012 0955        djohnson    Use List instead of ArrayList.
 * Sep 24, 2012 1209        djohnson    Add copy constructor.
 * Sep 28, 2012 1187        djohnson    {@link #setEndDate(Date)} was incorrectly changing the start date.
 * Nov 19, 2012 1166        djohnson    Clean up JAXB representation of registry objects.
 * Jun 04, 2013  223        mpduff      Added interval field.
 * Jun 06, 2013 2038        djohnson    Remove throws ParseException.
 * Sept 26, 2013 1797       dhladky     Separated Gridded fields from this class once and for all.
 * Oct 10, 2013 1797        bgonzale    Refactored registry Time objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@XmlSeeAlso({ GriddedTime.class, PointTime.class})
public abstract class Time implements Serializable {

    private static final long serialVersionUID = -7032078355732493125L;
 
    @XmlAttribute
    @DynamicSerializeElement
    protected int numTimes;

    @XmlElement(type = Date.class)
    @DynamicSerializeElement
    protected Date start;

    @XmlElement(type = Date.class)
    @DynamicSerializeElement
    protected Date end;

    @XmlAttribute
    @DynamicSerializeElement
    protected String format;

    @XmlElement(type = Date.class)
    @DynamicSerializeElement
    protected Date requestStart;

    @XmlElement(type = Date.class)
    @DynamicSerializeElement
    protected Date requestEnd;

    public Time() {
    }

    public Time(Time toCopy) {
        this.numTimes = toCopy.numTimes;
        this.start = toCopy.start;
        this.end = toCopy.end;
        this.format = toCopy.format;
        this.requestEnd = toCopy.requestEnd;
        this.requestStart = toCopy.requestStart;
    }

    public int getNumTimes() {
        return numTimes;
    }

    public void setNumTimes(int numTimes) {
        this.numTimes = numTimes;
    }

    /**
     * Get the start date
     * 
     * @return start
     */
    public Date getStart() {
        return start;
    }

    /**
     * Set the start date
     * 
     */
    public void setStart(Date startDate) {
        this.start = startDate;
    }

    /**
     * Set the start date
     * 
     * @throws ParseException
     */
    public void setStartDate(String startDate) throws ParseException {
        this.start = parseDate(startDate);
    }

    /**
     * Get the end date
     * 
     * @return end
     */
    public Date getEnd() {
        return this.end;
    }

    /**
     * Set the end date
     * 
     * @return
     */
    public void setEnd(Date endDate) {
        this.end = endDate;
    }

    /**
     * Set the end date
     * 
     * @throws ParseException
     */
    public void setEndDate(String endDate) throws ParseException {
        this.end = parseDate(endDate);
    }

    /**
     * 
     * Enumeration of the duration units
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum STEP_UNIT {
        SECOND("second"), MINUTE("minute"), HOUR("hour"), DAY("day"), WEEK(
                "week"), MONTH("month");

        private final String durationUnit;

        private STEP_UNIT(String name) {
            durationUnit = name;
        }

        public String getDurationUnit() {
            return durationUnit;
        }
    }

    /**
     * variations on step unit
     * 
     * @param inStep
     * @return
     */
    public static STEP_UNIT findStepUnit(String inStep) {

        if (inStep.equals("hr")) {
            return STEP_UNIT.HOUR;
        } else if (inStep.equals("min")) {
            return STEP_UNIT.MINUTE;
        } else if (inStep.equals("mo")) {
            return STEP_UNIT.MONTH;
        } else if (inStep.equals("mon")) {
            return STEP_UNIT.MONTH;
        } else if (inStep.equals("d")) {
            return STEP_UNIT.DAY;
        } else if (inStep.equals("dy")) {
            return STEP_UNIT.DAY;
        } else if (inStep.equals("s")) {
            return STEP_UNIT.SECOND;
        } else if (inStep.equals("sec")) {
            return STEP_UNIT.SECOND;
        }

        return null;
    }

    /**
     * Set the end date
     * 
     * @return
     */
    public void setRequestStart(Date requestStartDate) {
        this.requestStart = requestStartDate;
    }

    /**
     * Get the request start date
     * 
     * @return
     */
    public Date getRequestStart() {
        return requestStart;
    }

    /**
     * Set the request start date
     * 
     * @throws ParseException
     */
    public void setRequestStartDate(String requestStartDate)
            throws ParseException {
        this.requestStart = parseDate(requestStartDate);
    }

    /**
     * Set the request end date
     * 
     * @return
     */
    public void setRequestEnd(Date requestEndDate) {
        this.requestEnd = requestEndDate;
    }

    /**
     * Set the request end date
     * 
     * @throws ParseException
     */
    public void setRequestEndDate(String requestEndDate) throws ParseException {
        this.requestEnd = parseDate(requestEndDate);
    }

    /**
     * Get the request end date
     * 
     * @return
     */
    public Date getRequestEnd() {
        return requestEnd;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    /**
     * parse the date string using format.
     * 
     * @throws ParseException
     */
    private Date parseDate(String date) throws ParseException {
        if (this.format != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat(this.format);
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
            return dateFormat.parse(date);
        } else {
            throw new ParseException("No Date Format defined.", 0);
        }
    }

}

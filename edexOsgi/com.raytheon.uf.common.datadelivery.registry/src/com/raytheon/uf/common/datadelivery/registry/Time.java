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
import javax.xml.bind.annotation.XmlSeeAlso;

import com.raytheon.uf.common.serialization.ISerializableObject;
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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@XmlSeeAlso({ GriddedTime.class, PointTime.class})
public abstract class Time implements ISerializableObject, Serializable {

    private static final long serialVersionUID = -7032078355732493125L;
 
    @XmlAttribute
    @DynamicSerializeElement
    protected int numTimes;

    @XmlAttribute
    @DynamicSerializeElement
    protected String start;

    @XmlAttribute
    @DynamicSerializeElement
    protected String end;

    @XmlAttribute
    @DynamicSerializeElement
    protected String format;

    @XmlAttribute
    @DynamicSerializeElement
    protected String requestStart;

    @XmlAttribute
    @DynamicSerializeElement
    protected String requestEnd;

    protected Date startDate = null;

    protected Date requestEndDate = null;

    protected Date endDate = null;

    protected Date requestStartDate = null;

    public int getNumTimes() {
        return numTimes;
    }

    public void setNumTimes(int numTimes) {
        this.numTimes = numTimes;
    }

    public String getStart() {
        return start;
    }

    /**
     * Get the start date
     * 
     * @return
     * @throws ParseException
     */
    public Date getStartDate() throws ParseException {

        if (startDate == null) {
            if (getStart() != null && getFormat() != null) {
                SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
                dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
                this.startDate = dateFormat.parse(getStart());
            }
        }

        return startDate;
    }

    public void setStart(String start) {
        this.start = start;
    }

    /**
     * Set the start date
     * 
     * @return
     * @throws ParseException
     */
    public void setStartDate(Date startDate) {
        this.startDate = startDate;
        if (startDate != null && getFormat() != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
            setStart(dateFormat.format(startDate));
        }
    }

    public String getEnd() {
        return end;
    }

    /**
     * Get the end date
     * 
     * @return
     * @throws ParseException
     */
    public Date getEndDate() throws ParseException {
        if (endDate == null) {
            if (getEnd() != null && getFormat() != null) {
                SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
                dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
                this.endDate = dateFormat.parse(getEnd());
            }
        }

        return endDate;
    }

    public void setEnd(String end) {
        this.end = end;
    }

    /**
     * Set the end date
     * 
     * @return
     * @throws ParseException
     */
    public void setEndDate(Date endDate) {
        this.endDate = endDate;
        if (endDate != null && getFormat() != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
            setEnd(dateFormat.format(endDate));
        }
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
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

    public String getRequestStart() {
        return requestStart;
    }

    public void setRequestStart(String requestStart) {
        this.requestStart = requestStart;
    }

    public String getRequestEnd() {
        return requestEnd;
    }

    public void setRequestEnd(String requestEnd) {
        this.requestEnd = requestEnd;
    };

    /**
     * Set the end date
     * 
     * @return
     * @throws ParseException
     */
    public void setRequestStartAsDate(Date requestStartDate) {
        this.requestStartDate = requestStartDate;
        if (requestStartDate != null && getFormat() != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
            setRequestStart(dateFormat.format(requestStartDate));
        }
    }

    /**
     * Get the end date
     * 
     * @return
     * @throws ParseException
     */
    public Date getRequestStartAsDate() throws ParseException {
        if (requestStartDate == null) {
            if (getRequestStart() != null && getFormat() != null) {
                SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
                dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
                this.requestStartDate = dateFormat.parse(getRequestStart());
            }
        }

        return requestStartDate;
    }

    /**
     * Set the end date
     * 
     * @return
     * @throws ParseException
     */
    public void setRequestEndAsDate(Date requestEndDate) {
        this.requestEndDate = requestEndDate;
        if (requestEndDate != null && getFormat() != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
            setRequestEnd(dateFormat.format(requestEndDate));
        }
    }

    /**
     * Get the end date
     * 
     * @return
     * @throws ParseException
     */
    public Date getRequestEndAsDate() throws ParseException {
        if (requestEndDate == null) {
            if (getRequestEnd() != null && getFormat() != null) {
                SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
                dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
                this.requestEndDate = dateFormat.parse(getRequestEnd());
            }
        }

        return requestEndDate;
    }

}

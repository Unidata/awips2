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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Request Time XML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2011             dhladky     Initial creation
 * Jul 24, 2012    955      djohnson    Use List instead of ArrayList.
 * Jun 04, 2013    223      mpduff      Cleanup.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GriddedTime extends Time implements ISerializableObject,
        Serializable {

    private static final long serialVersionUID = -7032078355732493125L;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GriddedTime.class);

    /**
     * Default Constructor.
     */
    public GriddedTime() {

    }

    @XmlElement(name = "step")
    @DynamicSerializeElement
    private Double step;

    @XmlElement(name = "stepUnit")
    @DynamicSerializeElement
    private String stepUnit;

    @XmlElements({ @XmlElement(name = "selectedTimeIndices", type = Integer.class) })
    @DynamicSerializeElement
    private List<Integer> selectedTimeIndices;

    @XmlElements({ @XmlElement(name = "cycleTimes", type = Integer.class) })
    @DynamicSerializeElement
    private List<Integer> cycleTimes;

    private Date startDate = null;

    private Date requestEndDate = null;

    private Date endDate = null;

    private Date requestStartDate = null;

    /**
     * Clone constructor.
     * 
     * @param the
     *            {@link Time} to clone
     */
    public GriddedTime(GriddedTime toCopy) {
        List<Integer> incomingCycleTimes = toCopy.getCycleTimes();
        this.cycleTimes = (incomingCycleTimes == null) ? null
                : new ArrayList<Integer>(incomingCycleTimes);
        this.end = toCopy.end;
        this.format = toCopy.format;
        this.numTimes = toCopy.numTimes;
        this.requestEnd = toCopy.requestEnd;
        this.requestStart = toCopy.requestStart;
        List<Integer> incomingSelectedTimeIndices = toCopy.selectedTimeIndices;
        this.selectedTimeIndices = (incomingSelectedTimeIndices == null) ? null
                : new ArrayList<Integer>(incomingSelectedTimeIndices);
        this.start = toCopy.start;
        this.step = toCopy.step;
        this.stepUnit = toCopy.stepUnit;
    }

    /**
     * Get the start date
     * 
     * @return
     * @throws ParseException
     */
    @Override
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

    /**
     * Set the start date
     * 
     * @return
     * @throws ParseException
     */
    @Override
    public void setStartDate(Date startDate) {
        this.startDate = startDate;
        if (startDate != null && getFormat() != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
            setStart(dateFormat.format(startDate));
        }
    }

    /**
     * Get the end date
     * 
     * @return
     * @throws ParseException
     */
    @Override
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

    /**
     * Set the end date
     * 
     * @return
     * @throws ParseException
     */
    @Override
    public void setEndDate(Date endDate) {
        this.endDate = endDate;
        if (endDate != null && getFormat() != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
            setStart(dateFormat.format(endDate));
        }
    }

    @Override
    public Double getStep() {
        return step;
    }

    @Override
    public void setStep(Double step) {
        this.step = step;
    }

    /**
     * Get the cycle times.
     * 
     * @return List of cycle times
     */
    @Override
    public List<Integer> getCycleTimes() {
        return this.cycleTimes;
    }

    /**
     * Set the cycle times.
     * 
     * @param cycleTimes
     *            ArrayList of cycle times.
     */
    @Override
    public void setCycleTimes(List<Integer> cycleTimes) {
        this.cycleTimes = cycleTimes;
    }

    /**
     * Add a cycle time.
     * 
     * @param cycleTime
     *            The cycle time to add
     */
    @Override
    public void addCycleTime(int cycleTime) {
        this.cycleTimes.add(cycleTime);
    }

    /**
     * Get me the date of the time requested
     * 
     * @param timeInt
     * @return
     */
    @Override
    public Date getTimeAsDate(int timeInt) {

        try {
            long unitStepFactor = getUnitStepFactor();

            for (int i = 0; i < getNumTimes(); i++) {
                if (timeInt == i) {

                    long time = 0l;
                    if (i == 0) {
                        time = getStartDate().getTime();
                    } else {
                        time = (long) (getStartDate().getTime() + (unitStepFactor
                                * getStep() * i));
                    }
                    return new Date(time);
                }
            }
        } catch (ParseException pe) {
            statusHandler.handle(Priority.ERROR, "Can't parse time as date.",
                    pe);
        }
        return null;
    }

    /**
     * Sets the request start as a date
     * 
     * @param timeInt
     */
    @Override
    public void setRequestStartTimeAsInt(Integer timeInt) {
        try {
            if (getStartDate() != null) {

                long unitStepFactor = getUnitStepFactor();

                for (int i = 0; i < getNumTimes(); i++) {
                    if (timeInt == i) {

                        long time = 0l;
                        if (i == 0) {
                            time = getStartDate().getTime();
                        } else {
                            time = (long) (getStartDate().getTime() + (unitStepFactor
                                    * getStep() * i));
                        }
                        Date date = new Date(time);
                        setRequestStartAsDate(date);
                        break;
                    }

                }
            }

        } catch (ParseException pe) {
            statusHandler.handle(Priority.ERROR, "Can't parse time as date.",
                    pe);
        }
    }

    /**
     * Get the start time as an int
     * 
     * @return
     */
    @Override
    public int getRequestStartTimeAsInt() {

        int timeInt = 0;

        try {
            if (getRequestStartAsDate() != null && getStartDate() != null) {

                long unitStepFactor = getUnitStepFactor();

                for (int i = 0; i < getNumTimes(); i++) {
                    // System.out.println("StartDate: "+getStartDate());
                    long time = 0l;
                    if (i == 0) {
                        time = getStartDate().getTime();
                    } else {
                        time = (long) (getStartDate().getTime() + (unitStepFactor
                                * getStep() * i));
                    }
                    Date stepDate = new Date(time);

                    if (stepDate.equals(getRequestStartAsDate())) {
                        timeInt = i;
                        break;
                    }
                }
            }

        } catch (ParseException pe) {
            statusHandler.handle(Priority.ERROR,
                    "Can't parse requested time as date.", pe);
        }
        return timeInt;

    }

    /**
     * Set the end time with a know integer in the list
     * 
     * @param timeInt
     */
    @Override
    public void setRequestEndTimeAsInt(Integer timeInt) {
        try {
            if (getEndDate() != null) {

                long unitStepFactor = getUnitStepFactor();

                for (int i = getNumTimes() - 1; i > 0; i--) {
                    // System.out.println("EndDate: " + getEndDate());

                    if (i == timeInt) {

                        long time = (long) (getStartDate().getTime() + (unitStepFactor
                                * getStep() * i));
                        Date date = new Date(time);
                        setRequestEndAsDate(date);
                        break;
                    }
                }
            }
        } catch (ParseException pe) {
            statusHandler.handle(Priority.ERROR,
                    "Can't parse requested time as int.", pe);
        }

    }

    /**
     * Get the end time as an int
     * 
     * @return
     */
    @Override
    public int getRequestEndTimeAsInt() {

        int timeInt = 0;

        try {
            if (getRequestEndAsDate() != null && getEndDate() != null) {

                long unitStepFactor = getUnitStepFactor();

                for (int i = 0; i < getNumTimes(); i++) {
                    // System.out.println("StartDate: "+getStartDate());
                    long time = 0l;
                    if (i == getNumTimes() - 1) {
                        time = getEndDate().getTime();
                    } else {
                        time = (long) (getStartDate().getTime() + (unitStepFactor
                                * getStep() * i));
                    }
                    Date stepDate = new Date(time);

                    if (stepDate.equals(getRequestEndAsDate())) {
                        timeInt = i;
                        break;
                    }
                }
            }

        } catch (ParseException pe) {
            statusHandler.handle(Priority.ERROR,
                    "Can't parse requested time as int.", pe);
        }
        return timeInt;

    }

    @Override
    public String getStepUnit() {
        return stepUnit;
    }

    @Override
    public void setStepUnit(String stepUnit) {
        this.stepUnit = stepUnit;
    }

    /**
     * get the primitive
     * 
     * @return
     */
    private long getUnitStepFactor() {

        long millis = 0l;

        if (getStepUnit().equals(STEP_UNIT.SECOND.getDurationUnit())) {

            return 1000;

        } else if (getStepUnit().equals(STEP_UNIT.MINUTE.getDurationUnit())) {
            millis = 1000 * 60;

        } else if (getStepUnit().equals(STEP_UNIT.HOUR.getDurationUnit())) {
            millis = 1000 * 60 * 60;

        } else if (getStepUnit().equals(STEP_UNIT.DAY.getDurationUnit())) {
            millis = 1000 * 60 * 60 * 24;

        } else if (getStepUnit().equals(STEP_UNIT.WEEK.getDurationUnit())) {
            millis = 1000 * 60 * 60 * 24 * 7;

        }
        return millis;
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
     * Finds forecast step in seconds
     * 
     * @return
     */
    @Override
    public int findForecastStepUnit() {

        if (getStepUnit().equals("hour")) {
            return (int) (getStep() * 60 * 60);
        } else if (getStepUnit().equals("minute")) {
            return (int) (getStep() * 60);
        } else if (getStepUnit().equals("month")) {
            return (int) (getStep() * 30 * 24 * 60);
        } else if (getStepUnit().equals("week")) {
            return (int) (getStep() * 7 * 24 * 60);
        } else if (getStepUnit().equals("second")) {
            return getStep().intValue();
        } else if (getStepUnit().equals("day")) {
            return (int) (getStep() * 24 * 60 * 60);
        }

        return -99999;
    }

    /**
     * Set the end date
     * 
     * @return
     * @throws ParseException
     */
    @Override
    public void setRequestStartAsDate(Date requestStartDate)
            throws ParseException {
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
    @Override
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
    @Override
    public void setRequestEndAsDate(Date requestEndDate) throws ParseException {
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
    @Override
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

    /**
     * gets the FCST hours
     * 
     * @return
     */
    @Override
    public List<String> getFcstHours() {
        List<String> hours = new ArrayList<String>();

        int hour = 0;
        hours.add(String.valueOf(hour));

        for (int i = 1; i < getNumTimes(); i++) {
            hour += getStep();
            hours.add(String.valueOf(hour));
        }

        return hours;
    }

    /**
     * @return the selectedTimeIndices
     */
    @Override
    public List<Integer> getSelectedTimeIndices() {
        return selectedTimeIndices;
    }

    /**
     * @param selectedTimeIndices
     *            the selectedTimeIndices to set
     */
    @Override
    public void setSelectedTimeIndices(List<Integer> selectedTimeIndices) {
        this.selectedTimeIndices = selectedTimeIndices;
    }

    /**
     * Gets the time breakups needs to split retrievals for a subscription and
     * limit size of retrievals
     * 
     * @return
     */
    @Override
    public List<List<Integer>> getTimeSequences(int sfactor) {

        List<List<Integer>> sequences = new ArrayList<List<Integer>>();
        List<Integer> al = new ArrayList<Integer>();

        if (selectedTimeIndices.size() > 0) {
            int previous = selectedTimeIndices.get(0);
            al.add(previous);
            for (int i = 1; i < selectedTimeIndices.size(); i++) {
                int next = selectedTimeIndices.get(i);
                if (next - previous == 1 && al.size() <= sfactor) {
                    al.add(next);
                    previous = next;
                } else {
                    sequences.add(al);
                    al = new ArrayList<Integer>();
                    al.add(next);
                    previous = next;
                }
            }

            sequences.add(al);
        }

        return sequences;
    }
}

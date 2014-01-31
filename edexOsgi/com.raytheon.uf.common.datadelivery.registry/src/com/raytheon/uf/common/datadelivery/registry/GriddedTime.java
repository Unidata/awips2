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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

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
 * Jul 24, 2012    955      djohnson    Use List instead of ArrayList.
 * Jun 04, 2013    223      mpduff      Cleanup.
 * Jun 06, 2013 2038        djohnson    Remove throws ParseException.
 * Sept 25, 2013 1797       dhladky     separated overrides from time.
 * Oct 10, 2013 1797        bgonzale    Refactored registry Time objects.
 * Oct 24, 2013  2454       dhladky     Trouble with general gridded cycles that don't exist being null instead of empty.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GriddedTime extends Time implements
        Serializable {

    private static final long serialVersionUID = -7032078355732493125L;

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
    private List<Integer> cycleTimes = new ArrayList<Integer>();

    /**
     * Default Constructor.
     */
    public GriddedTime() {
    }
   
    /**
     * Clone constructor.
     * 
     * @param the
     *            {@link GriddedTime} to clone
     */
    public GriddedTime(GriddedTime toCopy) {
        super(toCopy);
        List<Integer> incomingCycleTimes = toCopy.getCycleTimes();
        this.cycleTimes = (incomingCycleTimes == null) ? null
                : new ArrayList<Integer>(incomingCycleTimes);
        List<Integer> incomingSelectedTimeIndices = toCopy.selectedTimeIndices;
        this.selectedTimeIndices = (incomingSelectedTimeIndices == null) ? null
                : new ArrayList<Integer>(incomingSelectedTimeIndices);
        this.step = toCopy.step;
        this.stepUnit = toCopy.stepUnit;
    }

    /**
     * Get the step of time
     * @return
     */
    public Double getStep() {
        return step;
    }

    /**
     * Sets the step of time
     * @param step
     */
    public void setStep(Double step) {
        this.step = step;
    }

    /**
     * Get the cycle times.
     * 
     * @return List of cycle times
     */
    public List<Integer> getCycleTimes() {
        return this.cycleTimes;
    }

    /**
     * Set the cycle times.
     * 
     * @param cycleTimes
     *            ArrayList of cycle times.
     */
    public void setCycleTimes(List<Integer> cycleTimes) {
        this.cycleTimes = cycleTimes;
    }

    /**
     * Add a cycle time.
     * 
     * @param cycleTime
     *            The cycle time to add
     */
    public void addCycleTime(int cycleTime) {
        if (this.cycleTimes == null) {
            this.cycleTimes = new ArrayList<Integer>();
        }
        this.cycleTimes.add(cycleTime);
    }

    /**
     * Get me the date of the time requested
     * 
     * @param timeInt
     * @return
     */
    public Date getTimeAsDate(int timeInt) {

        long unitStepFactor = getUnitStepFactor();

        for (int i = 0; i < getNumTimes(); i++) {
            if (timeInt == i) {

                long time = 0l;
                if (i == 0) {
                    time = getStart().getTime();
                } else {
                    time = (long) (getStart().getTime() + (unitStepFactor
                            * getStep() * i));
                }
                return new Date(time);
            }
        }
        return null;
    }

    /**
     * Sets the request start as a date
     * 
     * @param timeInt
     */
    public void setRequestStartTimeAsInt(Integer timeInt) {
        if (getStart() != null) {

            long unitStepFactor = getUnitStepFactor();

            for (int i = 0; i < getNumTimes(); i++) {
                if (timeInt == i) {

                    long time = 0l;
                    if (i == 0) {
                        time = getStart().getTime();
                    } else {
                        time = (long) (getStart().getTime() + (unitStepFactor
                                * getStep() * i));
                    }
                    Date date = new Date(time);
                    setRequestStart(date);
                    break;
                }

            }
        }
    }

    /**
     * Get the start time as an int
     * 
     * @return
     */
    public int getRequestStartTimeAsInt() {

        int timeInt = 0;

        if (getRequestStart() != null && getStart() != null) {

            long unitStepFactor = getUnitStepFactor();

            for (int i = 0; i < getNumTimes(); i++) {
                // System.out.println("StartDate: "+getStartDate());
                long time = 0l;
                if (i == 0) {
                    time = getStart().getTime();
                } else {
                    time = (long) (getStart().getTime() + (unitStepFactor
                            * getStep() * i));
                }
                Date stepDate = new Date(time);

                if (stepDate.equals(getRequestStart())) {
                    timeInt = i;
                    break;
                }
            }
        }

        return timeInt;

    }

    /**
     * Set the end time with a know integer in the list
     * 
     * @param timeInt
     */
    public void setRequestEndTimeAsInt(Integer timeInt) {
        if (getEnd() != null) {

            long unitStepFactor = getUnitStepFactor();

            for (int i = getNumTimes() - 1; i > 0; i--) {
                // System.out.println("EndDate: " + getEndDate());

                if (i == timeInt) {

                    long time = (long) (getStart().getTime() + (unitStepFactor
                            * getStep() * i));
                    Date date = new Date(time);
                    setRequestEnd(date);
                    break;
                }
            }
        }
    }

    /**
     * Get the end time as an int
     * 
     * @return
     */
    public int getRequestEndTimeAsInt() {

        int timeInt = 0;

        if (getRequestEnd() != null && getEnd() != null) {

            long unitStepFactor = getUnitStepFactor();

            for (int i = 0; i < getNumTimes(); i++) {
                // System.out.println("StartDate: "+getStartDate());
                long time = 0l;
                if (i == getNumTimes() - 1) {
                    time = getEnd().getTime();
                } else {
                    time = (long) (getStart().getTime() + (unitStepFactor
                            * getStep() * i));
                }
                Date stepDate = new Date(time);

                if (stepDate.equals(getRequestEnd())) {
                    timeInt = i;
                    break;
                }
            }
        }
        return timeInt;

    }

    /**
     * Gets the step unit of time
     * @return
     */
    public String getStepUnit() {
        return stepUnit;
    }

    /**
     * Set the step unit of time
     * @param stepUnit
     */
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
     * gets the FCST hours
     * 
     * @return
     */
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
    public List<Integer> getSelectedTimeIndices() {
        return selectedTimeIndices;
    }

    /**
     * @param selectedTimeIndices
     *            the selectedTimeIndices to set
     */
    public void setSelectedTimeIndices(List<Integer> selectedTimeIndices) {
        this.selectedTimeIndices = selectedTimeIndices;
    }

    /**
     * Gets the time breakups needs to split retrievals for a subscription and
     * limit size of retrievals
     * 
     * @return
     */
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

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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.google.common.collect.Lists;
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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Time implements ISerializableObject, Serializable {

    private static final long serialVersionUID = -7032078355732493125L;

    /**
     * Default Constructor.
     */
    public Time() {

    }

    /**
     * Clone constructor.
     * 
     * @param the
     *            {@link Time} to clone
     */
    public Time(Time toCopy) {
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
                : new ArrayList<Integer>(
                incomingSelectedTimeIndices);
        this.start = toCopy.start;
        this.step = toCopy.step;
        this.stepUnit = toCopy.stepUnit;
    }

    @XmlAttribute
    @DynamicSerializeElement
    private int numTimes;

    @XmlAttribute
    @DynamicSerializeElement
    private String start;

    @XmlAttribute
    @DynamicSerializeElement
    private String end;

    @XmlAttribute
    @DynamicSerializeElement
    private Double step;

    @XmlAttribute
    @DynamicSerializeElement
    private String stepUnit;

    @XmlAttribute
    @DynamicSerializeElement
    private String format;

    @XmlAttribute
    @DynamicSerializeElement
    private String requestStart;

    @XmlAttribute
    @DynamicSerializeElement
    private String requestEnd;

    @XmlElements({ @XmlElement(name = "selectedTimeIndices", type = Integer.class) })
    @DynamicSerializeElement
    private List<Integer> selectedTimeIndices = new ArrayList<Integer>();

    @XmlElements({ @XmlElement(name = "cycleTimes", type = Integer.class) })
    @DynamicSerializeElement
    private List<Integer> cycleTimes = new ArrayList<Integer>();

    private Date startDate = null;

    private Date requestEndDate = null;

    private Date endDate = null;

    private Date requestStartDate = null;

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
    public void setStartDate(Date startDate) throws ParseException {
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
    public void setEndDate(Date endDate) throws ParseException {
        this.endDate = endDate;
        if (endDate != null && getFormat() != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat(getFormat());
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
            setEnd(dateFormat.format(endDate));
        }
    }

    public Double getStep() {
        return step;
    }

    public void setStep(Double step) {
        this.step = step;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
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
     * @param cycleTimes ArrayList of cycle times.
     */
    public void setCycleTimes(List<Integer> cycleTimes) {
        this.cycleTimes = cycleTimes;
    }

    /**
     * Add a cycle time.
     *
     * @param cycleTime The cycle time to add
     */
    public void addCycleTime(int cycleTime) {
        if (cycleTimes == null) {
            cycleTimes = Lists.newArrayList();
        }

        this.cycleTimes.add(cycleTime);
    }

    /**
     * Get me the date of the time requested
     * @param timeInt
     * @return
     */
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
			System.err.println("Can't parse the requested time: "
					+ pe.getMessage());
		}
		return null;
	}

    /**
     * Sets the request start as a date
     *
     * @param timeInt
     */
    public void setRequestStartTimeAsInt(Integer timeInt) {
        try {
            if (getStartDate() != null) {

                long unitStepFactor = getUnitStepFactor();

                for (int i = 0; i < getNumTimes(); i++) {
                    // System.out.println("StartDate: "+getStartDate());
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
            System.err.println("Can't parse the requested time: "
                    + pe.getMessage());
        }
    }

    /**
     * Get the start time as an int
     *
     * @return
     */
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
            System.err.println("Can't parse the requested time: "
                    + pe.getMessage());
        }
        return timeInt;

    }

    /**
     * Set the end time with a know integer in the list
     *
     * @param timeInt
     */
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
            System.err.println("Can't parse the requested time: "
                    + pe.getMessage());
        }

    }

    /**
     * Get the end time as an int
     *
     * @return
     */
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
            System.err.println("Can't parse the requested time: "
                    + pe.getMessage());
        }
        return timeInt;

    }

    public String getStepUnit() {
        return stepUnit;
    }

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
    public List<String> getFcstHours() {
        List<String> hours = new ArrayList<String>();

        int hour = 0;
        hours.add(String.valueOf(hour));

        for (int i = 1; i < numTimes; i++) {
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

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
package com.raytheon.viz.hydrocommon.cresthistory;

import java.sql.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * This class contains crest data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Sep 2008             lvenable    Initial creation.
 * 18 Nov 2008             dhladky     Made Interactive.
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * June 10 2016 18507      jingtaoD    Hydrobase creset history checkbox
 * Feb 22, 2017 6035       njensen     Updated getFormattedData()
 * Apr 20, 2018 6892       mduff       Better handling of Crest Times.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class CrestData implements Comparable<CrestData> {

    private static final String UNDEF = "UNDEF";

    private static final String X = "X";

    /**
     * Stage data.
     */
    private double stage = HydroConstants.MISSING_VALUE;

    /**
     * Flow data.
     */
    private Integer flow = HydroConstants.MISSING_VALUE;

    /**
     * Date.
     */
    private Calendar crestDate = null;

    /**
     * Remarks.
     */
    private String remarks = null;

    private boolean oldDatum = false;

    private boolean iceJam = false;

    private boolean highWater = false;

    private boolean suppress = false;

    private String prelim = "O"; // default

    /**
     * Indicator on how to sort the data.
     */
    private ICrestDataSortType sortTypeCB;

    /** Formatted time string */
    private String timeString;

    public CrestData() {

    }

    /**
     * Public constructor
     * 
     * @param objects
     */
    public CrestData(Object[] objects) {
        if (objects[0] != null) {
            setStage((Double) objects[0]);
        }
        if (objects[1] != null) {
            setFlow(((Number) objects[1]).intValue());
        }
        if (objects[2] != null) {
            Calendar cal = TimeUtil.newGmtCalendar();
            cal.setTimeInMillis(((Date) objects[2]).getTime());

            int hours = 0;
            int mins = 0;

            String time = (String) objects[3];
            if (!"unk".equals(time) && !UNDEF.equals(time)) {
                if (time.contains(":")) {
                    String[] line = time.split(":");
                    hours = Integer.parseInt(line[0]);
                    mins = Integer.parseInt(line[1]);

                    cal.set(Calendar.HOUR_OF_DAY, hours);
                    cal.set(Calendar.MINUTE, mins);
                    timeString = time;
                } else if (time.isEmpty() || time == null) {
                    timeString = UNDEF;
                } else {
                    timeString = time;
                }
            } else {
                timeString = time;
            }

            setCrestDate(cal);
        }
        if (objects[4] != null) {
            setRemarks((String) objects[4]);
        }
        if (objects[5] != null && objects[5].toString().contains(X)) {
            setHighWater(true);
        }
        if (objects[6] != null && objects[6].toString().contains(X)) {
            setIceJam(true);
        }
        if (objects[7] != null && objects[7].toString().contains(X)) {
            setOldDatum(true);
        }
        if (objects[8] != null && objects[8].toString().contains(X)) {
            setSuppress(true);
        }
        if (objects[9] != null) {
            setPrelim((String) objects[9]);
        }

    }

    public void setStage(double stage) {
        this.stage = stage;
    }

    public void setFlow(int flow) {
        this.flow = flow;
    }

    public void setCrestDate(Calendar crestDate) {
        this.crestDate = crestDate;
    }

    public Calendar getCrestDate() {
        return crestDate;
    }

    public void setRemarks(String remarks) {
        this.remarks = remarks;
    }

    public void setHighWater(boolean highWater) {
        this.highWater = highWater;
    }

    public boolean isHighWater() {
        return highWater;
    }

    public void setIceJam(boolean iceJam) {
        this.iceJam = iceJam;
    }

    public boolean isIce() {
        return iceJam;
    }

    public void setOldDatum(boolean oldDatum) {
        this.oldDatum = oldDatum;
    }

    public boolean isOldDatum() {
        return oldDatum;
    }

    public void setSuppress(boolean suppress) {
        this.suppress = suppress;
    }

    public boolean isSuppress() {
        return suppress;
    }

    public void setPrelim(String prelim) {
        this.prelim = prelim;
    }

    public String getPrelim() {
        return prelim;
    }

    /**
     * Set the sort type callback used to retrieve which field to sort on.
     * 
     * @param cb
     *            Callback.
     */
    public void setSortCallback(ICrestDataSortType cb) {
        sortTypeCB = cb;
    }

    /**
     * Get the year.
     * 
     * @return The year.
     */
    public int getYear() {
        return crestDate.get(Calendar.YEAR);
    }

    /**
     * Get the stage data.
     * 
     * @return The stage data.
     */
    public double getStage() {
        return stage;
    }

    /**
     * Get the flow data.
     * 
     * @return The flow data.
     */
    public int getFlow() {
        return flow;
    }

    /**
     * Get the date in a yyyy-MM-dd format.
     * 
     * @return The date.
     */
    public String getDateString() {
        DateFormat df = new SimpleDateFormat("MM/dd/yyyy");
        df.setTimeZone(TimeZone.getTimeZone("GMT"));

        return df.format(crestDate.getTime());
    }

    /**
     * Get the time in a HH:MM:SS format.
     * 
     * @return
     */
    public String getTimeString() {
        if (timeString == null) {
            timeString = UNDEF;
        }
        return timeString;
    }

    /**
     * set the formatted time string.
     * 
     * @param time
     */
    public void setTimeString(String time) {
        timeString = time;
    }

    /**
     * Get the remarks.
     * 
     * @return The remarks.
     */
    public String getRemarks() {
        return remarks;
    }

    /**
     * Get the data (stage, flow, date, time, and crest type) in an array of
     * formatted strings.
     * 
     * @return The formatted data.
     */
    public String[] getFormattedData() {
        String[] formatted = new String[5];

        if (stage == HydroConstants.MISSING_VALUE) {
            formatted[0] = String.format("%10s", HydroConstants.MISSING_STRING);
        } else {
            formatted[0] = String.format("%9.2f", stage);
        }

        if (flow == HydroConstants.MISSING_VALUE) {
            formatted[1] = String.format("%9s", HydroConstants.MISSING_STRING);
        } else {
            formatted[1] = String.format("%9s", flow);
        }

        formatted[2] = String.format("  %10s  ", getDateString());
        formatted[3] = String.format("%10s", getTimeString());

        String prelim = getPrelim();
        String crestType = "";
        if (prelim.equalsIgnoreCase(X) || "P".equalsIgnoreCase(prelim)) {
            crestType = "Prelim";
        } else if ("R".equalsIgnoreCase(prelim)) {
            crestType = "Record";
        }
        formatted[4] = String.format("%-6s", crestType);
        return formatted;
    }

    /**
     * Compare method used to determine what data type to sort on.
     * 
     * Note: This class has a natural ordering that is inconsistent with equals.
     */
    @Override
    public int compareTo(CrestData obj) {
        CrestData otherObject = obj;

        String sortType = sortTypeCB.getSortType();

        if (sortType.compareTo("Flow") == 0) {
            int x = flow.compareTo(otherObject.getFlow());

            if (x < 0) {
                return 1;
            } else if (x > 0) {
                return -1;
            }
        } else if (sortType.compareTo("Date") == 0) {
            if (crestDate.getTimeInMillis() < otherObject.crestDate
                    .getTimeInMillis()) {
                return 1;
            } else if (crestDate.getTimeInMillis() > otherObject.crestDate
                    .getTimeInMillis()) {
                return -1;
            }
        } else {
            if (stage < otherObject.getStage()) {
                return 1;
            } else if (stage > otherObject.getStage()) {
                return -1;
            }
        }

        return 0;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((crestDate == null) ? 0 : crestDate.hashCode());
        result = prime * result + ((flow == null) ? 0 : flow.hashCode());
        long temp;
        temp = Double.doubleToLongBits(stage);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        CrestData other = (CrestData) obj;
        if (crestDate == null) {
            if (other.crestDate != null) {
                return false;
            }
        } else if (!crestDate.equals(other.crestDate)) {
            return false;
        }
        if (flow == null) {
            if (other.flow != null) {
                return false;
            }
        } else if (!flow.equals(other.flow)) {
            return false;
        }
        if (Double.doubleToLongBits(stage) != Double
                .doubleToLongBits(other.stage)) {
            return false;
        }
        return true;
    }
}

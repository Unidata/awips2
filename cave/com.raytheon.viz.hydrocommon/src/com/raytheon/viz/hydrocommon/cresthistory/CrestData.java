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
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class CrestData implements Comparable<CrestData> {
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
     * Tells wether the time is crap or not
     */
    private boolean isTime = false;

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
     * Flag indicating if the data is record data or not.
     */
    private final boolean record = false;

    /**
     * Indicator on how to sort the data.
     */
    private ICrestDataSortType sortTypeCB;

    // small public constuctor
    public CrestData() {

    }

    /**
     * Public constuctor
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
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.setTimeInMillis(((Date) objects[2]).getTime());

            int hours = 0;
            int mins = 0;

            if (!((String) objects[3]).equals("unk")
                    && !((String) objects[3]).equals("UNDEF")) {

                String[] line = ((String) objects[3]).split(":");
                hours = Integer.parseInt(line[0]);
                mins = Integer.parseInt(line[1]);

                cal.set(Calendar.HOUR_OF_DAY, hours);
                cal.set(Calendar.MINUTE, mins);

                // has a time
                setIsTime(true);
            }

            setCrestDate(cal);
        }
        if (objects[4] != null) {
            setRemarks((String) objects[4]);
        }
        if (objects[5] != null) {
            setHighWater(true);
        }
        if (objects[6] != null) {
            setIceJam(true);
        }
        if (objects[7] != null) {
            setOldDatum(true);
        }
        if (objects[8] != null) {
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
        if (!isTime) {
            return "UNDEF";
        }

        DateFormat df = new SimpleDateFormat("HH:mm");
        df.setTimeZone(TimeZone.getTimeZone("GMT"));

        return df.format(crestDate.getTime());
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
     * Get the record flag.
     * 
     * @return The record flag.
     */
    public boolean getRecordFlag() {
        return record;
    }

    /**
     * get the time flag;
     * 
     * @return
     */
    public boolean isTime() {
        return isTime;
    }

    /**
     * Sets the isTime
     * 
     * @param isTime
     */
    public void setIsTime(boolean isTime) {
        this.isTime = isTime;
    }

    /**
     * Get the data (stage, flow, date, time, and crest type) in a formatted
     * string.
     * 
     * @return The formatted data.
     */
    public String getFormattedData() {
        String str = "";
        String prelim = getPrelim();
        String crestType = "";
        if (prelim.equalsIgnoreCase("X") || prelim.equalsIgnoreCase("P")) {
            crestType = "Prelim";
        } else if (prelim.equalsIgnoreCase("R")) {
            crestType = "Record";
        }

        if (stage == HydroConstants.MISSING_VALUE) {
            str = String.format("%10s   %8s    %10s  %10s %-6s",
                    HydroConstants.MISSING_STRING, flow, getDateString(),
                    getTimeString(), crestType);
        } else if (flow == HydroConstants.MISSING_VALUE) {
            str = String.format("  %8.2f   %8s    %10s  %10s %-6s", stage,
                    HydroConstants.MISSING_STRING, getDateString(),
                    getTimeString(), crestType);
        } else {
            str = String.format("  %8.2f   %8s    %10s  %10s %-6s", stage,
                    flow, getDateString(), getTimeString(), crestType);
        }

        return str;
    }

    /**
     * Compare method used to determine what data type to sort on.
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
}

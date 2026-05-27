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
package com.raytheon.viz.hydrocommon.data;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 19, 2018   7379     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class StationDisplayData {
    private boolean gage = false;

    private boolean id = false;

    private boolean name = false;

    private boolean time = false;

    private boolean pe = false;

    private boolean elevation = false;

    private boolean value = false;

    /**
     * Construct an event to alter the drawing of the Display gages.
     * 
     * @param source
     * @param gage
     * @param id
     * @param name
     * @param time
     * @param pe
     * @param elevation
     */
    public StationDisplayData(Object source, boolean value, boolean gage,
            boolean id, boolean name, boolean time, boolean pe,
            boolean elevation) {
        this.value = value;
        this.gage = gage;
        this.id = id;
        this.name = name;
        this.time = time;
        this.pe = pe;
        this.elevation = elevation;
    }

    public StationDisplayData() {
        // Empty constructor
    }

    /**
     * gage on/off
     * 
     * @return
     */
    public boolean isGage() {
        return gage;
    }

    /**
     * id on/off
     * 
     * @return
     */
    public boolean isId() {
        return id;
    }

    /**
     * name on/off
     * 
     * @return
     */
    public boolean isName() {
        return name;
    }

    /**
     * time on/off
     * 
     * @return
     */
    public boolean isTime() {
        return time;
    }

    /**
     * pe on/off
     * 
     * @return
     */
    public boolean isPe() {
        return pe;
    }

    /**
     * elevation on/off
     * 
     * @return
     */
    public boolean isElevation() {
        return elevation;
    }

    /**
     * Value on/off
     * 
     * @return the value
     */
    public boolean isValue() {
        return value;
    }

    public void setPe(boolean pe) {
        this.pe = pe;
    }

    public void setId(boolean id) {
        this.id = id;
    }

    public void setGage(boolean gage) {
        this.gage = gage;
    }

    public void setName(boolean name) {
        this.name = name;
    }

    public void setTime(boolean time) {
        this.time = time;
    }

    public void setElevation(boolean elevation) {
        this.elevation = elevation;
    }

    public void setValue(boolean value) {
        this.value = value;
    }
}

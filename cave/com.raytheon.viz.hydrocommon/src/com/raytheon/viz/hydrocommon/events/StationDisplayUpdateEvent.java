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
package com.raytheon.viz.hydrocommon.events;

/**
 * 
 * StationDisplayUpdateEvent
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08Nov, 2008   #1628     dhladky     Initial creation
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 * </pre>
 * 
 * @author dhladky
 *
 */

import java.util.EventObject;

import com.raytheon.viz.hydrocommon.data.StationDisplayData;

public class StationDisplayUpdateEvent extends EventObject {

    /**
     */
    private static final long serialVersionUID = 189889789742098618L;

    private StationDisplayData displayData = new StationDisplayData();

    /**
     * Construct an event to alter the drawing of the Display gages in Hydro.
     * 
     * @param source
     * @param gage
     * @param id
     * @param name
     * @param time
     * @param pe
     * @param elevation
     */
    public StationDisplayUpdateEvent(Object source, boolean value, boolean gage,
            boolean id, boolean name, boolean time, boolean pe,
            boolean elevation) {

        super(source);
        this.displayData.setValue(value);
        this.displayData.setGage(gage);
        this.displayData.setId(id);
        this.displayData.setName(name);
        this.displayData.setTime(time);
        this.displayData.setPe(pe);
        this.displayData.setElevation(elevation);
    }

    /**
     * Construct an event to alter the drawing of the Display gages in D2D.
     * 
     * @param source
     * @param gage
     * @param id
     * @param name
     * @param time
     */
    public StationDisplayUpdateEvent(Object source, boolean value, boolean gage,
            boolean id, boolean name, boolean time) {
        this(source, value, gage, id, name, time, false, false);
    }

    /**
     * gage on/off
     * 
     * @return
     */
    public boolean isGage() {
        return displayData.isGage();
    }

    /**
     * id on/off
     * 
     * @return
     */
    public boolean isId() {
        return displayData.isId();
    }

    /**
     * name on/off
     * 
     * @return
     */
    public boolean isName() {
        return displayData.isName();
    }

    /**
     * time on/off
     * 
     * @return
     */
    public boolean isTime() {
        return displayData.isTime();
    }

    /**
     * pe on/off
     * 
     * @return
     */
    public boolean isPE() {
        return displayData.isPe();
    }

    /**
     * elevation on/off
     * 
     * @return
     */
    public boolean isElevation() {
        return displayData.isElevation();
    }

    /**
     * Value on/off
     * 
     * @return the value
     */
    public boolean isValue() {
        return displayData.isValue();
    }

    public StationDisplayData getDisplayData() {
        return displayData;
    }
}

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
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 *
 */

import java.util.EventObject;

public class StationDisplayUpdateEvent extends EventObject {

    /**
     */
    private static final long serialVersionUID = 189889789742098618L;

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
    public StationDisplayUpdateEvent(Object source, boolean value,
            boolean gage, boolean id, boolean name, boolean time, boolean pe,
            boolean elevation) {

        super(source);
        this.value = value;
        this.gage = gage;
        this.id = id;
        this.name = name;
        this.time = time;
        this.pe = pe;
        this.elevation = elevation;

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
    public boolean isID() {
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
    public boolean isPE() {
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
     * @return the value
     */
    public boolean isValue() {
        return value;
    }
}

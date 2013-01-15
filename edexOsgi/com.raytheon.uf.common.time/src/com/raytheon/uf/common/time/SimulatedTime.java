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
package com.raytheon.uf.common.time;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Simulated time clock with offset and scale capabilities.
 * 
 * Provides a simulated time which can be offset from real time, frozen, and/or
 * accelerated/decelerated.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2008            randerso    Initial creation
 * Aug 24, 2012 0743       djohnson    Add option to use milliseconds for operations, change to singleton.
 * Nov 02, 2012 1302       djohnson    Change mistakenly public constructor to private.
 * Jan 07, 2013 1442       rferrel     Changes to add/remove/notify Simulated Time Change Listeners.
 *                                      Use SimulatedTimeTest for unit testing.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public final class SimulatedTime {
    /**
     * The system global simulated time instance
     */
    private static final SimulatedTime systemTime = new SimulatedTime();

    private List<ISimulatedTimeChangeListener> listeners;

    private boolean doNotify;

    /**
     * Retrieve the system global simulate time instance
     * 
     * @return
     */
    public static SimulatedTime getSystemTime() {
        return systemTime;
    }

    /**
     * Base time for simulation
     */
    private long baseTime;

    /**
     * Offset between real time and simulated time
     */
    private long offset;

    /**
     * Simulated time scale
     */
    private double scale;

    /**
     * True if simulated time is frozen
     */
    private boolean isFrozen;

    /**
     * Time when simulated time was last frozen
     */
    private long frozenTime;

    /**
     * Convenience method for getting current time
     * 
     * @return
     */
    private long now() {
        return System.currentTimeMillis();
    }

    /**
     * Creates a simulated time that matches real time.
     * 
     */
    private SimulatedTime() {
        this.listeners = new ArrayList<ISimulatedTimeChangeListener>();
        this.doNotify = true;
        setRealTime();
    }

    public synchronized void addSimulatedTimeChangeListener(
            ISimulatedTimeChangeListener listener) {
        listeners.add(listener);
    }

    public synchronized void removeSimulatedTimeChangeListener(
            ISimulatedTimeChangeListener listener) {
        listeners.remove(listener);
    }

    /**
     * Determine is simulated time = real time
     * 
     * @return true if simulated time = real time
     */
    public boolean isRealTime() {
        return offset == 0 && scale == 1.0 && !isFrozen;
    }

    /**
     * Set the simulated time to real time
     */
    public void setRealTime() {
        baseTime = now();
        offset = 0;
        scale = 1.0;
        isFrozen = false;
        fireTimeChangeListeners();
    }

    /**
     * Get the current simulated time
     * 
     * @return current simulated time
     */
    public Date getTime() {
        return new Date(getMillis());
    }

    public long getMillis() {
        if (isFrozen) {
            return frozenTime;
        } else {
            return Math.round((now() - baseTime) * scale) + baseTime + offset;
        }
    }

    /**
     * Set the current simulated time
     * 
     * @param date
     */
    public void setTime(Date date) {
        setTime(date.getTime());
    }

    /**
     * Set the current simulated time
     * 
     * @param millis
     */
    public void setTime(long millis) {
        if (isFrozen) {
            frozenTime = millis;
        } else {
            baseTime = now();
            offset = millis - baseTime;
        }
        fireTimeChangeListeners();
    }

    /**
     * Get the simulated time scale (acceleration/deceleration)
     * 
     * @return 1.0 for normal time rate, >1.0 for accelerated time < 1.0 for
     *         decelerated time. If negative time will run backward.
     */
    public double getScale() {
        return scale;
    }

    /**
     * Set the simulated time scale (acceleration/deceleration)
     * 
     * @param scale
     *            1.0 for normal time rate, >1.0 for accelerated time < 1.0 for
     *            decelerated time. If negative time will run backward.
     */
    public void setScale(double scale) {
        if (this.scale != scale) {
            this.scale = scale;
            fireTimeChangeListeners();
        }
    }

    /**
     * Get the simulated time frozen state
     * 
     * @return true if time is frozen
     */
    public boolean isFrozen() {
        return isFrozen;
    }

    /**
     * Freeze/unfreeze simulated time
     * 
     * @param isFrozen
     *            true to freeze, false to unfreeze
     */
    public void setFrozen(boolean isFrozen) {
        if (this.isFrozen == isFrozen) {
            return;
        }

        if (isFrozen) {
            frozenTime = getMillis();
        } else {
            baseTime = now();
            offset = frozenTime - baseTime;
        }
        this.isFrozen = isFrozen;
        fireTimeChangeListeners();
    }

    /**
     * Allows turning off notification of listeners when making several updates
     * at the same time. Changing to false turns off notification. Changing to
     * true allows notification and triggers a notification.
     * 
     * @param doNotify
     */
    public void notifyListeners(boolean doNotify) {
        if (this.doNotify != doNotify) {
            this.doNotify = doNotify;
            fireTimeChangeListeners();
        }
    }

    /**
     * Notify listeners of time change.
     */
    private void fireTimeChangeListeners() {
        if (doNotify) {
            // Copy to make thread safe.
            List<ISimulatedTimeChangeListener> list = new ArrayList<ISimulatedTimeChangeListener>(
                    listeners);
            for (ISimulatedTimeChangeListener listener : list) {
                listener.timechanged();
            }
        }
    }
}

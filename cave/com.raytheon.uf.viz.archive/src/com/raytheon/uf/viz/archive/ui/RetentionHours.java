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
package com.raytheon.uf.viz.archive.ui;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Convenience class for taking retention hours and converting to days/hours.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2013  1966       rferrel     Initial creation
 * May 28, 2014 3171       rferrel     Change retention labels.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class RetentionHours {

    /** Minimum value for the spinner. */
    private final int minUnit;

    /** The retention hours. */
    private int retentionHours;

    /** Spinner for the time assume hours or days. */
    private Spinner timeSpnr;

    /** Combo box assume to determine use of Hours or Days. */
    private Combo timeUnitCombo;

    /**
     * Keep track of previous time unit so recalculation only done when changed.
     */
    private int prevTimeUnitSelected = 0;

    /**
     * Set when user modified values.
     */
    private boolean modifyState = false;

    /**
     * Listeners to inform when user performs a modification.
     */
    private final List<IModifyListener> listeners = new CopyOnWriteArrayList<IModifyListener>();

    private final String tooltip;

    /**
     * Constructor with default 7 day retention.
     */
    public RetentionHours(int minUnit, Spinner timeSpnr, Combo timeUnitCombo,
            String tooltip) {
        this.minUnit = minUnit;
        this.timeSpnr = timeSpnr;
        this.timeUnitCombo = timeUnitCombo;
        this.tooltip = tooltip;
        init();
    }

    /**
     * Set up Listeners on the combo boxes for time conversion.
     */
    private void init() {
        timeSpnr.setToolTipText(tooltip);
        timeSpnr.setMinimum(minUnit);
        timeSpnr.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateAndCheckTimeSelection();
            }
        });

        timeUnitCombo.removeAll();
        timeUnitCombo.add("Hours");
        timeUnitCombo.add("Days");
        timeUnitCombo.select(prevTimeUnitSelected);

        timeUnitCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (prevTimeUnitSelected != timeUnitCombo.getSelectionIndex()) {
                    prevTimeUnitSelected = timeUnitCombo.getSelectionIndex();
                    handleTimeUnitSelection();
                    updateAndCheckTimeSelection();
                }
            }
        });
        timeUnitCombo.setData(timeUnitCombo.getItem(timeUnitCombo
                .getSelectionIndex()));
        timeUnitCombo.setToolTipText(tooltip);
    }

    /**
     * Update the retention hours and determine if modification listeners should
     * be notified.
     */
    private void updateAndCheckTimeSelection() {
        // Make sure this is called so retention hours is updated properly.
        boolean modified = handleTimeSelection();

        // Do notification when state becomes true.
        if (!modifyState && modified) {
            modifyState = true;
            for (IModifyListener iModifyListener : listeners) {
                iModifyListener.modified();
            }
        }
    }

    /**
     * Get retention in hours.
     * 
     * @return
     */
    public int getHours() {
        return retentionHours;
    }

    /**
     * Set number hours of retention.
     * 
     * @param hours
     */
    public void setHours(int hours) {
        if (hours < minUnit) {
            hours = minUnit;
        }

        retentionHours = hours;
        int time = retentionHours;
        if (timeUnitCombo.getItem(timeUnitCombo.getSelectionIndex()).equals(
                "Days")) {
            time /= TimeUtil.HOURS_PER_DAY;
            if (time < minUnit) {
                time = minUnit;
            }
        }

        timeSpnr.setSelection(time);

        // Based on the time unit retentionHours may need updating.
        handleTimeSelection();
    }

    /**
     * Handle the retention selection for both minimum and extended retention.
     * 
     * @param timeUnitCombo
     *            Retention combo box.
     * @param timeSpinner
     *            Retention spinner.
     * @return hours entered if changed; -1 if not changed
     */
    private void handleTimeUnitSelection() {
        int time = 0;

        if (timeUnitCombo.getItem(timeUnitCombo.getSelectionIndex()).equals(
                "Hours")) {
            time = convertTime(true, timeSpnr.getSelection());
        } else {
            time = convertTime(false, timeSpnr.getSelection());
        }

        timeSpnr.setSelection(time);
        timeUnitCombo.setData(timeUnitCombo.getItem(timeUnitCombo
                .getSelectionIndex()));
    }

    /**
     * Covert time from either hours to days or days to hours.
     * 
     * @param daysToHours
     *            Flag indicating how to convert the time.
     * @param time
     *            Time to be converted.
     * @return The converted time.
     */
    private int convertTime(boolean daysToHours, int time) {
        int convertedTime = 0;

        if (daysToHours) {
            convertedTime = time * TimeUtil.HOURS_PER_DAY;
            retentionHours = convertedTime;
        } else {
            convertedTime = time / TimeUtil.HOURS_PER_DAY;
            retentionHours = convertedTime * TimeUtil.HOURS_PER_DAY;
        }
        return convertedTime;
    }

    /**
     * Adjust retention hours based on combo boxes current values.
     */
    protected boolean handleTimeSelection() {
        int time = timeSpnr.getSelection();
        boolean modified = false;
        if (timeUnitCombo.getItem(timeUnitCombo.getSelectionIndex()).equals(
                "Days")) {
            time *= TimeUtil.HOURS_PER_DAY;
        }

        if (retentionHours != time) {
            retentionHours = time;
            modified = true;
        }
        return modified;
    }

    /**
     * Reset the modify state.
     */
    public void clearModified() {
        modifyState = false;
    }

    /**
     * 
     * @param iModifyListener
     */
    public void addModifyListener(IModifyListener iModifyListener) {
        listeners.add(iModifyListener);
    }

    /**
     * 
     * @param iModifyListener
     */
    public void removeModifyListener(IModifyListener iModifyListener) {
        listeners.remove(iModifyListener);
    }
}

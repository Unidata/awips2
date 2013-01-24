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
package com.raytheon.uf.viz.datadelivery.filter;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.datadelivery.filter.FilterImages.ExpandItemState;

/**
 * 
 * Abstract class used by the filters for the data browser. All filters must
 * extend this abstract class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2012            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class AbstractFilterComp extends Composite {

    /**
     * Expand item index.
     */
    protected int expItemIdx = -1;

    /**
     * Callback called when the filter has been updated.
     */
    protected IFilterUpdate callback = null;

    /**
     * Parent composite.
     */
    protected Composite parent;

    /**
     * Flag indicating if the filter is enabled.
     */
    protected boolean enabled = true;

    /**
     * Current state of the filter.
     */
    protected ExpandItemState currentState = ExpandItemState.NoEntries;

    /**
     * Flag indicating that the filter has changed.
     */
    protected boolean filterChanged = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param callback
     *            Callback.
     * @param idx
     *            Filter index.
     */
    public AbstractFilterComp(Composite parent, IFilterUpdate callback, int idx) {
        super(parent, SWT.BORDER);
        this.expItemIdx = idx;
        this.callback = callback;
        this.parent = parent;
    }

    /**
     * Get the flag to determine if the filter is enabled.
     * 
     * @return True if the filter is enabled, false if it is disabled.
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Set the flag that determines if the filter is enabled.
     * 
     * @param enabled
     *            Flag indicating if the filter is enabled.
     */
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
        callbackUpdate();
    }

    /**
     * Set the current state of the filter.
     * 
     * @param state
     *            The expand item state.
     */
    protected void setCurrentState(ExpandItemState state) {
        currentState = state;
        callbackUpdate();
    }

    /**
     * Get the current state of the filter.
     * 
     * @return The current state of the filter.
     */
    public ExpandItemState getCurrentState() {
        return currentState;
    }

    /**
     * Callback to inform the filter has been updated.
     */
    private void callbackUpdate() {

        if (isEnabled() == false) {
            callback.filterUpdate(expItemIdx, ExpandItemState.Disabled);
        }
        else {
            callback.filterUpdate(expItemIdx, currentState);
        }

        filterChanged = true;
    }

    /**
     * Reset the controls.
     */
    protected abstract void resetControls();

}

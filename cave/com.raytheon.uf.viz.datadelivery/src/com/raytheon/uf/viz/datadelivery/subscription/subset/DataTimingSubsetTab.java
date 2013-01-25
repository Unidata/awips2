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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.viz.datadelivery.subscription.subset.presenter.IDataTimingSubsetView;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.widgets.duallist.IUpdate;

/**
 * Date/Cycle/Forecast tab.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2012            mpduff       Initial creation.
 * Jun  4, 2012   645      jpiatt       Added tooltips.
 * Jun  6, 2012   684      jpiatt       Fixed Forecast Hour issue.
 * Jun 21, 2012   736      djohnson     Change OPERATION_STATUS to OperationStatus.
 * Jul 24, 2012   955      djohnson     Get list of cycle times and forecast hours for the whole dataset.
 * Aug 08, 2012   863      jpiatt       Added clean & dirty checks.
 * Aug 02, 2012   955      djohnson     Type-safe registry query/responses.
 * Aug 10, 2012  1002      mpduff       Implementing dataset size estimation.
 * Aug 10, 2012  1020      djohnson     Use {@link GriddedDataSet}.
 * Aug 20, 2012  0743      djohnson     Add support for doing a specific date adhoc query, sub-class for data type specific operations.
 * Aug 29, 2012  0223      mpduff       Overrode new method from interface.
 * Sep 24, 2012  1209      djohnson     Remove isValid().
 * Nov 20, 2012 1286       djohnson     Implement displayYesNoPopup.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataTimingSubsetTab extends SubsetTab
        implements IUpdate, IDataTimingSubsetView {

    /** Parent composite */
    protected final Composite parentComp;

    /** Callback for data size changes */
    private final IDataSize callback;

    /** Flag for date/cycle/forecast dirty. */
    private boolean dirty;

    private final Shell shell;

    /**
     * Constructor.
     *
     * @param parentComp
     * @param callback
     * @param shell
     */
    public DataTimingSubsetTab(Composite parentComp, IDataSize callback,
            Shell shell) {
        this.parentComp = parentComp;
        this.callback = callback;
        this.shell = shell;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDirty(boolean dirty) {
        this.dirty = dirty;
    }

    /**
     *
     * {@inheritDoc}
     */
    @Override
    public boolean isDirty() {
        return dirty;
    }

    @Override
    public void hasEntries(boolean entries) {
        // Not used
    }

    @Override
    public void selectionChanged() {
        callback.updateDataSize();
        this.dirty = true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void displayPopup(String title, String message) {
        DataDeliveryUtils.showMessage(shell, SWT.OK, title, message);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean displayOkCancelPopup(String title, String message) {
        return DataDeliveryUtils.showMessage(shell, SWT.CANCEL | SWT.OK, title,
                message) == SWT.OK;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void displayErrorPopup(String title, String message) {
        DataDeliveryUtils.showMessage(shell, SWT.ERROR,
                title, message);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean displayYesNoPopup(String title, String message) {
        return DataDeliveryUtils.showYesNoMessage(shell, title, message) == SWT.YES;
    }
}

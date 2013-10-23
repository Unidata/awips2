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
package com.raytheon.uf.viz.datadelivery.system;

import java.util.Set;
import java.util.TreeSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.widgets.ApplyCancelComposite;
import com.raytheon.viz.ui.widgets.IApplyCancelAction;

/**
 * Bandwidth settings composite
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug  6, 2013    2180     mpduff      Initial creation
 * Oct 17, 2013    2455     skorolev    Fixed a problem with Changes Applied window.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class BandwidthComposite extends Composite implements IApplyCancelAction {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BandwidthComposite.class);

    /** Available bandwidth spinner widget */
    private Spinner availBandwidthSpinner;

    /** The listener that should be used to signify changes were made **/
    private final Runnable changesWereMade = new Runnable() {
        @Override
        public void run() {
            buttonComp.enableButtons(true);
        }
    };

    /** Button composite */
    private ApplyCancelComposite buttonComp;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Composite
     * @param style
     *            Style bits
     */
    public BandwidthComposite(Composite parent, int style) {
        super(parent, style);
        init();
    }

    /**
     * Initialize the class
     */
    private void init() {
        GridLayout gl = new GridLayout(1, true);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        this.setLayout(gl);
        this.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.VERTICAL, SWT.DEFAULT, true, false);
        Composite configurationComposite = new Composite(this, SWT.NONE);
        configurationComposite.setLayout(gl);
        configurationComposite.setLayoutData(gd);

        // Label for directions
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label directionsLabel = new Label(configurationComposite, SWT.NONE);
        directionsLabel.setLayoutData(gd);
        directionsLabel
                .setText("Please enter the available bandwidth for the OPSNET network.");

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        Composite outerComp = new Composite(configurationComposite, SWT.NONE);
        outerComp.setLayoutData(gd);
        outerComp.setLayout(gl);

        // Bandwidth spinner
        gd = new GridData(165, SWT.DEFAULT);
        Label availBandwith = new Label(outerComp, SWT.NONE);
        availBandwith.setLayoutData(gd);
        availBandwith.setText("OPSNET Bandwidth (kB/s):");

        final Spinner availBandwidthSpinner = new Spinner(outerComp, SWT.BORDER);
        availBandwidthSpinner.setMinimum(0);
        availBandwidthSpinner.setMaximum(10000);
        availBandwidthSpinner.setToolTipText("Select bandwidth in Kilobytes");
        this.availBandwidthSpinner = availBandwidthSpinner;

        // Buttons
        buttonComp = new ApplyCancelComposite(this, SWT.NONE, this);

        loadConfiguration();
    }

    /**
     * Load the configuration
     */
    private void loadConfiguration() {
        DataDeliveryGUIUtils.removeListeners(availBandwidthSpinner,
                SWT.Selection, SWT.DefaultSelection);

        final int availableBandwidth = SystemRuleManager
                .getAvailableBandwidth(Network.OPSNET);

        availBandwidthSpinner.setSelection(availableBandwidth);
        availBandwidthSpinner.addSelectionListener(DataDeliveryGUIUtils
                .addValueChangedSelectionListener(availableBandwidth,
                        availBandwidthSpinner, changesWereMade));
    }

    /**
     * Save the configuration.
     * 
     * @return true if saved
     */
    private boolean saveConfiguration() {
        boolean changesApplied = false;
        final int bandwidth = availBandwidthSpinner.getSelection();

        Set<String> unscheduledSubscriptions = SystemRuleManager
                .setAvailableBandwidth(Network.OPSNET, bandwidth);
        if (unscheduledSubscriptions == null) {
            return changesApplied;
        }
        if (unscheduledSubscriptions.isEmpty()) {
            changesApplied = true;
        } else {
            Set<String> subscriptionNames = new TreeSet<String>(
                    unscheduledSubscriptions);

            StringBuilder sb = new StringBuilder(StringUtil.createMessage(
                    "Changing the bandwidth for " + Network.OPSNET
                            + " will unschedule the following subscriptions:",
                    subscriptionNames));
            sb.append(StringUtil.NEWLINE).append(StringUtil.NEWLINE);
            sb.append("Would you like to change the bandwidth anyway?");
            int response = DataDeliveryUtils.showMessage(
                    getParent().getShell(), SWT.YES | SWT.NO,
                    "Bandwidth Amount", sb.toString());
            if (response == SWT.YES) {
                changesApplied = SystemRuleManager.forceSetAvailableBandwidth(
                        Network.OPSNET, bandwidth);
                if (!changesApplied) {
                    statusHandler
                            .handle(Priority.ERROR,
                                    "Bandwidth Change",
                                    "Unable to change the bandwidth for network "
                                            + Network.OPSNET
                                            + ".  Please check the server for details.");

                }
            }
        }

        return changesApplied;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean apply() {
        if (saveConfiguration()) {
            DataDeliveryUtils.showChangesWereAppliedMessage(getShell());
            return true;
        }

        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cancel() {
        loadConfiguration();
    }
}

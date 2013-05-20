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
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * Subscription overlap configuration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2013 2000       djohnson     Initial creation
 * May 23, 2013 1650       djohnson     Reword change bandwidth message.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BandwidthTab extends SystemApplyCancelTab {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BandwidthTab.class);

    /** Available bandwidth spinner widget */
    private Spinner availBandwidthSpinner;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            The Composite holding these controls
     */
    public BandwidthTab(Composite parentComp) {
        super(parentComp);
    }

    /**
     * Get the tab text.
     * 
     * @return the tab text
     */
    @Override
    public String getTabText() {
        return "Bandwidth";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeTabComponents(Composite mainComp) {
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.VERTICAL, SWT.DEFAULT, true, false);
        Composite configurationComposite = new Composite(mainComp,
                SWT.NONE);
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
        availBandwith.setText("OPSNET Bandwidth (KB):");

        final Spinner availBandwidthSpinner = new Spinner(outerComp, SWT.BORDER);
        availBandwidthSpinner.setMinimum(0);
        availBandwidthSpinner.setMaximum(10000);
        availBandwidthSpinner.setToolTipText("Select bandwidth in Kilobytes");
        this.availBandwidthSpinner = availBandwidthSpinner;

    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void loadConfiguration() {
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
     * {@inheritDoc}
     */
    @Override
    protected boolean saveConfiguration() {
        boolean changesApplied = false;
        final int bandwidth = availBandwidthSpinner.getSelection();

        Set<Subscription> unscheduledSubscriptions = SystemRuleManager
                .setAvailableBandwidth(Network.OPSNET, bandwidth);
        if (unscheduledSubscriptions.isEmpty()) {
            changesApplied = true;
        } else {
            Set<String> subscriptionNames = new TreeSet<String>();
            for (Subscription subscription : unscheduledSubscriptions) {
                subscriptionNames.add(subscription.getName());
            }

            StringBuilder sb = new StringBuilder(StringUtil.createMessage(
                    "Changing the bandwidth for " + Network.OPSNET
                            + " will unschedule the following subscriptions:",
                    subscriptionNames));
            sb.append(StringUtil.NEWLINE).append(StringUtil.NEWLINE);
            sb.append("Would you like to change the bandwidth anyway?");
            int response = DataDeliveryUtils.showMessage(parentComp.getShell(),
                    SWT.YES | SWT.NO, "Bandwidth Amount", sb.toString());
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

}
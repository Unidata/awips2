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

import java.util.EnumMap;
import java.util.Map.Entry;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.datadelivery.service.subscription.ISubscriptionOverlapService;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.viz.ui.widgets.ApplyCancelComposite;
import com.raytheon.viz.ui.widgets.IApplyCancelAction;

/**
 * Subscription settings composite.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 07, 2013    2180    mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionComposite extends Composite implements
        IApplyCancelAction {
    /**
     * Overlap Spinners enum.
     */
    private static enum OverlapSpinners {
        PARAMETERS("Parameters:") {
            @Override
            public int getValue(SubscriptionOverlapConfig config) {
                return config.getMaxAllowedParameterDuplication();
            }

            @Override
            public void setValue(SubscriptionOverlapConfig config, int value) {
                config.setMaxAllowedParameterDuplication(value);
            }
        },
        FORECAST_HOURS("Forecast Hours:") {
            @Override
            public int getValue(SubscriptionOverlapConfig config) {
                return config.getMaxAllowedForecastHourDuplication();
            }

            @Override
            public void setValue(SubscriptionOverlapConfig config, int value) {
                config.setMaxAllowedForecastHourDuplication(value);
            }
        },
        CYCLES("Cycles:") {
            @Override
            public int getValue(SubscriptionOverlapConfig config) {
                return config.getMaxAllowedCycleDuplication();
            }

            @Override
            public void setValue(SubscriptionOverlapConfig config, int value) {
                config.setMaxAllowedCycleDuplication(value);
            }
        },
        SPATIAL("Spatial:") {
            @Override
            public int getValue(SubscriptionOverlapConfig config) {
                return config.getMaxAllowedSpatialDuplication();
            }

            @Override
            public void setValue(SubscriptionOverlapConfig config, int value) {
                config.setMaxAllowedSpatialDuplication(value);
            }
        };

        private String labelText;

        private OverlapSpinners(String labelText) {
            this.labelText = labelText;
        }

        /**
         * Get the value from the configuration for this spinner.
         * 
         * @param config
         *            the config
         * @return the value
         */
        public abstract int getValue(SubscriptionOverlapConfig config);

        /**
         * Set the configuration value from the spinner.
         * 
         * @param config
         *            the configuration
         * @param value
         *            the value
         */
        public abstract void setValue(SubscriptionOverlapConfig config,
                int value);
    }

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionComposite.class);

    /** Subscription overlap service */
    private final ISubscriptionOverlapService overlapService = DataDeliveryServices
            .getSubscriptionOverlapService();

    /** Match strategy combo. */
    private Combo matchStrategyCombo;

    /** Associates the enum spinner configuration to its spinner */
    private final EnumMap<OverlapSpinners, Spinner> spinnerMap = new EnumMap<SubscriptionComposite.OverlapSpinners, Spinner>(
            OverlapSpinners.class);

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
    public SubscriptionComposite(Composite parent, int style) {
        super(parent, style);
        init();
    }

    /**
     * Initialize class.
     */
    private void init() {
        GridLayout gl = new GridLayout(1, true);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        this.setLayout(gl);
        this.setLayoutData(gd);

        gl = new GridLayout(1, false);
        Composite configurationComposite = new Composite(this, SWT.NONE);
        configurationComposite.setLayout(gl);

        // Label for directions
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label directionsLabel = new Label(configurationComposite, SWT.NONE);
        directionsLabel.setLayoutData(gd);
        directionsLabel
                .setText("Please select a percentage of common items between two "
                        + "\nsubscriptions that would cause the subscriptions to be "
                        + "\nconsidered overlapping.\n");

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        Composite outerComp = new Composite(configurationComposite, SWT.NONE);
        outerComp.setLayoutData(gd);
        outerComp.setLayout(gl);

        for (final OverlapSpinners overlapSpinner : OverlapSpinners.values()) {

            gl = new GridLayout(2, false);
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

            Composite spinnerComp = new Composite(outerComp, SWT.NONE);
            spinnerComp.setLayoutData(gd);
            spinnerComp.setLayout(gl);

            gd = new GridData(100, SWT.DEFAULT);
            Label label = new Label(spinnerComp, SWT.NONE);
            label.setLayoutData(gd);
            label.setText(overlapSpinner.labelText);

            gd = new GridData(100, SWT.DEFAULT);
            final Spinner spinner = new Spinner(spinnerComp, SWT.BORDER);
            spinner.setMinimum(0);
            spinner.setMaximum(100);
            spinnerMap.put(overlapSpinner, spinner);
        }

        Composite matchStrategyComposite = new Composite(outerComp, SWT.NONE);
        gl = new GridLayout(2, false);
        matchStrategyComposite.setLayout(gl);

        gd = new GridData(100, SWT.DEFAULT);
        Label label = new Label(matchStrategyComposite, SWT.NONE);
        label.setLayoutData(gd);
        label.setText("Match:");

        // Match Strategy
        matchStrategyCombo = new Combo(matchStrategyComposite, SWT.READ_ONLY);
        matchStrategyCombo
                .setToolTipText("Select the manner in which the rules should consider two subscriptions to overlap");

        for (SubscriptionOverlapMatchStrategy matchStrategy : SubscriptionOverlapMatchStrategy
                .values()) {
            matchStrategyCombo.add(matchStrategy.getDisplayString());
        }

        // Buttons
        buttonComp = new ApplyCancelComposite(this, SWT.NONE, this);

        loadConfiguration();
    }

    /**
     * Load configuration data
     */
    private void loadConfiguration() {
        SubscriptionOverlapConfig config;
        try {
            config = overlapService.readConfig();
        } catch (LocalizationException e) {
            statusHandler
                    .handle(Priority.ERROR,
                            "Unable to load the subscription overlap rules.  "
                                    + "Defaulting to configuration that will never overlap.",
                            e);
            config = SubscriptionOverlapConfig.NEVER_OVERLAPS;
        }

        for (Entry<OverlapSpinners, Spinner> entry : spinnerMap.entrySet()) {
            final Spinner spinner = entry.getValue();
            final int initialValue = entry.getKey().getValue(config);

            DataDeliveryGUIUtils.removeListeners(spinner, SWT.Selection,
                    SWT.DefaultSelection);

            spinner.setSelection(initialValue);
            spinner.addSelectionListener(DataDeliveryGUIUtils
                    .addValueChangedSelectionListener(initialValue, spinner,
                            changesWereMade));
        }

        DataDeliveryGUIUtils.removeListeners(matchStrategyCombo, SWT.Selection,
                SWT.DefaultSelection);

        final int indexOfConfigValue = matchStrategyCombo.indexOf(config
                .getMatchStrategy().getDisplayString());
        matchStrategyCombo.select(indexOfConfigValue);
        matchStrategyCombo.addSelectionListener(DataDeliveryGUIUtils
                .addValueChangedSelectionListener(indexOfConfigValue,
                        matchStrategyCombo, changesWereMade));
    }

    /**
     * Save configuration data
     * 
     * @return true if saved
     * @throws LocalizationException
     */
    private boolean saveConfiguration() throws LocalizationException {
        SubscriptionOverlapConfig config = new SubscriptionOverlapConfig();

        for (Entry<OverlapSpinners, Spinner> entry : spinnerMap.entrySet()) {
            final OverlapSpinners key = entry.getKey();
            key.setValue(config, entry.getValue().getSelection());
        }

        config.setMatchStrategy(SubscriptionOverlapMatchStrategy.values()[matchStrategyCombo
                .getSelectionIndex()]);

        overlapService.writeConfig(config);

        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean apply() {
        try {
            return saveConfiguration();
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to save configuration changes.", e);
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
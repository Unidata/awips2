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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.service.subscription.GridSubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * Gridded subscription overlap rules composite.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2013    2386     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GridSubscriptionRuleComposite extends SubscriptionComposite {
    /** Forecast hour spinner */
    private Spinner fcstHrSpinner;

    /** Cycles spinner */
    private Spinner cycleSpinner;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Composite
     */
    public GridSubscriptionRuleComposite(Composite parent) {
        super(parent, DataType.GRID);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initTypeSpecific(Group grp) {
        grp.setText(" Gridded Attributes ");

        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite comp = new Composite(grp, SWT.NONE);
        comp.setLayout(gl);
        comp.setLayoutData(gd);

        gl = new GridLayout(2, false);
        gl.marginRight = 10;
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        Composite fcstComp = new Composite(comp, SWT.NONE);
        fcstComp.setLayout(gl);
        fcstComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Label fcstLbl = new Label(fcstComp, SWT.NONE);
        fcstLbl.setText("Forecast Hours:");
        fcstLbl.setLayoutData(gd);

        fcstHrSpinner = new Spinner(fcstComp, SWT.BORDER);
        fcstHrSpinner.setMinimum(0);
        fcstHrSpinner.setMaximum(100);
        fcstHrSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setButtonsEnabled();
            }
        });

        gl = new GridLayout(2, false);
        gl.marginRight = 10;
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        Composite cycleComp = new Composite(comp, SWT.NONE);
        cycleComp.setLayout(gl);
        cycleComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Label cycleLbl = new Label(cycleComp, SWT.NONE);
        cycleLbl.setText("Cycles:");
        cycleLbl.setLayoutData(gd);

        cycleSpinner = new Spinner(cycleComp, SWT.BORDER);
        cycleSpinner.setMinimum(0);
        cycleSpinner.setMaximum(100);
        cycleSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setButtonsEnabled();
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected final void loadConfiguration() {
        super.loadConfiguration();
        GridSubscriptionOverlapConfig config = (GridSubscriptionOverlapConfig) ruleManager
                .getSubscriptionOverlapRules(DATA_TYPE);
        if (config != null) {
            this.cycleSpinner.setSelection(config
                    .getMaxAllowedCycleDuplication());
            this.fcstHrSpinner.setSelection(config
                    .getMaxAllowedForecastHourDuplication());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    boolean saveConfiguration() throws LocalizationException {
        GridSubscriptionOverlapConfig config = (GridSubscriptionOverlapConfig) ruleManager
                .getSubscriptionOverlapRules(DATA_TYPE);
        config.setMaxAllowedParameterDuplication(commonComp.getParameterValue());
        config.setMaxAllowedSpatialDuplication(commonComp.getSpatialValue());
        config.setMaxAllowedCycleDuplication(this.cycleSpinner.getSelection());
        config.setMaxAllowedForecastHourDuplication(this.fcstHrSpinner
                .getSelection());
        config.setMatchStrategy((SubscriptionOverlapMatchStrategy) matchStrategyCombo
                .getData(matchStrategyCombo.getText()));
        boolean applyAll = commonComp.isApplyAll();
        if (applyAll) {
            if (ruleManager.saveOverlapRule(config, DATA_TYPE)) {
                return super.applyAll(DATA_TYPE);
            } else {
                return false;
            }
        }

        return ruleManager.saveOverlapRule(config, DATA_TYPE);
    }
}

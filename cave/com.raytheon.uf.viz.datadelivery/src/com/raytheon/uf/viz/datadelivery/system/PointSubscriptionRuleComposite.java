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
import com.raytheon.uf.common.datadelivery.service.subscription.PointSubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * Point subscription overlap rules composite.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2013   2386      mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointSubscriptionRuleComposite extends SubscriptionComposite {
    /** Time spinner */
    private Spinner timeSpinner;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Composite
     */
    public PointSubscriptionRuleComposite(Composite parent) {
        super(parent, DataType.POINT);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initTypeSpecific(Group grp) {
        grp.setText(" Point Attributes ");

        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite comp = new Composite(grp, SWT.NONE);
        comp.setLayout(gl);
        comp.setLayoutData(gd);

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite attrComp = new Composite(comp, SWT.NONE);
        attrComp.setLayout(gl);
        attrComp.setLayoutData(gd);

        Label label = new Label(attrComp, SWT.NONE);
        label.setText("Time:");

        timeSpinner = new Spinner(attrComp, SWT.BORDER);
        timeSpinner.setMinimum(0);
        timeSpinner.setMaximum(100);
        timeSpinner.addSelectionListener(new SelectionAdapter() {
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
        PointSubscriptionOverlapConfig config = (PointSubscriptionOverlapConfig) ruleManager
                .getSubscriptionOverlapRules(DATA_TYPE);
        if (config != null) {
            timeSpinner.setSelection(config.getMaxAllowedTimeDuplication());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    boolean saveConfiguration() throws LocalizationException {
        PointSubscriptionOverlapConfig config = (PointSubscriptionOverlapConfig) ruleManager
                .getSubscriptionOverlapRules(DATA_TYPE);
        config.setMatchStrategy((SubscriptionOverlapMatchStrategy) matchStrategyCombo
                .getData(matchStrategyCombo.getText()));
        config.setMaxAllowedParameterDuplication(this.commonComp
                .getParameterValue());
        config.setMaxAllowedSpatialDuplication(commonComp.getSpatialValue());
        config.setMaxAllowedTimeDuplication(timeSpinner.getSelection());
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

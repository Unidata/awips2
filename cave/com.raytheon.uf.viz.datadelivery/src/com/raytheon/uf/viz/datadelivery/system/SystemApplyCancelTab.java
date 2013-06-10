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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * Base class for a System Management tab that has an apply/cancel button
 * combination.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2013 2000       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class SystemApplyCancelTab extends SystemTab {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionTab.class);

    /** Priority Composite */
    private Composite mainComp;

    /** Apply changes button. */
    protected Button applyButton;

    /** Cancel changes button. */
    protected Button cancelButton;

    /** Button Height. */
    private final int buttonHeight = SWT.DEFAULT;

    /** Button Width. */
    private final int buttonWidth = 70;

    /** The listener that should be used to signify changes were made **/
    protected final Runnable changesWereMade = new Runnable() {
        @Override
        public void run() {
            enableButtons();
        }
    };

    /**
     * Constructor.
     * 
     * @param parentComp
     */
    protected SystemApplyCancelTab(Composite parentComp) {
        super(parentComp);
    }

    /**
     * Initialize the tab.
     */
    @Override
    public void init() {
        createBaseTab();
        createSideButtons();
        loadConfiguration();
    }

    /**
     * Create the base tab.
     */
    private void createBaseTab() {
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);

        mainComp = new Composite(parentComp, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        initializeTabComponents(mainComp);
    }

    /**
     * Create the move up/down controls
     */
    private void createSideButtons() {
        GridData actionData = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        GridLayout actionLayout = new GridLayout(1, false);
        Composite actionComp = new Composite(mainComp, SWT.NONE);
        actionComp.setLayout(actionLayout);
        actionComp.setLayoutData(actionData);

        GridData btnData = new GridData(buttonWidth, buttonHeight);
        btnData.horizontalAlignment = SWT.RIGHT;

        applyButton = new Button(actionComp, SWT.PUSH);
        applyButton.setText("Apply");
        applyButton.setLayoutData(btnData);
        applyButton.setToolTipText("Apply changes");
        applyButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (saveConfigurationChanges()) {
                    disableButtons();
                }
            }
        });

        btnData = new GridData(buttonWidth, buttonHeight);
        btnData.horizontalAlignment = SWT.RIGHT;

        cancelButton = new Button(actionComp, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.setLayoutData(btnData);
        cancelButton.setToolTipText("Cancel any changes");
        cancelButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loadConfiguration();
                disableButtons();
            }
        });

        btnData = new GridData(buttonWidth, buttonHeight);
        btnData.horizontalAlignment = SWT.RIGHT;

        Button invisibleButtonToAlignWithThreeButtonTabs = new Button(actionComp, SWT.PUSH);
        invisibleButtonToAlignWithThreeButtonTabs.setText("");
        invisibleButtonToAlignWithThreeButtonTabs.setLayoutData(btnData);
        invisibleButtonToAlignWithThreeButtonTabs.setVisible(false);

        disableButtons();
    }

    /**
     * Save the configuration changes.
     * 
     * @return true if the configuration changes were saved
     */
    private boolean saveConfigurationChanges() {
        boolean configurationSaved = false;
        try {
            configurationSaved = saveConfiguration();

            if (configurationSaved) {
                DataDeliveryUtils.showChangesWereAppliedMessage(parentComp
                        .getShell());
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to save configuration changes.", e);
        }
        return configurationSaved;
    }

    private void enableButtons() {
        applyButton.setEnabled(true);
        cancelButton.setEnabled(true);
    }

    private void disableButtons() {
        applyButton.setEnabled(false);
        cancelButton.setEnabled(false);
    }

    /**
     * Create the main tab components
     * 
     * @param mainComp
     *            the main tab composite
     */
    protected abstract void initializeTabComponents(Composite mainComp);

    /**
     * Load the configuration.
     */
    protected abstract void loadConfiguration();

    /**
     * Save the user's configuration changes.
     * 
     * @return true if the configuration was actually saved
     * 
     * @throws Exception
     *             if the configuration cannot be saved
     */
    protected abstract boolean saveConfiguration() throws Exception;
}

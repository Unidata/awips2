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
package com.raytheon.viz.gfe.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.Parm.CreateFromScratchMode;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The create from scratch dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Feb 26, 2008					Eric Babin Initial Creation
 * Oct 24, 2012 1287       rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class CreateFromScratchDialog extends CaveJFACEDialog {

    private Composite top;

    private Button defaultButton, pickupButton;

    private Scale intervalScale, durationScale;

    private Label durationLabel, intervalLabel;

    private boolean displayInterval;

    private boolean displayDuration;

    private int quantum;

    private int createDuration;

    private int createInterval;

    public CreateFromScratchDialog(Shell parent) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);

    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(2, false);
        top.setLayout(layout);

        initializeComponents();

        return top;
    }

    private void initializeComponents() {

        defaultButton = new Button(top, SWT.RADIO);
        defaultButton.setText("Default Value");
        GridData data = new GridData();
        data.horizontalSpan = 2;
        defaultButton.setLayoutData(data);
        defaultButton.setSelection(true);

        pickupButton = new Button(top, SWT.RADIO);
        pickupButton.setText("Pick Up Value");
        pickupButton.setLayoutData(data);

        commonTC();

        if (displayInterval) {
            Label lab = new Label(top, SWT.NONE);
            lab.setText("Creation Interval In Hours");
            data = new GridData();
            data.horizontalSpan = 2;
            lab.setLayoutData(data);

            intervalScale = new Scale(top, SWT.HORIZONTAL);
            intervalScale.setMinimum(quantum);
            intervalScale.setMaximum(24);
            intervalScale.setIncrement(quantum);
            intervalScale.setPageIncrement(quantum);
            intervalScale.setSelection(this.createInterval);
            data = new GridData(200, SWT.DEFAULT);
            intervalScale.setLayoutData(data);
            intervalScale.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent arg0) {
                    intervalScaleChanged();
                }
            });
            intervalLabel = new Label(top, SWT.NONE);
            data = new GridData(30, SWT.DEFAULT);
            intervalLabel.setLayoutData(data);
            intervalLabel
                    .setText(Integer.toString(intervalScale.getSelection()));

        }

        if (displayDuration) {
            Label lab2 = new Label(top, SWT.NONE);
            lab2.setText("Duration of Grids In Hours");
            data = new GridData();
            data.horizontalSpan = 2;
            lab2.setLayoutData(data);

            durationScale = new Scale(top, SWT.HORIZONTAL);
            durationScale.setMinimum(quantum);
            durationScale.setMaximum(24);
            durationScale.setSelection(this.createDuration);
            data = new GridData(200, SWT.DEFAULT);
            durationScale.setLayoutData(data);
            durationScale.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent arg0) {
                    durationScaleChanged();
                }
            });
            durationLabel = new Label(top, SWT.NONE);
            data = new GridData(30, SWT.DEFAULT);
            durationLabel.setLayoutData(data);
            durationLabel
                    .setText(Integer.toString(durationScale.getSelection()));
        }
    }

    private void intervalScaleChanged() {
        intervalLabel.setText(Integer.toString(intervalScale.getSelection()));

        if (displayDuration) {
            durationScale.setSelection(intervalScale.getSelection());
            durationLabel
                    .setText(Integer.toString(durationScale.getSelection()));
        }
    }

    private void durationScaleChanged() {
        if (durationScale.getSelection() >= intervalScale.getSelection()) {
            durationScale.setSelection(intervalScale.getSelection());
        }
        durationLabel.setText(Integer.toString(durationScale.getSelection()));
    }

    private void commonTC() {
        Parm[] parms = DataManager.getCurrentInstance().getParmManager()
                .getSelectedParms();

        int minRepeatInterval = 24 * 3600;
        this.displayInterval = true;
        this.displayDuration = true;
        int compositeRepeat = 0;
        int compositeDuration = 0;

        for (Parm parm : parms) {
            if (parm.getGridInfo().getTimeConstraints().getRepeatInterval() != parm
                    .getGridInfo().getTimeConstraints().getDuration()) {
                displayDuration = false;
            }
            int repeatInterval = parm.getGridInfo().getTimeConstraints()
                    .getRepeatInterval();
            if (repeatInterval < minRepeatInterval) {
                minRepeatInterval = repeatInterval;
            }
            if (compositeRepeat == 0) {
                compositeRepeat = repeatInterval;
                compositeDuration = parm.getGridInfo().getTimeConstraints()
                        .getDuration();
            }
            if (compositeRepeat != repeatInterval) {
                displayInterval = false;
            }
            if (compositeDuration != parm.getGridInfo().getTimeConstraints()
                    .getDuration()) {
                displayDuration = false;
            }
        }

        // If the minimum repeat interval is == 24 hours, the interval slider
        // lets the user select intervals up to 100 hours, but the dialog
        // ignores them and uses 24 hours.
        // It's better to not show the slider at all in this case.
        if (minRepeatInterval >= 24 * 3600) {
            displayInterval = false;
        }

        this.quantum = minRepeatInterval / 3600;
        this.createDuration = compositeDuration / 3600;
        this.createInterval = compositeRepeat / 3600;

        int configInterval = GFEPreference
                .getIntPreference("CreateScratchDefaultInterval");
        int configDuration = GFEPreference
                .getIntPreference("CreateScratchDefaultDuration");

        // Sanity check config values.
        configInterval = Math.max(0, configInterval);
        configInterval = Math.min(configInterval, 24);

        configDuration = Math.max(0, configDuration);
        configDuration = Math.min(configDuration, configInterval);

        // At this point, createInterval is the minimum allowed by the parms.
        // We can't lower it, but we can raise it to the configured value.
        if (createInterval < configInterval) {
            createInterval = configInterval;
        }

        // At this point, createDuration is the minimum allowed by the parms.
        // We can't lower it, but we can raise it to the configured value.
        if (createDuration < configDuration) {
            createDuration = configDuration;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @Override
    protected void okPressed() {
        try {
            CreateFromScratchMode mode = Parm.CreateFromScratchMode.DEFAULT;
            if (pickupButton.getSelection()) {
                mode = CreateFromScratchMode.PICKUP;
            }

            int createInterval = 0;
            if (displayInterval) {
                createInterval = intervalScale.getSelection();
            }
            int createDuration = 0;
            if (displayDuration) {
                createDuration = durationScale.getSelection();
            }

            DataManager
                    .getCurrentInstance()
                    .getParmOp()
                    .createFromScratchSelected(mode, createInterval * 3600,
                            createDuration * 3600);

        } catch (GFEOperationFailedException e) {
            e.printStackTrace();
        }

        super.okPressed();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);

        shell.setText("Create From Scratch Dialog");
    }

}

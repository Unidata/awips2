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

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
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
 * Feb 26, 2008            Eric Babin  Initial Creation
 * Oct 24, 2012 1287       rferrel     Changes for non-blocking dialog.
 * Nov 30, 2015 5133       kbisanz     In durationScaleChanged(), ensure
 *                                     intervalScale is valid before
 *                                     accessing it.
 * Jul 28, 2016 5554       dgilling    Display error message if no parms or valid TRs 
 *                                     are selected, code cleanup.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class CreateFromScratchDialog extends CaveJFACEDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final DataManager dataMgr;

    private Button defaultButton;

    private Button pickupButton;

    private Scale intervalScale;

    private Scale durationScale;

    private Label durationLabel;

    private Label intervalLabel;

    private boolean displayInterval;

    private boolean displayDuration;

    private int quantum;

    private int createDuration;

    private int createInterval;

    public CreateFromScratchDialog(Shell parent, DataManager dataMgr) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);

        this.dataMgr = dataMgr;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);
        top.setLayout(new GridLayout(2, false));

        initializeComponents(top);

        return top;
    }

    private void initializeComponents(Composite parent) {
        commonTC();

        if (!displayDuration && !displayInterval) {
            Label label = new Label(parent, SWT.CENTER);
            label.setText("No parms  are selected or no valid time range is selected.");
            GridData data = new GridData(SWT.CENTER, SWT.CENTER, true, true);
            data.horizontalSpan = 2;
            label.setLayoutData(data);
            return;
        }

        defaultButton = new Button(parent, SWT.RADIO);
        defaultButton.setText("Default Value");
        GridData data = new GridData(SWT.LEFT, SWT.TOP, true, false);
        data.horizontalSpan = 2;
        defaultButton.setLayoutData(data);
        defaultButton.setSelection(true);

        pickupButton = new Button(parent, SWT.RADIO);
        pickupButton.setText("Pick Up Value");
        data = new GridData(SWT.LEFT, SWT.TOP, true, false);
        data.horizontalSpan = 2;
        pickupButton.setLayoutData(data);

        if (displayInterval) {
            Label lab = new Label(parent, SWT.NONE);
            lab.setText("Creation Interval In Hours");
            data = new GridData(SWT.LEFT, SWT.CENTER, true, false);
            data.horizontalSpan = 2;
            lab.setLayoutData(data);

            intervalScale = new Scale(parent, SWT.HORIZONTAL);
            intervalScale.setMinimum(quantum);
            intervalScale.setMaximum(TimeUtil.HOURS_PER_DAY);
            intervalScale.setIncrement(quantum);
            intervalScale.setPageIncrement(quantum);
            intervalScale.setSelection(this.createInterval);
            intervalScale.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                    true, false));
            intervalScale.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent arg0) {
                    intervalScaleChanged();
                }
            });

            intervalLabel = new Label(parent, SWT.NONE);
            intervalLabel
                    .setText(Integer.toString(intervalScale.getSelection()));

            GC gc = new GC(intervalLabel);
            int charWidth = gc.getFontMetrics().getAverageCharWidth();
            gc.dispose();
            data = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
            data.widthHint = charWidth * 3;
            intervalLabel.setLayoutData(data);
        }

        if (displayDuration) {
            Label lab2 = new Label(parent, SWT.NONE);
            lab2.setText("Duration of Grids In Hours");
            data = new GridData(SWT.LEFT, SWT.CENTER, true, false);
            data.horizontalSpan = 2;
            lab2.setLayoutData(data);

            durationScale = new Scale(parent, SWT.HORIZONTAL);
            durationScale.setMinimum(quantum);
            durationScale.setMaximum(TimeUtil.HOURS_PER_DAY);
            durationScale.setSelection(this.createDuration);
            durationScale.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                    true, false));
            durationScale.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent arg0) {
                    durationScaleChanged();
                }
            });
            durationLabel = new Label(parent, SWT.NONE);
            durationLabel
                    .setText(Integer.toString(durationScale.getSelection()));

            GC gc = new GC(durationLabel);
            int charWidth = gc.getFontMetrics().getAverageCharWidth();
            gc.dispose();
            data = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
            data.widthHint = charWidth * 3;
            durationLabel.setLayoutData(data);
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
        if (displayInterval
                && durationScale.getSelection() >= intervalScale.getSelection()) {
            durationScale.setSelection(intervalScale.getSelection());
        }
        durationLabel.setText(Integer.toString(durationScale.getSelection()));
    }

    private void commonTC() {
        this.displayInterval = true;
        this.displayDuration = true;

        Parm[] parms = dataMgr.getParmManager().getSelectedParms();

        if (ArrayUtils.isEmpty(parms)) {
            displayInterval = false;
            displayDuration = false;
            return;
        }

        boolean isAnySelectedTR = false;
        for (Parm parm : parms) {
            if (parm.getParmState().getSelectedTimeRange().isValid()) {
                isAnySelectedTR = true;
                break;
            }
        }
        if (!isAnySelectedTR) {
            displayInterval = false;
            displayDuration = false;
            return;
        }

        int minRepeatInterval = TimeUtil.HOURS_PER_DAY
                * TimeUtil.SECONDS_PER_HOUR;
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

        /*
         * If the minimum repeat interval is == 24 hours, the interval slider
         * lets the user select intervals up to 100 hours, but the dialog
         * ignores them and uses 24 hours. It's better to not show the slider at
         * all in this case.
         */
        if (minRepeatInterval >= TimeUtil.HOURS_PER_DAY
                * TimeUtil.SECONDS_PER_HOUR) {
            displayInterval = false;
        }

        this.quantum = minRepeatInterval / TimeUtil.SECONDS_PER_HOUR;
        this.createDuration = compositeDuration / TimeUtil.SECONDS_PER_HOUR;
        this.createInterval = compositeRepeat / TimeUtil.SECONDS_PER_HOUR;

        int configInterval = GFEPreference
                .getIntPreference("CreateScratchDefaultInterval");
        int configDuration = GFEPreference
                .getIntPreference("CreateScratchDefaultDuration");

        // Sanity check config values.
        configInterval = Math.max(0, configInterval);
        configInterval = Math.min(configInterval, TimeUtil.HOURS_PER_DAY);

        configDuration = Math.max(0, configDuration);
        configDuration = Math.min(configDuration, configInterval);

        /*
         * At this point, createInterval is the minimum allowed by the parms. We
         * can't lower it, but we can raise it to the configured value.
         */
        if (createInterval < configInterval) {
            createInterval = configInterval;
        }

        /*
         * At this point, createDuration is the minimum allowed by the parms. We
         * can't lower it, but we can raise it to the configured value.
         */
        if (createDuration < configDuration) {
            createDuration = configDuration;
        }
    }

    @Override
    protected void okPressed() {
        if (displayDuration || displayInterval) {
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

                statusHandler.debug("OK: mode=" + mode + ", interval="
                        + createInterval + ", duration=" + createDuration);
                dataMgr.getParmOp().createFromScratchSelected(mode,
                        createInterval * TimeUtil.SECONDS_PER_HOUR,
                        createDuration * TimeUtil.SECONDS_PER_HOUR);

            } catch (GFEOperationFailedException e) {
                statusHandler.error("Error creating scratch grids.", e);
            }
        }

        super.okPressed();
    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);

        shell.setText("Create From Scratch Dialog");
    }
}

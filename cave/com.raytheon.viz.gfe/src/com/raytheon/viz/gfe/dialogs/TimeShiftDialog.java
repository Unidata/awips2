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

import java.text.DecimalFormat;
import java.text.FieldPosition;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.edex.util.MathUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.LabeledScale;

/**
 * The time shift dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Feb 26, 2008					Eric Babin Initial Creation
 * Oct 30, 2012 1298       rferrel     Code clean up for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class TimeShiftDialog extends CaveJFACEDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TimeShiftDialog.class);

    private final int SECONDS_PER_HOUR = 3600;

    private Composite top;

    private Button copyButton, moveButton;

    private LabeledScale intervalScale;

    private int interval = 1;

    private DataManager dataManager;

    private ScalingFormatter formatter;

    public TimeShiftDialog(Shell parent, DataManager dataManager) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
        this.dataManager = dataManager;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        Parm[] parms = this.dataManager.getParmManager().getSelectedParms();
        if (parms == null || parms.length == 0) {
            Label label = new Label(top, SWT.CENTER);
            GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
            label.setLayoutData(gd);
            label.setText("No Grids selected.");
        } else {
            initializeComponents();
        }

        return top;
    }

    private void initializeComponents() {
        GridLayout layout = (GridLayout) top.getLayout();
        layout.verticalSpacing = 0;

        Group group1 = new Group(top, SWT.SHADOW_NONE);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        group1.setLayoutData(layoutData);
        layout = new GridLayout(1, false);
        layout.verticalSpacing = 0;
        layout.marginWidth = 0;
        group1.setLayout(layout);

        copyButton = new Button(group1, SWT.RADIO);
        copyButton.setText("Copy");
        copyButton.setSelection(true);

        moveButton = new Button(group1, SWT.RADIO);
        moveButton.setText("Move");

        Group group2 = new Group(top, SWT.SHADOW_NONE);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        group2.setLayoutData(layoutData);
        layout = new GridLayout(1, false);
        layout.verticalSpacing = 0;
        layout.marginWidth = 0;
        group2.setLayout(layout);
        group2.setText("Time Shift In Hours");

        int quantum = commonQuantum();

        layoutData = new GridData(481, SWT.DEFAULT);
        intervalScale = new LabeledScale(group2);
        intervalScale.setMinimum(0);
        intervalScale.setMaximum(480 / quantum);
        intervalScale.setIncrement(1);
        intervalScale.setPageIncrement(1);
        intervalScale.setLayoutData(layoutData);
        intervalScale.setSelection(intervalScale.getMaximum() / 2);

        formatter = new ScalingFormatter();
        formatter.setOffset(-intervalScale.getMaximum() / 2);
        formatter.setScale(quantum);
        intervalScale.setFormatter(formatter);
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

        shell.setText("Time Shift Dialog");
    }

    public int getInterval() {
        return interval;
    }

    private int commonQuantum() {
        Parm[] parms = this.dataManager.getParmManager().getSelectedParms();

        int commonRepeatInterval = 1;

        for (Parm parm : parms) {
            int repeatInterval = parm.getGridInfo().getTimeConstraints()
                    .getRepeatInterval();
            commonRepeatInterval = MathUtil.lcm(commonRepeatInterval,
                    repeatInterval);
        }

        return commonRepeatInterval / SECONDS_PER_HOUR;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @Override
    protected void okPressed() {
        if (formatter != null) {
            // Tell the dataManager about the time shift
            int secondsToShift = (int) formatter.getScaledValue(intervalScale
                    .getSelection()) * SECONDS_PER_HOUR;
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.handle(Priority.DEBUG,
                        "Time Shift " + copyButton.getSelection() + " "
                                + secondsToShift);
            }
            this.dataManager.getParmOp().timeShift(secondsToShift,
                    copyButton.getSelection());
        }
        super.okPressed();
    }

    private static class ScalingFormatter extends DecimalFormat {
        private static final long serialVersionUID = 1L;

        int offset = 0;

        int scale = 1;

        /*
         * (non-Javadoc)
         * 
         * @see java.text.DecimalFormat#format(long, java.lang.StringBuffer,
         * java.text.FieldPosition)
         */
        @Override
        public StringBuffer format(long number, StringBuffer result,
                FieldPosition fieldPosition) {
            return super.format(getScaledValue(number), result, fieldPosition);
        }

        public long getScaledValue(long number) {
            return (number + offset) * scale;
        }

        /**
         * @param offset
         *            the offset to set
         */
        public void setOffset(int offset) {
            this.offset = offset;
        }

        /**
         * @param scale
         *            the scale to set
         */
        public void setScale(int scale) {
            this.scale = scale;
        }

    }
}

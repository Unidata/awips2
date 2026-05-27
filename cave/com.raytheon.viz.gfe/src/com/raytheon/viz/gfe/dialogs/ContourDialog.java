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

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog for getting contour intervals.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 5, 2009            randerso     Initial creation
 * Oct 30, 2012 1298       rferrel     Code cleanup for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ContourDialog extends CaveJFACEDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContourDialog.class);

    private final int RESET_ID = IDialogConstants.CLIENT_ID;

    private Parm parm;

    private Text intervalText;

    private float defaultInterval = 10;

    private float interval;

    private float precision;

    /**
     * @param parentShell
     */
    public ContourDialog(Shell parentShell, Parm parm) {
        super(parentShell);
        this.parm = parm;
        this.interval = getInterval();
        this.precision = (float) Math.pow(10.0, -parm.getGridInfo()
                .getPrecision());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Contour Interval for "
                + parm.getGridInfo().getParmID().getCompositeName());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(2, false);
        top.setLayout(layout);
        GridData gridData;

        if (this.interval <= 0) {
            Label warning = new Label(top, SWT.NONE);
            gridData = new GridData(SWT.FILL, SWT.TOP, true, false, 2, 1);
            warning.setLayoutData(gridData);
            warning.setText("WARNING -- NON-LINEAR CONTOUR VALUES EXIST!\n"
                    + "Click Cancel to keep pre-defined values.\n"
                    + "Click OK to use the specified Interval.\n");
            this.interval = this.defaultInterval;
        }

        Label lab1 = new Label(top, SWT.NONE);
        lab1.setText("Interval:");

        intervalText = new Text(top, SWT.BORDER | SWT.SINGLE);
        intervalText.setText(Float.toString(this.interval));
        gridData = new GridData(SWT.FILL, SWT.TOP, true, false);
        intervalText.setLayoutData(gridData);

        return top;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
                true);
        Button resetButton = createButton(parent, RESET_ID, "Reset", false);
        resetButton.setEnabled(!parm.getDisplayAttributes().getContourValues()
                .equals(parm.getDisplayAttributes().getBaseContourValues()));
        resetButton.setToolTipText("Restore to default values.");
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case RESET_ID:
            parm.getDisplayAttributes().setContourValues(
                    parm.getDisplayAttributes().getBaseContourValues());
            super.okPressed();
            break;
        case IDialogConstants.OK_ID:
            try {
                try {
                    float interval = Float.parseFloat(intervalText.getText());
                    if (interval < this.precision) {
                        MessageDialog.openError(getParentShell(),
                                "Invalid Interval",
                                "Can't be set to less than " + this.precision);
                    } else {
                        parm.getDisplayAttributes()
                                .setContourInterval(interval);
                        super.okPressed();
                    }
                } catch (NumberFormatException e) {
                    MessageDialog.openError(getParentShell(),
                            "Numeric Required",
                            "Interval Must be Numeric Value");
                }
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM, "");
                break;
            }
            break;
        default:
            super.buttonPressed(buttonId);
        }
    }

    private float getInterval() {
        float[] cValues = parm.getDisplayAttributes().getContourValues();
        if (cValues.length < 2) {
            return this.defaultInterval;
        }
        // Find the interval between values
        // Check to see if uniform throughout values
        // If not, set interval to None
        boolean linear = true;
        float interval = cValues[1] - cValues[0];
        defaultInterval = interval;
        float prevValue = cValues[1];
        for (int i = 2; i < cValues.length; i++) {
            if ((cValues[i] - prevValue) != interval) {
                linear = false;
                break;
            }
            prevValue = cValues[i];
        }
        if (!linear) {
            return -1.0f;
        } else {
            return interval;
        }
    }
}

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
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.smarttool.SmartToolConstants;
import com.raytheon.viz.gfe.smarttool.SmartUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog to adjust a grid's delta value.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2011            dgilling     Initial creation
 * Nov 13, 2012 1298       rferrel     Code clean up for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SetDeltaDialog extends CaveJFACEDialog implements
        IDisplayedParmListChangedListener, IActivatedParmChangedListener {

    private static SetDeltaDialog dialog;

    private final int ADJUST_DOWN = 97;

    private final int ADJUST_UP = 98;

    private DataManager dataManager;

    private Parm parm;

    private Composite dialogFrame;

    private Label label;

    private Scale scale;

    private Label entryLabel;

    private Text entryField;

    private float origValue;

    private float res;

    private final SelectionAdapter scaleListener = new SelectionAdapter() {

        @Override
        public void widgetSelected(SelectionEvent e) {
            float value = scale.getSelection() * res;
            int prec = parm.getGridInfo().getPrecision() + 1;
            String valueString = String.format("%-6." + prec + "f", value);
            entryField.setText(valueString.trim());
            setNewDelta();
        }
    };

    private final SelectionAdapter entryListener = new SelectionAdapter() {

        @Override
        public void widgetDefaultSelected(SelectionEvent e) {
            float value = getEntry();
            if (!Float.isNaN(value)) {
                // in AWIPS1, it appears that setting a new value fired the
                // callback that we have ported into scaleListener. With SWT,
                // programmatically changing the Scale's value does not fire the
                // SelectionEvent, so this is performed manually. This is only
                // actually useful/important when setting values that aren't
                // multiple of res. In this case, the value is rounded down to
                // the closest multiple of res.
                scale.setSelection((int) (value / res));
                scaleListener.widgetSelected(e);
            }
            setNewDelta();
        }
    };

    /**
     * Allow only one instance of the dialog to exist at any given time.
     */
    public static void openDialog() {
        if (dialog == null) {
            Shell parent = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            DataManager dataManager = DataManager.getCurrentInstance();
            dialog = new SetDeltaDialog(parent, dataManager);
            dialog.setBlockOnOpen(false);
        }
        dialog.open();
    }

    /**
     * Private use the static method openDialog.
     * 
     * @param parent
     * @param dataManager
     */
    private SetDeltaDialog(Shell parent, DataManager dataManager) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.RESIZE);
        this.dataManager = dataManager;
        this.parm = dataManager.getSpatialDisplayManager().getActivatedParm();

        dataManager.getParmManager().addDisplayedParmListChangedListener(this);
        dataManager.getSpatialDisplayManager().addActivatedParmChangedListener(
                this);
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButton(parent, ADJUST_DOWN, "Adjust Down", false);
        super.createButton(parent, ADJUST_UP, "Adjust Up", false);
        super.createButton(parent, IDialogConstants.CANCEL_ID, "Dismiss", false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        setReturnCode(buttonId);
        if (buttonId == ADJUST_UP) {
            setNewDelta();
            SmartUtil.runTool(SmartToolConstants.ADJUST_UP);
        } else if (buttonId == ADJUST_DOWN) {
            setNewDelta();
            SmartUtil.runTool(SmartToolConstants.ADJUST_DOWN);
        } else {
            close();
        }
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
        newShell.setText("Delta Value");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        dialogFrame = (Composite) super.createDialogArea(parent);
        GridLayout layout = new GridLayout(2, false);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        dialogFrame.setLayout(layout);
        dialogFrame.setLayoutData(data);
        setUpDialog(dialogFrame);
        return dialogFrame;
    }

    private void setUpDialog(Composite master) {
        // remove any existing ones first
        if (label != null) {
            label.dispose();
            label = null;
        }
        if (scale != null) {
            scale.removeSelectionListener(scaleListener);
            scale.dispose();
            scale = null;
        }
        if (entryField != null) {
            entryField.removeSelectionListener(entryListener);
            entryField.dispose();
            entryField = null;
        }
        if (entryLabel != null) {
            entryLabel.dispose();
            entryLabel = null;
        }

        // Set up the display for setting delta value
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        data.horizontalSpan = 2;
        label = new Label(master, SWT.CENTER);
        label.setText("No active weather element");
        label.setLayoutData(data);

        if (parm != null) {
            origValue = parm.getParmState().getDeltaValue();

            String item = parm.getParmID().compositeNameUI();
            label.setText(item + " Delta");

            int prec = parm.getGridInfo().getPrecision();
            res = (float) (Math.pow(10, -prec) / 2.0);
            float maxLimit;
            if (Activator.getDefault().getPreferenceStore()
                    .contains(item + "_MaxDeltaDialogValue")) {
                maxLimit = Activator.getDefault().getPreferenceStore()
                        .getFloat(item + "_MaxDeltaDialogValue");
            } else {
                maxLimit = (parm.getGridInfo().getMaxValue() - parm
                        .getGridInfo().getMinValue()) / 5;
            }

            scale = new Scale(master, SWT.HORIZONTAL);
            scale.setMinimum(0);
            scale.setMaximum((int) (maxLimit / res));
            scale.setIncrement((int) (1 / res));
            scale.setPageIncrement((int) (1 / res));
            scale.setSelection((int) (origValue / res));
            data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            data.horizontalSpan = 2;
            scale.setLayoutData(data);
            scale.addSelectionListener(scaleListener);

            entryLabel = new Label(master, SWT.NONE);
            data = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
            entryLabel.setText("Enter Value:");
            entryLabel.setLayoutData(data);

            data = new GridData(80, SWT.DEFAULT);
            entryField = new Text(master, SWT.BORDER);
            entryField.setText(String.format("%-6." + (prec + 1) + "f",
                    origValue).trim());
            entryField.setLayoutData(data);
            entryField.addSelectionListener(entryListener);
        }

        master.layout();
        getShell().pack(true);
    }

    @Override
    public boolean close() {
        dataManager.getParmManager().removeDisplayedParmListChangedListener(
                this);
        dataManager.getSpatialDisplayManager()
                .removeActivatedParmChangedListener(this);
        SetDeltaDialog.dialog = null;
        return super.close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener#
     * activatedParmChanged(com.raytheon.viz.gfe.core.parm.Parm)
     */
    @Override
    public void activatedParmChanged(Parm newParm) {
        parm = newParm;
        setUpDialog(dialogFrame);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener#
     * displayedParmListChanged(com.raytheon.viz.gfe.core.parm.Parm[],
     * com.raytheon.viz.gfe.core.parm.Parm[],
     * com.raytheon.viz.gfe.core.parm.Parm[])
     */
    @Override
    public void displayedParmListChanged(Parm[] parms, Parm[] deletions,
            Parm[] additions) {
        if (parm != null) {
            for (Parm deleted : deletions) {
                if (parm.equals(deleted)) {
                    parm = null;
                    setUpDialog(dialogFrame);
                }
            }
        }
    }

    /**
     * Set new delta value.
     */
    private void setNewDelta() {
        if (parm != null) {
            float value = getEntry();
            if (!Float.isNaN(value)) {
                parm.getParmState().setDeltaValue(value);
            } else {
                parm.getParmState().setDeltaValue(origValue);
                scale.setSelection((int) (origValue / res));
                int prec = parm.getGridInfo().getPrecision() + 1;
                entryField.setText(String
                        .format("%-6." + prec + "f", origValue).trim());
            }
        }
    }

    private float getEntry() {
        try {
            float value = Float.parseFloat(entryField.getText());
            if (value >= 0.0) {
                return value;
            } else {
                return Float.NaN;
            }
        } catch (NumberFormatException e) {
            return Float.NaN;
        }
    }
}

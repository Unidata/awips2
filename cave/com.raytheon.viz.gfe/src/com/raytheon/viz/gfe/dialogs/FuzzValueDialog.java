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

import java.util.Arrays;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The fuzz value dialog.
 * 
 * <PRE>
 *  # fuzz values
 *  # fuzz values define the value considered to be the same during a
 *  # homogenous area select using the GridPoint Tool. For example, if the
 *  # fuzz is 2.0 degrees for Temperature and you click on 40 degrees, then
 *  # all points between 38 and 42 will be selected as long as they are
 *  # contiguous to the click point.  If not specified, the fuzz is set
 *  # to 1/100 of the parm range.  Format is parmName_fuzzValue = value.
 *  # Be sure to include a decimal point.
 *  #parmName_fuzzValue = 10.0
 * </PRE>
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Mar 10, 2008					Eric Babin Initial Creation
 * Nov 10, 2012 1298       rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class FuzzValueDialog extends CaveJFACEDialog implements
        IDisplayedParmListChangedListener, IActivatedParmChangedListener {

    private Scale fuzzSlider;

    private Composite top;

    private Label scaleSlider;

    private Label lab;

    private Parm parm;

    private float minimum = 0.0f;

    private float maximum = 0.0f;

    private float fuzzValue = 0.0f;

    private double resolution = 10;

    private DataManager dataManager;

    private static FuzzValueDialog dialog;

    public static void openDialog(DataManager dataManager) {
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            dialog = new FuzzValueDialog(shell, dataManager);

            dialog.setBlockOnOpen(false);
            dialog.open();
        } else {
            dialog.bringToTop();
        }
    }

    private FuzzValueDialog(Shell parent, DataManager dataManager) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
        this.dataManager = dataManager;
        this.parm = dataManager.getSpatialDisplayManager().getActivatedParm();
        this.minimum = parm.getGridInfo().getMinValue();
        this.maximum = parm.getGridInfo().getMaxValue();
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(2, false);
        top.setLayout(layout);

        initializeComponents();

        dataManager.getParmManager().addDisplayedParmListChangedListener(this);
        dataManager.getSpatialDisplayManager().addActivatedParmChangedListener(
                this);

        return top;
    }

    private void initializeComponents() {
        GridData data = new GridData(GridData.FILL_HORIZONTAL);
        data.horizontalSpan = 2;
        data.horizontalAlignment = SWT.CENTER;
        lab = new Label(top, SWT.BORDER);
        lab.setText(this.parm.getParmID().getParmName() + " Fuzz");
        lab.setLayoutData(data);

        data = new GridData(200, SWT.DEFAULT);
        data.horizontalAlignment = SWT.CENTER;
        fuzzSlider = new Scale(top, SWT.HORIZONTAL);
        fuzzSlider.setLayoutData(data);

        int precision = parm.getGridInfo().getPrecision();
        resolution = Math.pow(10, precision);

        scaleSlider = new Label(top, SWT.NONE);
        data = new GridData(40, SWT.DEFAULT);
        data.horizontalAlignment = SWT.CENTER;
        scaleSlider.setLayoutData(data);

        fuzzSlider.setMinimum(0);
        fuzzSlider.setMaximum((int) (maximum - minimum) / 5 * (int) resolution);
        fuzzSlider.setSelection((int) (resolution * this.parm.getParmState()
                .getFuzzValue()));

        fuzzSlider.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent arg0) {
            }

            public void widgetSelected(SelectionEvent arg0) {
                setValues();
            }
        });
        setValues();
    }

    private void setValues() {
        scaleSlider.setText(String.valueOf(fuzzSlider.getSelection()
                / resolution));
        fuzzValue = fuzzSlider.getSelection() / (float) resolution;
        Parm p = DataManager.getCurrentInstance().getSpatialDisplayManager()
                .getActivatedParm();
        p.getParmState().setFuzzValue(fuzzValue);
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButton(parent, Window.OK, "Dismiss", false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#close()
     */
    @Override
    public boolean close() {
        dataManager.getParmManager().removeDisplayedParmListChangedListener(
                this);
        dataManager.getSpatialDisplayManager()
                .removeActivatedParmChangedListener(this);
        return super.close();
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

        shell.setText("Fuzz Value");
    }

    public float getFuzzValue() {
        return fuzzValue;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener#
     * activatedParmChanged(com.raytheon.viz.gfe.core.parm.Parm)
     */
    @Override
    public void activatedParmChanged(Parm newParm) {
        parm = newParm;
        if (parm != null) {
            lab.setText(parm.getParmID().getParmName() + " Delta");
            minimum = parm.getGridInfo().getMinValue();
            maximum = parm.getGridInfo().getMaxValue();
            int precision = parm.getGridInfo().getPrecision();
            resolution = Math.pow(10, precision);
            fuzzSlider.setMaximum((int) (maximum - minimum) / 5
                    * (int) resolution);
            fuzzSlider.setSelection((int) (resolution * this.parm
                    .getParmState().getDeltaValue()));
            setValues();
        } else {
            lab.setText("No active weather element");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener#
     * displayedParmListChanged(com.raytheon.viz.gfe.core.parm.Parm[],
     * com.raytheon.viz.gfe.core.parm.Parm[],
     * com.raytheon.viz.gfe.core.parm.Parm[])
     */
    @Override
    public void displayedParmListChanged(Parm[] parms, Parm[] deletions,
            Parm[] additions) {
        if (parm != null) {
            if (Arrays.asList(deletions).contains(parm)) {
                parm = null;
            }
        }
    }
}

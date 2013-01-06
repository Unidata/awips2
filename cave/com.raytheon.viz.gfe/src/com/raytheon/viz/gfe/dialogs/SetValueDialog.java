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

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.Geometry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.smarttool.SmartToolConstants;
import com.raytheon.viz.gfe.smarttool.SmartUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Contains the SetValueDialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2009 #1318      randerso    Ported AWIPS I pickup value dialogs
 * Nov 13, 2012 #1298      rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SetValueDialog extends CaveJFACEDialog implements
        IDisplayedParmListChangedListener, IActivatedParmChangedListener {

    private static SetValueDialog dialog;

    private final int DISMISS_ID = IDialogConstants.CLIENT_ID + 1;

    private final int ASSIGN_ID = IDialogConstants.CLIENT_ID + 0;

    private DataManager dataManager;

    private Composite topFrame;

    private AbstractSetValue setValue;

    private Parm activeParm;

    private Label pnLabel;

    private Composite valueFrame;

    public static void openDialog() {
        if (dialog == null) {
            Shell parent = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            DataManager dataManager = DataManager.getCurrentInstance();
            dialog = new SetValueDialog(parent, dataManager);
            dialog.setBlockOnOpen(false);
        }
        dialog.open();
    }

    /**
     * Constructor is private use openDialog
     * 
     * @param parentShell
     *            parent shell
     * @param dataManager
     *            DataManager for the associated window
     */
    private SetValueDialog(Shell parentShell, DataManager dataManager) {
        super(parentShell);

        this.dataManager = dataManager;
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS | SWT.RESIZE);
        activeParm = dataManager.getSpatialDisplayManager().getActivatedParm();
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
        newShell.setText("PickUp Value");
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
        createButton(parent, ASSIGN_ID, "Assign Value", false);
        createButton(parent, DISMISS_ID, "Dismiss", false);
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
        topFrame = (Composite) super.createDialogArea(parent);
        GridLayout layout = (GridLayout) topFrame.getLayout();
        layout.marginWidth = 0;

        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pnLabel = new Label(topFrame, SWT.CENTER);
        pnLabel.setText("No active weather element");
        pnLabel.setLayoutData(layoutData);

        valueFrame = new Composite(topFrame, SWT.NONE);
        layout = new GridLayout(1, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        valueFrame.setLayout(layout);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        valueFrame.setLayoutData(layoutData);

        setValue = null;
        setSetValueOrder();

        dataManager.getParmManager().addDisplayedParmListChangedListener(this);
        dataManager.getSpatialDisplayManager().addActivatedParmChangedListener(
                this);

        return topFrame;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createContents(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createContents(Composite parent) {
        Control contents = super.createContents(parent);

        Point size = getInitialSize();
        getShell().setSize(size);
        getShell().setLocation(getInitialLocation(size));

        return contents;
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

        if (setValue != null) {
            setValue.dispose();
            setValue = null;
        }

        SetValueDialog.dialog = null;

        return super.close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#getInitialLocation(org.eclipse.swt.graphics
     * .Point)
     */
    @Override
    protected Point getInitialLocation(Point initialSize) {
        Composite parent = getShell().getParent();

        Monitor monitor = getShell().getDisplay().getPrimaryMonitor();
        if (parent != null) {
            monitor = parent.getMonitor();
        }

        Rectangle monitorBounds = monitor.getClientArea();
        Point origin;
        if (parent != null) {
            origin = Geometry.getLocation(parent.getBounds());
        } else {
            origin = Geometry.getLocation(monitorBounds);
        }

        return new Point(origin.x + 50, Math.max(
                monitorBounds.y,
                Math.min(origin.y + 550, monitorBounds.y + monitorBounds.height
                        - initialSize.y)));
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
        if (activeParm != null) {
            if (Arrays.asList(deletions).contains(activeParm)) {
                activeParm = null;
                setSetValueOrder();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener#
     * activatedParmChanged(com.raytheon.viz.gfe.core.parm.Parm)
     */
    @Override
    public void activatedParmChanged(Parm newParm) {
        activeParm = newParm;
        setSetValueOrder();
    }

    private void setSetValueOrder() {
        // no active parm case
        if (activeParm == null) {
            pnLabel.setText("No active weather element");
            if (setValue != null) {
                // setValue.pack_forget();
                setValue.dispose();
                setValue = null;
            }
            return;
        }

        // no change in parm
        if (setValue != null && setValue.getParm() == activeParm) {
            return;
        }

        // change in parm
        if (setValue != null) {
            // delete the old one
            // setValue.pack_forget();
            // setValue.unregister();
            // setValue.destroy();
            setValue.dispose();
            setValue = null;
        }

        pnLabel.setText(activeParm.getParmID().compositeNameUI());

        GridType gridType = activeParm.getGridInfo().getGridType();
        if (gridType.equals(GridType.SCALAR)) {
            setValue = new ScalarSetValue(valueFrame, activeParm);

        } else if (gridType.equals(GridType.VECTOR)) {
            setValue = new VectorSetValue(valueFrame, activeParm);

        } else if (gridType.equals(GridType.WEATHER)) {
            setValue = new WxSetValue(valueFrame, activeParm, true, true, true);

        } else if (gridType.equals(GridType.DISCRETE)) {
            setValue = new DiscreteSetValue(valueFrame, activeParm, true, true,
                    true);
        }

        setValue.pack(true);
        valueFrame.layout();
        valueFrame.setSize(valueFrame.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        getShell().layout();
        getShell().pack(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == ASSIGN_ID) {
            assign();
        } else if (buttonId == DISMISS_ID) {
            setReturnCode(CANCEL);
            close();
        }
    }

    private void assign() {
        if (activeParm != null) {
            if (activeParm.getGridInfo().getGridType().equals(GridType.WEATHER)) {
                // set the pickup value first
                ((WxSetValue) setValue).setWxPickup();
            } else if (activeParm.getGridInfo().getGridType()
                    .equals(GridType.DISCRETE)) {
                // set the pickup value first
                ((DiscreteSetValue) setValue).setWxPickup();
            }
        }

        SmartUtil.runTool(SmartToolConstants.ASSIGN);
    }

}

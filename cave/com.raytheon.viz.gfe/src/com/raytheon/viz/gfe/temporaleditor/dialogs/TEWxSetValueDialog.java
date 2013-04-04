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
package com.raytheon.viz.gfe.temporaleditor.dialogs;

import java.util.Date;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.Geometry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.dialogs.WxSetValue;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The define discrete dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2009 #1318      randerso     Initial creation
 * Nov 13, 2012 #1298      rferrel     Code clean up for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TEWxSetValueDialog extends CaveJFACEDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TEWxSetValueDialog.class);

    private Parm parm;

    private Date date;

    private Composite topFrame;

    private WxSetValue setValue;

    public TEWxSetValueDialog(Shell parentShell, Parm parm, Date date) {
        super(parentShell);

        this.parm = parm;
        this.date = date;
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

        newShell.setText("Set Weather");
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
        if (setValue != null) {
            setValue.dispose();
            setValue = null;
        }

        return super.close();
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
        // pnLabel = new Label(topFrame, SWT.CENTER);
        // pnLabel.setText("No active weather element");
        // pnLabel.setLayoutData(layoutData);

        Composite valueFrame = new Composite(topFrame, SWT.NONE);
        layout = new GridLayout(1, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        valueFrame.setLayout(layout);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        valueFrame.setLayoutData(layoutData);

        setValue = new WxSetValue(valueFrame, parm, true, false, false);

        setValue.pack(true);
        valueFrame.layout();
        valueFrame.setSize(valueFrame.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        getShell().layout();
        getShell().pack(true);

        return topFrame;
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
        createButton(parent, IDialogConstants.OK_ID, "Set", true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @Override
    protected void okPressed() {

        try {
            parm.getDataManager().getParmOp().clearUndoParmList();
            IGridData gridData = parm.startParmEdit(date);
            if (gridData != null) {
                Grid2DBit gridArea = parm.getDataManager().getRefManager()
                        .getActiveRefSet().getGrid();
                WxValue value = setValue.getWxPickup();
                gridData.setValue(value, gridArea);
            }
            parm.endParmEdit();
        } catch (GFEOperationFailedException exc) {
            statusHandler.handle(Priority.PROBLEM, "Grid edit failed", exc);
        }

        super.okPressed();
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

        return new Point(origin.x + 50, origin.y + 50);
    }

}

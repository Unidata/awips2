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
package com.raytheon.viz.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.core.rsc.capabilities.LabelableCapability;

/**
 * Dialog change the location of labels.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 3, 2011            randerso     Initial creation
 * Oct 17, 2012 1229       rferrel     Set origyOffset to the proper value.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class LabelOffsetDialog extends CaveJFACEDialog {

    private static final int MAX_OFFSET = 100;

    private LabelableCapability cap;

    private int origxOffset;

    private int origyOffset;

    /**
     * @param parentShell
     */
    public LabelOffsetDialog(Shell parentShell) {
        super(parentShell);
    }

    /**
     * @param parentShell
     * @param cap
     */
    public LabelOffsetDialog(Shell parentShell, LabelableCapability cap) {
        super(parentShell);
        this.cap = cap;
        this.origxOffset = cap.getxOffset();
        this.origyOffset = cap.getyOffset();
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
        newShell.setText("Map Label Offset");
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
        Composite comp = (Composite) super.createDialogArea(parent);
        GridLayout layout = (GridLayout) comp.getLayout();
        layout.numColumns = 2;
        GridData layoutData = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        comp.setLayoutData(layoutData);

        Label xLabel = new Label(comp, SWT.NONE);
        layoutData = new GridData(SWT.DEFAULT, SWT.CENTER, false, false);
        xLabel.setLayoutData(layoutData);
        xLabel.setText("X:");

        final Spinner xSpin = new Spinner(comp, SWT.BORDER);
        layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        xSpin.setLayoutData(layoutData);
        xSpin.setValues(cap.getxOffset(), -MAX_OFFSET, MAX_OFFSET, 0, 1, 10);
        xSpin.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                cap.setxOffset(xSpin.getSelection());
            }
        });

        Label yLabel = new Label(comp, SWT.NONE);
        layoutData = new GridData(SWT.DEFAULT, SWT.CENTER, false, false);
        yLabel.setLayoutData(layoutData);
        yLabel.setText("Y:");

        final Spinner ySpin = new Spinner(comp, SWT.BORDER);
        layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        ySpin.setLayoutData(layoutData);
        ySpin.setValues(cap.getyOffset(), -MAX_OFFSET, MAX_OFFSET, 0, 1, 10);
        ySpin.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                cap.setyOffset(ySpin.getSelection());
            }
        });

        return comp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
     */
    @Override
    protected void cancelPressed() {
        cap.setxOffset(origxOffset);
        cap.setyOffset(origyOffset);
        super.cancelPressed();
    }
}

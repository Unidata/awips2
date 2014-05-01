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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.RefSetAppearanceChangedMsg;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.colordialog.ColorWheelComp;
import com.raytheon.viz.ui.widgets.SpinScale;

/**
 * Reference Set Appearance Dialog <br>
 * <br>
 * Allows the user to change the color and border width of the edit area.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2009            randerso     Initial creation
 * Oct 25, 2012 1287       rferrel     Code clean up for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class RefSetAppearanceDialog extends CaveJFACEDialog {
    private final int APPLY_ID = IDialogConstants.CLIENT_ID;

    private final String APPLY_LABEL = "Apply";

    private ColorWheelComp colorWheel;

    private SpinScale widthScale;

    private RefSetAppearanceChangedMsg lastMsg;

    public RefSetAppearanceDialog(Shell parentShell) {
        super(parentShell);
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
        newShell.setText("Edit Area Appearance");
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
        GridData layoutData;

        Composite widthComp = new Composite(comp, SWT.NONE);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        widthComp.setLayoutData(layoutData);
        GridLayout layout = new GridLayout(2, false);
        widthComp.setLayout(layout);

        Label label = new Label(widthComp, SWT.HORIZONTAL);
        label.setText("Line Width:");

        widthScale = new SpinScale(widthComp, SWT.HORIZONTAL);
        widthScale.setMinimum(1);
        widthScale.setMaximum(5);
        widthScale.setIncrement(1);
        widthScale.setPageIncrement(1);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        widthScale.setLayoutData(layoutData);

        colorWheel = new ColorWheelComp(comp, "", true);
        colorWheel.showRgbSliders(false);

        lastMsg = Message.inquireLastMessage(RefSetAppearanceChangedMsg.class);
        widthScale.setSelection(lastMsg.getLineWidth());
        colorWheel.setColor(lastMsg.getColor());

        return comp;
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
        createButton(parent, APPLY_ID, APPLY_LABEL, false);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createButtonBar(Composite parent) {
        return super.createButtonBar(parent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == APPLY_ID) {
            apply();
            return;
        } else if (buttonId == IDialogConstants.OK_ID) {
            apply();
        } else {
            // CANCEL
            apply(lastMsg);
        }
        super.buttonPressed(buttonId);
    }

    private void apply() {
        RefSetAppearanceChangedMsg msg = new RefSetAppearanceChangedMsg(
                colorWheel.getColorData().rgbColor, widthScale.getSelection());
        apply(msg);
    }

    private void apply(RefSetAppearanceChangedMsg msg) {
        msg.send();
    }
}

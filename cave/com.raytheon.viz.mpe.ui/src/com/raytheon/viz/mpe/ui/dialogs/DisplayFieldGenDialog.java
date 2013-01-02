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
package com.raytheon.viz.mpe.ui.dialogs;

import java.util.Date;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.mpe.core.RegenHrFlds;
import com.raytheon.viz.mpe.ui.Activator;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResource;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2008            snaples     Initial creation
 * Nov 10, 2008  1649      snaples     Added handlers for Yes button
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class DisplayFieldGenDialog extends CaveJFACEDialog {

    private Label dlgQ;

    public DisplayFieldGenDialog(Shell parentShell) {
        super(parentShell);
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
        this.getShell().setText("Regenerate Hour Fields");

        Composite composite = (Composite) super.createDialogArea(parent);
        composite.setLayout(new GridLayout(2, false));
        composite
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Composite comp1 = new Composite(composite, SWT.NONE);
        comp1.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        dlgQ = new Label(comp1, SWT.CENTER);
        dlgQ.setText("Are you sure you want to regenerate hour fields?");
        dlgQ.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        gd.verticalIndent = 20;
        Button yesButton = new Button(comp1, SWT.PUSH);
        yesButton.setText("Yes");
        yesButton.setLayoutData(gd);
        yesButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                MPEDisplayManager instance = MPEDisplayManager.getCurrent();
                Date dt = instance.getCurrentEditDate();
                try {
                    // TODO: This process should send alert message that things
                    // were regenerated to keep CAVEs in sync with each other.
                    // TODO: Should this clear polygon edits/does it delete them
                    // on the file system?
                    RegenHrFlds.getInstance().regenFields(dt);
                    MPEFieldResource rsc = instance.getDisplayedFieldResource();
                    if (rsc != null) {
                        rsc.getResourceData().update(dt);
                    }
                } catch (VizException ex) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            "Error regenerating hourly fields", ex);
                }
                DisplayFieldGenDialog.this.close();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        gd.verticalIndent = 20;
        Button noButton = new Button(comp1, SWT.PUSH);
        noButton.setText("No");
        noButton.setLayoutData(gd);
        noButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                DisplayFieldGenDialog.this.close();
            }

        });
        return composite;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonBar(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createButtonBar(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayoutData(new GridData(0, 0));
        return composite;
    }

}

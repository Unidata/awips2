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
package com.raytheon.uf.viz.collaboration.ui.login;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * Dialog for users to remove user servers to options in login dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2014  2563      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class ServerRemoveDialog extends Dialog {

    private String selection = null;

    private final String[] options;

    /**
     * @param parentShell
     */
    public ServerRemoveDialog(Shell parentShell, String[] options) {
        super(parentShell);
        this.options = options;
    }

    /**
     * @param parentShell
     */
    public ServerRemoveDialog(IShellProvider parentShell, String[] options) {
        super(parentShell);
        this.options = options;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        Composite innerComposite = new Composite(composite, SWT.NONE);
        innerComposite.setLayoutData(new GridData());
        GridLayout gl = new GridLayout();
        gl.numColumns = 2;
        innerComposite.setLayout(gl);

        Label label = new Label(innerComposite, SWT.NONE);
        label.setText("Server to Remove:");
        label.setLayoutData(new GridData());

        final Combo combo = new Combo(innerComposite, SWT.READ_ONLY);
        for (int i = 0; i < options.length; i++) {
            combo.add(options[i]);
        }
        selection = combo.getItem(0);
        GridData gd = new GridData();
        gd.widthHint = convertWidthInCharsToPixels(getMaxStringLength());
        gd.minimumWidth = 200;
        combo.setLayoutData(gd);
        combo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selection = combo.getItem(combo.getSelectionIndex());
            }
        });
        applyDialogFont(composite);
        return composite;
    }

    /**
     * @return longest string length in options or 0 if options is empty
     */
    private int getMaxStringLength() {
        int max = 0;
        for (int i = 0; i < options.length; i++) {
            max = Math.max(max, options[i].length());
        }
        return max;
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
        newShell.setText("Remove Server");
    }

    /**
     * @return the selection
     */
    public String getSelection() {
        return selection;
    }

}

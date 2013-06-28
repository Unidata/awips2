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
package com.raytheon.uf.viz.archive.ui;

import java.io.File;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to obtain and verify a case name.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 5, 2013  1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class CaseNameDialog extends CaveSWTDialog {

    /** The location directory. */
    private File locationDir;

    /** Text field for entering the case name's directory. */
    private Text caseNameText;

    /** Non-blocking modal constructor. */
    protected CaseNameDialog(Shell parent, File locationDir) {
        super(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL,
                CAVE.DO_NOT_BLOCK);
        this.locationDir = locationDir;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        mainComp.setLayout(gl);
        init();
    }

    /**
     * create layout of the dialog.
     */
    private void init() {
        setText("Case Name");
        createFieldsComp();
        GuiUtil.addSeparator(shell, SWT.HORIZONTAL);
        createBottomActionButtons();
    }

    /**
     * The xomposite with case name text field.
     */
    private void createFieldsComp() {
        Composite fieldComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        fieldComp.setLayout(gl);
        fieldComp.setLayoutData(gd);

        Label nameLbl = new Label(fieldComp, SWT.LEFT);
        nameLbl.setText("Name: ");

        caseNameText = new Text(fieldComp, SWT.SINGLE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = 200;
        caseNameText.setLayoutData(gd);
        caseNameText.addKeyListener(new KeyAdapter() {

            @Override
            public void keyReleased(KeyEvent e) {
                super.keyReleased(e);
                if (e.character == SWT.CR || e.character == SWT.KEYPAD_CR) {
                    if (verifySelection()) {
                        close();
                    }
                }
            }
        });
    }

    /**
     * The composite with the action buttons.
     */
    private void createBottomActionButtons() {
        Composite actionControlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        actionControlComp.setLayout(gl);
        actionControlComp.setLayoutData(gd);

        Button okBtn = new Button(actionControlComp, SWT.PUSH);
        okBtn.setText(" OK ");
        okBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (verifySelection()) {
                    close();
                }
            }
        });

        Button cancelBtn = new Button(actionControlComp, SWT.PUSH);
        cancelBtn.setText(" Cancel ");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Validate the case's directory does not exist and set the return value
     * when it doesn't exist.
     * 
     * @return true when case's directory does not exist.
     */
    private boolean verifySelection() {
        String caseName = caseNameText.getText().trim();
        if (caseName.length() == 0) {
            MessageDialog.openError(shell, "Error", "Enter a case name.");
            return false;
        }

        File caseDir = new File(locationDir, caseName);
        if (caseDir.exists()) {
            MessageDialog.openError(shell, "Error",
                    "Case already exists choose another name.");
            caseNameText.selectAll();
            caseNameText.setFocus();
            return false;
        }

        setReturnValue(caseDir);
        return true;
    }

}

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

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.ArchiveConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to display a list of select files for Load, Save As or Delete.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2013 2221       rferrel     Initial creation
 * Aug 26, 2013 2225       rferrel     Make perspective independent.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class CaseLoadSaveDeleteDlg extends CaveSWTDialog {
    public static enum Type {
        SaveAs("Save As", " Save "), Load("Load", " Load "), Delete("Delete",
                " Delete ");
        private String title;

        private String btnLabel;

        private Type(String title, String btnLabel) {
            this.title = title;
            this.btnLabel = btnLabel;
        }

        public String getTitle() {
            return title;
        }

        public String getBtnLabel() {
            return btnLabel;
        }
    }

    private Type type;

    private List caseList;

    private Text fileNameText;

    private Button okBtn;

    private Button cancelBtn;

    protected CaseLoadSaveDeleteDlg(Shell parentShell, Type type) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK
                | CAVE.PERSPECTIVE_INDEPENDENT);
        this.type = type;
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
        setText(type.getTitle() + " Case");
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        mainComp.setLayout(gl);
        init();
    }

    /**
     * Set up dialog layout.
     */
    private void init() {
        createCaseControls();
        GuiUtil.addSeparator(shell, SWT.HORIZONTAL);
        createBottomActionButtons();
    }

    /**
     * Main body of the dialog.
     */
    private void createCaseControls() {
        Composite caseComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        caseComp.setLayout(gl);
        caseComp.setLayoutData(gd);

        caseList = new List(caseComp, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumHeight = 300;
        caseList.setLayoutData(gd);

        caseList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                fileNameText.setText(caseList.getSelection()[0]);
            }
        });

        fileNameText = new Text(caseComp, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        fileNameText.setLayoutData(gd);
        fileNameText.setEditable(type == Type.SaveAs);
    }

    /**
     * Button layout at the bottom of the dialog.
     */
    private void createBottomActionButtons() {
        Composite actionControlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        actionControlComp.setLayout(gl);
        actionControlComp.setLayoutData(gd);
        okBtn = new Button(actionControlComp, SWT.PUSH);
        okBtn.setText(type.getBtnLabel());
        okBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                String name = verifyAction();
                if (name != null) {
                    setReturnValue(name);
                    close();
                }
            }
        });

        cancelBtn = new Button(actionControlComp, SWT.PUSH);
        cancelBtn.setText(" Cancel ");
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Verify pending action and return select case name to continue the action.
     * 
     * @return name when ok to perform action otherwise null
     */
    private String verifyAction() {
        String name = fileNameText.getText().trim();

        if (name.isEmpty()) {
            MessageDialog.openError(shell, "Case Error", "Invalid case name.");
            name = null;
        } else {
            switch (type) {
            case Load:
                // No need to check since text is not editable.
                break;
            case SaveAs:
                for (String cName : caseList.getItems()) {
                    if (name.equals(cName)) {
                        boolean response = MessageDialog
                                .openConfirm(
                                        shell,
                                        "Case Confirmation",
                                        "Case \""
                                                + name
                                                + "\" exists.\nSelect OK to overwrite.");
                        if (!response) {
                            name = null;
                            fileNameText.selectAll();
                            fileNameText.forceFocus();
                        }
                        break;
                    }
                }
                break;
            case Delete:
                if (!MessageDialog.openConfirm(shell, "Case Confirmation",
                        "Press OK to delete \"" + name + "\".")) {
                    name = null;
                }
                break;
            default:
                name = null;
            }
        }
        return name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        populateList();
    }

    /**
     * Populate case names in the list.
     */
    private void populateList() {
        caseList.add(ArchiveConstants.defaultSelectName);
        String[] names = ArchiveConfigManager.getInstance().getSelectionNames(
                ArchiveConstants.Type.Case);
        for (String name : names) {
            if (!ArchiveConstants.defaultSelectName.equals(name)) {
                caseList.add(name);
            }
        }
        caseList.select(0);
        fileNameText.setText(caseList.getItem(0));
    }
}

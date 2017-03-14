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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.DialogUtil;

/**
 * This class displays the dialog for create a new category or source.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 05, 2008           lvenable  Initial creation.
 * Sep 21, 2016  5901     randerso  Fix dialog centering issue introduced in
 *                                  Eclipse 4
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 *
 */
public class NewSourceCategoryDlg extends Dialog {
    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    /**
     * Return object when the shell is disposed.
     */
    private Boolean returnObj = null;

    /**
     * Flag indicating if the category window title should be used.
     */
    private boolean useCategoryTitle;

    /**
     * Text key string.
     */
    private String textKey = "";

    /**
     * Description.
     */
    private String description = "";

    /**
     * Text key text control.
     */
    private Text textKeyTF;

    /**
     * Description text control.
     */
    private Text descriptionTF;

    /**
     * Set of existing category/source names.
     */
    private Set<String> existingNames;

    /**
     * Constructor.
     *
     * @param parentShell
     *            Parent shell.
     * @param useCategoryTitle
     *            Use category title flag.
     * @param existingNames
     *            Set of existing names.
     */
    public NewSourceCategoryDlg(Shell parentShell, boolean useCategoryTitle,
            Set<String> existingNames) {
        super(parentShell, 0);

        this.useCategoryTitle = useCategoryTitle;
        this.existingNames = existingNames;
    }

    /**
     * Open method used to display the dialog.
     *
     * @return True/False.
     */
    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);

        if (useCategoryTitle == true) {
            shell.setText("New Category");
        } else {
            shell.setText("New Source");
        }

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();
        DialogUtil.centerOnParentShell(parent, shell);
        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return returnObj;
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        createTextControls();

        // Create the buttons at the bottom of the display.
        createBottomButtons();
    }

    /**
     * Create the label and text controls.
     */
    private void createTextControls() {
        Composite textComp = new Composite(shell, SWT.NONE);
        textComp.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label textKeyLbl = new Label(textComp, SWT.NONE);
        textKeyLbl.setText("Text Key: ");
        textKeyLbl.setLayoutData(gd);

        gd = new GridData(250, SWT.DEFAULT);
        textKeyTF = new Text(textComp, SWT.BORDER);
        textKeyTF.setLayoutData(gd);
        textKeyTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent fe) {
                String str = textKeyTF.getText().trim();
                textKeyTF.setText(str.toUpperCase());
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label descLbl = new Label(textComp, SWT.NONE);
        descLbl.setText("Description: ");
        descLbl.setLayoutData(gd);

        gd = new GridData(250, SWT.DEFAULT);
        descriptionTF = new Text(textComp, SWT.BORDER);
        descriptionTF.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Button saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String tmpStr = textKeyTF.getText().trim().toUpperCase();
                if (tmpStr.length() == 0) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Text Key Error");
                    mb
                    .setMessage("You do not have an entry in the 'Text Key' field.");
                    mb.open();
                    return;
                }

                if (tmpStr.indexOf(" ") != -1) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Text Key Error");
                    mb
                    .setMessage("You cannot have blank spaces in the 'Text Key' field.");
                    mb.open();
                    return;
                }

                if (existingNames.contains(textKeyTF.getText().trim()) == true) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Text Key Error");
                    mb.setMessage(textKeyTF.getText().trim()
                            + " already exists."
                            + "\nPlease enter another name.");
                    mb.open();
                    return;
                }

                if (!textKeyTF.getText().matches("(\\w)+")) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Text Key Error");
                    mb.setMessage(textKeyTF.getText().trim()
                            + " contains invalid characters."
                            + "\nPlease only use A-Z,a-z,0-9,_");
                    mb.open();
                    return;
                }

                // TODO save information...
                textKey = textKeyTF.getText();
                description = descriptionTF.getText();
                returnObj = true;
                shell.dispose();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                returnObj = false;
                shell.dispose();
            }
        });
    }

    /**
     * Get the text key.
     *
     * @return The text key.
     */
    public String getTextKey() {
        return textKey;
    }

    /**
     * Get the description.
     *
     * @return The description.
     */
    public String getDescription() {
        return description;
    }
}

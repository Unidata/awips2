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
package com.raytheon.viz.ghg.monitor;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the GHG Save/Delete Filter Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GhgSaveDeleteFilterDlg extends CaveSWTDialog {

    /**
     * Filter list control.
     */
    private List filterList;

    /**
     * Filter name text control.
     */
    private Text filterNameTF;

    /**
     * List of filter names.
     */
    private String[] namesArray;

    /**
     * Flag indicating if the dialog should be a save or delete dialog. True is
     * Save, false is Delete.
     */
    private boolean saveFilter;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param names
     *            Array of filter names.
     * @param saveFilter
     *            Save filter flag. True is save, false is delete.
     */
    public GhgSaveDeleteFilterDlg(Shell parent, String[] names,
            boolean saveFilter) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);

        namesArray = names;
        this.saveFilter = saveFilter;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        if (saveFilter == true) {
            shell.setText("Save GHG Display Filter");
        } else {
            shell.setText("Delete GHG Display Filter");
        }

        createFilterControls();
        createBottomButtons();

        populateFilterList();
    }

    /**
     * Create the filter list & text controls.
     */
    private void createFilterControls() {
        Composite filterComp = new Composite(shell, SWT.NONE);
        filterComp.setLayout(new GridLayout(1, false));

        Label topLabel = new Label(filterComp, SWT.CENTER);
        topLabel.setText("Be sure to save your GHG Configuration\n"
                + "after making changes.");

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 300;
        filterList = new List(filterComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        filterList.setLayoutData(gd);
        filterList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                filterNameTF.setText(filterList.getItem(filterList
                        .getSelectionIndex()));
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        filterNameTF = new Text(filterComp, SWT.BORDER);
        filterNameTF.setLayoutData(gd);
        filterNameTF.setEnabled(saveFilter);
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(2, true));

        gd = new GridData(120, SWT.DEFAULT);
        Button actionBtn = new Button(buttons, SWT.PUSH);

        if (saveFilter == true) {
            actionBtn.setText("Save");
        } else {
            actionBtn.setText("Delete");
        }
        actionBtn.setLayoutData(gd);
        actionBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(filterNameTF.getText());
                shell.dispose();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
                shell.dispose();
            }
        });
    }

    /**
     * Populate the filter list with the array for filter names.
     */
    private void populateFilterList() {
        for (String name : namesArray) {
            filterList.add(name);
        }
    }
}
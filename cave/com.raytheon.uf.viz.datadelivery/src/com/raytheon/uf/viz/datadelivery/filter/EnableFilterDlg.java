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
package com.raytheon.uf.viz.datadelivery.filter;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * Dialog that allows the user to choose which filters they want disabled.
 * Disabling a filter will retain the selection but will skip using the the
 * filtering information when updating the dataset table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2012            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class EnableFilterDlg extends CaveSWTDialog {

    /**
     * Name of the filters.
     */
    ArrayList<String> filterNames;

    /**
     * Array of check boxes for each filter.
     */
    ArrayList<Button> filterCheckBoxes;

    /**
     * Array of indexes representing the enabled filters.
     */
    ArrayList<Integer> enabledIndexes;

    /**
     * Array of selected indexes.
     */
    ArrayList<Integer> selectedIndexes;

    public EnableFilterDlg(Shell parentShell, ArrayList<String> names, ArrayList<Integer> enabledIndexes) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.NONE);

        setText("Enable Filters");

        filterNames = names;

        this.enabledIndexes = enabledIndexes;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    @Override
    protected void disposed() {

    }

    @Override
    protected void initializeComponents(Shell shell) {
        createFilterCheckBoxes();
        createBottomButtons();
    }

    private void createFilterCheckBoxes() {

        Label headerLbl = new Label(shell, SWT.NONE);
        headerLbl.setText("Enabled Filters:");
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.verticalIndent = 5;
        headerLbl.setLayoutData(gd);

        filterCheckBoxes = new ArrayList<Button>();

        Composite checkBoxComp = new Composite(shell, SWT.NONE);
        checkBoxComp.setLayout(new GridLayout(1, false));

        for (String name : filterNames) {
            Button chkBtn = new Button(checkBoxComp, SWT.CHECK);
            chkBtn.setText(name);
            filterCheckBoxes.add(chkBtn);
        }

        for (Integer i : enabledIndexes) {
            filterCheckBoxes.get(i).setSelection(true);
        }
    }

    private void createBottomButtons() {
        addSeparator(shell);

        Composite btnComp = new Composite(shell, SWT.NONE);
        btnComp.setLayout(new GridLayout(2, false));
        btnComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        Button okBtn = new Button(btnComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveSelectedIndexes();
                setReturnValue(true);
                close();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        Button cancelBtn = new Button(btnComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(false);
                close();
            }
        });
    }

    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout)parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    private void saveSelectedIndexes() {
        selectedIndexes = new ArrayList<Integer>();

        Button chkBtn;
        for (int i = 0; i < filterCheckBoxes.size(); i++) {
            chkBtn = filterCheckBoxes.get(i);
            if (chkBtn.getSelection() == true) {
                selectedIndexes.add(i);
            }
        }
    }

    public ArrayList<Integer> getSelectedIndexes() {
        return selectedIndexes;
    }
}

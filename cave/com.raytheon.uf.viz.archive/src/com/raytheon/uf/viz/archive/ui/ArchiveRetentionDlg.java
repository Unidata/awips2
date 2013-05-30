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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.archive.data.IArchiveTotals;
import com.raytheon.uf.viz.archive.ui.ArchiveTableComp.TableType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Archive retention dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2013 #1964      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ArchiveRetentionDlg extends CaveSWTDialog implements IArchiveTotals {

    /** Table composite that holds the table controls. */
    private ArchiveTableComp tableComp;

    /** Archive config combo box. */
    private Combo archCfgCbo;

    /** Category combo box. */
    private Combo categoryCbo;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public ArchiveRetentionDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN, CAVE.DO_NOT_BLOCK
                | CAVE.MODE_INDEPENDENT | CAVE.INDEPENDENT_SHELL);
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;

        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setText("Archive Retention");
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        mainComp.setLayout(gl);

        init();
    }

    /**
     * Initialize method to create all of the composite & controls.
     */
    private void init() {
        createRetentionControls();
        createTable();
        addSeparator(shell, SWT.HORIZONTAL);
        createBottomActionButtons();

        // TODO : Remove this when functionality is implemented
        populateComboBoxes();
    }

    /**
     * Create the retention controls.
     */
    private void createRetentionControls() {
        Composite retentionComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(5, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        retentionComp.setLayout(gl);
        retentionComp.setLayoutData(gd);

        /*
         * Top row of controls.
         */
        Label archCfgLbl = new Label(retentionComp, SWT.NONE);
        archCfgLbl.setText("Archive Config: ");

        gd = new GridData(200, SWT.DEFAULT);
        archCfgCbo = new Combo(retentionComp, SWT.VERTICAL | SWT.DROP_DOWN
                | SWT.BORDER | SWT.READ_ONLY);
        archCfgCbo.setLayoutData(gd);
        archCfgCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                /*
                 * TODO - add code to update the category combo box
                 */
            }
        });

        gd = new GridData();
        gd.horizontalIndent = 20;
        Label minRetentionLbl = new Label(retentionComp, SWT.NONE);
        minRetentionLbl.setText("Minimum Retention: ");
        minRetentionLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        final Spinner minRetentionSpnr = new Spinner(retentionComp, SWT.BORDER);
        minRetentionSpnr.setIncrement(1);
        minRetentionSpnr.setPageIncrement(5);
        minRetentionSpnr.setMaximum(Integer.MAX_VALUE);
        minRetentionSpnr.setMinimum(1);
        minRetentionSpnr.setLayoutData(gd);

        final Combo minRetentionCbo = new Combo(retentionComp, SWT.VERTICAL
                | SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        minRetentionCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleRetentionSelection(minRetentionCbo, minRetentionSpnr);
            }
        });
        minRetentionCbo.add("Hours");
        minRetentionCbo.add("Days");
        minRetentionCbo.select(0);
        minRetentionCbo.setData(minRetentionCbo.getItem(minRetentionCbo
                .getSelectionIndex()));

        /*
         * Bottom row of controls.
         */
        Label catLbl = new Label(retentionComp, SWT.NONE);
        catLbl.setText("Category: ");

        gd = new GridData(200, SWT.DEFAULT);
        categoryCbo = new Combo(retentionComp, SWT.VERTICAL | SWT.DROP_DOWN
                | SWT.BORDER | SWT.READ_ONLY);
        categoryCbo.setLayoutData(gd);
        categoryCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                /*
                 * TODO - add code to update the information in the table
                 */
            }
        });

        gd = new GridData();
        gd.horizontalIndent = 20;
        Label extRetentionLbl = new Label(retentionComp, SWT.NONE);
        extRetentionLbl.setText("Extended Retention: ");
        extRetentionLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        final Spinner extRetentionSpnr = new Spinner(retentionComp, SWT.BORDER);
        extRetentionSpnr.setIncrement(1);
        extRetentionSpnr.setPageIncrement(5);
        extRetentionSpnr.setMaximum(Integer.MAX_VALUE);
        extRetentionSpnr.setMinimum(1);
        extRetentionSpnr.setLayoutData(gd);

        final Combo extRetentionCbo = new Combo(retentionComp, SWT.VERTICAL
                | SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        extRetentionCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleRetentionSelection(extRetentionCbo, extRetentionSpnr);
            }
        });
        extRetentionCbo.add("Hours");
        extRetentionCbo.add("Days");
        extRetentionCbo.select(0);
        extRetentionCbo.setData(extRetentionCbo.getItem(extRetentionCbo
                .getSelectionIndex()));
    }

    /**
     * Create the table control.
     */
    private void createTable() {
        tableComp = new ArchiveTableComp(shell, TableType.Case, this);
    }

    /**
     * Create the bottom action buttons.
     */
    private void createBottomActionButtons() {

        Composite actionControlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        actionControlComp.setLayout(gl);
        actionControlComp.setLayoutData(gd);

        Button calcSizeBtn = new Button(actionControlComp, SWT.PUSH);
        calcSizeBtn.setText(" Calculate Sizes ");
        calcSizeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                // TODO : add calculate size functionality
            }
        });

        Button saveBtn = new Button(actionControlComp, SWT.PUSH);
        saveBtn.setText(" Save... ");
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                // TODO : add save functionality
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(actionControlComp, SWT.PUSH);
        closeBtn.setText(" Close ");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Handle the retention selection for both minimum and extended retention.
     * 
     * @param comboBox
     *            Retention combo box.
     * @param spinner
     *            Retention spinner.
     */
    private void handleRetentionSelection(Combo comboBox, Spinner spinner) {
        // If the selection didn't change then just return.
        if (comboBox.getItem(comboBox.getSelectionIndex()).equals(
                (String) comboBox.getData())) {
            return;
        }

        int time = 0;

        if (comboBox.getItem(comboBox.getSelectionIndex()).equals("Hours")) {
            time = convertTime(true, spinner.getSelection());
        } else {
            time = convertTime(false, spinner.getSelection());
        }

        spinner.setSelection(time);
        comboBox.setData(comboBox.getItem(comboBox.getSelectionIndex()));
    }

    /**
     * Covert time from either hours to days or days to hours.
     * 
     * @param daysToHours
     *            Flag indicating how to convert the time.
     * @param time
     *            Time to be converted.
     * @return The converted time.
     */
    private int convertTime(boolean daysToHours, int time) {
        int convertedTime = 0;

        if (daysToHours) {
            convertedTime = time * 24;
        } else {
            convertedTime = time / 24;
        }

        return convertedTime;
    }

    /**
     * Add a separator line to the provided container.
     * 
     * @param container
     *            Composite.
     * @param orientation
     *            Vertical or horizontal orientation.
     */
    private void addSeparator(Composite container, int orientation) {
        // Separator label
        GridData gd;

        if (orientation == SWT.HORIZONTAL) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        } else {
            gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        }

        Label sepLbl = new Label(container, SWT.SEPARATOR | orientation);
        sepLbl.setLayoutData(gd);
    }

    /********************************************************
     * TEST METHODS - to be removed when functionality is implemented.
     * ******************************************************
     */
    private void populateComboBoxes() {
        archCfgCbo.add("Raw");
        archCfgCbo.add("Processed");
        archCfgCbo.select(0);

        categoryCbo.add("Radar");
        categoryCbo.add("Point");
        categoryCbo.add("Satellite");
        categoryCbo.select(0);
    }

    @Override
    public int getTotalSelectedItems() {
        // TODO Auto-generated method stub
        return 0;
    }
}

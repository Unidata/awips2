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

import java.util.Calendar;

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

import com.raytheon.uf.common.archive.config.ArchiveConfig;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.CategoryConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

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
 * May 31, 2013 #1965      bgonzale     Initial work for updating retention configurations.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ArchiveRetentionDlg extends AbstractArchiveDlg {

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchiveRetentionDlg.class);

    private Spinner minRetentionSpnr;

    private Spinner extRetentionSpnr;

    // TODO in the future, get this value from a user text box
    protected static final String ARCHIVE_DIR = "/archive_dir";

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
        addSeparator(shell, SWT.HORIZONTAL);
        createBottomActionButtons();
        selectionsUpdated();
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
        createComboControls(retentionComp);

        // composite for retention time selection
        Composite selectionComp = new Composite(retentionComp, SWT.NONE);
        selectionComp.setLayout(new GridLayout(3, true));
        selectionComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        gd = new GridData();
        gd.horizontalIndent = 20;
        Label minRetentionLbl = new Label(selectionComp, SWT.NONE);
        minRetentionLbl.setText("Minimum Retention: ");
        minRetentionLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        minRetentionSpnr = new Spinner(selectionComp, SWT.BORDER);
        minRetentionSpnr.setIncrement(1);
        minRetentionSpnr.setPageIncrement(5);
        minRetentionSpnr.setMaximum(Integer.MAX_VALUE);
        minRetentionSpnr.setMinimum(1);
        minRetentionSpnr.setLayoutData(gd);

        final Combo minRetentionCbo = new Combo(selectionComp, SWT.VERTICAL
                | SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        minRetentionCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int retentionHours = handleRetentionSelection(minRetentionCbo,
                        minRetentionSpnr);
                if (retentionHours != -1) {
                    ArchiveConfig archive = getSelectedArchive();
                    if (archive != null) {
                        archive.setRetentionHours(retentionHours);
                    }
                }
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

        gd = new GridData();
        gd.horizontalIndent = 20;
        Label extRetentionLbl = new Label(selectionComp, SWT.NONE);
        extRetentionLbl.setText("Extended Retention: ");
        extRetentionLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        extRetentionSpnr = new Spinner(selectionComp, SWT.BORDER);
        extRetentionSpnr.setIncrement(1);
        extRetentionSpnr.setPageIncrement(5);
        extRetentionSpnr.setMaximum(Integer.MAX_VALUE);
        extRetentionSpnr.setMinimum(1);
        extRetentionSpnr.setLayoutData(gd);

        final Combo extRetentionCbo = new Combo(selectionComp, SWT.VERTICAL
                | SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        extRetentionCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int retentionHours = handleRetentionSelection(extRetentionCbo,
                        extRetentionSpnr);
                if (retentionHours != -1) {
                    CategoryConfig category = getSelectedCategory();
                    if (category != null) {
                        category.setRetentionHours(retentionHours);
                    }
                }
            }
        });
        extRetentionCbo.add("Hours");
        extRetentionCbo.add("Days");
        extRetentionCbo.select(0);
        extRetentionCbo.setData(extRetentionCbo.getItem(extRetentionCbo
                .getSelectionIndex()));
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
                // With Roger's automated size calculation code, this doesn't
                // seem relevant unless it is for calculating compressed size
            }
        });

        Button saveBtn = new Button(actionControlComp, SWT.PUSH);
        saveBtn.setText(" Save... ");
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent selectionEvent) {
                ArchiveConfigManager manager = ArchiveConfigManager
                        .getInstance();
                // TODO
                // List<DisplayData> allSelected = getAllSelected();
                // manager.save();
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
     * @return hours entered if changed; -1 if not changed
     */
    private int handleRetentionSelection(Combo comboBox, Spinner spinner) {
        // If the selection didn't change then just return.
        if (comboBox.getItem(comboBox.getSelectionIndex()).equals(
                (String) comboBox.getData())) {
            return -1;
        }

        int time = 0;

        if (comboBox.getItem(comboBox.getSelectionIndex()).equals("Hours")) {
            time = convertTime(true, spinner.getSelection());
        } else {
            time = convertTime(false, spinner.getSelection());
        }

        spinner.setSelection(time);
        comboBox.setData(comboBox.getItem(comboBox.getSelectionIndex()));
        return time;
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

    @Override
    protected void setTotalSizeText(String sizeStringText) {
        // TODO Auto-generated method stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#getStart()
     */
    @Override
    protected Calendar getStart() {
        // display all elements so no start bound
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#getEnd()
     */
    @Override
    protected Calendar getEnd() {
        // display all elements so no end bound
        return null;
    }

    @Override
    protected void selectionsUpdated() {
        ArchiveConfig archive = getSelectedArchive();
        if (archive != null) {
            if (minRetentionSpnr != null) {
                minRetentionSpnr.setSelection(archive.getRetentionHours());
                CategoryConfig category = getSelectedCategory();
                if (category != null) {
                    extRetentionSpnr.setSelection(category.getRetentionHours());
                }
            }
        }
    }

}

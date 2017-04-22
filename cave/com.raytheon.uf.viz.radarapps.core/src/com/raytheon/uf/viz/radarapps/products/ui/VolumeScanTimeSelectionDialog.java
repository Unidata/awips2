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
package com.raytheon.uf.viz.radarapps.products.ui;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.viz.ui.dialogs.DialogUtil;

/**
 * Dialog used to a volume scan time. In AWIPS 1, this is almost the same as
 * D-2D's "Set Time" dialog. Thus, I am copying the equivalent AWIPS 2 dialog
 * code.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 21, 2016  5901     randerso  Fix dialog centering issue introduced in
 *                                  Eclipse 4
 *
 * </pre>
 *
 * @author ????
 */
class VolumeScanTimeSelectionDialog extends Dialog {
    /**
     * Set time dialog shell
     */
    private Shell shell;

    /**
     * Display component
     */
    private Display display;

    /**
     * Title of the dialog.
     */
    private String dialogTitle;

    /**
     * Use current time radio button.
     */
    private Button useCurrentTimeRdo;

    /**
     * Set time radio button.
     */
    private Button setTimeRdo;

    /**
     * Year spinner control.
     */
    private Spinner yearSpnr;

    /**
     * Month spinner control.
     */
    private Spinner monthSpnr;

    /**
     * Day spinner control.
     */
    private Spinner daySpnr;

    /**
     * Hour spinner control.
     */
    private Spinner hourSpnr;

    /**
     * Minute spinner control.
     */
    private Spinner minuteSpnr;

    /**
     * Second spinner control.
     */
    private Spinner secondSpnr;

    private Calendar time = new GregorianCalendar(TimeZone.getTimeZone("GMT"));

    private boolean okSelected;

    public VolumeScanTimeSelectionDialog(Shell parent) {
        super(parent, 0);
        dialogTitle = "Set Time";
    }

    /**
     * Constructor.
     *
     * @param parent
     *            Parent widget
     * @param title
     *            Window title.
     */
    public VolumeScanTimeSelectionDialog(Shell parent, String title) {
        super(parent, 0);

        if (title == null) {
            dialogTitle = "Set Time";
        } else {
            dialogTitle = title;
        }
    }

    public void setTime(Calendar time) {
        this.time.setTimeInMillis(time.getTimeInMillis());
        Calendar cal = this.time;
        this.yearSpnr.setSelection(cal.get(Calendar.YEAR));
        this.monthSpnr.setSelection(cal.get(Calendar.MONTH) + 1);
        this.daySpnr.setSelection(cal.get(Calendar.DAY_OF_MONTH));

        this.hourSpnr.setSelection(cal.get(Calendar.HOUR_OF_DAY));
        this.minuteSpnr.setSelection(cal.get(Calendar.MINUTE));
        this.secondSpnr.setSelection(cal.get(Calendar.SECOND));
    }

    public Calendar getTime() {
        GregorianCalendar gc = new GregorianCalendar(time.getTimeZone());
        gc.setTimeInMillis(time.getTimeInMillis());
        return gc;
    }

    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }

    /**
     * Open method used to display the text editor.
     *
     * @return True/False/null.
     */
    public boolean open() {

        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        shell.setText(dialogTitle);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
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

        return okSelected;
    }

    /**
     * Initialize all of the components on the display.
     */
    private void initializeComponents() {
        createTopRadioButtons();
        createTimeControlGroup();
        createBottomButtons();
        setTimeRdo.setSelection(true);
        enableSetTimeControls(true);
    }

    /**
     * Create the top radio buttons.
     */
    private void createTopRadioButtons() {
        useCurrentTimeRdo = new Button(shell, SWT.RADIO);
        useCurrentTimeRdo.setText("Use current real time");
        useCurrentTimeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                enableSetTimeControls(false);
            }
        });
        setTimeRdo = new Button(shell, SWT.RADIO);
        setTimeRdo.setText("Set time");
        setTimeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                enableSetTimeControls(true);
            }
        });

    }

    /**
     * Create the set time controls
     */
    private void createTimeControlGroup() {
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group setTimeGroup = new Group(shell, SWT.NONE);
        setTimeGroup.setText(" Set time ");
        GridLayout gridLayout = new GridLayout(6, false);
        gridLayout.verticalSpacing = 10;
        setTimeGroup.setLayout(gridLayout);
        setTimeGroup.setLayoutData(mainGridData);

        // Setup the label and spinner widths
        int labelWidth = 90;
        int spinnerWidth = 40;

        GridData gd = new GridData(70, SWT.DEFAULT);
        Label yearLbl = new Label(setTimeGroup, SWT.RIGHT);
        yearLbl.setText("Year:");
        yearLbl.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        yearSpnr = new Spinner(setTimeGroup, SWT.BORDER);
        yearSpnr.setValues(2007, 1900, 3000, 0, 1, 5);
        yearSpnr.setLayoutData(gd);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label monthLbl = new Label(setTimeGroup, SWT.RIGHT);
        monthLbl.setText("Month:");
        monthLbl.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        monthSpnr = new Spinner(setTimeGroup, SWT.BORDER);
        monthSpnr.setValues(1, 1, 12, 0, 1, 5);
        monthSpnr.setLayoutData(gd);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label dayLbl = new Label(setTimeGroup, SWT.RIGHT);
        dayLbl.setText("Day:");
        dayLbl.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        daySpnr = new Spinner(setTimeGroup, SWT.BORDER);
        daySpnr.setValues(1, 1, 31, 0, 1, 5);
        daySpnr.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        Label hoursLbl = new Label(setTimeGroup, SWT.RIGHT);
        hoursLbl.setText("Hours:");
        hoursLbl.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        hourSpnr = new Spinner(setTimeGroup, SWT.BORDER);
        hourSpnr.setValues(0, 0, 23, 0, 1, 5);
        hourSpnr.setLayoutData(gd);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label minutesLbl = new Label(setTimeGroup, SWT.RIGHT);
        minutesLbl.setText("Minutes:");
        minutesLbl.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        minuteSpnr = new Spinner(setTimeGroup, SWT.BORDER);
        minuteSpnr.setValues(0, 0, 59, 0, 1, 5);
        minuteSpnr.setLayoutData(gd);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label secondLbl = new Label(setTimeGroup, SWT.RIGHT);
        secondLbl.setText("Second:");
        secondLbl.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        secondSpnr = new Spinner(setTimeGroup, SWT.BORDER);
        secondSpnr.setValues(0, 0, 59, 0, 1, 5);
        secondSpnr.setLayoutData(gd);

        setTime(time);
    }

    /**
     * Create the OK and Cancel buttons and center them on the screen.
     */
    private void createBottomButtons() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button okBtn = new Button(centeredComp, SWT.NONE);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                okSelected = true;
                commitTime();
                shell.dispose();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button cancelBtn = new Button(centeredComp, SWT.NONE);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    private void commitTime() {
        if (useCurrentTimeRdo.getSelection()) {
            // Should this be using D-2D SimulatedTime ?
            time.setTimeInMillis(System.currentTimeMillis());
        } else if (setTimeRdo.getSelection()) {
            time.clear();
            time.set(this.yearSpnr.getSelection(),
                    this.monthSpnr.getSelection() - 1, this.daySpnr
                    .getSelection(), this.hourSpnr.getSelection(),
                    this.minuteSpnr.getSelection(), this.secondSpnr
                    .getSelection());
        }

    }

    /**
     * Enable or disable the set time controls.
     *
     * @param enabled
     *            Flag indicating if the controls should be enabled or disabled.
     */
    private void enableSetTimeControls(boolean enabled) {
        yearSpnr.setEnabled(enabled);
        monthSpnr.setEnabled(enabled);
        daySpnr.setEnabled(enabled);
        hourSpnr.setEnabled(enabled);
        minuteSpnr.setEnabled(enabled);
        secondSpnr.setEnabled(enabled);
    }

}

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
package com.raytheon.viz.ui.dialogs;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

/**
 * Awips Calendar Date Selection Dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 9, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class AwipsCalendar extends CaveSWTDialogBase {

    /** The date selection calendar class */
    private DateTime calendar;

    /** The hour selection spinner class */
    private Spinner hourSpinner;

    /** The showHour flag */
    private boolean showHour = true;

    /** The showHour flag */
    private Date date = null;

    /**
     * Constructor.
     *
     * @param parentShell
     * @param showHour
     */
    public AwipsCalendar(Shell parentShell, boolean showHour) {
        super(parentShell, SWT.DIALOG_TRIM);
        setText("Calendar");
        this.showHour = showHour;
    }

    /**
     * Constructor.
     *
     * @param parentShell
     */
    public AwipsCalendar(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM);
        setText("Calendar");


    }

    /**
     * Constructor.
     *
     * @param parentShell
     * @param d Date to preset the calendar widget
     * @param showHour true to display the hour spinner
     */
    public AwipsCalendar(Shell parentShell, Date d, boolean showHour) {
        super(parentShell, SWT.DIALOG_TRIM);
        setText("Calendar");
        this.date = d;
        this.showHour = showHour;
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

        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        if (date != null) {
            cal.setTime(date);
        }


        if (showHour) {
            createTopWidgets(cal.get(Calendar.HOUR_OF_DAY));
        }

        calendar = new DateTime(shell, SWT.CALENDAR | SWT.BORDER_SOLID);
        calendar.setDate(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH));

        createButtons();
    }

    /**
     * Create the top widgets
     */
    private void createTopWidgets(int hour) {
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        Composite top = new Composite(shell, SWT.NONE);
        top.setLayout(gl);
        top.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        Label lbl = new Label(top, SWT.NONE);
        lbl.setLayoutData(gd);
        lbl.setText("Select Hour (Z) and Date: ");

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);

        hourSpinner = new Spinner(top, SWT.BORDER | SWT.RIGHT);
        hourSpinner.setLayoutData(gd);
        hourSpinner.setMinimum(0);
        hourSpinner.setMaximum(23);
        hourSpinner.setSelection(hour);
        hourSpinner.setIncrement(1);
        hourSpinner.setPageIncrement(1);
    }

    /**
     * Create the buttons
     */
    private void createButtons() {
        int buttonWidth = 75;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);
        Composite btnComp = new Composite(shell, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        Button okBtn = new Button(btnComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleOk();
                shell.dispose();
            }
        });

        Button cancelBtn = new Button(btnComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Event handler action for OK button
     */
    private void handleOk() {
        Calendar selectedDate = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        selectedDate.set(Calendar.YEAR, calendar.getYear());
        selectedDate.set(Calendar.MONTH, calendar.getMonth());
        selectedDate.set(Calendar.DAY_OF_MONTH, calendar.getDay());
        if (showHour) {
            selectedDate.set(Calendar.HOUR_OF_DAY,
                    Integer.parseInt(hourSpinner.getText()));
        } else {
            selectedDate.set(Calendar.HOUR_OF_DAY, 0);
        }
        selectedDate.set(Calendar.MINUTE, 0);
        selectedDate.set(Calendar.SECOND, 0);
        selectedDate.set(Calendar.MILLISECOND, 0);
        this.setReturnValue(selectedDate);
    }

    /**
     * Main
     *
     * @param args
     */
    public static void main(String[] args) {
        AwipsCalendar ac = new AwipsCalendar(new Shell());
        ac.open();
    }

}

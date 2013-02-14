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

import java.text.SimpleDateFormat;
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

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.ui.widgets.TimeEntry;

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

    /** The date selection calendar widget */
    private DateTime calendar;

    /** the time selection widget */
    private TimeEntry timeEntry;

    private int timeFieldCount;

    private Date date = null;

    private TimeZone timeZone;

    /**
     * Constructor.
     * 
     * @param parentShell
     */
    public AwipsCalendar(Shell parentShell) {
        this(parentShell, null, 0);
    }

    /**
     * Constructor.
     * 
     * @param parentShell
     * @param timeFieldCount
     *            number of time fields to display
     * 
     *            <pre>
     *   0 - do not display time field 
     *   1 - display hours 
     *   2 - display hours and minutes 
     *   3 - display hours, minutes, and seconds
     * </pre>
     */
    public AwipsCalendar(Shell parentShell, int timeFieldCount) {
        this(parentShell, null, timeFieldCount);
    }

    /**
     * Constructor.
     * 
     * @param parentShell
     * @param d
     *            Date to preset the calendar widget
     * @param timeFieldCount
     *            number of time fields to display
     * 
     *            <pre>
     *   0 - do not display time field 
     *   1 - display hours 
     *   2 - display hours and minutes
     *   3 - display hours, minutes, and seconds
     * </pre>
     */
    public AwipsCalendar(Shell parentShell, Date d, int timeFieldCount) {
        super(parentShell, SWT.DIALOG_TRIM);
        setText("Calendar");
        this.date = d;
        if (timeFieldCount < 0 || timeFieldCount > 3) {
            throw new IllegalArgumentException(
                    "timeFieldCount must be 0, 1, 2, or 3");
        }
        this.timeFieldCount = timeFieldCount;
        this.timeZone = TimeZone.getTimeZone("GMT");
    }

    /**
     * The time zone used for displaying the time field
     * 
     * @param timeZone
     */
    public void setTimeZone(TimeZone timeZone) {
        this.timeZone = timeZone;
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

        Calendar cal = TimeUtil.newCalendar(this.timeZone);
        if (date != null) {
            cal.setTime(date);
        }

        GridLayout layout = new GridLayout(2, false);
        shell.setLayout(layout);

        if (timeFieldCount > 0) {
            Label lbl = new Label(shell, SWT.NONE);
            GridData layoutData = new GridData(SWT.LEFT, SWT.CENTER, true,
                    false);
            lbl.setLayoutData(layoutData);
            TimeZone tz = cal.getTimeZone();
            String tzId = "Z";
            if (tz.getRawOffset() != 0) {
                SimpleDateFormat sdf = new SimpleDateFormat("z");
                sdf.setTimeZone(tz);
                // date below is not used
                // we just want to get the tz abbreviation
                tzId = sdf.format(new Date());
            }

            if (timeFieldCount == 1) {
                lbl.setText("Select Hour (" + tzId + ") and Date: ");
            } else {
                lbl.setText("Select Time (" + tzId + ") and Date: ");
            }

            timeEntry = new TimeEntry(shell, timeFieldCount);
            layoutData = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
            timeEntry.setLayoutData(layoutData);
            timeEntry.setTime(cal.get(Calendar.HOUR_OF_DAY),
                    cal.get(Calendar.MINUTE), cal.get(Calendar.SECOND));
        }

        calendar = new DateTime(shell, SWT.CALENDAR | SWT.BORDER);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false,
                false);
        layoutData.horizontalSpan = 2;
        calendar.setLayoutData(layoutData);
        calendar.setDate(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH),
                cal.get(Calendar.DAY_OF_MONTH));

        createButtons();
    }

    /**
     * Create the buttons
     */
    private void createButtons() {
        int buttonWidth = 75;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
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
        Calendar selectedDate = Calendar.getInstance(this.timeZone);
        selectedDate.set(Calendar.YEAR, calendar.getYear());
        selectedDate.set(Calendar.MONTH, calendar.getMonth());
        selectedDate.set(Calendar.DAY_OF_MONTH, calendar.getDay());
        if (timeFieldCount > 0) {
            selectedDate.set(Calendar.HOUR_OF_DAY, timeEntry.getHours());
            selectedDate.set(Calendar.MINUTE, timeEntry.getMinutes());
            selectedDate.set(Calendar.SECOND, timeEntry.getSeconds());
        } else {
            selectedDate.set(Calendar.HOUR_OF_DAY, 0);
            selectedDate.set(Calendar.MINUTE, 0);
            selectedDate.set(Calendar.SECOND, 0);
        }
        selectedDate.set(Calendar.MILLISECOND, 0);

        this.setReturnValue(selectedDate.getTime());
    }

    /**
     * Main
     * 
     * @param args
     */
    public static void main(String[] args) {
        AwipsCalendar ac = new AwipsCalendar(new Shell(), 1);
        ac.setTimeZone(TimeZone.getDefault());
        Date date = (Date) ac.open();
        if (date == null) {
            System.out.println("null");
        } else {
            SimpleDateFormat sdf = new SimpleDateFormat(
                    "yyyy-MM-dd HH:mm:ss'Z'");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            System.out.println(sdf.format(date));
        }
    }
}

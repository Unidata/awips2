package com.raytheon.viz.warngen.gui;

import java.util.Calendar;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
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

import com.raytheon.uf.common.time.SimulatedTime;

/**
 * 
 * Displays a Time Period dialog when the "Change..." button is selected on the
 * Warngen Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2011            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class ValidPeriodDialog extends Dialog {
    private Shell shell;

    private Font font;

    private Button okBtn;

    private Button cancelBtn;

    private Calendar startTime;

    private Calendar endTime;

    private Spinner startYearSpinner;

    private Spinner startMonthSpinner;

    private Spinner startDaySpinner;

    private Spinner startHourSpinner;

    private Spinner startMinuteSpinner;

    private Spinner startSecondSpinner;

    private Spinner endYearSpinner;

    private Spinner endMonthSpinner;

    private Spinner endDaySpinner;

    private Spinner endHourSpinner;

    private Spinner endMinuteSpinner;

    private Spinner endSecondSpinner;

    private int duration = -1;

    private Timer timer;

    private TimerTask updateTimeTask;

    public ValidPeriodDialog(Shell parentShell, Calendar startTime,
            Calendar endTime) {
        super(parentShell);
        this.startTime = startTime;
        this.endTime = endTime;
    }

    public int open() {
        Shell parent = getParent();
        Display display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
        shell.setText("Valid Period");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        font.dispose();

        return duration;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        createStartTimeComp();
        createEndTimeComp();
        createButtonComp();
        startTimeTimer();
    }

    private void createStartTimeComp() {
        // create date area
        Group dateComp = new Group(shell, SWT.SHADOW_ETCHED_IN);
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dateComp.setLayoutData(data);
        GridLayout gl = new GridLayout(6, false);
        dateComp.setLayout(gl);
        dateComp.setText("Start time");

        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label yearLabel = new Label(dateComp, SWT.NONE);
        yearLabel.setLayoutData(gd);
        yearLabel.setText("Year:");

        startYearSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        startYearSpinner.setLayoutData(data);
        startYearSpinner.setMinimum(2011);
        startYearSpinner.setMaximum(3000);
        startYearSpinner.setEnabled(false);
        startYearSpinner.setSelection(startTime.get(Calendar.YEAR));

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label monthLabel = new Label(dateComp, SWT.NONE);
        monthLabel.setLayoutData(gd);
        monthLabel.setText("Month:");

        startMonthSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        startMonthSpinner.setLayoutData(data);
        startMonthSpinner.setEnabled(false);
        startMonthSpinner.setSelection(startTime.get(Calendar.MONTH) + 1);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label dateLabel = new Label(dateComp, SWT.NONE);
        dateLabel.setLayoutData(gd);
        dateLabel.setText("Date:");

        startDaySpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        startDaySpinner.setLayoutData(data);
        startDaySpinner.setEnabled(false);
        startDaySpinner.setSelection(startTime.get(Calendar.DAY_OF_MONTH));

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label hrLabel = new Label(dateComp, SWT.NONE);
        hrLabel.setLayoutData(gd);
        hrLabel.setText("Hour:");

        startHourSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        startHourSpinner.setLayoutData(data);
        startHourSpinner.setEnabled(false);
        startHourSpinner.setSelection(startTime.get(Calendar.HOUR_OF_DAY));

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label minuteLabel = new Label(dateComp, SWT.NONE);
        minuteLabel.setLayoutData(gd);
        minuteLabel.setText("Minute:");

        startMinuteSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        startMinuteSpinner.setLayoutData(data);
        startMinuteSpinner.setEnabled(false);
        startMinuteSpinner.setSelection(startTime.get(Calendar.MINUTE));

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label secondLabel = new Label(dateComp, SWT.NONE);
        secondLabel.setLayoutData(gd);
        secondLabel.setText("Second:");

        startSecondSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        startSecondSpinner.setLayoutData(data);
        startSecondSpinner.setEnabled(false);
        startSecondSpinner.setSelection(startTime.get(Calendar.SECOND));
    }

    private void createEndTimeComp() {
        // create date area
        Group dateComp = new Group(shell, SWT.SHADOW_ETCHED_IN);
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dateComp.setLayoutData(data);
        GridLayout gl = new GridLayout(6, false);
        dateComp.setLayout(gl);
        dateComp.setText("End time");

        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label yearLabel = new Label(dateComp, SWT.NONE);
        yearLabel.setLayoutData(gd);
        yearLabel.setText("Year:");

        endYearSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        endYearSpinner.setLayoutData(data);
        endYearSpinner.setMinimum(2011);
        endYearSpinner.setMaximum(3000);
        endYearSpinner.setSelection(endTime.get(Calendar.YEAR));
        endYearSpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int year = endYearSpinner.getSelection();
                endTime.set(Calendar.YEAR, year);
                validateEndTime();
            }

        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label monthLabel = new Label(dateComp, SWT.NONE);
        monthLabel.setLayoutData(gd);
        monthLabel.setText("Month:");

        endMonthSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        endMonthSpinner.setLayoutData(data);
        endMonthSpinner.setMinimum(1);
        endMonthSpinner.setMaximum(12);
        endMonthSpinner.setSelection(endTime.get(Calendar.MONTH) + 1);
        endMonthSpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int month = endMonthSpinner.getSelection();
                endTime.set(Calendar.MONTH, month - 1);
                validateEndTime();
            }

        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label dateLabel = new Label(dateComp, SWT.NONE);
        dateLabel.setLayoutData(gd);
        dateLabel.setText("Date:");

        endDaySpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        endDaySpinner.setLayoutData(data);
        endDaySpinner.setMinimum(1);
        endDaySpinner.setMaximum(32);
        endDaySpinner.setSelection(endTime.get(Calendar.DAY_OF_MONTH));
        endDaySpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int day = endDaySpinner.getSelection();
                endTime.set(Calendar.DAY_OF_MONTH, day);
                validateEndTime();
            }

        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label hrLabel = new Label(dateComp, SWT.NONE);
        hrLabel.setLayoutData(gd);
        hrLabel.setText("Hour:");

        endHourSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        endHourSpinner.setLayoutData(data);
        endHourSpinner.setMinimum(0); // this works in eclipse 3.4+
        endHourSpinner.setMaximum(23);
        endHourSpinner.setSelection(endTime.get(Calendar.HOUR_OF_DAY));
        endHourSpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int hour = endHourSpinner.getSelection();
                endTime.set(Calendar.HOUR_OF_DAY, hour);
                validateEndTime();
            }

        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label minuteLabel = new Label(dateComp, SWT.NONE);
        minuteLabel.setLayoutData(gd);
        minuteLabel.setText("Minute:");

        endMinuteSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        endMinuteSpinner.setLayoutData(data);
        endMinuteSpinner.setMinimum(0); // this works in eclipse 3.4+
        endMinuteSpinner.setMaximum(59);
        endMinuteSpinner.setSelection(endTime.get(Calendar.MINUTE));
        endMinuteSpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int minute = endMinuteSpinner.getSelection();
                endTime.set(Calendar.MINUTE, minute);
                validateEndTime();
            }

        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label secondLabel = new Label(dateComp, SWT.NONE);
        secondLabel.setLayoutData(gd);
        secondLabel.setText("Second:");

        endSecondSpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 40;
        endSecondSpinner.setLayoutData(data);
        endSecondSpinner.setMinimum(0); // this works in eclipse 3.4+
        endSecondSpinner.setMaximum(59);
        endSecondSpinner.setSelection(endTime.get(Calendar.SECOND));
        endSecondSpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int second = endSecondSpinner.getSelection();
                endTime.set(Calendar.SECOND, second);
                validateEndTime();
            }

        });
    }

    private void createButtonComp() {
        // Create a container to hold the button.
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite okBtnComp = new Composite(shell, SWT.NONE);
        GridLayout okBtnCompLayout = new GridLayout(2, true);
        okBtnComp.setLayout(okBtnCompLayout);
        okBtnComp.setLayoutData(gd);

        GridData bd = new GridData(110, 30);
        okBtn = new Button(okBtnComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(bd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                calculateDuration();
                shell.dispose();
            }
        });

        bd = new GridData(110, 30);
        cancelBtn = new Button(okBtnComp, SWT.PUSH);
        cancelBtn.setText("Close");
        cancelBtn.setLayoutData(bd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                duration = -1;
                shell.dispose();
            }
        });
    }

    private void calculateDuration() {
        duration = (int) (endTime.getTimeInMillis() - startTime
                .getTimeInMillis()) / (60 * 1000);
    }

    private void validateEndTime() {
        if (endTime.before(startTime)) {
            okBtn.setEnabled(false);
        } else {
            okBtn.setEnabled(true);
        }
    }

    public boolean isDisposed() {
        if (shell != null) {
            return shell.isDisposed();
        }
        return true;
    }

    private void startTimeTimer() {
        timer = new Timer();

        updateTimeTask = new TimerTask() {
            @Override
            public void run() {

                shell.getDisplay().syncExec(new Runnable() {
                    public void run() {
                        startTime.setTime(SimulatedTime.getSystemTime()
                                .getTime());
                        startYearSpinner.setSelection(startTime
                                .get(Calendar.YEAR));
                        startMonthSpinner.setSelection(startTime
                                .get(Calendar.MONTH) + 1);
                        startDaySpinner.setSelection(startTime
                                .get(Calendar.DAY_OF_MONTH));
                        startHourSpinner.setSelection(startTime
                                .get(Calendar.HOUR_OF_DAY));
                        startMinuteSpinner.setSelection(startTime
                                .get(Calendar.MINUTE));
                        startSecondSpinner.setSelection(startTime
                                .get(Calendar.SECOND));
                        validateEndTime();
                    }
                });
            }
        };

        timer.schedule(updateTimeTask, 60000, 60000);
    }
}

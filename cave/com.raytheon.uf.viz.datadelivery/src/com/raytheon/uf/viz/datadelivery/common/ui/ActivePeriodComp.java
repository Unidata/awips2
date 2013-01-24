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
package com.raytheon.uf.viz.datadelivery.common.ui;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.datadelivery.subscription.AwipsCalendar;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * This is the subscription active period composite. This class is intended to
 * be extended so common classes can be created and shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012   702      jpiatt      Initial creation.
 * Aug 29, 2012   223      mpduff      Removed date order checks.
 * Sep 06, 2012  1137      jpiatt      Corrected validation.
 * Sep 06, 2012   687      mpduff      Make dates relative when validating.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class ActivePeriodComp extends Composite {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ActivePeriodComp.class);

    /** TableItem object. */
    private final Composite parentComp;

    /** The always active option radio button */
    public Button alwaysActiveChk;

    /** The active start date text field */
    private Text activeStartText;

    /** The active start date button */
    private Button activeStartDateBtn;

    /** The active end date button */
    private Button activeEndDateBtn;

    /** The active end date text field */
    private Text activeEndText;

    /** The active starting date */
    private Date activeStartDate;

    /** The active ending date */
    private Date activeEndDate;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public ActivePeriodComp(Composite parent) {
        super(parent, SWT.NONE);
        this.parentComp = parent;
        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        /*
         * Setup the layout for the composite
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createActiveGroup();

    }

    /**
     * Create the Active Period Group
     */
    private void createActiveGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        Group activeGrp = new Group(this, SWT.NONE);
        activeGrp.setLayout(gl);
        activeGrp.setLayoutData(gd);
        activeGrp.setText("  Subscription Active Period  ");

        alwaysActiveChk = new Button(activeGrp, SWT.CHECK);
        alwaysActiveChk.setText("Always Active");
        alwaysActiveChk.setSelection(true);
        alwaysActiveChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                boolean alwaysActive = !alwaysActiveChk.getSelection();
                activeStartText.setEnabled(alwaysActive);
                activeStartDateBtn.setEnabled(alwaysActive);
                activeEndText.setEnabled(alwaysActive);
                activeEndDateBtn.setEnabled(alwaysActive);

            }
        });

        Composite windowComp = new Composite(activeGrp, SWT.NONE);
        windowComp.setLayout(new GridLayout(3, false));
        windowComp.setLayoutData(gd);

        int textWidth = 100;
        GridData textData = new GridData(textWidth, SWT.DEFAULT);
        int buttonWidth = 85;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);

        Label winStartLbl = new Label(windowComp, SWT.LEFT);
        winStartLbl.setLayoutData(gd);
        winStartLbl.setText("Active Period Start: ");

        activeStartText = new Text(windowComp, SWT.BORDER);
        activeStartText.setLayoutData(textData);
        activeStartText.setEnabled(false);
        activeStartText.addFocusListener(new FocusAdapter() {

            @Override
            public void focusLost(FocusEvent e) {
                if (!DataDeliveryGUIUtils.validateDate(false,
                        activeStartText.getText())) {
                    DataDeliveryUtils.showMessage(getShell(), SWT.ERROR,
                            "Invalid Date/Time",
                            "Invalid End Date/Time entered.\n\n"
                                    + "Please use the Select Date button\n"
                                    + "to select the date/time.");
                }
            }
        });

        activeStartDateBtn = new Button(windowComp, SWT.PUSH);
        activeStartDateBtn.setText("Select Date");
        activeStartDateBtn.setLayoutData(btnData);
        activeStartDateBtn.setEnabled(false);
        activeStartDateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getActiveStartDate();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);

        Label relEndLbl = new Label(windowComp, SWT.LEFT);
        relEndLbl.setLayoutData(gd);
        relEndLbl.setText("Active Period End: ");

        textData = new GridData(textWidth, SWT.DEFAULT);
        activeEndText = new Text(windowComp, SWT.BORDER);
        activeEndText.setLayoutData(textData);
        activeEndText.setEnabled(false);
        activeEndText.addFocusListener(new FocusAdapter() {

            @Override
            public void focusLost(FocusEvent e) {
                if (!DataDeliveryGUIUtils.validateDate(false,
                        activeEndText.getText())) {
                    DataDeliveryUtils.showMessage(getShell(), SWT.ERROR,
                            "Invalid Date/Time",
                            "Invalid End Date/Time entered.\n\n"
                                    + "Please use the Select Date button\n"
                                    + "to select the date/time.");
                }
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        activeEndDateBtn = new Button(windowComp, SWT.PUSH);
        activeEndDateBtn.setText("Select Date");
        activeEndDateBtn.setLayoutData(btnData);
        activeEndDateBtn.setEnabled(false);
        activeEndDateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getActiveEndDate();
            }
        });
    }

    /**
     * Get the active start date selected by the user.
     */
    private void getActiveStartDate() {
        Date d = getDate(activeStartDate, false);
        if (d != null) {
            activeStartDate = d;
            try {
                activeStartText.setText(DataDeliveryGUIUtils.getActiveFormat()
                        .format(activeStartDate));
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Invalid date format - format needs to be MM/dd", e);
            }
        }
    }

    /**
     * Get the active end date selected by the user
     */
    private void getActiveEndDate() {
        Date d = getDate(activeEndDate, false);
        if (d != null) {
            activeEndDate = d;
            try {
                activeEndText.setText(DataDeliveryGUIUtils.getActiveFormat()
                        .format(activeEndDate));
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Invalid date format - format needs to be MM/dd", e);
            }
        }
    }

    /**
     * Get the date from the Calendar widget
     * 
     * @return The selected Date
     */
    private Date getDate(Date date, boolean showHour) {

        if (date == null) {
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            date = cal.getTime();
        }

        AwipsCalendar ac = new AwipsCalendar(getShell(), date, showHour);
        Object obj = ac.open();

        if ((obj != null) && (obj instanceof Calendar)) {
            return ((Calendar) obj).getTime();
        }

        return null;
    }

    /**
     * Enable or disable text boxes.
     * 
     * @param flag
     */
    public void resetTextBoxes(boolean flag) {
        this.activeStartText.setEnabled(flag);
        this.activeEndText.setEnabled(flag);
    }

    /**
     * Get the active period start text.
     * 
     * @return active period start
     */
    public String getActiveStartText() {

        return activeStartText.getText().trim();
    }

    /**
     * Get the active period end text.
     * 
     * @return active period end
     */
    public String getActiveEndText() {
        return activeEndText.getText().trim();
    }

    /**
     * Set the start btn.
     * 
     * @param flag
     */
    public void setStartBtnEnabled(boolean flag) {
        activeStartDateBtn.setEnabled(flag); 
    }

    /**
     * Set the start btn.
     * 
     * @param flag
     */
    public void setEndBtnEnabled(boolean flag) {
        activeEndDateBtn.setEnabled(flag); 
    }

    /**
     * Set the start date.
     * 
     * @param activeStartDate
     */
    public void setStartDate(Date activeStartDate) {
        activeStartText.setText(DataDeliveryGUIUtils.getActiveFormat().format(
                activeStartDate));
        activeStartText.setEnabled(true);
        activeStartDateBtn.setEnabled(true);
        this.activeStartDate = activeStartDate;
    }

    /**
     * Set the end date.
     * 
     * @param activeEndDate
     */
    public void setEndDate(Date activeEndDate) {
        activeEndText.setText(DataDeliveryGUIUtils.getActiveFormat().format(
                activeEndDate));
        activeEndText.setEnabled(true);
        activeEndDateBtn.setEnabled(true);
        this.activeEndDate = activeEndDate;
    }

    /**
     * Check the Always Active check box
     * 
     * @return true if area has been selected
     */
    public boolean isAlwaysChk() {
        return alwaysActiveChk.getSelection();
    }

    /**
     * Set the always active check box.
     * 
     * @param flag
     */
    public void setAlwaysActive(boolean flag) {
        alwaysActiveChk.setSelection(flag);
    }

    /**
     * Check if dates are valid.
     * 
     * @return true if dates are valid
     */
    public boolean isValidChk() {
        boolean activeDatesValid = false;

        if (!isAlwaysChk()) {
            boolean validateAct = DataDeliveryGUIUtils.validateDate(false,
                    getActiveStartText());
            if (validateAct) {
                validateAct = DataDeliveryGUIUtils.validateDate(false,
                        getActiveEndText());
                if (validateAct) {
                    activeDatesValid = true;
                }
            }
        } else {
            activeDatesValid = true;
        }

        // Display error message
        if (!activeDatesValid) {
            DataDeliveryUtils.showMessage(parentComp.getShell(), SWT.ERROR,
                    "Invalid Date",
                    "Invalid Subscription Active Period values entered.\n\n"
                            + "Please use the Select Date button\n"
                            + "to select the date.");
        }

        return activeDatesValid;
    }
}
